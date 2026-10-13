box::use(
  commons[semantic_layer],
  coro[async_generator, await, yield],
  DBI[dbConnect, dbDisconnect, dbWriteTable],
  duckdb[duckdb],
  htmltools[div, renderTags],
  ellmer[AssistantTurn, ContentText, ContentToolRequest, ContentToolResult, stream_controller, UserTurn],
  promises[promise_resolve],
  purrr[keep, map_chr],
  S7[S7_inherits],
  stringr[fixed, str_c, str_replace_all],
  testthat[expect_equal, expect_error, expect_false, expect_message, expect_s3_class, expect_true, test_that],
  withr[defer, local_envvar, local_options],
)

box::use(
  app / logic / agent[agent_instructions, chart_stream, ground_agent, grounded_stream, has_lookup, new_agent, reasoning_args, wdi_source],
  app / logic / config[world_bank],
  app / logic / wdi[split_panel],
  app / measures / analysis[country_profile],
  app / measures / charts[indicator_chart],
  app / measures / `world-bank`[compound_growth, indicator_ranking, indicator_series, latest_values],
)

panel <- split_panel(fake_wdi_raw())
panel$downloaded <- as.Date("2026-10-08")

# The measures file as commons reads it (by path)
measures_file <- file.path("..", "..", "app", "measures", "world-bank.R")

# The `wdi` connection commons passes to the measures: the panel's tables in duckdb
local_wdi <- function(env = parent.frame()) {
  con <- dbConnect(duckdb())
  dbWriteTable(con, "countries", panel$countries)
  dbWriteTable(con, "aggregates", panel$aggregates)
  defer(dbDisconnect(con, shutdown = TRUE), envir = env)
  con
}

test_that("commons reads the four measures from the measures file", {
  expect_message(print(semantic_layer(measures_file)), "4 measures")
})

test_that("commons reads every measure in the measures directory", {
  expect_message(print(semantic_layer(dirname(measures_file))), "6 measures")
})

test_that("indicator_chart() shows the user a chart and gives the model a text summary of it", {
  wdi <- local_wdi()
  chart <- indicator_chart(wdi, "inflation_pct", "CHL; brazil", start_year = 2021)

  expect_true(S7_inherits(chart, ContentToolResult))
  # Words only for the model: the free models can't read images
  expect_true(is.character(chart@value))
  expect_match(chart@value, "Brazil: 2021-2022, first 8, last 9, min 8 (2021), max 9 (2022)", fixed = TRUE)
  expect_match(chart@value, "Chile: 2021-2021, first 7", fixed = TRUE)
  # The chart itself, for chart_stream() to put in the reply, with percentages as 0.00%
  expect_match(as.character(chart@extra$chart), "echarts4r", fixed = TRUE)
  expect_match(as.character(chart@extra$chart), "toFixed(2) + '%'", fixed = TRUE)
  # shinychat must find the chart's JavaScript, or the reply shows a blank space instead
  dependencies <- map_chr(renderTags(chart@extra$chart)$dependencies, "name")
  expect_true(all(c("htmlwidgets", "echarts4r") %in% dependencies))
  expect_s3_class(chart@extra$chart, "shiny.tag")
  expect_equal(chart@extra$data$economy, c("Brazil", "Chile", "Brazil"))

  expect_error(indicator_chart(wdi, "inflation_pct", "Atlantis"), "No World Bank economy named Atlantis")
  expect_error(indicator_chart(wdi, "happiness", "Brazil"), "Unknown indicator 'happiness'")
  expect_error(indicator_chart(wdi, "inflation_pct", "Brazil", end_year = 1990), "No inflation_pct data")
})

test_that("a chart called through commons reaches the model as text, with the chart kept for the user", {
  local_envvar(OPENROUTER_API_KEY = "test-key")
  local_options(commons.allow_unsafe_fallback = TRUE)
  agent <- new_agent("google/gemma:free", "Gemma (free)", wdi_source(panel))

  result <- agent$get_tools()$call_measure(
    name = "indicator_chart",
    arguments = list(indicator = "gdp_usd", economies = "World")
  )

  expect_null(result@error)
  expect_true(is.character(result@value))
  expect_match(result@value, "World: 2020-2022, first 1,000", fixed = TRUE)
  expect_match(as.character(result@extra$chart), "echarts4r", fixed = TRUE)
})

test_that("chart_stream() puts a chart measure's chart in the reply, after its tool result", {
  with_chart <- ContentToolResult(value = "Line chart.", extra = list(chart = div("chart")))
  plain <- ContentToolResult(value = "A table.")
  request <- ContentToolRequest("1", "call_measure", list())
  reply <- async_generator(function() {
    for (chunk in list("Here: ", request, with_chart, plain, "Inflation fell.")) {
      await(promise_resolve(NULL))
      yield(chunk)
    }
  })

  expect_equal(collect_stream(chart_stream(reply())), "Here: [html]Inflation fell.")
})

test_that("country_profile() gives each indicator's latest value and rank among countries", {
  wdi <- local_wdi()
  profile <- country_profile(wdi, "bra")
  # Indicators without data are left out; Brazil's latest GDP is 2021 (2022 is missing)
  expect_equal(profile$indicator, c("gdp_usd", "inflation_pct"))
  expect_equal(profile$year, c(2021, 2022))
  expect_equal(profile$value, c(121, 9))
  expect_equal(profile$rank_highest, c(2, 1))
  # Chile has no 2022 inflation, so only two countries are ranked
  expect_equal(profile$countries_ranked, c(3, 2))

  in_2020 <- country_profile(wdi, "Brazil", year = 2020)
  expect_equal(in_2020$value, c(100, 3))
  expect_error(country_profile(wdi, "World"), "No World Bank country named World")
  expect_error(country_profile(wdi, "Brazil", year = 1990), "No data for Brazil in 1990")
})

test_that("latest_values() gives each economy's most recent value, by name or ISO3 code in any case", {
  latest <- latest_values(local_wdi(), "gdp_usd", "brazil; KOR ;World;")

  expect_equal(latest$economy, c("Brazil", "Korea, Rep.", "World"))
  expect_equal(latest$year, c(2021L, 2022L, 2022L))
  expect_equal(latest$gdp_usd, c(121, 220, 1210))
})

test_that("unknown economies, indicators and missing data are errors the model can act on", {
  wdi <- local_wdi()
  expect_error(latest_values(wdi, "gdp_usd", "Brazil; South Korea"), "South Korea.*ISO3")
  expect_error(latest_values(wdi, "happiness", "Brazil"), "Unknown indicator 'happiness'")
  expect_error(latest_values(wdi, "gini", "Brazil"), "No gini data for Brazil")
})

test_that("indicator_series() returns non-missing values in the year range", {
  series <- indicator_series(local_wdi(), "inflation_pct", "CHL; BRA", start_year = 2021)

  expect_equal(series$economy, c("Brazil", "Brazil", "Chile"))
  expect_equal(series$year, c(2021L, 2022L, 2021L))
})

test_that("indicator_ranking() ranks countries only, in the latest well-covered year", {
  wdi <- local_wdi()
  ranked <- indicator_ranking(wdi, "gdp_usd")
  # 2022 is missing for Brazil, but still reported by 2 of 3 countries
  expect_equal(ranked$year, c(2022L, 2022L))
  expect_equal(ranked$country, c("Korea, Rep.", "Chile"))
  expect_equal(ranked$rank, 1:2)

  lowest <- indicator_ranking(wdi, "inflation_pct", year = 2021, lowest_first = TRUE, top_n = 1)
  expect_equal(lowest$country, "Korea, Rep.")

  latin <- indicator_ranking(wdi, "gdp_usd", year = 2020, region = "Latin America & Caribbean")
  expect_equal(latin$country, c("Brazil", "Chile"))
  expect_error(indicator_ranking(wdi, "gdp_usd", region = "Atlantis"), "Unknown region")
})

test_that("compound_growth() computes the annual growth rate between two years", {
  wdi <- local_wdi()
  growth <- compound_growth(wdi, "gdp_usd", "Chile; World; Brazil", 2020, 2022)

  expect_equal(growth$annual_growth_pct, c(NA, 10, 10))
  expect_error(compound_growth(wdi, "gdp_usd", "Chile", 2022, 2020), "after start_year")
})

test_that("new_agent() builds a commons agent with guarded streaming over the World Bank tables", {
  # Dummy key: the agent is created but no request is ever sent
  local_envvar(OPENROUTER_API_KEY = "test-key")
  # Windows has no OS sandbox for commons
  local_options(commons.allow_unsafe_fallback = TRUE)

  agent <- new_agent("google/gemma:free", "Gemma (free)", wdi_source(panel))
  expect_s3_class(agent, "Chat")
  expect_true(inherits(agent, "Commons"))
})

# Stand-in for a commons agent: `replies` is one list of chunks per stream_async() call; a reply that
# contains a tool request records a turn with one, like ellmer does
fake_agent <- function(replies) {
  agent <- new.env()
  agent$turns <- list()
  agent$calls <- list()
  agent$get_turns <- function() agent$turns
  agent$set_turns <- function(value) agent$turns <- value
  agent$stream_async <- function(...) {
    agent$calls <- c(agent$calls, list(list(...)))
    chunks <- replies[[length(agent$calls)]]
    agent$turns <- c(agent$turns, list(UserTurn(list(ContentText("question")))))
    agent$turns <- c(agent$turns, list(AssistantTurn(keep(chunks, \(chunk) !is.character(chunk)))))
    async_generator(function() {
      for (chunk in chunks) {
        await(promise_resolve(NULL))
        yield(chunk)
      }
    })()
  }
  agent
}

lookup <- ContentToolRequest("1", "call_measure", list())

grounded <- function(agent, controller = NULL) {
  args <- list("GDP of Brazil?", stream = "content", controller = controller)
  collect_stream(grounded_stream(agent$stream_async, agent, args, "Gemma"))
}

test_that("a reply that looks the data up is passed through, including text before the lookup", {
  agent <- fake_agent(list(list("Let me check. ", lookup, "About 2.28 trillion.")))

  expect_equal(grounded(agent), "Let me check. About 2.28 trillion.")
  expect_equal(length(agent$calls), 1)
})

test_that("a reply from memory is dropped and the question asked again with the nudge", {
  agent <- fake_agent(list(list("From memory: 2.17 trillion."), list(lookup, "From the data: 2.28 trillion.")))

  expect_equal(grounded(agent), "From the data: 2.28 trillion.")
  expect_equal(unname(agent$calls[[2]][1:2]), list("GDP of Brazil?", world_bank$lookup_nudge))
  expect_equal(agent$calls[[2]]$stream, "content")
  # The dropped attempt is gone from the conversation
  expect_equal(length(agent$turns), 2)
})

test_that("a model that never looks anything up gets the no-lookup message instead of its answer", {
  # e.g. made-up figures after fake tool calls written as text
  agent <- fake_agent(list(list('{"tool": "browser.search"} About 2.17 trillion.'), list("Still 2.17 trillion.")))

  expected <- str_replace_all(world_bank$no_lookup, fixed("{model}"), "Gemma")
  expect_equal(grounded(agent), expected)
  expect_equal(length(agent$calls), 2)
  # Neither made-up answer stays in the conversation
  expect_equal(agent$turns, list())
})

test_that("a reply the user stopped before any lookup shows nothing and isn't retried", {
  agent <- fake_agent(list(list("Partial answer")))
  controller <- stream_controller()
  controller$cancel()

  expect_equal(grounded(agent, controller), "")
  expect_equal(length(agent$calls), 1)
})

test_that("the grounded agent accepts shinychat's spliced call, stream_async(!!!user_input, ...)", {
  agent <- ground_agent(fake_agent(list(list(lookup, "From the data."))), "Gemma")
  user_input <- list("GDP of Brazil?")

  expect_equal(collect_stream(agent$stream_async(!!!user_input, stream = "content")), "From the data.")
  expect_equal(agent$calls[[1]][[1]], "GDP of Brazil?")
})

test_that("has_lookup() counts successful tool results only", {
  ask <- UserTurn(list(ContentText("GDP of Brazil?")))
  found <- UserTurn(list(ContentToolResult(value = "2.28e12", request = lookup)))
  failed <- UserTurn(list(ContentToolResult(error = "Unknown indicator", request = lookup)))

  expect_true(has_lookup(list(ask, found)))
  expect_false(has_lookup(list(ask, failed)))
  expect_false(has_lookup(list()))
})

test_that("once data was looked up, a follow-up answered from it is shown, not dropped or retried", {
  agent <- fake_agent(list(list("As found above: 2.28 trillion (2025).")))
  agent$turns <- list(
    UserTurn(list(ContentText("GDP of Brazil?"))),
    AssistantTurn(list(lookup)),
    UserTurn(list(ContentToolResult(value = "2.28e12", request = lookup)))
  )

  expect_equal(grounded(agent), "As found above: 2.28 trillion (2025).")
  expect_equal(length(agent$calls), 1)
})

test_that("reasoning_args() sends OpenRouter's reasoning effort, or nothing when it's empty", {
  expect_equal(reasoning_args("low"), list(reasoning = list(effort = "low")))
  expect_equal(reasoning_args(""), list())
  expect_equal(reasoning_args(NULL), list())
})

test_that("agent_instructions() lists every configured indicator for direct measure calls", {
  text <- agent_instructions()

  expect_match(text, "latest_values(indicator, economies)", fixed = TRUE)
  expect_match(text, "indicator_chart(indicator, economies, start_year?, end_year?)", fixed = TRUE)
  for (indicator in c("gdp_usd", "population", "gov_debt_pct_gdp", "gini")) {
    expect_match(text, indicator, fixed = TRUE)
  }
  expect_false(grepl("{indicators}", text, fixed = TRUE))
})
