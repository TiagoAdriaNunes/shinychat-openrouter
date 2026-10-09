box::use(
  purrr[keep],
  stringr[fixed, str_c, str_replace_all],
  testthat[expect_equal, expect_error, expect_false, expect_message, expect_s3_class, expect_true, test_that],
  withr[defer, local_envvar, local_options],
)

box::use(
  app / logic / agent[agent_instructions, ground_agent, grounded_stream, has_lookup, new_agent, reasoning_args, wdi_source],
  app / logic / config[world_bank],
  app / logic / wdi[split_panel],
)

panel <- split_panel(fake_wdi_raw())
panel$downloaded <- as.Date("2026-10-08")

# The measures file, loaded on its own like commons does (it isn't a box module)
measures_file <- file.path("..", "..", "app", "measures", "world-bank.R")
measures <- new.env()
sys.source(measures_file, envir = measures)

# The `wdi` connection commons passes to the measures: the panel's tables in duckdb
local_wdi <- function(env = parent.frame()) {
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbWriteTable(con, "countries", panel$countries)
  DBI::dbWriteTable(con, "aggregates", panel$aggregates)
  defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  con
}

test_that("commons reads the four measures from the measures file", {
  expect_message(print(commons::semantic_layer(measures_file)), "4 measures")
})

test_that("latest_values() gives each economy's most recent value, by name or ISO3 code in any case", {
  latest <- measures$latest_values(local_wdi(), "gdp_usd", "brazil; KOR ;World;")

  expect_equal(latest$economy, c("Brazil", "Korea, Rep.", "World"))
  expect_equal(latest$year, c(2021L, 2022L, 2022L))
  expect_equal(latest$gdp_usd, c(121, 220, 1210))
})

test_that("unknown economies, indicators and missing data are errors the model can act on", {
  wdi <- local_wdi()
  expect_error(measures$latest_values(wdi, "gdp_usd", "Brazil; South Korea"), "South Korea.*ISO3")
  expect_error(measures$latest_values(wdi, "happiness", "Brazil"), "Unknown indicator 'happiness'")
  expect_error(measures$latest_values(wdi, "gini", "Brazil"), "No gini data for Brazil")
})

test_that("indicator_series() returns non-missing values in the year range", {
  series <- measures$indicator_series(local_wdi(), "inflation_pct", "CHL; BRA", start_year = 2021)

  expect_equal(series$economy, c("Brazil", "Brazil", "Chile"))
  expect_equal(series$year, c(2021L, 2022L, 2021L))
})

test_that("indicator_ranking() ranks countries only, in the latest well-covered year", {
  wdi <- local_wdi()
  ranked <- measures$indicator_ranking(wdi, "gdp_usd")
  # 2022 is missing for Brazil, but still reported by 2 of 3 countries
  expect_equal(ranked$year, c(2022L, 2022L))
  expect_equal(ranked$country, c("Korea, Rep.", "Chile"))
  expect_equal(ranked$rank, 1:2)

  lowest <- measures$indicator_ranking(wdi, "inflation_pct", year = 2021, lowest_first = TRUE, top_n = 1)
  expect_equal(lowest$country, "Korea, Rep.")

  latin <- measures$indicator_ranking(wdi, "gdp_usd", year = 2020, region = "Latin America & Caribbean")
  expect_equal(latin$country, c("Brazil", "Chile"))
  expect_error(measures$indicator_ranking(wdi, "gdp_usd", region = "Atlantis"), "Unknown region")
})

test_that("compound_growth() computes the annual growth rate between two years", {
  wdi <- local_wdi()
  growth <- measures$compound_growth(wdi, "gdp_usd", "Chile; World; Brazil", 2020, 2022)

  expect_equal(growth$annual_growth_pct, c(NA, 10, 10))
  expect_error(measures$compound_growth(wdi, "gdp_usd", "Chile", 2022, 2020), "after start_year")
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
    agent$turns <- c(agent$turns, list(ellmer::UserTurn(list(ellmer::ContentText("question")))))
    agent$turns <- c(agent$turns, list(ellmer::AssistantTurn(keep(chunks, \(chunk) !is.character(chunk)))))
    coro::async_generator(function() {
      for (chunk in chunks) {
        coro::await(promises::promise_resolve(NULL))
        coro::yield(chunk)
      }
    })()
  }
  agent
}

lookup <- ellmer::ContentToolRequest("1", "call_measure", list())

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
  controller <- ellmer::stream_controller()
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
  ask <- ellmer::UserTurn(list(ellmer::ContentText("GDP of Brazil?")))
  found <- ellmer::UserTurn(list(ellmer::ContentToolResult(value = "2.28e12", request = lookup)))
  failed <- ellmer::UserTurn(list(ellmer::ContentToolResult(error = "Unknown indicator", request = lookup)))

  expect_true(has_lookup(list(ask, found)))
  expect_false(has_lookup(list(ask, failed)))
  expect_false(has_lookup(list()))
})

test_that("once data was looked up, a follow-up answered from it is shown, not dropped or retried", {
  agent <- fake_agent(list(list("As found above: 2.28 trillion (2025).")))
  agent$turns <- list(
    ellmer::UserTurn(list(ellmer::ContentText("GDP of Brazil?"))),
    ellmer::AssistantTurn(list(lookup)),
    ellmer::UserTurn(list(ellmer::ContentToolResult(value = "2.28e12", request = lookup)))
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
  for (indicator in c("gdp_usd", "population", "gov_debt_pct_gdp", "gini")) {
    expect_match(text, indicator, fixed = TRUE)
  }
  expect_false(grepl("{indicators}", text, fixed = TRUE))
})
