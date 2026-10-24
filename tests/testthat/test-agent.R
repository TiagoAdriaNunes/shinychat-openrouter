box::use(
  commons[semantic_layer],
  coro[async_generator, await, yield],
  DBI[dbConnect, dbDisconnect, dbWriteTable],
  duckdb[duckdb],
  htmltools[div, renderTags],
  ellmer[
    AssistantTurn, chat_openrouter, ContentText, ContentThinking, ContentToolRequest, ContentToolResult, stream_controller,
    tool, UserTurn,
  ],
  httr2[local_mocked_responses, response_json],
  promises[promise_resolve],
  purrr[keep, map_chr],
  S7[S7_inherits],
  stringr[fixed, regex, str_c, str_extract, str_remove, str_replace_all],
  testthat[
    expect_equal, expect_error, expect_false, expect_match, expect_message, expect_null, expect_s3_class, expect_true,
    test_that,
  ],
  withr[defer, local_envvar, local_options],
)

box::use(
  app / logic / agent[agent_context, agent_instructions, chart_stream, drop_empty_reply, ground_agent, grounded_stream, groups_table, has_lookup, new_agent, check_arguments, reasoning_args, require_lookup, wdi_source],
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
  dbWriteTable(con, "groups", data.frame(group = c("Mercosur", "Pacific Alliance"), iso3c = c("BRA", "CHL")))
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
  # No rows for commons to offer to run_r: re-analysing them there marks the answer untrusted
  expect_null(chart@extra$data)

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

test_that("indicator_ranking() ranks only a group's members, the group named in any case", {
  wdi <- local_wdi()
  # Korea has the highest GDP, but isn't in Mercosur
  mercosur <- indicator_ranking(wdi, "gdp_usd", year = 2020, group = "mercosur")
  expect_equal(mercosur$country, "Brazil")
  expect_error(indicator_ranking(wdi, "gdp_usd", group = "Eurozone"), "Unknown group 'Eurozone'.*Mercosur")
})

test_that("the economy measures take a group's members, with or without named economies", {
  wdi <- local_wdi()
  expect_equal(latest_values(wdi, "gdp_usd", group = "Mercosur")$economy, "Brazil")
  expect_equal(latest_values(wdi, "gdp_usd", "World", group = "Mercosur")$economy, c("Brazil", "World"))
  expect_equal(unique(indicator_series(wdi, "inflation_pct", group = "Pacific Alliance")$economy), "Chile")
  mercosur_chart <- indicator_chart(wdi, "inflation_pct", group = "Mercosur")@value
  expect_match(mercosur_chart, "Brazil:", fixed = TRUE)
  expect_false(grepl("Chile:", mercosur_chart, fixed = TRUE))
  expect_error(latest_values(wdi, "gdp_usd"), "Give economies, a group, or both")
  expect_error(indicator_chart(wdi, "gdp_usd"), "Give economies, a group, or both")
})

test_that("compound_growth() gives total and annual growth from start_year's level, highest first", {
  wdi <- local_wdi()
  growth <- compound_growth(wdi, "gdp_usd", "World; Brazil; KOR", 2020, 2022)

  # Brazil has no 2022 GDP, so it is last with NA
  expect_equal(growth$economy, c("World", "Korea, Rep.", "Brazil"))
  expect_equal(growth$total_growth_pct, c(21, 10, NA))
  expect_equal(growth$annual_growth_pct, c(10, 100 * (sqrt(1.1) - 1), NA))
  expect_equal(
    compound_growth(wdi, "gdp_usd", start_year = 2020, end_year = 2021, group = "Mercosur")$economy,
    "Brazil"
  )
  expect_error(compound_growth(wdi, "gdp_usd", "Chile", 2022, 2020), "after start_year")
  # Adding up yearly rates would be wrong: point to the real GDP level instead
  expect_error(compound_growth(wdi, "gdp_growth_pct", "Chile", 2020, 2022), "use gdp_constant_usd")
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
    # Only ellmer content goes in a turn (a chart is added to the stream, not to the conversation)
    agent$turns <- c(agent$turns, list(AssistantTurn(keep(chunks, \(chunk) S7_inherits(chunk)))))
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

test_that("a reply that looks the data up but writes no answer is asked once for it", {
  result <- ContentToolResult(value = "2.28e12", request = lookup)
  agent <- fake_agent(list(list(lookup, result), list("From the data: 2.28 trillion (2025).")))

  expect_equal(grounded(agent), "From the data: 2.28 trillion (2025).")
  expect_equal(length(agent$calls), 2)
  expect_equal(agent$calls[[2]][[1]], world_bank$answer_nudge)
})

test_that("a model that still writes no answer gets the no-answer message, not silence", {
  result <- ContentToolResult(value = "2.28e12", request = lookup)
  agent <- fake_agent(list(list("Let me check. ", lookup, result), list(ContentThinking("It is 2.28 trillion."))))

  expected <- str_c("Let me check. ", str_replace_all(world_bank$no_answer, fixed("{model}"), "Gemma"))
  expect_equal(grounded(agent), expected)
  expect_equal(length(agent$calls), 2)
})

test_that("an answer already shown isn't asked for again when the model then runs more tools", {
  result <- ContentToolResult(value = "2.28e12", request = lookup)
  check <- ContentToolRequest("2", "run_r", list())
  agent <- fake_agent(list(list(lookup, result, "About 2.28 trillion (2025).", check, ContentToolResult(value = "ok"))))

  expect_equal(grounded(agent), "About 2.28 trillion (2025).")
  expect_equal(length(agent$calls), 1)
})

test_that("a chart counts as the answer, and a stopped reply isn't asked again", {
  result <- ContentToolResult(value = "chart", request = lookup)
  agent <- fake_agent(list(list(lookup, result, div("chart"))))
  expect_equal(grounded(agent), "[html]")
  expect_equal(length(agent$calls), 1)

  controller <- stream_controller()
  controller$cancel()
  stopped <- fake_agent(list(list(lookup, result)))
  expect_equal(grounded(stopped, controller), "")
  expect_equal(length(stopped$calls), 1)
})

test_that("drop_empty_reply() removes a last reply without text or tool calls only", {
  agent <- fake_agent(list())
  question <- UserTurn(list(ContentText("GDP of Brazil?")))
  agent$turns <- list(question, AssistantTurn(list(ContentThinking("hmm"))))
  drop_empty_reply(agent)
  expect_equal(length(agent$turns), 1)

  agent$turns <- list(question, AssistantTurn(list(ContentText("2.28 trillion"))))
  drop_empty_reply(agent)
  expect_equal(length(agent$turns), 2)

  agent$turns <- list(question, AssistantTurn(list(lookup)))
  drop_empty_reply(agent)
  expect_equal(length(agent$turns), 2)
})

test_that("reasoning_args() sends OpenRouter's reasoning effort, or nothing when it's empty", {
  expect_equal(reasoning_args("low"), list(reasoning = list(effort = "low")))
  expect_equal(reasoning_args(""), list())
  expect_equal(reasoning_args(NULL), list())
})

test_that("agent_instructions() lists every configured indicator for direct measure calls", {
  text <- agent_instructions()

  expect_match(text, "latest_values(indicator, economies?, group?)", fixed = TRUE)
  expect_match(text, "indicator_chart(indicator, economies?, start_year?, end_year?, group?)", fixed = TRUE)
  for (name in c("gdp_usd", "gdp_constant_usd", "population", "gov_debt_pct_gdp", "gini", "Euro area", "Mercosur")) {
    expect_match(text, name, fixed = TRUE)
  }
  expect_false(grepl("{indicators}", text, fixed = TRUE))
  expect_false(grepl("{groups}", text, fixed = TRUE))
})

test_that("latest_values() lists every configured indicator column for the model", {
  docs <- str_c(readLines(measures_file), collapse = "\n")
  indicator_param <- str_extract(
    docs,
    regex("@param indicator `string` Indicator column:.*?@param economies", dotall = TRUE)
  )
  for (name in map_chr(world_bank$indicators, "name")) {
    expect_match(indicator_param, str_c("\\b", name, "\\b"), info = name)
  }
})

test_that("agent_context() adds the panel's indicator definitions to the context files", {
  files <- agent_context(panel)
  defined <- panel$definitions$name[!is.na(panel$definitions$definition)]
  expect_equal(length(files), length(world_bank$context_files) + length(defined))
  # One file per indicator, so a context search returns one whole definition
  inflation <- str_c(readLines(files[basename(files) == "inflation_pct.md"], encoding = "UTF-8"), collapse = "\n")
  expect_match(inflation, str_c("# ", world_bank$definitions$heading, ": "), fixed = TRUE)
  expect_match(inflation, "consumer price index", fixed = TRUE)

  # A panel without definitions (an old cache) keeps the configured notes only
  expect_equal(length(agent_context(list())), length(world_bank$context_files))
})

test_that("wdi_source() has the configured groups as a table of members", {
  groups <- groups_table()
  expect_equal(names(groups), c("group", "iso3c"))
  expect_equal(groups$iso3c[groups$group == "Mercosur"], c("ARG", "BRA", "PRY", "URY", "BOL"))
  expect_equal(sum(groups$group == "Euro area"), 21)
  expect_equal(sum(groups$group == "European Union"), 27)
  expect_equal(sum(groups$group == "OECD"), 38)
  # Every member is an ISO3 code (YAML can't have turned one into TRUE/FALSE)
  expect_true(all(grepl("^[A-Z]{3}$", groups$iso3c)))
})

test_that("require_lookup() requires a tool call only for a new question without earlier data", {
  local_envvar(OPENROUTER_API_KEY = "test-key")
  local_options(ellmer_max_tries = 1)
  sent <- list()
  replies <- list(
    # The question: a tool call
    list(tool_calls = list(list(id = "c1", type = "function", `function` = list(name = "lookup", arguments = "{}")))),
    # After the tool result: the answer
    list(content = "It is 42."),
    # The follow-up, answered from the earlier result
    list(content = "Still 42.")
  )
  local_mocked_responses(function(req) {
    sent[[length(sent) + 1]] <<- req$body$data
    message <- c(list(role = "assistant"), replies[[length(sent)]])
    response_json(
      body = list(
        model = "a/alpha:free",
        choices = list(list(index = 0, message = message, finish_reason = "stop")),
        usage = list(prompt_tokens = 1, completion_tokens = 1, total_tokens = 2)
      )
    )
  })
  client <- chat_openrouter(model = "a/alpha:free", echo = "none")
  client$register_tool(tool(function() "42", "Look the figure up", name = "lookup"))
  require_lookup(client)

  client$chat("What is the figure?")
  client$chat("And again?")
  expect_equal(length(sent), 3)
  expect_equal(sent[[1]]$tool_choice, "required")
  expect_null(sent[[2]]$tool_choice)
  expect_null(sent[[3]]$tool_choice)
})

test_that("check_arguments() passes a JSON object and rejects anything else, showing the format", {
  ok <- '{"indicator": "inflation_pct", "economies": "Brazil; Mexico", "start_year": 2000}'
  expect_equal(check_arguments(ok), ok)
  expect_equal(check_arguments(" {} "), " {} ")
  # Some models send an object, which commons accepts
  expect_equal(check_arguments(list(indicator = "gdp_usd")), list(indicator = "gdp_usd"))

  # Seen from free models
  bad <- c(
    "{",
    "{'indicator': 'inflation_pct', 'economies': 'Brazil; Mexico'}",
    "indicator:inflation_pct,economies:Brazil; Mexico,start_year:2000",
    "{}/* need to check args */",
    "{} wishes to compute: inflation",
    '"gdp_usd"'
  )
  for (arguments in bad) {
    expect_error(check_arguments(arguments), "must be one JSON object", fixed = TRUE)
    expect_error(check_arguments(arguments), str_c("You sent: ", arguments), fixed = TRUE)
  }
  expect_error(check_arguments("{"), '"arguments": "{\\"indicator\\": \\"gdp_usd\\"', fixed = TRUE)
})

test_that("the agent's call_measure rejects invalid arguments with the format and runs valid ones", {
  local_envvar(OPENROUTER_API_KEY = "test-key")
  local_options(commons.allow_unsafe_fallback = TRUE)
  agent <- new_agent("google/gemma:free", "Gemma (free)", wdi_source(panel))
  call_measure <- agent$get_tools()$call_measure

  expect_error(call_measure(name = "latest_values", arguments = "{"), "must be one JSON object", fixed = TRUE)
  expect_null(call_measure(name = "latest_values", arguments = '{"indicator": "gdp_usd", "economies": "World"}')@error)
})

test_that("each measure is a tool of its own that runs through commons", {
  local_envvar(OPENROUTER_API_KEY = "test-key")
  local_options(commons.allow_unsafe_fallback = TRUE)
  agent <- new_agent("google/gemma:free", "Gemma (free)", wdi_source(panel))
  tools <- agent$get_tools()

  expect_true(all(
    c(
      "latest_values",
      "indicator_series",
      "indicator_ranking",
      "compound_growth",
      "country_profile",
      "indicator_chart"
    ) %in%
      names(tools)
  ))
  # The model sees the measure's own arguments, not the `wdi` connection commons fills in
  expect_equal(names(tools$latest_values@arguments@properties), c("indicator", "economies", "group"))

  # The same table as through call_measure (each result gets its own run_r name, r1, r2, ...)
  table <- function(result) str_remove(result@value, "Available to `run_r`[\\s\\S]*")
  latest <- tools$latest_values(indicator = "gdp_usd", economies = "World")
  expect_null(latest@error)
  expect_match(latest@value, "|World   |WLD   | 2022|    1210|", fixed = TRUE)
  same <- tools$call_measure(name = "latest_values", arguments = '{"indicator": "gdp_usd", "economies": "World"}')
  expect_equal(table(latest), table(same))

  # Omitted optional arguments (NULL) aren't sent; a chart keeps its chart for the user
  chart <- tools$indicator_chart(indicator = "gdp_usd", economies = "World", start_year = NULL)
  expect_null(chart@error)
  expect_match(as.character(chart@extra$chart), "echarts4r", fixed = TRUE)

  # Each tool runs its own measure, and the measure's checks still apply (called outside a chat, the
  # error is thrown; in a chat ellmer returns it to the model)
  expect_error(tools$latest_values(indicator = "happiness", economies = "World"), "Unknown indicator 'happiness'")
  expect_error(tools$compound_growth(indicator = "gdp_usd", economies = "World"), "requires argument `start_year`")
})

test_that("measure tools mark optional arguments and treat placeholders as left out", {
  local_envvar(OPENROUTER_API_KEY = "test-key")
  local_options(commons.allow_unsafe_fallback = TRUE)
  agent <- new_agent("google/gemma:free", "Gemma (free)", wdi_source(panel))
  growth <- agent$get_tools()$compound_growth
  arguments <- growth@arguments@properties

  expect_match(arguments$group@description, world_bank$optional_argument, fixed = TRUE)
  expect_match(arguments$economies@description, world_bank$optional_argument, fixed = TRUE)
  expect_false(grepl(world_bank$optional_argument, arguments$start_year@description, fixed = TRUE))

  # Seen from models that thought group was required
  for (group in c("{}", "", "null", "None", ".", " - ")) {
    result <- growth(indicator = "gdp_usd", economies = "World", start_year = 2020L, end_year = 2022L, group = group)
    expect_null(result@error)
  }
  # A real but unknown group still fails, saying group is optional
  expect_error(
    growth(indicator = "gdp_usd", economies = "World", start_year = 2020L, end_year = 2022L, group = "Atlantis"),
    "send null when the question isn't about one of these groups"
  )
})
