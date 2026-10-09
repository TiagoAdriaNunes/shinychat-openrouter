box::use(
  commons[commons, context_layer, data_source, semantic_layer],
  coro[async_generator, await_each, yield],
  ellmer[chat_openrouter],
  glue[glue],
  logger[log_info, log_warn],
  purrr[map, map_chr, some],
  rlang[list2],
  stringr[str_c],
  withr[with_options],
  yaml[write_yaml],
)

box::use(
  . / client[guard_stream, wrap_stream],
  . / config[fallback_model, system_prompt, world_bank],
)

# Layer files from config.yml, resolved from the project root so they work from any working directory
project_path <- function(paths) map_chr(paths, \(path) box::file("..", "..", path))
measure_files <- project_path(world_bank$measure_files)
context_files <- project_path(world_bank$context_files)

# Data dictionary (data-dict.yaml) describing the agent's tables, written to a temporary file
dictionary_file <- function(panel) {
  indicator_columns <- map(world_bank$indicators, \(i) {
    list(name = i$name, description = glue("{i$description}. World Bank series {i$code}."))
  })
  dictionary <- list(
    name = "World Bank World Development Indicators",
    description = glue(
      "Economic indicators from the World Bank's World Development Indicators, {world_bank$start_year} onward, ",
      "downloaded with the WDI R package on {panel$downloaded}."
    ),
    details = str_c(
      "Values are missing (NULL) when the World Bank has no estimate; the latest year with data differs by ",
      "country and indicator. Country names follow World Bank spelling (e.g. 'Korea, Rep.', 'Turkiye')."
    ),
    tables = list(
      list(
        name = "countries",
        description = "One row per country (economy) and year.",
        details = "Only individual economies; never sum them to get regional or world totals, use `aggregates`.",
        columns = c(
          list(
            list(name = "country", description = "Economy name, World Bank spelling."),
            list(name = "iso3c", description = "ISO 3166-1 alpha-3 code."),
            list(name = "region", description = "World Bank geographic region."),
            list(name = "income", description = "World Bank income group (e.g. 'High income', 'Low income')."),
            list(name = "year", description = "Calendar year.")
          ),
          indicator_columns
        )
      ),
      list(
        name = "aggregates",
        description = "One row per World Bank aggregate and year: World, regions, income groups, Euro area, OECD members and other groupings.",
        details = "Computed by the World Bank (weighted where appropriate), so use these instead of combining countries.",
        columns = c(
          list(
            list(name = "aggregate", description = "Aggregate name, e.g. 'World', 'Euro area', 'High income'."),
            list(name = "iso3c", description = "World Bank aggregate code, e.g. 'WLD', 'EMU', 'HIC'."),
            list(name = "year", description = "Calendar year.")
          ),
          indicator_columns
        )
      )
    )
  )
  path <- tempfile("wdi-", fileext = ".yaml")
  write_yaml(dictionary, path)
  path
}

#' commons data source with the panel's `countries` and `aggregates` tables (an in-memory duckdb).
#' Create one per Shiny session and reuse it for every agent in that session.
#' @export
wdi_source <- function(panel) {
  data_source(countries = panel$countries, aggregates = panel$aggregates, dictionary = dictionary_file(panel))
}

#' TRUE if any turn after the first `from` turns requested a tool (a data lookup).
#' @export
used_tools <- function(turns, from) {
  new_turns <- turns[seq_along(turns) > from]
  some(new_turns, \(turn) some(turn@contents, \(content) inherits(content, "ellmer::ContentToolRequest")))
}

is_tool_request <- function(chunk) inherits(chunk, "ellmer::ContentToolRequest")
is_thinking <- function(chunk) inherits(chunk, "ellmer::ContentThinking")

# Split stream_async() arguments into the message contents (unnamed) and options (named)
split_args <- function(args) {
  named <- names(args) %||% rep("", length(args))
  list(inputs = args[named == ""], options = args[named != ""])
}

#' Stream an agent reply that looks the data up. Free models often answer data questions from memory,
#' and commons can offer tools but not force them (ellmer has no tool_choice), so:
#' 1. The reply's text is held back until the model requests a tool, then everything is passed on.
#'    Thinking is shown as it comes.
#' 2. A reply that ends without a tool request is dropped: the turns are restored and the question is
#'    asked again with `world_bank$lookup_nudge` added to the message.
#' 3. If the retry also skips the data, it is shown with `world_bank$unchecked_note`, because commons
#'    only marks answers that used its tools and this one would otherwise look like data.
#' A reply the user stops is never retried. A failed request errors out of the loop (see safe_stream()).
#' `original` is the agent's own stream_async(); `args` its arguments. Like safe_stream(), the body must
#' end with NULL (coro emits the final value as a chunk).
#' @export
grounded_stream <- async_generator(function(original, agent, args) {
  args <- split_args(args)
  before <- agent$get_turns()
  held <- list()
  looked_up <- FALSE
  for (chunk in await_each(do.call(original, c(args$inputs, args$options)))) {
    if (!looked_up && is_tool_request(chunk)) {
      looked_up <- TRUE
      for (text in held) yield(text)
    }
    if (looked_up || is_thinking(chunk)) {
      yield(chunk)
    } else {
      held <- c(held, list(chunk))
    }
  }
  cancelled <- isTRUE(args$options$controller$cancelled)
  if (!looked_up && cancelled) {
    for (text in held) yield(text)
  }
  if (!looked_up && !cancelled) {
    agent$set_turns(before)
    retry <- c(args$inputs, list(world_bank$lookup_nudge), args$options)
    for (chunk in await_each(do.call(original, retry))) yield(chunk)
    if (!used_tools(agent$get_turns(), length(before))) {
      yield(str_c("\n\n", world_bank$unchecked_note))
    }
  }
  NULL
})

#' commons agent on an OpenRouter model, answering from `source` (a wdi_source()) with the trusted
#' measures in `world_bank$measure_files` and the notes in `world_bank$context_files`. Like new_client(), failed requests become a chat message naming `label`.
#' @export
new_agent <- function(model, label, source) {
  # commons decides at construction whether its R session can be sandboxed; without a sandbox it
  # aborts unless the commons.allow_unsafe_fallback option is set (by config.yml or the caller)
  unsafe <- if (isTRUE(world_bank$allow_unsafe_fallback)) list(commons.allow_unsafe_fallback = TRUE) else list()
  agent <- with_options(unsafe, {
    commons(
      chat_openrouter(model = model),
      data_sources = list(wdi = source),
      semantic_layer = semantic_layer(measure_files),
      context_layer = context_layer(context_files),
      instructions = str_c(system_prompt, "\n\n", world_bank$instructions),
      network = "none"
    )
  })
  guard_stream(ground_agent(agent), label)
}

#' Make `agent$stream_async()` go through grounded_stream().
#' @export
ground_agent <- function(agent) {
  # list2(), not list(): shinychat calls stream_async(!!!user_input, ...), and only rlang's dots
  # splice `!!!` (base list() would evaluate it as three logical NOTs: "invalid argument type")
  wrap_stream(agent, \(original, ...) grounded_stream(original, agent, list2(...)))
}

#' Build the agent's context search index (from the data dictionary) now instead of during the
#' first question. It is stored in the `commons.context_cache` directory, so later agents reuse it.
#' Never errors: if the agent can't be created (e.g. no OS sandbox), World Bank data mode will say so.
#' @export
prewarm_agent <- function(panel) {
  tryCatch(
    {
      new_agent(fallback_model, fallback_model, wdi_source(panel))$prewarm()
      log_info("World Bank agent context prewarmed")
    },
    error = function(e) log_warn("Could not prewarm the World Bank agent: {conditionMessage(e)}")
  )
  invisible(NULL)
}
