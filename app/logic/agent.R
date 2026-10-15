box::use(
  checkmate[test_null, test_string],
  commons[commons, context_layer, data_source, semantic_layer],
  coro[async_generator, await_each, yield],
  ellmer[chat_openrouter, ContentThinking, ContentToolRequest, ContentToolResult],
  glue[glue, glue_data],
  logger[log_info, log_warn],
  purrr[list_rbind, map, map_chr, some],
  rlang[list2],
  S7[S7_inherits],
  stringr[str_c, str_flatten_comma],
  withr[with_options],
  yaml[write_yaml],
)

box::use(
  . / client[guard_stream, set_api_arg, wrap_stream],
  . / config[fallback_model, world_bank],
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
        description = str_c(
          "One row per World Bank aggregate and year: World, regions, income groups, Euro area, OECD members ",
          "and other groupings."
        ),
        details = str_c(
          "Computed by the World Bank (weighted where appropriate), so use these instead of combining ",
          "countries."
        ),
        columns = c(
          list(
            list(name = "aggregate", description = "Aggregate name, e.g. 'World', 'Euro area', 'High income'."),
            list(name = "iso3c", description = "World Bank aggregate code, e.g. 'WLD', 'EMU', 'HIC'."),
            list(name = "year", description = "Calendar year.")
          ),
          indicator_columns
        )
      ),
      list(
        name = "groups",
        description = glue("Member countries of country groups: {str_flatten_comma(group_names())}."),
        details = str_c(
          "Current membership, applied to every year. Join to `countries` on iso3c to list or rank a ",
          "group's countries; the group's total is in `aggregates`."
        ),
        columns = list(
          list(name = "group", description = "Group name, e.g. 'Euro area', 'European Union', 'Mercosur'."),
          list(name = "iso3c", description = "ISO3 code of a member country.")
        )
      )
    )
  )
  path <- tempfile("wdi-", fileext = ".yaml")
  write_yaml(dictionary, path)
  path
}

group_names <- function() map_chr(world_bank$groups, "name")

#' The `world_bank$groups` memberships as a table: one row per group and member (columns group, iso3c).
#' @export
groups_table <- function() {
  map(world_bank$groups, \(g) data.frame(group = g$name, iso3c = as.character(unlist(g$members)))) |>
    list_rbind()
}

#' commons data source with the panel's `countries` and `aggregates` tables and the configured
#' `groups` (an in-memory duckdb). Create one per Shiny session and reuse it for every agent in it.
#' @export
wdi_source <- function(panel) {
  data_source(
    countries = panel$countries,
    aggregates = panel$aggregates,
    groups = groups_table(),
    dictionary = dictionary_file(panel)
  )
}

is_tool_request <- function(chunk) S7_inherits(chunk, ContentToolRequest)
is_thinking <- function(chunk) S7_inherits(chunk, ContentThinking)

#' TRUE if `turns` contain a successful tool result: data already looked up in this conversation.
#' @export
has_lookup <- function(turns) {
  some(turns, \(turn) {
    some(turn@contents, \(content) S7_inherits(content, ContentToolResult) && test_null(content@error))
  })
}

# Split stream_async() arguments into the message contents (unnamed) and options (named)
split_args <- function(args) {
  named <- names(args) %||% rep("", length(args))
  list(inputs = args[named == ""], options = args[named != ""])
}

#' Replace the descriptions of the agent's tools named in `descriptions` (a named list of text):
#' commons' own say to search before any data question, against our instructions. Keeps the tools.
#' @export
describe_tools <- function(agent, descriptions = world_bank$tool_descriptions) {
  tools <- agent$get_tools()
  for (name in intersect(names(descriptions), names(tools))) {
    tools[[name]]@description <- descriptions[[name]]
  }
  agent$set_tools(tools)
  agent
}

#' Make the first request for each new question require a tool call (`tool_choice = "required"`), so
#' the model looks the data up instead of answering from memory. Later requests in the tool loop go
#' without it, so the model can answer (and a failing tool isn't forced again), and so do questions in
#' a conversation that already holds a lookup (see grounded_stream()). Set per request from
#' $on_request_start(), which runs before each request is built. Some free models ignore it.
#' @export
require_lookup <- function(agent) {
  agent$on_request_start(function(turns) {
    question <- !some(turns[[length(turns)]]@contents, \(content) S7_inherits(content, ContentToolResult))
    set_api_arg(agent, "tool_choice", if (question && !has_lookup(turns)) "required")
  })
  agent
}

#' Stream an agent reply only if it looks the data up. Free models often answer data questions from
#' memory, some even writing fake tool calls as text, and some ignore require_lookup(), so:
#' 1. The reply's text is held back until the model makes a real tool call, then everything is passed
#'    on. Thinking is shown as it comes.
#' 2. A reply that ends without a tool call is dropped (the turns are restored) and the question is
#'    asked again with `world_bank$lookup_nudge` added, held back the same way.
#' 3. If the retry also skips the data, it is dropped too and `world_bank$no_lookup` (naming `model`)
#'    is shown instead, so made-up figures never appear as answers in World Bank data mode.
#' Once the conversation holds data looked up earlier, replies are passed through as they come: a
#' repeated or follow-up question can rightly be answered from those results without a new lookup.
#' A reply the user stops before any lookup shows nothing and isn't retried. A failed request errors
#' out of the loop (see safe_stream()). `original` is the agent's own stream_async(); `args` its
#' arguments. Like safe_stream(), the body must end with NULL (coro emits the final value as a chunk).
#' @export
grounded_stream <- async_generator(function(original, agent, args, model) {
  args <- split_args(args)
  before <- agent$get_turns()
  earlier <- has_lookup(before)
  if (earlier) {
    for (chunk in await_each(do.call(original, c(args$inputs, args$options)))) {
      if (is_tool_request(chunk)) {
        log_info("World Bank agent ({model}) called {chunk@name}")
      }
      yield(chunk)
    }
  }
  # (coro generators can't assign the result of an `if` expression)
  attempts <- list(args$inputs, c(args$inputs, list(world_bank$lookup_nudge)))
  if (earlier) {
    attempts <- list()
  }
  looked_up <- earlier
  cancelled <- FALSE
  for (inputs in attempts) {
    if (!looked_up && !cancelled) {
      agent$set_turns(before)
      held <- list()
      for (chunk in await_each(do.call(original, c(inputs, args$options)))) {
        if (is_tool_request(chunk)) {
          log_info("World Bank agent ({model}) called {chunk@name}")
        }
        if (!looked_up && is_tool_request(chunk)) {
          looked_up <- TRUE
          for (text in held) {
            yield(text)
          }
        }
        if (looked_up || is_thinking(chunk)) {
          yield(chunk)
        } else {
          held <- c(held, list(chunk))
        }
      }
      cancelled <- isTRUE(args$options$controller$cancelled)
      if (!looked_up && !cancelled) {
        log_warn("World Bank agent ({model}) answered without looking anything up; reply dropped")
      }
    }
  }
  if (!looked_up && !cancelled) {
    agent$set_turns(before)
    yield(as.character(glue_data(list(model = model), world_bank$no_lookup, .envir = baseenv())))
  }
  NULL
})

#' Pass an agent's `stream` through, adding the chart of each chart measure (the `chart` its tool
#' result carries, see app/measures/charts.R) to the reply itself, right after the tool's card:
#' shinychat shows HTML chunks in the message. The body must end with NULL (see safe_stream()).
#' @export
chart_stream <- async_generator(function(stream) {
  for (chunk in await_each(stream)) {
    yield(chunk)
    if (S7_inherits(chunk, ContentToolResult) && !test_null(chunk@extra$chart)) {
      yield(chunk@extra$chart)
    }
  }
  NULL
})

#' Make `agent$stream_async()` go through chart_stream().
#' @export
show_charts <- function(agent) {
  wrap_stream(agent, \(original, ...) chart_stream(original(...)))
}

#' `world_bank$instructions` with {indicators} and {groups} replaced by the configured names.
#' @export
agent_instructions <- function() {
  names <- list(
    indicators = str_flatten_comma(map_chr(world_bank$indicators, "name")),
    groups = str_flatten_comma(group_names())
  )
  as.character(glue_data(names, world_bank$instructions, .envir = baseenv()))
}

#' OpenRouter request fields for a reasoning effort (e.g. "low"), or none when `effort` is empty.
#' (ellmer's OpenRouter provider ignores params(reasoning_effort =), so it goes in `api_args`.)
#' @export
reasoning_args <- function(effort) {
  if (test_string(effort, min.chars = 1)) list(reasoning = list(effort = effort)) else list()
}

#' commons agent on an OpenRouter model, answering from `source` (a wdi_source()) with the trusted
#' measures in `world_bank$measure_files` and the notes in `world_bank$context_files`. Like new_client(),
#' failed requests become a chat message naming `label`.
#' @export
new_agent <- function(model, label, source) {
  # commons decides at construction whether its R session can be sandboxed; without a sandbox it
  # aborts unless the commons.allow_unsafe_fallback option is set (by config.yml or the caller)
  unsafe <- if (isTRUE(world_bank$allow_unsafe_fallback)) list(commons.allow_unsafe_fallback = TRUE) else list()
  agent <- with_options(unsafe, {
    commons(
      chat_openrouter(model = model, api_args = reasoning_args(world_bank$reasoning_effort)),
      data_sources = list(wdi = source),
      semantic_layer = semantic_layer(measure_files),
      context_layer = context_layer(context_files),
      instructions = agent_instructions(),
      network = "none"
    )
  })
  describe_tools(agent)
  if (isTRUE(world_bank$require_lookup)) {
    require_lookup(agent)
  }
  guard_stream(ground_agent(show_charts(agent), label), label)
}

#' Make `agent$stream_async()` go through grounded_stream(). `label` is the model name for messages,
#' or a function of the model id (see guard_stream()).
#' @export
ground_agent <- function(agent, label) {
  # list2(), not list(): shinychat calls stream_async(!!!user_input, ...), and only rlang's dots
  # splice `!!!` (base list() would evaluate it as three logical NOTs: "invalid argument type")
  wrap_stream(agent, \(original, ...) {
    name <- if (is.function(label)) label(agent$get_model()) else label
    grounded_stream(original, agent, list2(...), name)
  })
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
