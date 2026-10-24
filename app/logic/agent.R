# The Agent mode agent: a commons agent on an OpenRouter model, with the data source (dictionary.R),
# the tool adjustments (tools.R) and the reply wrappers (grounding.R)

box::use(
  checkmate[test_null, test_string],
  commons[commons, context_layer, semantic_layer],
  ellmer[chat_openrouter],
  glue[glue_data],
  logger[log_info, log_warn],
  memoise[memoise],
  purrr[map_chr, walk2],
  stringr[str_c, str_flatten_comma],
  withr[with_options],
)

box::use(
  . / client[guard_stream, set_response_timeout],
  . / config[fallback_model, world_bank],
  . / definitions[definitions_markdown],
  . / dictionary[group_names, wdi_source],
  . / grounding[ground_agent, require_lookup, show_charts],
  . / tools[check_measure_calls, describe_tools, measure_tools],
)

# Layer files from config.yml, resolved from the project root so they work from any working directory
project_path <- function(paths) map_chr(paths, \(path) box::file("..", "..", path))
measure_files <- project_path(world_bank$measure_files)
context_files <- project_path(world_bank$context_files)

# The measures, read once per R process (about 0.5 s) and shared by every agent: commons only reads them
measure_layer <- memoise(\() semantic_layer(measure_files))

# The panel's indicator definitions as Markdown files, one per indicator (see definitions_markdown()),
# or none when it has none. Temporary files are enough: commons caches the context index by text.
definitions_files <- function(panel) {
  if (test_null(panel$definitions)) {
    return(NULL)
  }
  documents <- definitions_markdown(panel$definitions)
  folder <- tempfile("indicator-definitions-")
  dir.create(folder)
  paths <- file.path(folder, str_c(names(documents), ".md"))
  walk2(documents, paths, \(document, path) writeLines(document, path, useBytes = TRUE))
  paths
}

#' The agent's context files: `world_bank$context_files` and the panel's indicator definitions.
#' @export
agent_context <- function(panel) c(context_files, definitions_files(panel))

#' `world_bank$instructions` with {indicators} and {groups} replaced by the configured names.
#' @export
agent_instructions <- function() {
  names <- list(
    indicators = str_flatten_comma(map_chr(world_bank$indicators, "name")),
    groups = str_flatten_comma(group_names())
  )
  as.character(glue_data(names, world_bank$instructions, .envir = baseenv()))
}

#' OpenRouter request fields for a reasoning effort (e.g. "low"), or none when `effort` is empty
#' (ellmer's OpenRouter provider ignores params(reasoning_effort =)).
#' @export
reasoning_args <- function(effort) {
  if (test_string(effort, min.chars = 1)) list(reasoning = list(effort = effort)) else list()
}

#' commons agent on an OpenRouter model, answering from `source` (a wdi_source()) with the trusted
#' measures in `world_bank$measure_files` and the notes in `context` (agent_context() for a panel).
#' Like new_client(), failed requests become a chat message naming `label`.
#' @export
new_agent <- function(model, label, source, context = context_files) {
  # Without an OS sandbox commons aborts unless commons.allow_unsafe_fallback is set
  unsafe <- if (isTRUE(world_bank$allow_unsafe_fallback)) list(commons.allow_unsafe_fallback = TRUE) else list()
  layer <- measure_layer()
  agent <- with_options(unsafe, {
    commons(
      chat_openrouter(model = model, api_args = reasoning_args(world_bank$reasoning_effort)),
      data_sources = list(wdi = source),
      semantic_layer = layer,
      context_layer = context_layer(context),
      instructions = agent_instructions(),
      network = "none"
    )
  })
  set_response_timeout(agent, world_bank$response_timeout_seconds)
  describe_tools(agent)
  measure_tools(agent, layer)
  check_measure_calls(agent)
  if (isTRUE(world_bank$require_lookup)) {
    require_lookup(agent)
  }
  guard_stream(ground_agent(show_charts(agent), label), label)
}

#' Build the agent's context search index now instead of during the first question; it is cached in
#' the `commons.context_cache` directory for later agents. Never errors: without an OS sandbox,
#' Agent mode says it is unavailable. Returns `panel` invisibly.
#' @export
prewarm_agent <- function(panel) {
  tryCatch(
    {
      new_agent(fallback_model, fallback_model, wdi_source(panel), agent_context(panel))$prewarm()
      log_info("World Bank agent context prewarmed")
    },
    error = function(e) log_warn("Could not prewarm the World Bank agent: {conditionMessage(e)}")
  )
  invisible(panel)
}
