# Adjustments to the commons agent's tools for free models (see new_agent() in agent.R)

box::use(
  checkmate[test_null, test_string],
  glue[glue_data],
  jsonlite[toJSON, validate],
  logger[log_warn],
  purrr[discard, map_chr],
  rlang[abort, list2],
  S7[`S7_data<-`],
  stringr[fixed, str_c, str_detect, str_starts, str_to_lower, str_trim],
)

box::use(
  . / config[world_bank],
)

#' Replace the descriptions of the agent's tools named in `descriptions` (a named list of text):
#' commons' own say to search before any data question, against our instructions. Keeps the tools.
#' @export
describe_tools <- function(agent, descriptions = world_bank$tool_descriptions) {
  tools <- agent$get_tools()
  for (name in intersect(names(descriptions), names(tools))) {
    tools[[name]]@description <- descriptions[[name]]
  }
  agent$set_tools(tools)
  invisible(agent)
}

#' Stop with `world_bank$invalid_arguments` (how to write them, and what was received) unless a
#' measure's `arguments` text is one JSON object: free models send almost-JSON or a cut-off `{`, and
#' commons' bare "parse error" doesn't say how to fix it. Non-text arguments go to commons as they are.
#' @export
check_arguments <- function(arguments) {
  if (!test_string(arguments) || (validate(arguments) && str_starts(str_trim(arguments), fixed("{")))) {
    return(arguments)
  }
  abort(glue_data(list(received = arguments), world_bank$invalid_arguments, .envir = baseenv()), call = NULL)
}

#' Make the agent's call_measure tool check `arguments` (check_arguments()) before commons parses
#' them; a failed check reaches the model as the tool's error.
#' @export
check_measure_calls <- function(agent) {
  tools <- agent$get_tools()
  if (!"call_measure" %in% names(tools)) {
    return(agent)
  }
  original <- tools$call_measure
  S7_data(tools$call_measure) <- function(name, arguments) original(name, check_arguments(arguments))
  agent$set_tools(tools)
  invisible(agent)
}

# Values models send for an argument they mean to leave out: text without a letter or digit ("",
# "{}", ".") or a word for nothing
is_placeholder <- function(value) {
  test_string(value) &&
    (!str_detect(value, "[[:alnum:]]") || str_to_lower(str_trim(value)) %in% c("null", "none", "na", "n/a", "nil"))
}

# Tool function that runs `measure` through commons' `call_measure` with the fields the model sent,
# leaving out the empty ones (NULL or a placeholder)
forward_measure <- function(call_measure, measure) {
  # Evaluate now: called in a loop, a lazy `measure` would end up as the loop's last value
  force(call_measure)
  force(measure)
  function(...) {
    arguments <- discard(list2(...), \(value) test_null(value) || is_placeholder(value))
    json <- if (length(arguments) == 0) "{}" else as.character(toJSON(arguments, auto_unbox = TRUE))
    call_measure(name = measure, arguments = json)
  }
}

# The tool definitions commons builds for the measures of `layer` (a semantic_layer()), read from its
# private fields: commons exports no accessor. Empty if that changes.
layer_measures <- function(layer) layer$.__enclos_env__$private$measures

#' Names of the measures of `layer` (a semantic_layer()), each also a tool of its own (measure_tools()).
#' @export
measure_names <- function(layer) map_chr(layer_measures(layer), \(measure) measure@name)

#' Register each measure of `layer` (a semantic_layer()) as a tool of its own, with the name,
#' description and arguments commons gives it, run through call_measure: models then send plain
#' fields instead of JSON inside a string, which some cut off. commons still runs the measure, so
#' results and trust are unchanged. Without the layer's measures (layer_measures()) the agent keeps
#' only call_measure.
#' @export
measure_tools <- function(agent, layer) {
  tools <- agent$get_tools()
  measures <- layer_measures(layer)
  if (!"call_measure" %in% names(tools) || length(measures) == 0) {
    log_warn("World Bank measures not registered as tools: commons' semantic layer changed")
    return(agent)
  }
  for (measure in measures) {
    tool <- measure
    S7_data(tool) <- forward_measure(tools$call_measure, measure@name)
    # The schema lists every argument as required, so say which ones are optional
    for (name in names(tool@arguments@properties)) {
      if (!tool@arguments@properties[[name]]@required) {
        tool@arguments@properties[[name]]@description <- str_c(
          world_bank$optional_argument,
          " ",
          tool@arguments@properties[[name]]@description
        )
      }
    }
    tools[[measure@name]] <- tool
  }
  agent$set_tools(tools)
  invisible(agent)
}
