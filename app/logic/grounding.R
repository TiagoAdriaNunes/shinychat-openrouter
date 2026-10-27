# Wrappers around the agent's streamed replies: answers only from looked-up data, never a silent
# end, and charts in the reply (see new_agent() in agent.R). Like safe_stream() in client.R, every
# async generator body must end with NULL: coro emits the final value as a chunk.

box::use(
  checkmate[test_null],
  coro[async_generator, await_each, yield],
  ellmer[AssistantTurn, ContentThinking, ContentToolRequest, ContentToolResult],
  logger[log_info, log_warn],
  purrr[some],
  rlang[exec, list2],
  S7[S7_inherits],
  stringr[str_detect],
)

box::use(
  . / client[chunk_text, model_message, set_api_arg, wrap_stream],
  . / config[world_bank],
)

is_tool_request <- function(chunk) S7_inherits(chunk, ContentToolRequest)
is_thinking <- function(chunk) S7_inherits(chunk, ContentThinking)

# A call to one of `data_tools`, the tools that read the data (a search for measures or notes doesn't)
is_data_request <- function(chunk, data_tools) is_tool_request(chunk) && chunk@name %in% data_tools

# Log the tools an agent calls: names only, never content
log_tool_call <- function(chunk, model) {
  if (is_tool_request(chunk)) {
    log_info("World Bank agent ({model}) called {chunk@name}")
  }
}

# Whether the user stopped the reply (shinychat's stream controller, in `args$options`)
is_cancelled <- function(args) isTRUE(args$options$controller$cancelled)

# Split stream_async() arguments into the message contents (unnamed) and options (named)
split_args <- function(args) {
  named <- names(args) %||% rep("", length(args))
  list(inputs = args[named == ""], options = args[named != ""])
}

#' TRUE if `turns` contain a successful result of one of `data_tools` (the tools that read the data):
#' data already looked up in this conversation. A search for measures or notes doesn't count.
#' @export
has_lookup <- function(turns, data_tools) {
  some(turns, function(turn) {
    some(turn@contents, function(content) {
      S7_inherits(content, ContentToolResult) &&
        test_null(content@error) &&
        is_data_request(content@request, data_tools)
    })
  })
}

#' Make the first request for each new question require a tool call (`tool_choice = "required"`), so
#' the model looks the data up instead of answering from memory. Later requests in the tool loop, and
#' questions in a conversation that already holds a lookup (has_lookup() with `data_tools`), go
#' without it so the model can answer. Set from $on_request_start(), which runs before each request
#' is built.
#' @export
require_lookup <- function(agent, data_tools) {
  agent$on_request_start(function(turns) {
    question <- !some(turns[[length(turns)]]@contents, \(content) S7_inherits(content, ContentToolResult))
    set_api_arg(agent, "tool_choice", if (question && !has_lookup(turns, data_tools)) "required")
  })
  invisible(agent)
}

#' Stream an agent reply only if it looks the data up:
#' 1. The reply's text is held back until the model calls one of `data_tools`, the tools that read the
#'    data (thinking still streams; a search for measures or notes isn't a lookup).
#' 2. A reply that ends without calling one is dropped (the turns are restored) and the question asked
#'    again with `world_bank$lookup_nudge`; if the retry also skips the data, `world_bank$no_lookup` is
#'    shown instead.
#' 3. A reply that looked the data up but wrote no answer is asked once with `world_bank$answer_nudge`,
#'    then `world_bank$no_answer` is shown.
#' Once the conversation holds data looked up earlier, replies pass through: a follow-up can rightly
#' be answered from it. A stopped reply isn't retried; a failed request errors out to safe_stream().
#' `original` is the agent's own stream_async(), `args` its arguments, `model` the name for messages.
#' @export
grounded_stream <- async_generator(function(original, agent, args, model, data_tools) {
  args <- split_args(args)
  before <- agent$get_turns()
  earlier <- has_lookup(before, data_tools)
  answered <- FALSE
  cancelled <- FALSE
  if (earlier) {
    for (chunk in await_each(exec(original, !!!args$inputs, !!!args$options))) {
      log_tool_call(chunk, model)
      answered <- answer_after(answered, chunk, looked_up = TRUE)
      yield(chunk)
    }
    cancelled <- is_cancelled(args)
  }
  # (coro generators can't assign the result of an `if` expression)
  attempts <- list(args$inputs, c(args$inputs, list(world_bank$lookup_nudge)))
  if (earlier) {
    attempts <- list()
  }
  looked_up <- earlier
  for (inputs in attempts) {
    if (!looked_up && !cancelled) {
      agent$set_turns(before)
      held <- list()
      answered <- FALSE
      for (chunk in await_each(exec(original, !!!inputs, !!!args$options))) {
        log_tool_call(chunk, model)
        if (!looked_up && is_data_request(chunk, data_tools)) {
          looked_up <- TRUE
          for (text in held) {
            yield(text)
          }
        }
        answered <- answer_after(answered, chunk, looked_up)
        if (looked_up || is_thinking(chunk)) {
          yield(chunk)
        } else {
          held <- c(held, list(chunk))
        }
      }
      cancelled <- is_cancelled(args)
      if (!looked_up && !cancelled) {
        log_warn("World Bank agent ({model}) answered without looking anything up; reply dropped")
      }
    }
  }
  if (!looked_up && !cancelled) {
    agent$set_turns(before)
    yield(model_message(world_bank$no_lookup, model))
  }
  if (looked_up && !answered && !cancelled) {
    log_warn("World Bank agent ({model}) wrote no answer after its lookup; asking for it")
    drop_empty_reply(agent)
    for (chunk in await_each(exec(original, world_bank$answer_nudge, !!!args$options))) {
      answered <- answer_after(answered, chunk, looked_up = TRUE)
      yield(chunk)
    }
    if (!answered && !is_cancelled(args)) {
      log_warn("World Bank agent ({model}) still wrote no answer")
      yield(model_message(world_bank$no_answer, model))
    }
  }
  NULL
})

#' Whether a reply has an answer: `answered` so far, updated with the next streamed `chunk`, once the
#' data has been looked up (`looked_up`). Non-blank text or a chart counts; text before the first
#' lookup doesn't. Later tool calls don't undo an answer already shown, or a model that re-checks its
#' results would be asked again and repeat itself.
#' @export
answer_after <- function(answered, chunk, looked_up) {
  text <- chunk_text(chunk)
  answered || (looked_up && (inherits(chunk, "shiny.tag") || (!test_null(text) && str_detect(text, "\\S"))))
}

#' Remove the agent's last turn when it is a reply without text or tool calls (e.g. only thinking):
#' some providers reject an empty reply in the next request.
#' @export
drop_empty_reply <- function(agent) {
  turns <- agent$get_turns()
  if (length(turns) == 0) {
    return(invisible(agent))
  }
  last <- turns[[length(turns)]]
  has_content <- some(last@contents, function(content) {
    is_tool_request(content) || answer_after(FALSE, content, looked_up = TRUE)
  })
  if (S7_inherits(last, AssistantTurn) && !has_content) {
    agent$set_turns(turns[-length(turns)])
  }
  invisible(agent)
}

#' Pass an agent's `stream` through, adding each chart measure's chart (its tool result's
#' `extra$chart`) to the reply itself, right after the tool's card.
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

#' Make `agent$stream_async()` go through grounded_stream(). `label` is the model name for messages,
#' or a function of the model id (see guard_stream()); `data_tools` the names of the tools that read
#' the data.
#' @export
ground_agent <- function(agent, label, data_tools) {
  # list2(), not list(): shinychat calls stream_async(!!!user_input, ...), and only rlang's dots
  # splice `!!!`
  wrap_stream(agent, function(original, ...) {
    name <- if (is.function(label)) label(agent$get_model()) else label
    grounded_stream(original, agent, list2(...), name, data_tools)
  })
}
