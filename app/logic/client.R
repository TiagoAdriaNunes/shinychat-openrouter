box::use(
  checkmate[test_null, test_number, test_string],
  coro[async_generator, await_each, yield],
  ellmer[AssistantPartialTurn, AssistantTurn, chat_openrouter],
  glue[glue, glue_data],
  logger[log_warn],
  purrr[keep, map_dbl],
  S7[S7_inherits],
  stringr[fixed, str_c, str_flatten, str_remove, str_starts],
)

box::use(
  . / config[error_messages],
)

# HTTP status of a failed request, looking through wrapped (parent) conditions
http_status <- function(cnd) {
  while (!test_null(cnd)) {
    if (inherits(cnd, "httr2_http")) {
      return(cnd$status)
    }
    cnd <- cnd$parent
  }
  NA_integer_
}

#' User-facing message for a failed model request (markdown), from the `errors` section of config.yml.
#' @export
error_message <- function(cnd, model) {
  status <- http_status(cnd)
  template <- if (isTRUE(status == 429)) {
    error_messages$rate_limited
  } else if (isTRUE(status %in% c(403, 404))) {
    error_messages$unavailable
  } else {
    error_messages$failed
  }
  # The template only sees {model}, not the app's own objects
  as.character(glue_data(list(model = model), template, .envir = baseenv()))
}

# Pass the response through; if the request fails, finish with a friendly message instead of
# letting shinychat show the raw error. Cancelling stops the stream without an error, so it is unaffected.
# Note: coro emits the generator's final value as one last chunk, so the body must end with an
# expression that returns NULL (shinychat ignores NULL chunks). Don't end it with a call that returns a value.
#' @export
safe_stream <- async_generator(function(stream, model) {
  failed <- NULL
  tryCatch(
    for (chunk in await_each(stream)) {
      yield(chunk)
    },
    error = function(e) failed <<- e
  )
  if (!test_null(failed)) {
    log_warn("Model {model} failed: {conditionMessage(failed)}")
    yield(str_c("\n\n", error_message(failed, model)))
  }
  NULL
})

#' Replace an ellmer Chat's `stream_async(...)` with `wrap(original, ...)`, in place: `wrap` gets
#' the original method and the call's arguments and returns the stream. (Not named `stream_async`:
#' shinychat passes `stream = "content"`, which R would partially match to it.) Works on any Chat,
#' including commons agents, whose class isn't exported and so can't be subclassed.
#' @export
wrap_stream <- function(client, wrap) {
  stream_async <- client$stream_async
  # R6 locks method bindings; unlock just this one to wrap it
  unlockBinding("stream_async", client)
  client$stream_async <- function(...) wrap(stream_async, ...)
  lockBinding("stream_async", client)
  client
}

#' Make an ellmer Chat's streamed responses never error (see safe_stream()). `label` is the model
#' name shown to users, or a function of the model id (for a client whose model changes with
#' `set_model()`).
#' @export
guard_stream <- function(client, label) {
  wrap_stream(client, \(original, ...) {
    safe_stream(original(...), if (is.function(label)) label(client$get_model()) else label)
  })
}

#' OpenRouter chat client that turns failed requests into a chat message asking to pick another model.
#' @export
new_client <- function(model, system_prompt, label = model) {
  guard_stream(chat_openrouter(system_prompt = system_prompt, model = model), label)
}

#' Send the request field `name` = `value` (an `api_args` entry) with an ellmer Chat's next requests,
#' or stop sending it when `value` is NULL. Keeps the conversation. ellmer 0.5 has no setter (api_args
#' are fixed when the client is made and `get_model_object()` returns a copy), so this edits the
#' client's own Model and reapplies it the way set_model() does. Fields stay after set_model().
#' Works inside $on_request_start() callbacks, which run before the request is built.
#' @export
set_api_arg <- function(client, name, value) {
  private <- client$.__enclos_env__$private
  private$model@extra_args[[name]] <- value
  client$set_model(client$get_model())
  invisible(client)
}

#' Send OpenRouter's reasoning effort (`reasoning.effort`, e.g. "high") with an ellmer Chat's next
#' requests, or nothing (the model's default) when `effort` is NULL. The effort stays after
#' set_model(), so reset it when the model changes.
#' @export
set_reasoning_effort <- function(client, effort = NULL) {
  set_api_arg(client, "reasoning", if (test_string(effort, min.chars = 1)) list(effort = effort))
}

#' The assistant turns of a client's last round: every model request of its last response (an agent
#' makes one per step of its tool loop). Empty if the conversation has none.
#' @export
response_turns <- function(client) {
  round <- client$last_round()
  if (test_null(round)) list() else keep(round@response, \(turn) S7_inherits(turn, AssistantTurn))
}

# Reasoning ("thinking") tokens among a turn's output tokens, as OpenRouter reports them; NA if absent
thinking_tokens <- function(turn) {
  tokens <- turn@json$usage$completion_tokens_details$reasoning_tokens
  if (test_number(tokens)) tokens else NA_real_
}

# Model id without the ":free" variant suffix, for comparing requested and answering models
base_model_id <- function(id) str_remove(id, ":free$")

#' Stats for the response that just finished: `turn` is the client's last turn after the response,
#' `previous_turn` its last turn before it, and `turns` all the response's assistant turns (see
#' response_turns()), whose tokens are added up. ellmer records a failed or stopped response as a
#' partial turn (with no token counts), so a partial or unchanged turn counts as incomplete.
#' `answered_by` is set when OpenRouter reports a different model than `model_id` (e.g. a router).
#' @export
response_stats <- function(model, model_id, seconds, turn, previous_turn, turns = list(turn)) {
  ok <- !test_null(turn) && !identical(turn, previous_turn) && !S7_inherits(turn, AssistantPartialTurn)
  stats <- list(model = model, ok = ok, seconds = seconds)
  if (ok) {
    stats$requests <- length(turns)
    stats$input_tokens <- sum(map_dbl(turns, \(t) t@tokens[["input"]]))
    stats$output_tokens <- sum(map_dbl(turns, \(t) t@tokens[["output"]]))
    thinking <- map_dbl(turns, thinking_tokens)
    if (!all(is.na(thinking))) {
      stats$thinking_tokens <- sum(thinking, na.rm = TRUE)
    }
    # OpenRouter's response names the model that answered; providers may append a version suffix
    answered <- turn@json$model
    if (
      test_string(answered, min.chars = 1) &&
        !str_starts(base_model_id(answered), fixed(base_model_id(model_id)))
    ) {
      stats$answered_by <- answered
    }
  }
  stats
}

# Seconds with one decimal, e.g. 2.43 -> "2.4", 1 -> "1.0"
format_seconds <- function(seconds) format(round(seconds, 1), nsmall = 1)

#' One-line summary of a response's stats, e.g. "Gemma · 2.4 s · 25 in / 186 out tokens · 77 tokens/s",
#' or "Free Models Router → google/gemma-4-31b-it:free · ..." when a router picked the model. Adds
#' the number of requests when there were several (an agent's tool loop) and the thinking tokens.
#' @export
format_stats <- function(stats) {
  seconds <- format_seconds(stats$seconds)
  if (!stats$ok) {
    return(as.character(glue("{stats$model} · incomplete after {seconds} s")))
  }
  model <- if (test_null(stats$answered_by)) stats$model else glue("{stats$model} → {stats$answered_by}")
  parts <- c(model, glue("{seconds} s"))
  if (isTRUE(stats$requests > 1)) {
    parts <- c(parts, glue("{stats$requests} requests"))
  }
  if (test_number(stats$output_tokens)) {
    tokens <- glue("{stats$input_tokens} in / {stats$output_tokens} out tokens")
    if (isTRUE(stats$thinking_tokens > 0)) {
      tokens <- glue("{tokens} ({stats$thinking_tokens} thinking)")
    }
    parts <- c(parts, tokens)
    if (stats$seconds > 0) parts <- c(parts, glue("{round(stats$output_tokens / stats$seconds)} tokens/s"))
  }
  str_flatten(parts, " · ")
}
