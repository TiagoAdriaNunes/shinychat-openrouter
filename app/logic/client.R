box::use(
  checkmate[test_null, test_number, test_string],
  coro[async_generator, await_each, yield],
  ellmer[chat_openrouter],
  glue[glue, glue_data],
  logger[log_warn],
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
    for (chunk in await_each(stream)) yield(chunk),
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

# Model id without the ":free" variant suffix, for comparing requested and answering models
base_model_id <- function(id) str_remove(id, ":free$")

#' Stats for the response that just finished: `turn` is the client's last turn after the response,
#' `previous_turn` its last turn before it. ellmer records a failed or stopped response as a partial
#' turn (with no token counts), so a partial or unchanged turn counts as incomplete.
#' `answered_by` is set when OpenRouter reports a different model than `model_id` (e.g. a router).
#' @export
response_stats <- function(model, model_id, seconds, turn, previous_turn) {
  ok <- !test_null(turn) && !identical(turn, previous_turn) && !inherits(turn, "ellmer::AssistantPartialTurn")
  stats <- list(model = model, ok = ok, seconds = seconds)
  if (ok) {
    stats$input_tokens <- turn@tokens[["input"]]
    stats$output_tokens <- turn@tokens[["output"]]
    # OpenRouter's response names the model that answered; providers may append a version suffix
    answered <- turn@json$model
    if (test_string(answered, min.chars = 1) &&
      !str_starts(base_model_id(answered), fixed(base_model_id(model_id)))) {
      stats$answered_by <- answered
    }
  }
  stats
}

# Seconds with one decimal, e.g. 2.43 -> "2.4", 1 -> "1.0"
format_seconds <- function(seconds) format(round(seconds, 1), nsmall = 1)

#' One-line summary of a response's stats, e.g. "Gemma · 2.4 s · 25 in / 186 out tokens · 77 tokens/s",
#' or "Free Models Router → google/gemma-4-31b-it:free · ..." when a router picked the model.
#' @export
format_stats <- function(stats) {
  seconds <- format_seconds(stats$seconds)
  if (!stats$ok) {
    return(as.character(glue("{stats$model} · incomplete after {seconds} s")))
  }
  model <- if (test_null(stats$answered_by)) stats$model else glue("{stats$model} → {stats$answered_by}")
  parts <- c(model, glue("{seconds} s"))
  if (test_number(stats$output_tokens)) {
    parts <- c(parts, glue("{stats$input_tokens} in / {stats$output_tokens} out tokens"))
    if (stats$seconds > 0) parts <- c(parts, glue("{round(stats$output_tokens / stats$seconds)} tokens/s"))
  }
  str_flatten(parts, " · ")
}
