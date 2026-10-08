box::use(
  coro[async_generator, await_each, yield],
  ellmer[Chat, chat_openrouter],
  logger[log_warn],
  R6[R6Class],
)

box::use(
  . / config[error_messages],
)

# HTTP status of a failed request, looking through wrapped (parent) conditions
http_status <- function(cnd) {
  while (!is.null(cnd)) {
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
  gsub("{model}", model, template, fixed = TRUE)
}

# Pass the response through; if the request fails, finish with a friendly message instead of
# letting shinychat show the raw error. Cancelling stops the stream without an error, so it is unaffected.
safe_stream <- async_generator(function(stream, model) {
  failed <- NULL
  tryCatch(
    for (chunk in await_each(stream)) yield(chunk),
    error = function(e) failed <<- e
  )
  if (!is.null(failed)) {
    log_warn("Model {model} failed: {conditionMessage(failed)}")
    yield(paste0("\n\n", error_message(failed, model)))
  }
})

# ellmer Chat whose streamed responses never error; `label` is the model name shown to users
SafeChat <- R6Class(
  "SafeChat",
  inherit = Chat,
  public = list(
    label = NULL,
    stream_async = function(...) {
      safe_stream(super$stream_async(...), self$label %||% self$get_model())
    }
  )
)

#' OpenRouter chat client that turns failed requests into a chat message asking to pick another model.
#' @export
new_client <- function(model, system_prompt, label = model) {
  base <- chat_openrouter(system_prompt = system_prompt, model = model)
  client <- SafeChat$new(provider = base$get_provider(), system_prompt = system_prompt)
  client$label <- label
  client
}
