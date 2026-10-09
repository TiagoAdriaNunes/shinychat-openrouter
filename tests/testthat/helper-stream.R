# Drain an ellmer/coro async generator into a single string, running the event loop until it finishes
collect_stream <- function(stream, timeout = 10) {
  out <- character()
  done <- FALSE
  failure <- NULL
  drain <- coro::async(function() {
    for (chunk in coro::await_each(stream)) {
      # Keep text; skip NULL (ignored by shinychat), thinking chunks from reasoning models and tool calls;
      # fail on anything else (e.g. a stray TRUE would be shown at the end of the reply)
      if (checkmate::test_null(chunk)) {
        next
      } else if (is.character(chunk)) {
        out <<- c(out, chunk)
      } else if (inherits(chunk, "ellmer::ContentText")) {
        out <<- c(out, chunk@text)
      } else if (!inherits(chunk, c("ellmer::ContentThinking", "ellmer::ContentToolRequest", "ellmer::ContentToolResult"))) {
        stop("Unexpected stream chunk of class ", stringr::str_flatten(class(chunk), "/"))
      }
    }
  })
  promises::then(
    drain(),
    onFulfilled = function(value) done <<- TRUE,
    onRejected = function(e) {
      failure <<- e
      done <<- TRUE
    }
  )
  deadline <- Sys.time() + timeout
  while (!done && Sys.time() < deadline) later::run_now(0.05)
  if (!checkmate::test_null(failure)) stop(failure)
  if (!done) stop("Stream did not finish in time")
  stringr::str_flatten(out)
}
