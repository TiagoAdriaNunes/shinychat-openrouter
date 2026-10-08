# Drain an ellmer/coro async generator into a single string, running the event loop until it finishes
collect_stream <- function(stream, timeout = 10) {
  out <- character()
  done <- FALSE
  failure <- NULL
  drain <- coro::async(function() {
    for (chunk in coro::await_each(stream)) {
      # Keep text; skip NULL (ignored by shinychat) and thinking chunks from reasoning models;
      # fail on anything else (e.g. a stray TRUE would be shown at the end of the reply)
      if (is.null(chunk)) {
        next
      } else if (is.character(chunk)) {
        out <<- c(out, chunk)
      } else if (inherits(chunk, "ellmer::ContentText")) {
        out <<- c(out, chunk@text)
      } else if (!inherits(chunk, "ellmer::ContentThinking")) {
        stop("Unexpected stream chunk of class ", paste(class(chunk), collapse = "/"))
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
  if (!is.null(failure)) stop(failure)
  if (!done) stop("Stream did not finish in time")
  paste(out, collapse = "")
}
