# Drain an ellmer/coro async generator into a single string, running the event loop until it finishes
collect_stream <- function(stream, timeout = 10) {
  out <- character()
  done <- FALSE
  failure <- NULL
  drain <- coro::async(function() {
    for (chunk in coro::await_each(stream)) {
      # Keep text only; reasoning models also stream thinking chunks
      out <<- c(out, if (is.character(chunk)) chunk else if (inherits(chunk, "ellmer::ContentText")) chunk@text)
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
