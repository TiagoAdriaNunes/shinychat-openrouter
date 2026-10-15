box::use(
  checkmate[test_null],
  coro[async, await_each],
  ellmer[ContentText, ContentThinking, ContentToolRequest, ContentToolResult],
  later[run_now],
  promises[then],
  S7[S7_inherits],
  stringr[str_flatten],
)

# Drain an ellmer/coro async generator into a single string, running the event loop until it finishes
collect_stream <- function(stream, timeout = 10) {
  out <- character()
  done <- FALSE
  failure <- NULL
  drain <- async(function() {
    for (chunk in await_each(stream)) {
      # Keep text; skip NULL (ignored by shinychat), thinking chunks from reasoning models and tool calls;
      # fail on anything else (e.g. a stray TRUE would be shown at the end of the reply)
      if (test_null(chunk)) {
        next
      } else if (is.character(chunk)) {
        out <<- c(out, chunk)
      } else if (S7_inherits(chunk, ContentText)) {
        out <<- c(out, chunk@text)
      } else if (inherits(chunk, c("shiny.tag", "shiny.tag.list"))) {
        # HTML shown in the reply (a chart measure's chart)
        out <<- c(out, "[html]")
      } else if (
        !(S7_inherits(chunk, ContentThinking) ||
          S7_inherits(chunk, ContentToolRequest) ||
          S7_inherits(chunk, ContentToolResult))
      ) {
        stop("Unexpected stream chunk of class ", str_flatten(class(chunk), "/"))
      }
    }
  })
  then(
    drain(),
    onFulfilled = function(value) done <<- TRUE,
    onRejected = function(e) {
      failure <<- e
      done <<- TRUE
    }
  )
  deadline <- Sys.time() + timeout
  while (!done && Sys.time() < deadline) {
    run_now(0.05)
  }
  if (!test_null(failure)) {
    stop(failure)
  }
  if (!done) {
    stop("Stream did not finish in time")
  }
  str_flatten(out)
}
