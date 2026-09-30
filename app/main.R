box::use(
  logger[log_info],
)

box::use(
  . / modules / chat,
)

#' @export
ui <- function() {
  chat$ui("chat")
}

#' @export
server <- function(input, output, session) {
  session_id <- substr(session$token, 1, 8)
  log_info("Session {session_id} started")
  session$onSessionEnded(function() {
    log_info("Session {session_id} ended")
  })

  chat$server("chat")
}
