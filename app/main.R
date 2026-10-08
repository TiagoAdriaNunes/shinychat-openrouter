box::use(
  logger[log_info],
  stringr[str_sub],
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
  session_id <- str_sub(session$token, 1, 8)
  log_info("Session {session_id} started")
  session$onSessionEnded(function() {
    log_info("Session {session_id} ended")
  })

  chat$server("chat")
}
