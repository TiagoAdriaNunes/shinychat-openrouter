box::use(
  bslib[page_fillable],
)

box::use(
  . / logic / config[app_title],
  . / modules / chat,
)

#' @export
ui <- function() {
  page_fillable(
    title = app_title,
    chat$ui("chat")
  )
}

#' @export
server <- function(input, output, session) {
  chat$server("chat")
}
