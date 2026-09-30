box::use(
  shinychat[page_chat_theme],
)

# Any bslib theme variable can be passed here; see ?shinychat::page_chat_theme
#' @export
chat_theme <- page_chat_theme(
  version = 5,
  primary = "#5b5fc7",
  "border-radius" = "0.75rem",
  "shiny-chat-user-message-border-radius" = "1.25rem",
  "shiny-chat-user-message-padding" = "0.625rem 1rem"
)
