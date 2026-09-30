box::use(
  ellmer[chat_openrouter],
  logger[log_error, log_info],
  shiny[icon, observeEvent],
  shinychat[chat_server, page_chat],
)

box::use(
  .. / logic / config[app_title, greeting, openrouter_model, system_prompt],
  .. / logic / theme[chat_theme],
)

# page_chat() owns the whole page, so this module is not namespaced: the same
# literal `id` is used for the UI and for chat_server().
#' @export
ui <- function(id) {
  page_chat(
    app_title,
    icon = icon("robot"),
    id = id,
    greeting = greeting,
    placeholder = "Ask me anything...",
    enable_cancel = TRUE,
    sidebar = FALSE,
    drawer = FALSE,
    theme = chat_theme
  )
}

# Needs OPENROUTER_API_KEY in the environment (e.g. in ~/.Renviron).
#' @export
server <- function(id) {
  client <- chat_openrouter(
    system_prompt = system_prompt,
    model = openrouter_model
  )

  log_info("Chat client created with model {openrouter_model}")

  chat <- chat_server(id, client, history = FALSE)

  # Log failed responses (e.g. rate limit, quota, dropped connection); never log message content
  observeEvent(chat$last_error(), {
    log_error("Chat response failed: {conditionMessage(chat$last_error())}")
  })
}
