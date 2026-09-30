box::use(
  ellmer[chat_openrouter],
  logger[log_error, log_info],
  shiny[icon, observeEvent, tags],
  shinychat[chat_server, page_chat],
)

box::use(
  .. / logic / config[app_title, disclaimer, greeting, openrouter_model, system_prompt],
  .. / logic / theme[chat_theme],
)

external_link <- function(href, ...) {
  tags$a(href = href, target = "_blank", rel = "noopener noreferrer", ...)
}

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
    footer = tags$span(
      "Model: ",
      external_link(paste0("https://openrouter.ai/", openrouter_model), tags$code(openrouter_model)),
      " via ",
      external_link("https://openrouter.ai", "OpenRouter"),
      " · ",
      disclaimer
    ),
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
