box::use(
  ellmer[chat_openrouter],
  shiny[moduleServer, NS],
  shinychat[chat_server, chat_ui],
)

box::use(
  .. / logic / config[greeting, openrouter_model, system_prompt],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  chat_ui(
    ns("chat"),
    greeting = greeting,
    enable_cancel = TRUE,
    drawer = FALSE,
    show_history = FALSE
  )
}

# Needs OPENROUTER_API_KEY in the environment (e.g. in ~/.Renviron).
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    client <- chat_openrouter(
      system_prompt = system_prompt,
      model = openrouter_model
    )

    chat_server("chat", client, history = FALSE)
  })
}
