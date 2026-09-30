library(shiny)
library(shinychat)
library(ellmer)

# Needs OPENROUTER_API_KEY in the environment (e.g. in ~/.Renviron).
# Free stealth model (1M context, multimodal input). Alternative: "openrouter/free",
# a router that picks a random free model per request.
openrouter_model <- "stealth/space-bunny-alpha"

ui <- page_chat(
  id = "chat",
  title = "shinychat + OpenRouter",
  sidebar = FALSE
)

server <- function(input, output, session) {
  chat <- chat_openrouter(
    system_prompt = "You are a helpful assistant.",
    model = openrouter_model
  )

  chat_server("chat", chat, history = FALSE)
}

shinyApp(ui, server)
