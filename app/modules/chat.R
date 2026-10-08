box::use(
  logger[log_error, log_info, log_warn],
  shiny[
    getDefaultReactiveDomain, icon, observeEvent, reactive, reactiveVal, req, tags,
    updateSelectInput,
  ],
  shinychat[chat_server, page_chat],
)

box::use(
  .. / logic / config[
    app_title, disclaimer, greeting, models_page_url, openrouter_model, openrouter_model_label,
    placeholder, system_prompt,
  ],
  .. / logic / client[new_client],
  .. / logic / models[free_models],
  .. / logic / theme[chat_theme],
)

external_link <- function(href, ...) {
  tags$a(href = href, target = "_blank", rel = "noopener noreferrer", ...)
}

model_input_id <- function(id) paste0(id, "_model")

# page_chat() owns the whole page, so this module is not namespaced: the same
# literal `id` is used for the UI and for chat_server().
#' @export
ui <- function(id) {
  page_chat(
    app_title,
    icon = icon("robot"),
    id = id,
    greeting = greeting,
    placeholder = placeholder,
    enable_cancel = TRUE,
    footer = tags$div(
      class = "d-flex flex-wrap align-items-center justify-content-center gap-2",
      tags$label(`for` = model_input_id(id), class = "mb-0", "Model:"),
      # Compact select sized to the footer text; the server fills in the free model list
      tags$select(
        id = model_input_id(id),
        class = "shiny-input-select form-select form-select-sm w-auto py-0",
        style = "font-size: inherit;",
        tags$option(value = openrouter_model, selected = NA, openrouter_model_label)
      ),
      tags$span(
        "via ",
        external_link(models_page_url, "OpenRouter"),
        " · ",
        disclaimer
      )
    ),
    sidebar = FALSE,
    drawer = FALSE,
    theme = chat_theme
  )
}

# Needs OPENROUTER_API_KEY in the environment (e.g. in ~/.Renviron).
# `models` is a named character vector of allowed models (label = name, value = id).
# Returns a reactive with the id of the model currently in use.
#' @export
server <- function(id, models = free_models(), session = getDefaultReactiveDomain()) {
  input <- session$input
  updateSelectInput(session, model_input_id(id), choices = models, selected = openrouter_model)

  # Failed requests become a chat message naming the model (see app/logic/client.R)
  client_for <- function(model) {
    label <- names(models)[models == model][1]
    new_client(model, system_prompt = system_prompt, label = if (is.na(label) || !nzchar(label)) model else label)
  }

  log_info("Chat client created with model {openrouter_model}")
  chat <- chat_server(id, client_for(openrouter_model), history = FALSE)
  current_model <- reactiveVal(openrouter_model)

  # Swap the model mid-conversation; set_client() carries over the turns so far
  observeEvent(input[[model_input_id(id)]], ignoreInit = TRUE, {
    model <- input[[model_input_id(id)]]
    if (!model %in% models) {
      log_warn("Ignoring unknown model selection")
      req(FALSE)
    }
    chat$set_client(client_for(model))
    current_model(model)
    log_info("Switched model to {model}")
  })

  # Model request failures are handled (and logged) by the client; this catches anything else.
  # Never log message content.
  observeEvent(chat$last_error(), {
    log_error("Chat response failed: {conditionMessage(chat$last_error())}")
  })

  reactive(current_model())
}
