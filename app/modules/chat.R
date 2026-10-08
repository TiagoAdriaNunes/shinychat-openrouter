box::use(
  bslib[input_dark_mode, toolbar],
  checkmate[test_null, test_string],
  logger[log_error, log_info, log_warn],
  shiny[
    getDefaultReactiveDomain, icon, observeEvent, reactive, reactiveVal, renderText, req, tags,
    textOutput, updateSelectInput,
  ],
  shinychat[chat_server, page_chat],
  stringr[str_c],
)

box::use(
  .. / logic / config[
    app_title, disclaimer, fallback_model, fallback_model_label, greeting, models_page_url,
    placeholder, show_response_stats, source_url, system_prompt,
  ],
  .. / logic / client[format_stats, new_client, response_stats],
  .. / logic / models[free_models],
  .. / logic / theme[chat_theme],
)

external_link <- function(href, ...) {
  tags$a(href = href, target = "_blank", rel = "noopener noreferrer", ...)
}

model_input_id <- function(id) str_c(id, "_model")
stats_output_id <- function(id) str_c(id, "_stats")

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
    # No file uploads for now (also hides the "+" button in the input)
    allow_attachments = FALSE,
    footer = tags$div(
      class = "d-flex flex-wrap align-items-center justify-content-center gap-2",
      # Last response's timings and tokens, on its own row (filled in by the server)
      if (show_response_stats) {
        tags$div(class = "w-100 text-center text-body-secondary", textOutput(stats_output_id(id), inline = TRUE))
      },
      tags$label(`for` = model_input_id(id), class = "mb-0", "Model:"),
      # Compact select sized to the footer text; the server replaces this placeholder option with
      # the free model list
      tags$select(
        id = model_input_id(id),
        class = "shiny-input-select form-select form-select-sm w-auto py-0",
        style = "font-size: inherit;",
        tags$option(value = fallback_model, selected = NA, fallback_model_label)
      ),
      tags$span(
        "via ",
        external_link(models_page_url, "OpenRouter"),
        " · ",
        disclaimer
      )
    ),
    # Top bar: link to the source code next to shinychat's default dark mode toggle
    toolbar_global = toolbar(
      external_link(
        source_url,
        class = "btn btn-link nav-link px-1",
        title = "Source code on GitHub",
        `aria-label` = "Source code on GitHub",
        icon("github")
      ),
      input_dark_mode()
    ),
    sidebar = FALSE,
    drawer = FALSE,
    theme = chat_theme
  )
}

# Needs OPENROUTER_API_KEY in the environment (e.g. in ~/.Renviron).
# `models` is a named character vector of allowed models (label = name, value = id).
# Returns a list of reactives: `model` (id of the model in use) and `stats` (last response's stats or NULL).
#' @export
server <- function(id, models = free_models(), session = getDefaultReactiveDomain()) {
  input <- session$input
  output <- session$output
  # Start with the first model: the most intelligent one (or the fallback if the list couldn't be fetched)
  initial_model <- unname(models[[1]])
  updateSelectInput(session, model_input_id(id), choices = models, selected = initial_model)

  stats <- reactiveVal(NULL)
  output[[stats_output_id(id)]] <- renderText({
    if (!test_null(stats())) format_stats(stats())
  })

  # Failed requests become a chat message naming the model (see app/logic/client.R)
  client_for <- function(model) {
    label <- names(models)[models == model][1]
    new_client(model, system_prompt = system_prompt, label = if (test_string(label, min.chars = 1)) label else model)
  }

  log_info("Chat client created with model {initial_model}")
  client <- client_for(initial_model)
  chat <- chat_server(id, client, history = FALSE)
  current_model <- reactiveVal(initial_model)

  # Time each response from shinychat's status ("streaming" -> "idle") and read its tokens from the
  # client that produced it (a model switch during a response is applied after it finishes)
  if (show_response_stats) {
    started <- NULL
    observeEvent(chat$status(), {
      if (chat$status() == "streaming") {
        started <<- list(time = Sys.time(), client = client, turn = client$last_turn())
      } else if (!test_null(started)) {
        stats(response_stats(
          model = started$client$label,
          model_id = started$client$get_model(),
          seconds = as.numeric(difftime(Sys.time(), started$time, units = "secs")),
          turn = started$client$last_turn(),
          previous_turn = started$turn
        ))
        started <<- NULL
      }
    })
  }

  # Swap the model mid-conversation; set_client() carries over the turns so far
  observeEvent(input[[model_input_id(id)]], ignoreInit = TRUE, {
    model <- input[[model_input_id(id)]]
    if (!model %in% models) {
      log_warn("Ignoring unknown model selection")
      req(FALSE)
    }
    client <<- client_for(model)
    chat$set_client(client)
    current_model(model)
    log_info("Switched model to {model}")
  })

  # Model request failures are handled (and logged) by the client; this catches anything else.
  # Never log message content.
  observeEvent(chat$last_error(), {
    log_error("Chat response failed: {conditionMessage(chat$last_error())}")
  })

  list(model = reactive(current_model()), stats = reactive(stats()))
}
