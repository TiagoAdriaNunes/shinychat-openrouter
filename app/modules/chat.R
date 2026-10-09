box::use(
  bslib[input_dark_mode, toolbar],
  checkmate[test_null, test_string],
  logger[log_error, log_info, log_warn],
  shiny[
    getDefaultReactiveDomain, icon, observeEvent, reactive, reactiveVal, renderText, req,
    showNotification, tagList, tags, textOutput, updateRadioButtons, updateSelectInput,
  ],
  shinychat[chat_server, page_chat],
  stringr[str_c, str_detect],
)

box::use(
  .. / logic / agent[new_agent, wdi_source],
  .. / logic / config[
    app_title, chat_mode_label, disclaimer, fallback_model, fallback_model_label, greeting, models_page_url,
    placeholder, show_response_stats, source_url, system_prompt, world_bank,
  ],
  .. / logic / client[format_stats, new_client, response_stats],
  .. / logic / models[free_models],
  .. / logic / theme[chat_theme],
  .. / logic / wdi[wdi_panel],
)

external_link <- function(href, ...) {
  tags$a(href = href, target = "_blank", rel = "noopener noreferrer", ...)
}

mode_input_id <- function(id) str_c(id, "_mode")

# "Chat" / "World Bank data" pills under the message box: a Shiny radio input (value "chat" or
# "world_bank") drawn as Bootstrap toggle buttons, so the mode is visible where people type
mode_tabs <- function(id) {
  input_id <- mode_input_id(id)
  option <- function(value, label, icon_name, checked = FALSE) {
    button_id <- str_c(input_id, "_", value)
    tagList(
      tags$input(
        type = "radio", class = "btn-check", name = input_id, id = button_id, value = value,
        autocomplete = "off", checked = if (checked) NA
      ),
      tags$label(class = "btn btn-sm btn-outline-primary rounded-pill px-3", `for` = button_id, icon(icon_name), label)
    )
  }
  tags$div(
    id = input_id,
    class = "shiny-input-radiogroup d-flex flex-wrap gap-2",
    role = "radiogroup",
    `aria-label` = "Mode",
    option("chat", chat_mode_label, "comments", checked = TRUE),
    option("world_bank", world_bank$label, "earth-americas")
  )
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
    # Plain chat or the World Bank data agent; switching starts a new conversation
    toolbar_input = toolbar(mode_tabs(id), align = "left"),
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
# `models` is a named character vector of allowed models (label = name, value = id) for plain chat;
# `agent_models` the same for World Bank data mode (models with tool calling). `world_bank_data`
# returns the World Bank panel or NULL, and is only called when that mode is first turned on.
# Returns a list of reactives: `model` (id of the model in use), `world_bank` (TRUE in World Bank
# data mode) and `stats` (last response's stats or NULL).
#' @export
server <- function(id, models = free_models(), agent_models = free_models(tools_only = TRUE),
                   world_bank_data = wdi_panel, session = getDefaultReactiveDomain()) {
  input <- session$input
  output <- session$output
  # Start with the first model: the most intelligent one (or the fallback if the list couldn't be fetched)
  initial_model <- unname(models[[1]])
  updateSelectInput(session, model_input_id(id), choices = models, selected = initial_model)

  stats <- reactiveVal(NULL)
  output[[stats_output_id(id)]] <- renderText({
    if (!test_null(stats())) format_stats(stats())
  })

  agent_mode <- reactiveVal(FALSE)
  current_model <- reactiveVal(initial_model)
  # World Bank data and its duckdb source, loaded the first time the mode is turned on and shared
  # by every agent in this session
  panel <- NULL
  source <- NULL

  mode_models <- function(on) if (on) agent_models else models
  label_for <- function(model, on) {
    choices <- mode_models(on)
    label <- names(choices)[choices == model][1]
    if (test_string(label, min.chars = 1)) label else model
  }

  # One client per mode: changing model calls set_model() on it, which keeps the conversation and,
  # for the World Bank agent, its R session and result handles (a new agent would lose them).
  # Failed requests become a chat message naming the model in use (see app/logic/client.R).
  client_for <- function(model, on) {
    label <- \(model_id) label_for(model_id, on)
    if (on) {
      new_agent(model, label, source)
    } else {
      new_client(model, system_prompt = system_prompt, label = label)
    }
  }

  log_info("Chat client created with model {initial_model}")
  client <- client_for(initial_model, FALSE)
  chat <- chat_server(id, client, history = FALSE)

  # sync = FALSE: by default set_client() copies the old client's turns, system prompt AND tools onto
  # the new one, which would replace the agent's commons prompt and tools with the plain chat's (none).
  # Switching mode starts a new conversation anyway.
  use_client <- function(new_client, model) {
    client <<- new_client
    chat$set_client(client, sync = FALSE)
    current_model(model)
  }

  set_model <- function(model) {
    client$set_model(model)
    current_model(model)
    log_info("Switched model to {model}")
  }

  # A model picked during a response is applied once it finishes, so one turn uses one model
  pending_model <- NULL
  started <- NULL
  observeEvent(chat$status(), {
    if (chat$status() == "streaming") {
      # Time each response from shinychat's status ("streaming" -> "idle") and read its tokens
      # from the client that produced it
      model_id <- client$get_model()
      started <<- list(
        time = Sys.time(), client = client, model_id = model_id,
        label = label_for(model_id, agent_mode()), turn = client$last_turn()
      )
      return()
    }
    if (show_response_stats && !test_null(started)) {
      stats(response_stats(
        model = started$label,
        model_id = started$model_id,
        seconds = as.numeric(difftime(Sys.time(), started$time, units = "secs")),
        turn = started$client$last_turn(),
        previous_turn = started$turn
      ))
    }
    started <<- NULL
    if (!test_null(pending_model)) {
      set_model(pending_model)
      pending_model <<- NULL
    }
  })

  observeEvent(input[[model_input_id(id)]], ignoreInit = TRUE, {
    model <- input[[model_input_id(id)]]
    if (!model %in% mode_models(agent_mode())) {
      log_warn("Ignoring unknown model selection")
      req(FALSE)
    }
    if (chat$status() == "streaming") {
      pending_model <<- if (model == current_model()) NULL else model
      req(FALSE)
    }
    # Also sent back after a mode switch updates the dropdown
    req(model != current_model())
    set_model(model)
  })

  # Plain chat <-> World Bank data. A plain chat can't replay the agent's tool calls, so switching
  # starts a new conversation. The dropdown keeps the model if the new mode lists it.
  observeEvent(input[[mode_input_id(id)]], ignoreInit = TRUE, {
    on <- identical(input[[mode_input_id(id)]], "world_bank")
    req(on != agent_mode())
    revert <- function(message) {
      updateRadioButtons(session, mode_input_id(id), selected = if (agent_mode()) "world_bank" else "chat")
      showNotification(message, type = "warning", session = session)
      req(FALSE)
    }
    if (chat$status() == "streaming") revert(world_bank$busy)

    choices <- mode_models(on)
    model <- if (current_model() %in% choices) current_model() else unname(choices[[1]])
    new_client <- tryCatch(
      {
        if (on && test_null(source)) {
          panel <<- world_bank_data()
          if (test_null(panel)) stop("no data")
          source <<- wdi_source(panel)
        }
        client_for(model, on)
      },
      error = function(e) {
        log_error("World Bank data mode unavailable: {conditionMessage(e)}")
        if (str_detect(conditionMessage(e), "sandbox")) {
          log_error("For local development without an OS sandbox (e.g. Windows), set R_CONFIG_ACTIVE=development")
        }
        NULL
      }
    )
    if (test_null(new_client)) revert(world_bank$unavailable)

    agent_mode(on)
    pending_model <<- NULL
    use_client(new_client, model)
    updateSelectInput(session, model_input_id(id), choices = choices, selected = model)
    # Each mode's greeting (World Bank data mode's has clickable example questions)
    chat$clear()
    chat$set_greeting(if (on) world_bank$greeting else greeting)
    stats(NULL)
    log_info("Switched to {if (on) 'World Bank data' else 'chat'} mode with model {model}")
  })

  # Model request failures are handled (and logged) by the client; this catches anything else.
  # Never log message content.
  observeEvent(chat$last_error(), {
    log_error("Chat response failed: {conditionMessage(chat$last_error())}")
  })

  list(model = reactive(current_model()), world_bank = reactive(agent_mode()), stats = reactive(stats()))
}
