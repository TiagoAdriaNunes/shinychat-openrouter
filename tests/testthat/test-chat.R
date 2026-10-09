box::use(
  httr2[local_mocked_responses, response],
  shiny[testServer],
  testthat[expect_equal, expect_false, expect_match, expect_null, expect_true, test_that],
  withr[local_envvar, local_options],
)

box::use(
  app / logic / config[system_prompt, world_bank],
  app / logic / wdi[split_panel],
  app / modules / chat,
)

fake_panel <- function() {
  panel <- split_panel(fake_wdi_raw())
  panel$downloaded <- as.Date("2026-10-08")
  panel
}

test_that("ui() renders the model dropdown with the default model", {
  html <- as.character(chat$ui("chat"))

  expect_match(html, 'id="chat_model"', fixed = TRUE)
  expect_match(html, 'value="openrouter/free"', fixed = TRUE)
  expect_match(html, 'id="chat_stats"', fixed = TRUE)
  expect_match(html, 'id="chat_mode"', fixed = TRUE)
  # Mode tabs: a radio input with a pill per mode, plain chat selected
  expect_match(html, 'name="chat_mode" id="chat_mode_chat" value="chat" autocomplete="off" checked', fixed = TRUE)
  expect_match(html, 'value="world_bank"', fixed = TRUE)
  # The chat greeting points to World Bank data mode
  expect_match(html, "World Bank data", fixed = TRUE)
  expect_match(html, 'href="https://github.com/TiagoAdriaNunes/shinychat-openrouter/"', fixed = TRUE)
  expect_match(html, "fa-github", fixed = TRUE)
})

test_that("server() starts with the first (most intelligent) model, switches to listed ones, ignores others", {
  # Dummy key: the client is created but no request is ever sent
  local_envvar(OPENROUTER_API_KEY = "test-key")

  testServer(
    function(input, output, session) {
      result <- chat$server("chat", models = fake_models, agent_models = fake_tool_models)
    },
    {
      expect_equal(result$model(), "b/beta:free")
      expect_null(result$stats())

      # The browser sends the initial value first; it must not trigger a swap
      session$setInputs(chat_model = "b/beta:free")
      session$setInputs(chat_model = "a/alpha:free")
      expect_equal(result$model(), "a/alpha:free")

      session$setInputs(chat_model = "evil/model")
      expect_equal(result$model(), "a/alpha:free")
    }
  )
})

test_that("World Bank data mode loads the data once, lists only tool-calling models and switches back", {
  local_envvar(OPENROUTER_API_KEY = "test-key")
  # Windows has no OS sandbox for commons
  local_options(commons.allow_unsafe_fallback = TRUE)
  loads <- 0
  world_bank_data <- function() {
    loads <<- loads + 1
    fake_panel()
  }

  testServer(
    function(input, output, session) {
      result <- chat$server(
        "chat",
        models = fake_models, agent_models = fake_tool_models, world_bank_data = world_bank_data
      )
    },
    {
      # The browser sends the initial values first
      session$setInputs(chat_model = "b/beta:free", chat_mode = "chat")
      expect_false(result$world_bank())

      # Beta doesn't support tools, so the agent starts with the first tool-calling model
      session$setInputs(chat_mode = "world_bank")
      expect_true(result$world_bank())
      expect_equal(result$model(), "a/alpha:free")

      session$setInputs(chat_model = "c/gamma:free")
      expect_equal(result$model(), "c/gamma:free")
      session$setInputs(chat_model = "b/beta:free")
      expect_equal(result$model(), "c/gamma:free")

      # Gamma is in the plain chat list too, so it stays selected
      session$setInputs(chat_mode = "chat")
      expect_false(result$world_bank())
      expect_equal(result$model(), "c/gamma:free")

      session$setInputs(chat_mode = "world_bank")
      expect_true(result$world_bank())
      expect_equal(loads, 1)
    }
  )
})

test_that("World Bank data mode stays off when the data can't be loaded", {
  local_envvar(OPENROUTER_API_KEY = "test-key")

  testServer(
    function(input, output, session) {
      result <- chat$server(
        "chat",
        models = fake_models, agent_models = fake_tool_models, world_bank_data = function() NULL
      )
    },
    {
      session$setInputs(chat_model = "b/beta:free", chat_mode = "chat")
      session$setInputs(chat_mode = "world_bank")
      expect_false(result$world_bank())
      expect_equal(result$model(), "b/beta:free")
    }
  )
})

test_that("World Bank data mode sends the commons agent's prompt and tools, plain chat sends neither", {
  local_envvar(OPENROUTER_API_KEY = "test-key")
  local_options(commons.allow_unsafe_fallback = TRUE, ellmer_max_tries = 1)
  sent <- list()
  local_mocked_responses(function(req) {
    sent[[length(sent) + 1]] <<- req$body$data
    response(status_code = 503)
  })
  last_request <- function() {
    body <- sent[[length(sent)]]
    list(
      model = body$model,
      tools = vapply(body$tools, \(tool) tool[["function"]]$name, ""),
      system = body$messages[[1]]$content,
      reasoning = body$reasoning
    )
  }

  testServer(
    function(input, output, session) {
      result <- chat$server(
        "chat",
        models = fake_models, agent_models = fake_tool_models, world_bank_data = function() fake_panel()
      )
    },
    {
      ask <- function(text) {
        n <- length(sent)
        session$setInputs(chat_user_input = list(text))
        deadline <- Sys.time() + 10
        while (length(sent) == n && Sys.time() < deadline) {
          later::run_now(0.05)
          session$flushReact()
        }
        # Let the failed response finish before the next step
        for (i in 1:20) {
          later::run_now(0.05)
          session$flushReact()
        }
        last_request()
      }

      session$setInputs(chat_model = "b/beta:free", chat_mode = "chat")
      plain <- ask("Hi")
      expect_equal(length(plain$tools), 0)
      expect_equal(plain$system, system_prompt)
      expect_null(plain$reasoning)

      session$setInputs(chat_mode = "world_bank")
      agent <- ask("GDP of Brazil?")
      expect_true(all(c("search_pool", "call_measure", "run_sql") %in% agent$tools))
      expect_match(agent$system, "## Additional instructions", fixed = TRUE)
      expect_match(agent$system, "Answer only from the data", fixed = TRUE)
      expect_false(grepl(system_prompt, agent$system, fixed = TRUE))
      expect_equal(agent$reasoning$effort, world_bank$reasoning_effort)

      # Changing model keeps the agent
      session$setInputs(chat_model = "c/gamma:free")
      changed <- ask("GDP of Chile?")
      expect_equal(changed$model, "c/gamma:free")
      expect_true("call_measure" %in% changed$tools)

      session$setInputs(chat_mode = "chat")
      back <- ask("Hi again")
      expect_equal(length(back$tools), 0)
      expect_equal(back$system, system_prompt)
    }
  )
})
