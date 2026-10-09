box::use(
  shiny[testServer],
  testthat[expect_equal, expect_false, expect_match, expect_null, expect_true, test_that],
  withr[local_envvar, local_options],
)

box::use(
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
  expect_match(html, 'id="chat_world_bank"', fixed = TRUE)
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
      session$setInputs(chat_model = "b/beta:free", chat_world_bank = FALSE)
      expect_false(result$world_bank())

      # Beta doesn't support tools, so the agent starts with the first tool-calling model
      session$setInputs(chat_world_bank = TRUE)
      expect_true(result$world_bank())
      expect_equal(result$model(), "a/alpha:free")

      session$setInputs(chat_model = "c/gamma:free")
      expect_equal(result$model(), "c/gamma:free")
      session$setInputs(chat_model = "b/beta:free")
      expect_equal(result$model(), "c/gamma:free")

      # Gamma is in the plain chat list too, so it stays selected
      session$setInputs(chat_world_bank = FALSE)
      expect_false(result$world_bank())
      expect_equal(result$model(), "c/gamma:free")

      session$setInputs(chat_world_bank = TRUE)
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
      session$setInputs(chat_model = "b/beta:free", chat_world_bank = FALSE)
      session$setInputs(chat_world_bank = TRUE)
      expect_false(result$world_bank())
      expect_equal(result$model(), "b/beta:free")
    }
  )
})
