box::use(
  shiny[testServer],
  testthat[expect_equal, expect_match, expect_null, test_that],
  withr[local_envvar],
)

box::use(
  app / modules / chat,
)

test_that("ui() renders the model dropdown with the default model", {
  html <- as.character(chat$ui("chat"))

  expect_match(html, 'id="chat_model"', fixed = TRUE)
  expect_match(html, 'value="openrouter/free"', fixed = TRUE)
  expect_match(html, 'id="chat_stats"', fixed = TRUE)
})

test_that("server() starts with the first (most intelligent) model, switches to listed ones, ignores others", {
  # Dummy key: the client is created but no request is ever sent
  local_envvar(OPENROUTER_API_KEY = "test-key")

  testServer(
    function(input, output, session) {
      result <- chat$server("chat", models = fake_models)
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
