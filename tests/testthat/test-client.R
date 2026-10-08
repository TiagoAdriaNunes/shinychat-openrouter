box::use(
  httr2[local_mocked_responses, req_perform, request, response],
  rlang[error_cnd],
  testthat[expect_equal, expect_s3_class, test_that],
  withr[local_envvar, local_options],
)

box::use(
  app / logic / client[error_message, new_client],
  app / logic / config[error_messages],
)

http_error <- function(status) {
  local_mocked_responses(function(req) response(status_code = status))
  tryCatch(req_perform(request("https://example.com")), error = identity)
}

expected <- function(template, model) gsub("{model}", model, template, fixed = TRUE)

test_that("error_message() picks the message for the HTTP status", {
  expect_equal(error_message(http_error(429), "Gemma"), expected(error_messages$rate_limited, "Gemma"))
  expect_equal(error_message(http_error(403), "Inkling"), expected(error_messages$unavailable, "Inkling"))
  expect_equal(error_message(http_error(404), "Gone"), expected(error_messages$unavailable, "Gone"))
  expect_equal(error_message(http_error(500), "Down"), expected(error_messages$failed, "Down"))
  expect_equal(error_message(simpleError("timeout"), "Slow"), expected(error_messages$failed, "Slow"))
})

test_that("error_message() finds the HTTP status in a wrapped error", {
  wrapped <- error_cnd(message = "Stream failed", parent = http_error(429))

  expect_equal(error_message(wrapped, "Gemma"), expected(error_messages$rate_limited, "Gemma"))
})

test_that("a rate-limited model streams the friendly message instead of erroring", {
  local_envvar(OPENROUTER_API_KEY = "test-key")
  local_options(ellmer_max_tries = 1) # no retries/backoff in tests
  local_mocked_responses(function(req) response(status_code = 429))

  client <- new_client("google/gemma:free", system_prompt = "Be brief.", label = "Gemma (free)")
  expect_s3_class(client, "Chat")

  text <- collect_stream(client$stream_async("Hi", stream = "content"))
  expect_equal(text, paste0("\n\n", expected(error_messages$rate_limited, "Gemma (free)")))
})
