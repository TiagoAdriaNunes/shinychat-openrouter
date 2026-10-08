box::use(
  coro[async_generator, await, yield],
  ellmer[AssistantTurn, ContentText],
  httr2[local_mocked_responses, req_perform, request, response],
  promises[promise_resolve],
  rlang[error_cnd],
  testthat[expect_equal, expect_false, expect_null, expect_s3_class, expect_true, test_that],
  withr[local_envvar, local_options],
)

box::use(
  app / logic / client[error_message, format_stats, new_client, response_stats, safe_stream],
  app / logic / config[error_messages],
)

http_error <- function(status) {
  local_mocked_responses(function(req) response(status_code = status))
  tryCatch(req_perform(request("https://example.com")), error = identity)
}

# Independent of glue: plain substitution of the {model} placeholder
expected <- function(template, model) gsub("{model}", model, template, fixed = TRUE)

# Stand-in for an ellmer response stream (httr2 can't mock streaming responses)
fake_stream <- async_generator(function(chunks) {
  for (chunk in chunks) {
    await(promise_resolve(NULL))
    yield(chunk)
  }
})

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

test_that("a rate-limited model streams the friendly message and its stats show it as incomplete", {
  local_envvar(OPENROUTER_API_KEY = "test-key")
  local_options(ellmer_max_tries = 1) # no retries/backoff in tests
  local_mocked_responses(function(req) response(status_code = 429))

  client <- new_client("google/gemma:free", system_prompt = "Be brief.", label = "Gemma (free)")
  expect_s3_class(client, "Chat")

  text <- collect_stream(client$stream_async("Hi", stream = "content"))
  expect_equal(text, paste0("\n\n", expected(error_messages$rate_limited, "Gemma (free)")))

  # ellmer records the failed request as a partial turn
  stats <- response_stats("Gemma (free)", seconds = 8, turn = client$last_turn(), previous_turn = NULL)
  expect_false(stats$ok)
  expect_equal(format_stats(stats), "Gemma (free) · incomplete after 8.0 s")
})

test_that("safe_stream() passes a reply through without extra chunks (e.g. a trailing TRUE)", {
  # collect_stream() fails on any chunk that isn't text, thinking or NULL
  expect_equal(collect_stream(safe_stream(fake_stream(list("Hello", " there")), "Alpha")), "Hello there")
})

test_that("response_stats() reads tokens from a new turn and flags an unchanged turn as incomplete", {
  old <- AssistantTurn(list(ContentText("Earlier")), tokens = c(input = 5, output = 7, cached_input = 0))
  new <- AssistantTurn(list(ContentText("Hi")), tokens = c(input = 25, output = 186, cached_input = 0))

  ok <- response_stats("Gemma", seconds = 2.43, turn = new, previous_turn = old)
  expect_true(ok$ok)
  expect_equal(c(ok$input_tokens, ok$output_tokens), c(25, 186))

  expect_false(response_stats("Gemma", seconds = 8, turn = old, previous_turn = old)$ok)
  expect_false(response_stats("Gemma", seconds = 8, turn = NULL, previous_turn = NULL)$ok)
})

test_that("format_stats() summarises a response on one line", {
  ok <- list(model = "Gemma", ok = TRUE, seconds = 2.43, input_tokens = 25, output_tokens = 186)
  expect_equal(format_stats(ok), "Gemma · 2.4 s · 25 in / 186 out tokens · 77 tokens/s")

  expect_equal(format_stats(list(model = "Gemma", ok = TRUE, seconds = 1)), "Gemma · 1.0 s")
  expect_equal(format_stats(list(model = "Gemma", ok = FALSE, seconds = 8.06)), "Gemma · incomplete after 8.1 s")
})
