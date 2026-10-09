box::use(
  coro[async_generator, await, yield],
  ellmer[AssistantTurn, ContentText],
  httr2[local_mocked_responses, req_perform, request, response],
  promises[promise_resolve],
  rlang[error_cnd],
  stringr[fixed, str_c, str_replace_all],
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
expected <- function(template, model) str_replace_all(template, fixed("{model}"), model)

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

  # Called the way shinychat does, splicing the message with !!!
  text <- collect_stream(client$stream_async(!!!list("Hi"), stream = "content"))
  expect_equal(text, str_c("\n\n", expected(error_messages$rate_limited, "Gemma (free)")))

  # ellmer records the failed request as a partial turn
  stats <- response_stats(
    "Gemma (free)", "google/gemma:free", seconds = 8, turn = client$last_turn(), previous_turn = NULL
  )
  expect_false(stats$ok)
  expect_equal(format_stats(stats), "Gemma (free) · incomplete after 8.0 s")
})

test_that("a client whose model changes names the current model when it fails", {
  local_envvar(OPENROUTER_API_KEY = "test-key")
  local_options(ellmer_max_tries = 1)
  local_mocked_responses(function(req) response(status_code = 429))
  labels <- c("google/gemma:free" = "Gemma (free)", "a/alpha:free" = "Alpha (free)")

  client <- new_client("google/gemma:free", system_prompt = "Be brief.", label = \(model) labels[[model]])
  client$set_model("a/alpha:free")

  # Called the way shinychat does, splicing the message with !!!
  text <- collect_stream(client$stream_async(!!!list("Hi"), stream = "content"))
  expect_equal(text, str_c("

", expected(error_messages$rate_limited, "Alpha (free)")))
})

test_that("safe_stream() passes a reply through without extra chunks (e.g. a trailing TRUE)", {
  # collect_stream() fails on any chunk that isn't text, thinking or NULL
  expect_equal(collect_stream(safe_stream(fake_stream(list("Hello", " there")), "Alpha")), "Hello there")
})

test_that("response_stats() reads tokens from a new turn and flags an unchanged turn as incomplete", {
  old <- AssistantTurn(list(ContentText("Earlier")), tokens = c(input = 5, output = 7, cached_input = 0))
  new <- AssistantTurn(list(ContentText("Hi")), tokens = c(input = 25, output = 186, cached_input = 0))

  ok <- response_stats("Gemma", "google/gemma:free", seconds = 2.43, turn = new, previous_turn = old)
  expect_true(ok$ok)
  expect_equal(c(ok$input_tokens, ok$output_tokens), c(25, 186))
  expect_null(ok$answered_by)

  expect_false(response_stats("Gemma", "google/gemma:free", seconds = 8, turn = old, previous_turn = old)$ok)
  expect_false(response_stats("Gemma", "google/gemma:free", seconds = 8, turn = NULL, previous_turn = NULL)$ok)
})

test_that("response_stats() names the model that answered when it differs from the one requested", {
  answered <- function(model) {
    AssistantTurn(list(ContentText("Hi")), json = list(model = model), tokens = c(input = 1, output = 2, cached_input = 0))
  }

  routed <- response_stats("Free Models Router", "openrouter/free", 1, answered("google/gemma-4-31b-it:free"), NULL)
  expect_equal(routed$answered_by, "google/gemma-4-31b-it:free")

  # Same model, reported without ":free" or with a version suffix: not shown
  expect_null(response_stats("Gemma", "google/gemma:free", 1, answered("google/gemma"), NULL)$answered_by)
  expect_null(response_stats("Gemma", "google/gemma:free", 1, answered("google/gemma-20260402"), NULL)$answered_by)
})

test_that("format_stats() summarises a response on one line", {
  ok <- list(model = "Gemma", ok = TRUE, seconds = 2.43, input_tokens = 25, output_tokens = 186)
  expect_equal(format_stats(ok), "Gemma · 2.4 s · 25 in / 186 out tokens · 77 tokens/s")

  routed <- modifyList(ok, list(model = "Free Models Router", answered_by = "google/gemma-4-31b-it:free"))
  expect_equal(
    format_stats(routed),
    "Free Models Router → google/gemma-4-31b-it:free · 2.4 s · 25 in / 186 out tokens · 77 tokens/s"
  )

  expect_equal(format_stats(list(model = "Gemma", ok = TRUE, seconds = 1)), "Gemma · 1.0 s")
  expect_equal(format_stats(list(model = "Gemma", ok = FALSE, seconds = 8.06)), "Gemma · incomplete after 8.1 s")
})
