box::use(
  cachem[cache_mem],
  httr2[local_mocked_responses, response, response_json],
  memoise[memoise],
  stringr[str_ends],
  testthat[expect_equal, test_that],
)

box::use(
  app / logic / models[fetch_free_models, fetch_reasoning_options, free_models, nearest_effort, reasoning_options],
)

test_that("free_models() keeps free text-only, non-excluded models, most intelligent first, low priority last", {
  local_mocked_responses(fake_openrouter())

  expect_equal(free_models(fetch = fetch_free_models), fake_models)
})

test_that("free_models(tools_only = TRUE) keeps only models that support tool calling", {
  local_mocked_responses(fake_openrouter())

  expect_equal(free_models(fetch = fetch_free_models, tools_only = TRUE), fake_tool_models)
})

test_that("free_models() leaves out models without full uptime in the last 30 minutes", {
  # Below the minimum, no figure, and a failed lookup
  local_mocked_responses(fake_openrouter(
    uptime = list("b/beta:free" = 99.5, "c/gamma:free" = NULL, "d/delta:free" = NA)
  ))

  expect_equal(
    free_models(fetch = fetch_free_models),
    c("Alpha (free)" = "a/alpha:free", "Ling" = "inclusionai/ling-3.1-flash")
  )
})

test_that("free_models() lists every model when none has full uptime", {
  uptime <- rep(list(50), length(fake_models))
  names(uptime) <- fake_models
  local_mocked_responses(fake_openrouter(uptime = uptime))

  expect_equal(free_models(fetch = fetch_free_models), fake_models)
})

test_that("free_models() falls back to the fallback model when the request fails", {
  local_mocked_responses(function(req) response(status_code = 500))

  expect_equal(free_models(fetch = fetch_free_models), c("Free Models Router" = "openrouter/free"))
})

test_that("free_models() falls back when the response has no free text models", {
  local_mocked_responses(function(req) response_json(body = list(data = list())))

  expect_equal(free_models(fetch = fetch_free_models), c("Free Models Router" = "openrouter/free"))
})

test_that("the memoised fetch reuses the list until it expires", {
  calls <- 0
  openrouter <- fake_openrouter()
  local_mocked_responses(function(req) {
    # Count catalogue requests, not the uptime lookups each one triggers
    if (!str_ends(req$url, "/endpoints")) {
      calls <<- calls + 1
    }
    openrouter(req)
  })
  cached <- memoise(fetch_free_models, cache = cache_mem(max_age = 1))

  free_models(fetch = cached)
  free_models(fetch = cached)
  expect_equal(calls, 1)

  Sys.sleep(1.5)
  free_models(fetch = cached)
  expect_equal(calls, 2)
})

test_that("the memoised fetch does not cache failures", {
  status <- 500
  openrouter <- fake_openrouter()
  local_mocked_responses(function(req) {
    if (status == 500) response(status_code = 500) else openrouter(req)
  })
  cached <- memoise(fetch_free_models, cache = cache_mem(max_age = 3600))

  expect_equal(free_models(fetch = cached), c("Free Models Router" = "openrouter/free"))

  status <- 200
  expect_equal(free_models(fetch = cached), fake_models)
})

test_that("reasoning_options() lists each model's effort levels and default, or none when it can't fetch them", {
  local_mocked_responses(fake_openrouter())
  expect_equal(reasoning_options(fetch = fetch_reasoning_options), fake_efforts)

  expect_equal(reasoning_options(fetch = function() stop("offline")), list())
})

test_that("nearest_effort() picks the offered level OpenRouter maps an effort to", {
  expect_equal(nearest_effort("low", c("high", "medium", "low")), "low")
  expect_equal(nearest_effort("low", c("high", "medium")), "medium")
  # A tie goes to the higher level
  expect_equal(nearest_effort("medium", c("high", "low")), "high")
})
