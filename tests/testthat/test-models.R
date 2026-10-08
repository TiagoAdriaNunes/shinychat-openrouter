box::use(
  cachem[cache_mem],
  httr2[local_mocked_responses, response, response_json],
  memoise[memoise],
  testthat[expect_equal, test_that],
)

box::use(
  app / logic / models[fetch_free_models, free_models],
)

test_that("free_models() keeps free text-only, non-excluded models, default first, then by intelligence", {
  local_mocked_responses(function(req) response_json(body = fake_catalogue))

  expect_equal(free_models(fetch = fetch_free_models), fake_models)
})

test_that("free_models() falls back to the default model when the request fails", {
  local_mocked_responses(function(req) response(status_code = 500))

  expect_equal(free_models(fetch = fetch_free_models), c("Free Models Router" = "openrouter/free"))
})

test_that("free_models() falls back when the response has no free text models", {
  local_mocked_responses(function(req) response_json(body = list(data = list())))

  expect_equal(free_models(fetch = fetch_free_models), c("Free Models Router" = "openrouter/free"))
})

test_that("the memoised fetch reuses the list until it expires", {
  calls <- 0
  local_mocked_responses(function(req) {
    calls <<- calls + 1
    response_json(body = fake_catalogue)
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
  local_mocked_responses(function(req) {
    if (status == 500) response(status_code = 500) else response_json(body = fake_catalogue)
  })
  cached <- memoise(fetch_free_models, cache = cache_mem(max_age = 3600))

  expect_equal(free_models(fetch = cached), c("Free Models Router" = "openrouter/free"))

  status <- 200
  expect_equal(free_models(fetch = cached), fake_models)
})
