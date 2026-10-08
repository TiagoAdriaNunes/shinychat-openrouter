box::use(
  testthat[expect_error, expect_invisible, expect_true, test_that],
  withr[local_envvar],
)

box::use(
  app / logic / checks[check_api_key, check_model],
)

test_that("check_api_key() errors when the key is missing", {
  local_envvar(OPENROUTER_API_KEY = "")

  expect_error(check_api_key(), "OPENROUTER_API_KEY", class = "rlang_error")
})

test_that("check_api_key() passes when the key is set", {
  local_envvar(OPENROUTER_API_KEY = "test-key")

  expect_true(expect_invisible(check_api_key()))
})

test_that("check_model() rejects anything but a single non-empty string", {
  expect_error(check_model(""), class = "rlang_error")
  expect_error(check_model(NA_character_), class = "rlang_error")
  expect_error(check_model(c("a", "b")), class = "rlang_error")
  expect_error(check_model(1), class = "rlang_error")
  expect_true(check_model("openrouter/free"))
})
