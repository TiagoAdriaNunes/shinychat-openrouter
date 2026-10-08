box::use(
  testthat[expect_gt, expect_true, test_that],
)

box::use(
  app / logic / config,
)

test_that("config.yml provides every text setting as a non-empty string", {
  for (name in c(
    "app_title", "greeting", "placeholder", "disclaimer", "system_prompt", "openrouter_model",
    "openrouter_model_label", "models_url", "models_page_url"
  )) {
    value <- config[[name]]
    expect_true(is.character(value) && length(value) == 1 && nzchar(value), info = name)
  }
})

test_that("config.yml provides positive numeric timings and a theme", {
  expect_gt(config$models_cache_seconds, 0)
  expect_gt(config$request_timeout_seconds, 0)
  expect_true(is.list(config$theme_settings) && length(config$theme_settings) > 0)
})
