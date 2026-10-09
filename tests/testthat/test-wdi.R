box::use(
  testthat[expect_equal, expect_error, expect_null, expect_true, test_that],
  withr[local_tempfile],
)

box::use(
  app / logic / wdi[load_wdi, split_panel, wdi_panel],
)

test_that("split_panel() separates countries from aggregates, sorted, without WDI labels", {
  panel <- split_panel(fake_wdi_raw())

  expect_equal(unique(panel$countries$country), c("Brazil", "Chile", "Korea, Rep."))
  expect_equal(unique(panel$aggregates$aggregate), c("High income", "World"))
  expect_equal(panel$countries$year[1:3], 2020:2022)
  expect_equal(names(panel$countries)[1:5], c("country", "iso3c", "region", "income", "year"))
  expect_null(attr(panel$countries$gdp_usd, "label"))
})

test_that("load_wdi() downloads once, then reuses the file until it is too old", {
  path <- local_tempfile(fileext = ".rds")
  calls <- 0
  fetch <- function() {
    calls <<- calls + 1
    split_panel(fake_wdi_raw())
  }

  first <- load_wdi(fetch = fetch, path = path, max_age_days = 7)
  again <- load_wdi(fetch = fetch, path = path, max_age_days = 7)
  expect_equal(calls, 1)
  expect_equal(again, first)

  load_wdi(fetch = fetch, path = path, max_age_days = 0)
  expect_equal(calls, 2)
})

test_that("load_wdi() falls back to a stale file when the download fails, and errors without one", {
  path <- local_tempfile(fileext = ".rds")
  failing <- function() stop("World Bank API down")

  expect_error(load_wdi(fetch = failing, path = path, max_age_days = 7), "API down")

  saved <- split_panel(fake_wdi_raw())
  saveRDS(saved, path)
  expect_equal(load_wdi(fetch = failing, path = path, max_age_days = 0), saved)
})

test_that("wdi_panel() returns NULL instead of an error", {
  expect_null(wdi_panel(load = function() stop("no network")))
  expect_true(is.list(wdi_panel(load = function() split_panel(fake_wdi_raw()))))
})
