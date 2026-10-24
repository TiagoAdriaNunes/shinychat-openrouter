box::use(
  httr2[local_mocked_responses, response_json],
  testthat[expect_equal, expect_error, expect_match, expect_null, expect_true, test_that],
  withr[local_tempfile],
)

box::use(
  app / logic / config[source_url, world_bank],
  app / logic / imf[fetch_imf],
  app / logic / wdi[add_imf, load_wdi, split_panel, wdi_panel],
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

test_that("load_wdi() downloads again when the cache lacks an indicator added to config.yml", {
  path <- local_tempfile(fileext = ".rds")
  old <- split_panel(fake_wdi_raw())
  old$countries$gdp_constant_usd <- NULL
  saveRDS(old, path)

  fresh <- load_wdi(fetch = function() split_panel(fake_wdi_raw()), path = path, max_age_days = 7)
  expect_true("gdp_constant_usd" %in% names(fresh$countries))
})

test_that("load_wdi() downloads again when an indicator's code changed in config.yml", {
  path <- local_tempfile(fileext = ".rds")
  old <- split_panel(fake_wdi_raw())
  old$codes[["gov_debt_pct_gdp"]] <- "wdi:GC.DOD.TOTL.GD.ZS"
  saveRDS(old, path)

  fresh <- load_wdi(fetch = function() split_panel(fake_wdi_raw()), path = path, max_age_days = 7)
  expect_equal(fresh$codes[["gov_debt_pct_gdp"]], "imf:GGXWDG_NGDP")
})

test_that("load_wdi() downloads again when the cache has no indicator definitions", {
  path <- local_tempfile(fileext = ".rds")
  old <- split_panel(fake_wdi_raw())
  old$definitions <- NULL
  saveRDS(old, path)

  fresh <- load_wdi(fetch = function() split_panel(fake_wdi_raw()), path = path, max_age_days = 7)
  expect_true(is.data.frame(fresh$definitions))
})

test_that("split_panel() adds the indicators the WDI download lacks as empty columns", {
  raw <- fake_wdi_raw()
  raw$gov_debt_pct_gdp <- NULL
  panel <- split_panel(raw)

  expect_true(all(is.na(panel$countries$gov_debt_pct_gdp)))
  expect_true(all(is.na(panel$aggregates$gov_debt_pct_gdp)))
  expect_equal(names(panel$countries)[-(1:5)], names(panel$codes))
})

test_that("add_imf() fills countries by ISO3 code and year, and leaves aggregates empty", {
  imf <- data.frame(iso3c = c("BRA", "BRA", "CHL", "WLD"), year = c(2021L, 2022L, 2022L, 2022L), value = c(1, 2, 3, 4))
  panel <- add_imf(split_panel(fake_wdi_raw()), "gov_debt_pct_gdp", imf)

  brazil <- panel$countries[panel$countries$country == "Brazil", ]
  expect_equal(brazil$gov_debt_pct_gdp, c(NA, 1, 2))
  expect_equal(panel$countries$gov_debt_pct_gdp[panel$countries$iso3c == "CHL"], c(NA, NA, 3))
  expect_true(all(is.na(panel$countries$gov_debt_pct_gdp[panel$countries$iso3c == "KOR"])))
  expect_true(all(is.na(panel$aggregates$gov_debt_pct_gdp)))
})

test_that("fetch_imf() identifies the app and retries failed requests", {
  sent <- NULL
  local_mocked_responses(function(req) {
    sent <<- req
    response_json(body = list(values = list(GGXWDG_NGDP = list(FRA = list(`2024` = 112.6)))))
  })

  fetch_imf("GGXWDG_NGDP", last_year = 2025)
  expect_match(sent$options$useragent, source_url, fixed = TRUE)
  expect_equal(sent$policies$retry_max_tries, world_bank$imf$max_tries)
  expect_true(sent$policies$retry_on_failure)
})

test_that("fetch_imf() reads the DataMapper values, recodes IMF codes and drops projections", {
  url <- NULL
  local_mocked_responses(function(req) {
    url <<- req$url
    response_json(
      body = list(
        values = list(
          GGXWDG_NGDP = list(
            FRA = list(`2023` = 109.8, `2024` = 112.6, `2030` = 120),
            UVK = list(`2024` = 17.3),
            EURO = list(`2024` = 88)
          )
        )
      )
    )
  })

  values <- fetch_imf("GGXWDG_NGDP", last_year = 2025)
  expect_match(url, "GGXWDG_NGDP$")
  expect_equal(values$iso3c, c("FRA", "FRA", "XKX", "EURO"))
  expect_equal(values$year, c(2023L, 2024L, 2024L, 2024L))
  expect_equal(values$value, c(109.8, 112.6, 17.3, 88))
})

test_that("fetch_imf() errors when the IMF returns no values for the indicator", {
  local_mocked_responses(function(req) response_json(body = list(values = list())))
  expect_error(fetch_imf("GGXWDG_NGDP"), "no data for GGXWDG_NGDP")
})

test_that("load_wdi() falls back to a stale file when the download fails, and errors without one", {
  path <- local_tempfile(fileext = ".rds")
  failing <- function() stop("World Bank API down")

  expect_error(load_wdi(fetch = failing, path = path, max_age_days = 7), "API down")

  saved <- split_panel(fake_wdi_raw())
  saveRDS(saved, path)
  expect_equal(load_wdi(fetch = failing, path = path, max_age_days = 0), saved)
})

test_that("load_wdi() doesn't download while a background download is running", {
  path <- local_tempfile(fileext = ".rds")
  fetch <- function() stop("must not download")
  running <- function() TRUE

  expect_error(load_wdi(fetch = fetch, path = path, max_age_days = 7, downloading = running), "still downloading")

  # An old copy is used meanwhile
  saved <- split_panel(fake_wdi_raw())
  saveRDS(saved, path)
  expect_equal(load_wdi(fetch = fetch, path = path, max_age_days = 0, downloading = running), saved)
})

test_that("wdi_panel() returns NULL instead of an error", {
  expect_null(wdi_panel(load = function() stop("no network")))
  expect_true(is.list(wdi_panel(load = function() split_panel(fake_wdi_raw()))))
})
