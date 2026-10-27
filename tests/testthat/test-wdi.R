box::use(
  httr2[local_mocked_responses, response_json],
  later[run_now],
  mirai[daemons],
  testthat[expect_equal, expect_error, expect_false, expect_match, expect_null, expect_true, test_that],
  withr[defer, local_tempfile],
)

box::use(
  app / logic / config[source_url, world_bank],
  app / logic / imf[fetch_imf, last_actual_year],
  app / logic / wdi[
    add_imf, download_wdi, load_wdi, read_wdi, record_wdi_progress, split_panel, start_wdi_download,
    wdi_downloading, wdi_panel, wdi_progress,
  ],
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

test_that("last_actual_year() stops before the edition's forecasts, even early the next year", {
  weo <- list(source = "World Economic Outlook (October 2026)")
  expect_equal(last_actual_year(weo, today = as.Date("2026-10-24")), 2025L)
  # The calendar alone would keep 2026, which this edition forecasts
  expect_equal(last_actual_year(weo, today = as.Date("2027-02-01")), 2025L)
  expect_equal(last_actual_year(list(source = "Global Debt Database (Sep 2025)"), as.Date("2026-10-24")), 2024L)

  # Without an edition, the year before the current one
  expect_equal(last_actual_year(NULL, today = as.Date("2027-02-01")), 2026L)
  expect_equal(last_actual_year(list(source = "World Economic Outlook"), as.Date("2027-02-01")), 2026L)
})

test_that("load_wdi() falls back to a stale file when the download fails, and errors without one", {
  path <- local_tempfile(fileext = ".rds")
  failing <- function() stop("World Bank API down")

  expect_error(load_wdi(fetch = failing, path = path, max_age_days = 7), "API down")

  saved <- split_panel(fake_wdi_raw())
  saveRDS(saved, path)
  expect_equal(load_wdi(fetch = failing, path = path, max_age_days = 0), saved)
})

test_that("download_wdi() saves the download, and fails with it instead of falling back", {
  path <- local_tempfile(fileext = ".rds")
  panel <- download_wdi(path, fetch = function() split_panel(fake_wdi_raw()))
  expect_equal(readRDS(path), panel)

  expect_error(download_wdi(path, fetch = function() stop("World Bank API down")), "API down")
})

test_that("a download's recorded progress can be read until it ends", {
  path <- local_tempfile(fileext = ".rds")
  expect_null(wdi_progress(path))

  record_wdi_progress(path, 3, 11)
  expect_equal(wdi_progress(path), list(done = 3, total = 11))

  # download_wdi() removes it when the download ends, whether it worked or not
  download_wdi(path, fetch = function() {
    record_wdi_progress(path, 1, 2)
    split_panel(fake_wdi_raw())
  })
  expect_null(wdi_progress(path))
  record_wdi_progress(path, 1, 2)
  expect_error(download_wdi(path, fetch = function() stop("API down")), "API down")
  expect_null(wdi_progress(path))
})

test_that("read_wdi() reads the cache file, even an old one, and never downloads", {
  path <- local_tempfile(fileext = ".rds")
  expect_error(read_wdi(path), "isn't downloaded yet")

  saved <- split_panel(fake_wdi_raw())
  saveRDS(saved, path)
  Sys.setFileTime(path, Sys.time() - 30 * 24 * 60 * 60)
  expect_equal(read_wdi(path), saved)
})

test_that("start_wdi_download() downloads in a mirai only when the file is missing or old", {
  # mirai's synchronous mode runs each task in this process, so the fake downloads below can be used
  daemons(sync = TRUE)
  defer(daemons(0))
  finish <- function() {
    deadline <- Sys.time() + 10
    while (wdi_downloading() && Sys.time() < deadline) {
      run_now(0.05)
    }
    run_now(0.05)
  }
  fake_download <- function(path) download_wdi(path, fetch = function() split_panel(fake_wdi_raw()))

  path <- local_tempfile(fileext = ".rds")
  expect_true(start_wdi_download(path = path, max_age_days = 7, download = fake_download))
  finish()
  expect_false(wdi_downloading())
  expect_true(file.exists(path))

  # The file is fresh now: nothing starts
  expect_false(start_wdi_download(path = path, max_age_days = 7, download = function(path) stop("must not run")))

  # A failed download ends (and is logged) without writing a file
  failed <- local_tempfile(fileext = ".rds")
  expect_true(start_wdi_download(path = failed, max_age_days = 7, download = function(path) stop("API down")))
  finish()
  expect_false(wdi_downloading())
  expect_false(file.exists(failed))
})

test_that("wdi_panel() starts a refresh, then reads the panel, or returns NULL instead of an error", {
  refreshed <- FALSE
  panel <- wdi_panel(load = function() split_panel(fake_wdi_raw()), refresh = function() refreshed <<- TRUE)
  expect_true(refreshed)
  expect_true(is.list(panel))

  expect_null(wdi_panel(load = function() stop("not downloaded yet"), refresh = function() FALSE))
})
