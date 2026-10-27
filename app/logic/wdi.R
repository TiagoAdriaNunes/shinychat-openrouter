box::use(
  cachem[cache_mem],
  checkmate[test_file_exists, test_null, test_subset],
  dplyr[across, all_of, arrange, everything, filter, mutate, rename, rows_update, select],
  logger[log_info, log_warn],
  memoise[forget, memoise],
  mirai[mirai, unresolved],
  promises[then],
  purrr[discard, keep, map_chr, set_names],
  rlang[`:=`, abort, try_fetch],
  stringr[str_c],
  WDI[WDI],
)

box::use(
  . / config[world_bank],
  . / definitions[indicator_definitions],
  . / imf[fetch_imf, fetch_imf_metadata, last_actual_year],
  . / indicators[is_imf],
)

imf_indicators <- keep(world_bank$indicators, is_imf)
wdi_indicators <- discard(world_bank$indicators, is_imf)

#' World Bank series codes to download with WDI, named by their column name in the agent's tables
#' (the indicators with `source: imf` come from fetch_imf()).
#' @export
indicator_codes <- set_names(map_chr(wdi_indicators, "code"), map_chr(wdi_indicators, "name"))

# Every indicator column, in config.yml order, with its source and code: saved with the data, so a
# cache made before a code changed in config.yml is downloaded again
indicator_names <- map_chr(world_bank$indicators, "name")
indicator_sources <- set_names(
  map_chr(world_bank$indicators, \(i) str_c(if (is_imf(i)) "imf" else "wdi", ":", i$code)),
  indicator_names
)

country_columns <- c("country", "iso3c", "region", "income", "year")

# WDI() labels each indicator column; plain columns keep the tables simple for duckdb
strip_labels <- function(df) {
  mutate(df, across(everything(), \(column) structure(column, label = NULL)))
}

#' Split a WDI(extra = TRUE) download into `countries` (one row per economy and year) and
#' `aggregates` (World, regions, income groups, ...). WDI marks aggregates with the region
#' "Aggregates" or leaves the region empty for groups missing from its metadata. Indicators the
#' download lacks (the IMF ones, filled in by add_imf()) are empty columns; `codes` records each
#' column's source and code, and `definitions` the indicators' official definitions (the IMF ones
#' are added by fetch_wdi()).
#' @export
split_panel <- function(raw) {
  raw[setdiff(indicator_names, names(raw))] <- NA_real_
  raw <- strip_labels(raw)

  countries <- raw |>
    filter(!is.na(region), region != "Aggregates") |>
    select(all_of(country_columns), all_of(indicator_names)) |>
    arrange(country, year)
  aggregates <- raw |>
    filter(is.na(region) | region == "Aggregates") |>
    select(aggregate = country, iso3c, year, all_of(indicator_names)) |>
    arrange(aggregate, year)
  list(countries = countries, aggregates = aggregates, codes = indicator_sources, definitions = indicator_definitions())
}

#' Fill the countries' `name` column with IMF `values` (from fetch_imf()), only for country-years the
#' World Bank data has (later IMF years wait for it); aggregates stay empty (IMF groups differ).
#' @export
add_imf <- function(panel, name, values) {
  panel$countries <- panel$countries |>
    mutate("{name}" := NA_real_) |>
    rows_update(rename(values, "{name}" := value), by = c("iso3c", "year"), unmatched = "ignore")
  panel
}

#' Download every indicator, uncached (~30 s): World Bank ones with WDI, IMF ones up to their last
#' actual year (last_actual_year()), with definitions from `fetch_metadata` when it works.
#' `progress(done, total)` is called as each part (World Bank, IMF definitions, each IMF indicator) ends.
#' @export
fetch_wdi <- function(
  fetch_imf_values = fetch_imf,
  fetch_metadata = fetch_imf_metadata,
  progress = function(done, total) NULL
) {
  total <- 2 + length(imf_indicators)
  progress(0, total)
  raw <- WDI(country = "all", indicator = indicator_codes, start = world_bank$start_year, end = NULL, extra = TRUE)
  if (!is.data.frame(raw) || nrow(raw) == 0) {
    abort("The World Bank returned no data.")
  }
  progress(1, total)
  imf_metadata <- tryCatch(fetch_metadata(), error = function(cnd) {
    log_warn("Could not fetch the IMF indicator definitions: {conditionMessage(cnd)}")
    list()
  })
  progress(2, total)
  panel <- split_panel(raw)
  for (k in seq_along(imf_indicators)) {
    i <- imf_indicators[[k]]
    panel <- add_imf(panel, i$name, fetch_imf_values(i$code, last_year = last_actual_year(imf_metadata[[i$code]])))
    progress(2 + k, total)
  }
  panel$definitions <- indicator_definitions(imf_metadata)
  panel$downloaded <- Sys.Date()
  panel
}

file_age_days <- function(path) as.numeric(difftime(Sys.time(), file.mtime(path), units = "days"))

# A cache saved before an indicator was added to config.yml lacks its column, one saved before an
# indicator's code changed has other codes, and one from before the definitions has none: each is
# downloaded again
matches_config <- function(panel) {
  test_subset(indicator_names, names(panel$countries)) &&
    identical(panel$codes, indicator_sources) &&
    !test_null(panel$definitions)
}

is_fresh <- function(path, max_age_days) {
  test_file_exists(path) && file_age_days(path) < max_age_days && matches_config(readRDS(path))
}

# The background download started by start_wdi_download(), if any (one per R process)
background <- new.env()

progress_file <- function(path) str_c(path, ".progress")

#' Record that `done` of `total` parts of the download to `path` are done, for wdi_progress(). The file
#' is replaced in one step, so a reader never sees it half written.
#' @export
record_wdi_progress <- function(path, done, total) {
  partial <- str_c(progress_file(path), ".part")
  saveRDS(list(done = done, total = total), partial)
  file.rename(partial, progress_file(path))
  invisible(path)
}

#' Progress of the download to `path` as list(done, total), or NULL when there is none to read.
#' @export
wdi_progress <- function(path = world_bank$cache_file) {
  if (!test_file_exists(progress_file(path))) {
    return(NULL)
  }
  # The download may be replacing the file right now: then there is nothing to read this time
  tryCatch(readRDS(progress_file(path)), error = function(e) NULL, warning = function(w) NULL)
}

#' Download the panel with `fetch` and save it at `path`, returning it; errors when the download fails.
#' By default the download records its progress (record_wdi_progress()) until it ends.
#' @export
download_wdi <- function(
  path = world_bank$cache_file,
  fetch = function() fetch_wdi(progress = function(done, total) record_wdi_progress(path, done, total))
) {
  on.exit(unlink(progress_file(path)), add = TRUE)
  panel <- fetch()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  # Written under another name first: another process may be reading the cache file
  partial <- str_c(path, ".part")
  saveRDS(panel, partial)
  file.rename(partial, path)
  log_info("Downloaded World Bank data: {nrow(panel$countries)} country-years")
  panel
}

#' Refresh the cache at `path` in a background R process (a mirai) when it is missing, older than
#' `max_age_days` or out of date with config.yml. Returns TRUE if a download started.
#' @export
start_wdi_download <- function(
  path = world_bank$cache_file,
  max_age_days = world_bank$cache_days,
  root = getwd(),
  download = NULL
) {
  if (wdi_downloading() || is_fresh(path, max_age_days)) {
    return(invisible(FALSE))
  }
  log_info("Downloading World Bank data in the background")
  unlink(progress_file(path))
  # Without daemons(), mirai starts a one-off process; tests pass `download` instead of download_wdi()
  background$task <- mirai(
    {
      if (is.null(download)) {
        options(box.path = root)
        box::use(app / logic / wdi[download_wdi])
        download <- download_wdi
      }
      nrow(download(path)$countries)
    },
    root = root,
    path = path,
    download = download
  )
  # The process's own output is discarded, so its result is logged here
  then(
    background$task,
    onFulfilled = function(rows) {
      log_info("Downloaded World Bank data in the background: {rows} country-years")
      forget(cached_load)
    },
    onRejected = function(cnd) log_warn("Could not download World Bank data in the background: {conditionMessage(cnd)}")
  )
  invisible(TRUE)
}

#' TRUE while the background download from start_wdi_download() is running.
#' @export
wdi_downloading <- function() !test_null(background$task) && unresolved(background$task)

#' The panel from the cache at `path`, downloading it with `fetch` in this process when the file is
#' missing or older than `max_age_days`, or the stale file if that fails (the Docker build's data step).
#' @export
load_wdi <- function(fetch = fetch_wdi, path = world_bank$cache_file, max_age_days = world_bank$cache_days) {
  if (is_fresh(path, max_age_days)) {
    return(readRDS(path))
  }
  cached <- test_file_exists(path)
  # try_fetch(), not tryCatch(): an error rethrown with `parent` keeps the download's backtrace
  try_fetch(download_wdi(path, fetch), error = function(cnd) {
    if (!cached) {
      abort("Could not download the World Bank and IMF data, and there is no cached copy.", parent = cnd)
    }
    log_warn("Could not refresh World Bank data, using the cached copy: {conditionMessage(cnd)}")
    readRDS(path)
  })
}

#' The app's copy of the panel: the cache file at `path`, even an old one, or an error when there is
#' none yet. The app itself never downloads: start_wdi_download() refreshes the file in the background.
#' @export
read_wdi <- function(path = world_bank$cache_file) {
  if (!test_file_exists(path)) {
    abort("The World Bank data isn't downloaded yet.")
  }
  readRDS(path)
}

# Shared by all sessions in this R process, and forgotten when a background download ends. memoise
# only stores returned values, so a missing file is looked for again on the next call.
cached_load <- memoise(read_wdi, cache = cache_mem(max_age = world_bank$cache_days * 24 * 60 * 60))

#' The World Bank panel (list of `countries`, `aggregates`, ...) read with `load`, or NULL when there is
#' none yet. First `refresh` starts a background download if the file is missing or old.
#' @export
wdi_panel <- function(load = cached_load, refresh = start_wdi_download) {
  tryCatch(
    {
      refresh()
      load()
    },
    error = function(e) {
      log_warn("World Bank data unavailable: {conditionMessage(e)}")
      NULL
    }
  )
}
