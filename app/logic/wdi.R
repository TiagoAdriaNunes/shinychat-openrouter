box::use(
  cachem[cache_mem],
  callr[r_bg],
  checkmate[test_file_exists, test_null, test_subset],
  dplyr[across, all_of, arrange, everything, filter, mutate, rename, rows_update, select],
  logger[log_info, log_warn],
  memoise[forget, memoise],
  purrr[discard, keep, map_chr, reduce, set_names],
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
#' @export
fetch_wdi <- function(fetch_imf_values = fetch_imf, fetch_metadata = fetch_imf_metadata) {
  raw <- WDI(country = "all", indicator = indicator_codes, start = world_bank$start_year, end = NULL, extra = TRUE)
  if (!is.data.frame(raw) || nrow(raw) == 0) {
    abort("The World Bank returned no data.")
  }
  imf_metadata <- tryCatch(fetch_metadata(), error = function(cnd) {
    log_warn("Could not fetch the IMF indicator definitions: {conditionMessage(cnd)}")
    list()
  })
  panel <- reduce(
    imf_indicators,
    function(panel, i) {
      add_imf(panel, i$name, fetch_imf_values(i$code, last_year = last_actual_year(imf_metadata[[i$code]])))
    },
    .init = split_panel(raw)
  )
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

#' Download the World Bank data in a background R process when the disk cache at `path` is missing
#' or older than `max_age_days`, so the app can start (and show that it is downloading) instead of
#' waiting about 30 seconds. `root` is the project root (box.path). Returns TRUE if one was started.
#' @export
start_wdi_download <- function(path = world_bank$cache_file, max_age_days = world_bank$cache_days, root = getwd()) {
  if (is_fresh(path, max_age_days) || wdi_downloading()) {
    return(invisible(FALSE))
  }
  log_info("Downloading World Bank data in the background")
  background$process <- r_bg(
    function(root, path, max_age_days) {
      options(box.path = root)
      box::use(app / logic / wdi[load_wdi])
      invisible(load_wdi(path = path, max_age_days = max_age_days))
    },
    args = list(root = root, path = path, max_age_days = max_age_days),
    wd = root
  )
  invisible(TRUE)
}

#' TRUE while the background download from start_wdi_download() is running.
#' @export
wdi_downloading <- function() {
  !test_null(background$process) && background$process$is_alive()
}

#' The World Bank panel from the disk cache at `path`, downloading it with `fetch` when the file is
#' missing or older than `max_age_days`. A stale file is still used if the download fails, or while
#' `downloading()` (a background download is running; without a file that is an error).
#' Errors when there is neither a download nor a cached file.
#' @export
load_wdi <- function(
  fetch = fetch_wdi,
  path = world_bank$cache_file,
  max_age_days = world_bank$cache_days,
  downloading = wdi_downloading
) {
  cached <- test_file_exists(path)
  if (is_fresh(path, max_age_days)) {
    return(readRDS(path))
  }
  if (downloading()) {
    if (!cached) {
      abort("The World Bank data is still downloading.")
    }
    return(readRDS(path))
  }
  # try_fetch(), not tryCatch(): an error rethrown with `parent` keeps the download's backtrace
  try_fetch(
    {
      panel <- fetch()
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      # Written under another name first: another process may be reading the cache file
      partial <- str_c(path, ".part")
      saveRDS(panel, partial)
      file.rename(partial, path)
      log_info("Downloaded World Bank data: {nrow(panel$countries)} country-years")
      panel
    },
    error = function(cnd) {
      if (!cached) {
        abort("Could not download the World Bank and IMF data, and there is no cached copy.", parent = cnd)
      }
      log_warn("Could not refresh World Bank data, using the cached copy: {conditionMessage(cnd)}")
      readRDS(path)
    }
  )
}

# Shared by all sessions in this R process. memoise only stores returned values,
# so a failed load is retried on the next call instead of being cached.
cached_load <- memoise(load_wdi, cache = cache_mem(max_age = world_bank$cache_days * 24 * 60 * 60))

#' The World Bank panel (list of `countries`, `aggregates` and the `downloaded` date), or NULL
#' when it can't be loaded. Uses `load` (cached in memory and on disk by default).
#' @export
wdi_panel <- function(load = cached_load) {
  # The background download just ended: drop the copy in memory (an old file read meanwhile)
  if (!test_null(background$process) && !wdi_downloading()) {
    background$process <- NULL
    forget(cached_load)
  }
  tryCatch(load(), error = function(e) {
    log_warn("World Bank data unavailable: {conditionMessage(e)}")
    NULL
  })
}
