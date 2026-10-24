box::use(
  cachem[cache_mem],
  callr[r_bg],
  checkmate[test_file_exists, test_null, test_subset],
  logger[log_info, log_warn],
  memoise[forget, memoise],
  purrr[discard, keep, map, map_chr, reduce, set_names],
  stringr[str_c],
  WDI[WDI],
)

box::use(
  . / config[world_bank],
  . / imf[fetch_imf],
)

is_imf <- function(indicator) identical(indicator$source, "imf")
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
aggregate_columns <- c("aggregate", "iso3c", "year")

# WDI() labels each indicator column; plain columns keep the tables simple for duckdb
strip_labels <- function(df) {
  df[] <- map(df, \(column) {
    attr(column, "label") <- NULL
    column
  })
  df
}

#' Split a WDI(extra = TRUE) download into `countries` (one row per economy and year) and
#' `aggregates` (World, regions, income groups, ...). WDI marks aggregates with the region
#' "Aggregates" or leaves the region empty for groups missing from its metadata. Indicators the
#' download lacks (the IMF ones, filled in by add_imf()) are empty columns; `codes` records each
#' column's source and code.
#' @export
split_panel <- function(raw) {
  for (name in setdiff(indicator_names, names(raw))) {
    raw[[name]] <- NA_real_
  }
  is_country <- !is.na(raw$region) & raw$region != "Aggregates"

  countries <- raw[is_country, c(country_columns, indicator_names)]
  aggregates <- raw[!is_country, c("country", "iso3c", "year", indicator_names)]
  names(aggregates)[1] <- "aggregate"

  tidy <- function(df, by) {
    df <- strip_labels(df[order(df[[by]], df$year), ])
    rownames(df) <- NULL
    df
  }
  list(countries = tidy(countries, "country"), aggregates = tidy(aggregates, "aggregate"), codes = indicator_sources)
}

#' Fill the `name` column of the panel's countries with IMF `values` (columns iso3c, year, value,
#' from fetch_imf()). Aggregates stay empty: the IMF's country groups aren't the World Bank's.
#' @export
add_imf <- function(panel, name, values) {
  found <- match(str_c(panel$countries$iso3c, "-", panel$countries$year), str_c(values$iso3c, "-", values$year))
  panel$countries[[name]] <- values$value[found]
  panel
}

#' Download the configured indicators for every country and aggregate, uncached (about 30 seconds):
#' the World Bank ones with WDI, the others with `fetch_imf`.
#' @export
fetch_wdi <- function(fetch_imf_values = fetch_imf) {
  raw <- WDI(country = "all", indicator = indicator_codes, start = world_bank$start_year, end = NULL, extra = TRUE)
  if (!is.data.frame(raw) || nrow(raw) == 0) {
    stop("The World Bank returned no data")
  }
  panel <- reduce(
    imf_indicators,
    \(panel, i) add_imf(panel, i$name, fetch_imf_values(i$code)),
    .init = split_panel(raw)
  )
  panel$downloaded <- Sys.Date()
  panel
}

file_age_days <- function(path) as.numeric(difftime(Sys.time(), file.mtime(path), units = "days"))

# A cache saved before an indicator was added to config.yml lacks its column, and one saved before
# an indicator's code changed has other codes, so either is downloaded again
matches_config <- function(panel) {
  test_subset(indicator_names, names(panel$countries)) && identical(panel$codes, indicator_sources)
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
      stop("The World Bank data is still downloading")
    }
    return(readRDS(path))
  }
  tryCatch(
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
    error = function(e) {
      if (!cached) {
        stop(e)
      }
      log_warn("Could not refresh World Bank data, using the cached copy: {conditionMessage(e)}")
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
