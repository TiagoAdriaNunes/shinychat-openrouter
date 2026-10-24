box::use(
  checkmate[test_list],
  dplyr[coalesce, distinct, filter, mutate],
  glue[glue],
  httr2[req_perform, req_retry, req_timeout, req_user_agent, request, resp_body_json],
  purrr[compact, imap, list_rbind],
  rlang[abort],
)

box::use(
  . / config[app_title, source_url, world_bank],
)

# The JSON at an IMF DataMapper API `url`, naming the app; failed requests are retried
# (`world_bank$imf$max_tries`)
get_imf <- function(url) {
  request(url) |>
    req_user_agent(glue("{app_title} ({source_url})")) |>
    req_timeout(world_bank$imf$timeout_seconds) |>
    req_retry(max_tries = world_bank$imf$max_tries, retry_on_failure = TRUE) |>
    req_perform() |>
    resp_body_json()
}

#' Yearly values of an IMF DataMapper indicator (e.g. GGXWDG_NGDP, general government gross debt)
#' as columns iso3c, year and value, up to `last_year`: the World Economic Outlook also has
#' projections for the coming years. IMF codes that differ from the World Bank's are recoded
#' (`world_bank$imf$iso3`); the IMF's country groups (EURO, WEOWORLD, ...) are kept as they are.
#' One row per economy and year (add_imf() needs unique keys).
#' @export
fetch_imf <- function(code, last_year = as.integer(format(Sys.Date(), "%Y")) - 1) {
  series <- get_imf(glue(world_bank$imf$url))$values[[code]]
  if (!test_list(series, min.len = 1)) {
    abort(glue("The IMF returned no data for {code}."))
  }
  recode <- unlist(world_bank$imf$iso3)
  imap(series, function(years, iso3c) {
    years <- compact(years)
    data.frame(iso3c = rep(iso3c, length(years)), year = as.integer(names(years)), value = as.numeric(unlist(years)))
  }) |>
    list_rbind() |>
    mutate(iso3c = coalesce(unname(recode[iso3c]), iso3c)) |>
    filter(year <= last_year) |>
    distinct(iso3c, year, .keep_all = TRUE)
}

#' Metadata of every IMF DataMapper indicator, named by code: a list with its `label`,
#' `description`, `source` (e.g. "World Economic Outlook (October 2026)"), `unit` and `dataset`.
#' @export
fetch_imf_metadata <- function() {
  get_imf(world_bank$imf$indicators_url)$indicators
}
