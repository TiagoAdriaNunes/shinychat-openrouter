box::use(
  checkmate[test_list],
  dplyr[coalesce, distinct, filter, mutate],
  glue[glue],
  httr2[req_perform, req_retry, req_timeout, req_user_agent, request, resp_body_json],
  purrr[compact, imap, list_rbind],
)

box::use(
  . / config[app_title, source_url, world_bank],
)

#' Yearly values of an IMF DataMapper indicator (e.g. GGXWDG_NGDP, general government gross debt)
#' as columns iso3c, year and value, up to `last_year`: the World Economic Outlook also has
#' projections for the coming years. IMF codes that differ from the World Bank's are recoded
#' (`world_bank$imf$iso3`); the IMF's country groups (EURO, WEOWORLD, ...) are kept as they are.
#' One row per economy and year (add_imf() needs unique keys). Failed requests are retried
#' (`world_bank$imf$max_tries`).
#' @export
fetch_imf <- function(code, last_year = as.integer(format(Sys.Date(), "%Y")) - 1) {
  body <- request(glue(world_bank$imf$url)) |>
    req_user_agent(glue("{app_title} ({source_url})")) |>
    req_timeout(world_bank$imf$timeout_seconds) |>
    req_retry(max_tries = world_bank$imf$max_tries, retry_on_failure = TRUE) |>
    req_perform() |>
    resp_body_json()
  series <- body$values[[code]]
  if (!test_list(series, min.len = 1)) {
    stop(glue("The IMF returned no data for {code}"))
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
