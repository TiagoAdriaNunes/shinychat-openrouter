# Analysis measures for the World Bank agent. Runs in commons' own R session: import packages with
# box::use(), but not app modules or other measure files.

box::use(
  checkmate[test_data_frame, test_null],
  DBI[dbReadTable],
  dplyr[all_of, arrange, desc, filter, min_rank, mutate, n, select, semi_join, slice_max],
  duckplyr[methods_overwrite],
  glue[glue],
  stringr[str_to_lower, str_trim],
  tidyr[pivot_longer],
)

# Run the dplyr verbs on duckdb, as in world-bank.R
Sys.setenv(DUCKPLYR_FALLBACK_COLLECT = 0)
suppressMessages(methods_overwrite())

# Columns of `countries` that aren't indicators
profile_id_columns <- c("country", "iso3c", "region", "income", "year")

#' Country profile: every indicator for one country, with its rank among countries
#'
#' Use for overview, profile, and best, worst, strongest or weakest indicator questions about one
#' country, e.g. "Worst indicator for Mexico", "Overview of India's economy".
#'
#' @param economy `string` One country, as a World Bank name or ISO3 code, e.g. 'Brazil' or 'BRA'.
#' @param year `integer` Year to profile. Default: each indicator's latest year for the country.
#' @return One row per indicator: country, indicator, year, value, rank_highest (1 = highest value;
#'   good for GDP, growth and share of world GDP; bad for inflation, unemployment, every kind of debt,
#'   interest and Gini; for gov_balance and current_account the highest is the largest surplus) and
#'   countries_ranked.
#' @measure
country_profile <- function(wdi, economy, year = NULL) {
  countries <- dbReadTable(wdi, "countries")
  indicators <- setdiff(names(countries), profile_id_columns)
  wanted <- str_to_lower(str_trim(economy))
  found <- countries |>
    filter(str_to_lower(country) == wanted | str_to_lower(iso3c) == wanted)
  if (!test_data_frame(found, min.rows = 1)) {
    stop(
      glue(
        "No World Bank country named {economy}. Use a World Bank name (e.g. 'Korea, Rep.', ",
        "'Egypt, Arab Rep.', 'Turkiye') or an ISO3 code (e.g. 'KOR')."
      ),
      call. = FALSE
    )
  }
  name <- found$country[1]
  wanted_year <- year

  values <- countries |>
    pivot_longer(all_of(indicators), names_to = "indicator", values_to = "value") |>
    filter(!is.na(value))
  own <- values |>
    filter(country == name)
  if (!test_null(wanted_year)) {
    own <- own |>
      filter(year == wanted_year)
  }
  if (!test_data_frame(own, min.rows = 1)) {
    when <- if (test_null(wanted_year)) "" else glue(" in {wanted_year}")
    stop(glue("No data for {name}{when}."), call. = FALSE)
  }
  latest <- own |>
    slice_max(year, n = 1, by = indicator) |>
    select(indicator, year)

  values |>
    semi_join(latest, by = c("indicator", "year")) |>
    mutate(rank_highest = min_rank(desc(value)), countries_ranked = n(), .by = indicator) |>
    filter(country == name) |>
    arrange(match(indicator, indicators)) |>
    select(country, indicator, year, value, rank_highest, countries_ranked) |>
    as.data.frame()
}
