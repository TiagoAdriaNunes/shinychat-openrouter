# Trusted calculations (commons measures) over the World Bank tables, loaded with
# commons::semantic_layer() as in https://posit-dev.github.io/commons/r/articles/commons.html.
#
# commons reads the roxygen blocks: `@measure` marks a measure, each `@param` with a type code span is
# an argument the model supplies, and `wdi` (no @param) is the connection to the data source of that
# name, filled in by commons. commons also copies these functions' source into the agent's sandboxed R
# session, so they must be self-contained: base R and packages imported with box::use(), no app
# modules or objects from the app.
#
# `economies` is one semicolon-separated string because commons 0.1.1 only accepts arrays of enums as
# measure arguments, and World Bank names contain commas ("Korea, Rep.").

box::use(
  checkmate[test_choice, test_data_frame, test_null, test_subset, test_true],
  DBI[dbListFields, dbReadTable],
  dplyr[
    all_of, arrange, bind_rows, count, desc, filter, first, if_else, mutate, pull, rename_with,
    row_number, select, slice_head, slice_max, summarise,
  ],
  duckplyr[methods_overwrite],
  glue[glue],
  stringr[fixed, str_flatten_comma, str_split_1, str_subset, str_to_lower, str_trim],
)

# Run the dplyr verbs on duckdb (duckplyr falls back to dplyr for what it can't translate),
# without saving fallback reports to disk
Sys.setenv(DUCKPLYR_FALLBACK_COLLECT = 0)
suppressMessages(methods_overwrite())

# Columns that aren't indicators
wdi_id_columns <- c("country", "aggregate", "iso3c", "region", "income", "year")

wdi_check_indicator <- function(wdi, indicator) {
  indicators <- setdiff(dbListFields(wdi, "countries"), wdi_id_columns)
  if (!test_choice(indicator, indicators)) {
    stop(glue("Unknown indicator '{indicator}'. Use one of: {str_flatten_comma(indicators)}."), call. = FALSE)
  }
}

# Non-missing values of `indicator` for the named countries or aggregates (World Bank names or ISO3
# codes, any case), as columns economy, iso3c, year and the indicator. Errors on unknown names so the
# model can correct them.
wdi_economy_values <- function(wdi, indicator, economies) {
  wdi_check_indicator(wdi, indicator)
  wanted <- str_subset(str_trim(str_split_1(economies, fixed(";"))), ".")
  rows <- bind_rows(
    dbReadTable(wdi, "countries") |> select(economy = country, iso3c, year, value = all_of(indicator)),
    dbReadTable(wdi, "aggregates") |> select(economy = aggregate, iso3c, year, value = all_of(indicator))
  )
  known <- str_to_lower(c(rows$economy, rows$iso3c))
  if (!test_subset(str_to_lower(wanted), known)) {
    unknown <- wanted[!str_to_lower(wanted) %in% known]
    stop(
      glue(
        "No World Bank economy named {str_flatten_comma(unknown)}. Use World Bank names ",
        "(e.g. 'Korea, Rep.', 'Egypt, Arab Rep.', 'Turkiye') or ISO3 codes (e.g. 'KOR')."
      ),
      call. = FALSE
    )
  }
  rows <- rows |>
    filter(
      str_to_lower(economy) %in% str_to_lower(wanted) | str_to_lower(iso3c) %in% str_to_lower(wanted),
      !is.na(value)
    ) |>
    arrange(economy, year) |>
    rename_with(\(name) indicator, value)
  if (!test_data_frame(rows, min.rows = 1)) {
    stop(glue("No {indicator} data for {str_flatten_comma(wanted)}."), call. = FALSE)
  }
  rows
}

#' Latest value of a World Bank indicator for a country
#'
#' Use for "what is", "how much", "how big" or "current" questions about one or more countries or
#' regions, e.g. "What is Brazil's GDP?", "Population of India", "Inflation in Argentina now".
#' Returns the most recent available value and its year, for any indicator in the data: GDP (gross
#' domestic product, size of the economy), GDP growth, GDP per capita (income per person), GDP per
#' capita PPP, inflation (consumer prices), unemployment, population, trade, exports, foreign direct
#' investment (FDI), government debt and the Gini index (inequality).
#'
#' @param indicator `string` Indicator column: gdp_usd (GDP, current US$), gdp_growth_pct (real GDP
#'   growth, %), gdp_per_capita_usd, gdp_per_capita_ppp, inflation_pct, unemployment_pct, population,
#'   trade_pct_gdp, exports_pct_gdp, fdi_inflows_pct_gdp, gov_debt_pct_gdp, gini.
#' @param economies `string` Countries or aggregates separated by semicolons, as World Bank names or
#'   ISO3 codes, e.g. 'Brazil; Korea, Rep.; World; Euro area; High income' or 'BRA; KOR; WLD'.
#' @return One row per economy: economy, iso3c, year, and the indicator.
#' @measure
latest_values <- function(wdi, indicator, economies) {
  wdi_economy_values(wdi, indicator, economies) |>
    slice_max(year, n = 1, by = economy)
}

#' Yearly values of a World Bank indicator over time
#'
#' Use for trends, history and "how has it changed" questions, e.g. "Brazil's GDP since 2010",
#' "Inflation in Turkey over the last decade", "Compare unemployment in Spain and Italy over time".
#' Returns one value per year for countries or regions, for any indicator in the data: GDP, GDP
#' growth, GDP per capita, PPP, inflation, unemployment, population, trade, exports, FDI, government
#' debt and the Gini index (inequality).
#'
#' @param indicator `string` Indicator column, e.g. gdp_usd, gdp_growth_pct, inflation_pct, population
#'   (same columns as latest_values).
#' @param economies `string` Countries or aggregates separated by semicolons, as World Bank names or
#'   ISO3 codes, e.g. 'Brazil; Chile' or 'BRA; CHL; WLD'.
#' @param start_year `integer` First year to include.
#' @param end_year `integer` Last year to include.
#' @return One row per economy and year: economy, iso3c, year, and the indicator.
#' @measure
indicator_series <- function(wdi, indicator, economies, start_year = NULL, end_year = NULL) {
  first_year <- if (test_null(start_year)) -Inf else start_year
  last_year <- if (test_null(end_year)) Inf else end_year
  wdi_economy_values(wdi, indicator, economies) |>
    filter(year >= first_year, year <= last_year)
}

#' Countries ranked by a World Bank indicator
#'
#' Use for "top", "largest", "highest", "lowest", "richest", "poorest" or "which countries"
#' questions, e.g. "Largest economies by GDP", "Highest inflation in Africa", "Poorest countries
#' by GDP per capita". Ranks countries (not aggregates) in one year, optionally within one World
#' Bank region, for any indicator: GDP, GDP growth, GDP per capita, PPP, inflation, unemployment,
#' population, trade, exports, FDI, government debt or the Gini index (inequality). Without a year,
#' uses the latest year reported by at least half as many countries as the best-covered year, so a
#' ranking isn't based on the few early reporters.
#'
#' @param indicator `string` Indicator column, e.g. gdp_usd, gdp_per_capita_ppp, inflation_pct
#'   (same columns as latest_values).
#' @param year `integer` Year to rank. Default: the latest year most countries report.
#' @param top_n `integer` How many countries to return. Default 10.
#' @param lowest_first `boolean` Rank from the lowest value instead of the highest.
#' @param region `string` Limit to one World Bank region, e.g. 'Latin America & Caribbean',
#'   'Sub-Saharan Africa', 'East Asia & Pacific', 'Europe & Central Asia'.
#' @return rank, country, iso3c, region, year, and the indicator.
#' @measure
indicator_ranking <- function(wdi, indicator, year = NULL, top_n = 10, lowest_first = FALSE, region = NULL) {
  wdi_check_indicator(wdi, indicator)
  values <- dbReadTable(wdi, "countries") |>
    select(country, iso3c, region, year, value = all_of(indicator)) |>
    filter(!is.na(value))
  # (the arguments have the same names as columns)
  wanted_region <- region
  wanted_year <- year
  if (!test_null(wanted_region)) {
    regions <- sort(unique(values$region))
    if (!test_choice(wanted_region, regions)) {
      stop(glue("Unknown region '{wanted_region}'. Use one of: {str_flatten_comma(regions)}."), call. = FALSE)
    }
    values <- values |>
      filter(region == wanted_region)
  }
  if (!test_data_frame(values, min.rows = 1)) {
    stop(glue("No data for {indicator}."), call. = FALSE)
  }
  if (test_null(wanted_year)) {
    wanted_year <- values |>
      count(year) |>
      filter(n >= max(n) / 2) |>
      pull(year) |>
      max()
  }
  values <- values |>
    filter(year == wanted_year)
  if (!test_data_frame(values, min.rows = 1)) {
    stop(glue("No countries report {indicator} for {wanted_year}."), call. = FALSE)
  }
  values |>
    arrange(if (lowest_first) value else desc(value)) |>
    mutate(rank = row_number()) |>
    select(rank, country, iso3c, region, year, value) |>
    rename_with(\(name) indicator, value) |>
    slice_head(n = top_n)
}

#' Compound annual growth rate of a World Bank indicator
#'
#' Use for "average annual growth between two years" questions, e.g. "How fast did China's GDP
#' grow per year from 2000 to 2020?". Returns the compound annual growth rate (%) of an amount (GDP,
#' GDP per capita or population) between two years, for countries or regions. Not for a single
#' year's GDP growth (use latest_values or indicator_series with gdp_growth_pct), nor for rates or
#' shares (inflation, unemployment, % of GDP).
#'
#' @param indicator `string` Amount column: gdp_usd, gdp_per_capita_usd, gdp_per_capita_ppp or population.
#' @param economies `string` Countries or aggregates separated by semicolons, as World Bank names or
#'   ISO3 codes, e.g. 'Brazil; Chile'.
#' @param start_year `integer` Start year.
#' @param end_year `integer` End year, after start_year.
#' @return economy, iso3c, start and end years and values, and annual_growth_pct (NA when either
#'   year is missing).
#' @measure
compound_growth <- function(wdi, indicator, economies, start_year, end_year) {
  if (!test_true(end_year > start_year)) {
    stop("end_year must be after start_year.", call. = FALSE)
  }
  years <- end_year - start_year
  wdi_economy_values(wdi, indicator, economies) |>
    rename_with(\(name) "value", all_of(indicator)) |>
    summarise(
      iso3c = first(iso3c),
      # first() of no rows is NA: that year is missing
      start_value = first(value[year == start_year]),
      end_value = first(value[year == end_year]),
      .by = economy
    ) |>
    mutate(
      start_year = start_year,
      end_year = end_year,
      annual_growth_pct = if_else(start_value > 0, 100 * ((end_value / start_value)^(1 / years) - 1), NA_real_)
    ) |>
    select(economy, iso3c, start_year, start_value, end_year, end_value, annual_growth_pct)
}
