# Trusted calculations (commons measures) over the World Bank tables, loaded with
# commons::semantic_layer() as in https://posit-dev.github.io/commons/r/articles/commons.html.
#
# commons reads the roxygen blocks: `@measure` marks a measure, each `@param` with a type code span is
# an argument the model supplies, and `wdi` (no @param) is the connection to the data source of that
# name, filled in by commons. commons also copies these functions' source into the agent's sandboxed R
# session, so they must be self-contained: base R and `pkg::fn()` calls only, no box imports or
# objects from the app.
#
# `economies` is one semicolon-separated string because commons 0.1.1 only accepts arrays of enums as
# measure arguments, and World Bank names contain commas ("Korea, Rep.").

# Columns that aren't indicators
wdi_id_columns <- c("country", "aggregate", "iso3c", "region", "income", "year")

wdi_check_indicator <- function(wdi, indicator) {
  indicators <- setdiff(DBI::dbListFields(wdi, "countries"), wdi_id_columns)
  if (!indicator %in% indicators) {
    stop(
      "Unknown indicator '", indicator, "'. Use one of: ", paste(indicators, collapse = ", "), ".",
      call. = FALSE
    )
  }
  DBI::dbQuoteIdentifier(wdi, indicator)
}

# Non-missing values of `indicator` for the named countries or aggregates (World Bank names or ISO3
# codes, any case), as columns economy, iso3c, year and the indicator. Errors on unknown names so the
# model can correct them.
wdi_economy_values <- function(wdi, indicator, economies) {
  column <- wdi_check_indicator(wdi, indicator)
  wanted <- trimws(strsplit(economies, ";", fixed = TRUE)[[1]])
  wanted <- wanted[wanted != ""]
  rows <- DBI::dbGetQuery(wdi, paste0(
    "SELECT country AS economy, iso3c, year, ", column, " AS value FROM countries ",
    "UNION ALL SELECT aggregate, iso3c, year, ", column, " FROM aggregates"
  ))
  known <- c(tolower(rows$economy), tolower(rows$iso3c))
  unknown <- wanted[!tolower(wanted) %in% known]
  if (length(unknown) > 0) {
    stop(
      "No World Bank economy named ", paste(unknown, collapse = ", "), ". Use World Bank names ",
      "(e.g. 'Korea, Rep.', 'Egypt, Arab Rep.', 'Turkiye') or ISO3 codes (e.g. 'KOR').",
      call. = FALSE
    )
  }
  keep <- (tolower(rows$economy) %in% tolower(wanted) | tolower(rows$iso3c) %in% tolower(wanted)) &
    !is.na(rows$value)
  rows <- rows[keep, ]
  if (nrow(rows) == 0) {
    stop("No ", indicator, " data for ", paste(wanted, collapse = ", "), ".", call. = FALSE)
  }
  rows <- rows[order(rows$economy, rows$year), ]
  names(rows)[names(rows) == "value"] <- indicator
  rownames(rows) <- NULL
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
  rows <- wdi_economy_values(wdi, indicator, economies)
  latest <- do.call(rbind, lapply(split(rows, rows$economy), function(df) df[which.max(df$year), ]))
  rownames(latest) <- NULL
  latest
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
  rows <- wdi_economy_values(wdi, indicator, economies)
  in_range <- rows$year >= (if (is.null(start_year)) -Inf else start_year) &
    rows$year <= (if (is.null(end_year)) Inf else end_year)
  rows <- rows[in_range, ]
  rownames(rows) <- NULL
  rows
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
  column <- wdi_check_indicator(wdi, indicator)
  values <- DBI::dbGetQuery(wdi, paste0(
    "SELECT country, iso3c, region, year, ", column, " AS value FROM countries WHERE ", column, " IS NOT NULL"
  ))
  if (!is.null(region)) {
    regions <- sort(unique(values$region))
    if (!region %in% regions) {
      stop("Unknown region '", region, "'. Use one of: ", paste(regions, collapse = ", "), ".", call. = FALSE)
    }
    values <- values[values$region == region, ]
  }
  if (nrow(values) == 0) {
    stop("No data for ", indicator, ".", call. = FALSE)
  }
  if (is.null(year)) {
    counts <- table(values$year)
    year <- as.integer(max(names(counts)[counts >= max(counts) / 2]))
  }
  values <- values[values$year == year, ]
  if (nrow(values) == 0) {
    stop("No countries report ", indicator, " for ", year, ".", call. = FALSE)
  }
  values <- values[order(values$value, decreasing = !lowest_first), ]
  values$rank <- seq_len(nrow(values))
  names(values)[names(values) == "value"] <- indicator
  ranked <- utils::head(values[c("rank", "country", "iso3c", "region", "year", indicator)], top_n)
  rownames(ranked) <- NULL
  ranked
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
  if (end_year <= start_year) {
    stop("end_year must be after start_year.", call. = FALSE)
  }
  rows <- wdi_economy_values(wdi, indicator, economies)
  growth <- do.call(rbind, lapply(split(rows, rows$economy), function(df) {
    first <- df[[indicator]][df$year == start_year]
    last <- df[[indicator]][df$year == end_year]
    first <- if (length(first) == 1) first else NA_real_
    last <- if (length(last) == 1) last else NA_real_
    ok <- !is.na(first) && !is.na(last) && first > 0
    data.frame(
      economy = df$economy[1],
      iso3c = df$iso3c[1],
      start_year = start_year,
      start_value = first,
      end_year = end_year,
      end_value = last,
      annual_growth_pct = if (ok) 100 * ((last / first)^(1 / (end_year - start_year)) - 1) else NA_real_
    )
  }))
  rownames(growth) <- NULL
  growth
}
