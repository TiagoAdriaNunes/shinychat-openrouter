# Trusted calculations (commons measures) over the World Bank and IMF tables, and the chart measure.
# commons reads the roxygen blocks: `@measure` marks a measure, each `@param` with a type is an
# argument the model supplies, and `wdi` (no @param) is the data source's connection. It copies the
# measures and helpers of this file into the agent's sandboxed R session, so they use only the
# packages imported here, never app modules. `economies` is one semicolon-separated string: commons
# 0.1.1 only accepts arrays of enums, and World Bank names contain commas ("Korea, Rep.").

box::use(
  checkmate[test_character, test_choice, test_data_frame, test_null, test_subset, test_true],
  DBI[dbListFields, dbReadTable],
  dplyr[
    all_of, arrange, bind_rows, count, desc, filter, first, group_by, if_else, mutate, pull, rename,
    rename_with, row_number, select, slice_head, slice_max, summarise,
  ],
  duckplyr[methods_overwrite],
  echarts4r[e_axis_labels, e_charts, e_grid, e_legend, e_line, e_text_style, e_toolbox_feature, e_tooltip, e_y_axis],
  ellmer[ContentToolResult],
  glue[glue],
  htmltools[as.tags, div],
  htmlwidgets[JS],
  rlang[abort],
  stringr[fixed, str_detect, str_flatten, str_flatten_comma, str_split_1, str_subset, str_to_lower, str_trim],
)

# Run the dplyr verbs on duckdb (duckplyr falls back to dplyr for what it can't translate),
# without saving fallback reports to disk
Sys.setenv(DUCKPLYR_FALLBACK_COLLECT = 0)
suppressMessages(methods_overwrite())

# Columns that aren't indicators
wdi_id_columns <- c("country", "aggregate", "iso3c", "region", "income", "year")

# Indicators that are amounts, so growth between two years is their change (the others are rates or shares)
wdi_amount_indicators <- c("gdp_constant_usd", "gdp_usd", "gdp_per_capita_usd", "gdp_per_capita_ppp", "population")

wdi_check_indicator <- function(wdi, indicator) {
  indicators <- setdiff(dbListFields(wdi, "countries"), wdi_id_columns)
  if (!test_choice(indicator, indicators)) {
    abort(glue("Unknown indicator '{indicator}'. Use one of: {str_flatten_comma(indicators)}."), call = NULL)
  }
}

# ISO3 codes of the members of a country group in the `groups` table (name in any case), or none
# without a group
wdi_group_members <- function(wdi, group) {
  if (test_null(group)) {
    return(character())
  }
  wanted_group <- str_to_lower(str_trim(group))
  groups <- dbReadTable(wdi, "groups")
  members <- groups |>
    filter(str_to_lower(group) == wanted_group) |>
    pull(iso3c)
  if (!test_character(members, min.len = 1)) {
    abort(
      glue(
        "Unknown group '{group}'. Use one of: {str_flatten_comma(unique(groups$group))}. ",
        "group is optional: send null when the question isn't about one of these groups."
      ),
      call = NULL
    )
  }
  members
}

# Non-missing values of `indicator` for the named countries or aggregates (World Bank names or ISO3
# codes, any case) and the members of `group`, as columns economy, iso3c, year and the indicator.
# Errors on unknown names so the model can correct them.
wdi_economy_values <- function(wdi, indicator, economies, group = NULL) {
  wdi_check_indicator(wdi, indicator)
  named <- if (test_null(economies)) character() else str_subset(str_trim(str_split_1(economies, fixed(";"))), ".")
  wanted <- c(named, wdi_group_members(wdi, group))
  if (!test_character(wanted, min.len = 1)) {
    abort("Give economies, a group, or both.", call = NULL)
  }
  rows <- bind_rows(
    dbReadTable(wdi, "countries") |> select(economy = country, iso3c, year, value = all_of(indicator)),
    dbReadTable(wdi, "aggregates") |> select(economy = aggregate, iso3c, year, value = all_of(indicator))
  )
  known <- str_to_lower(c(rows$economy, rows$iso3c))
  if (!test_subset(str_to_lower(wanted), known)) {
    unknown <- wanted[!str_to_lower(wanted) %in% known]
    abort(
      glue(
        "No World Bank economy named {str_flatten_comma(unknown)}. Use World Bank names ",
        "(e.g. 'Korea, Rep.', 'Egypt, Arab Rep.', 'Turkiye') or ISO3 codes (e.g. 'KOR')."
      ),
      call = NULL
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
    abort(glue("No {indicator} data for {str_flatten_comma(wanted)}."), call = NULL)
  }
  rows
}

#' Latest value of an indicator for a country
#'
#' Use for "what is", "how much", "how big" or "current" questions about one or more countries or
#' regions, e.g. "What is Brazil's GDP?", "Population of India", "Inflation in Argentina now".
#' Returns the most recent available value and its year, for any indicator in the World Bank and IMF
#' data: GDP (gross domestic product, size of the economy), GDP growth, GDP per capita (income per
#' person), GDP per capita PPP, share of world GDP, inflation (consumer prices), unemployment,
#' population, trade, exports, foreign direct investment (FDI), current account balance, government
#' debt, net debt, budget deficit or surplus, interest on public debt, private, household and
#' corporate debt, and the Gini index (inequality).
#'
#' @param indicator `string` Indicator column: gdp_usd (GDP, current US$), gdp_growth_pct (real GDP
#'   growth, %), gdp_constant_usd (real GDP, constant 2015 US$), gdp_per_capita_usd,
#'   gdp_per_capita_ppp, world_gdp_share_ppp_pct, inflation_pct,
#'   unemployment_pct, population, trade_pct_gdp, exports_pct_gdp, fdi_inflows_pct_gdp,
#'   current_account_pct_gdp, gov_debt_pct_gdp, gov_net_debt_pct_gdp, gov_balance_pct_gdp (budget
#'   surplus + or deficit -), gov_interest_pct_gdp, private_debt_pct_gdp, household_debt_pct_gdp,
#'   corporate_debt_pct_gdp, gini.
#' @param economies `string` Countries or aggregates separated by semicolons, as World Bank names or
#'   ISO3 codes, e.g. 'Brazil; Korea, Rep.; World; Euro area; High income' or 'BRA; KOR; WLD'.
#' @param group `string` Also include every member country of a group in the `groups` table, e.g.
#'   'Euro area', 'European Union', 'Mercosur', 'G7'.
#' @return One row per economy: economy, iso3c, year, and the indicator.
#' @measure
latest_values <- function(wdi, indicator, economies = NULL, group = NULL) {
  wdi_economy_values(wdi, indicator, economies, group) |>
    slice_max(year, n = 1, by = economy)
}

#' Yearly values of an indicator over time
#'
#' Use for trends, history and "how has it changed" questions, e.g. "Brazil's GDP since 2010",
#' "Inflation in Turkey over the last decade", "Compare unemployment in Spain and Italy over time".
#' Returns one value per year for countries or regions, for any indicator in the data: GDP, GDP
#' growth, GDP per capita, PPP, share of world GDP, inflation, unemployment, population, trade,
#' exports, FDI, current account, government debt, net debt, budget deficit, interest, private,
#' household and corporate debt, and the Gini index (inequality).
#'
#' @param indicator `string` Indicator column, e.g. gdp_usd, gdp_growth_pct, inflation_pct, population
#'   (same columns as latest_values).
#' @param economies `string` Countries or aggregates separated by semicolons, as World Bank names or
#'   ISO3 codes, e.g. 'Brazil; Chile' or 'BRA; CHL; WLD'.
#' @param start_year `integer` First year to include.
#' @param end_year `integer` Last year to include.
#' @param group `string` Also include every member country of a group in the `groups` table, e.g.
#'   'Euro area', 'European Union', 'Mercosur', 'G7'.
#' @return One row per economy and year: economy, iso3c, year, and the indicator.
#' @measure
indicator_series <- function(wdi, indicator, economies = NULL, start_year = NULL, end_year = NULL, group = NULL) {
  first_year <- if (test_null(start_year)) -Inf else start_year
  last_year <- if (test_null(end_year)) Inf else end_year
  wdi_economy_values(wdi, indicator, economies, group) |>
    filter(year >= first_year, year <= last_year)
}

#' Countries ranked by an indicator
#'
#' Use for "top", "largest", "highest", "lowest", "richest", "poorest" or "which countries"
#' questions, e.g. "Largest economies by GDP", "Highest inflation in Africa", "Poorest countries
#' by GDP per capita", "Top euro countries by GDP", "Biggest economies in the EU". Ranks countries
#' (not aggregates) in one year, optionally within one World Bank region or a country group (euro
#' area / eurozone, European Union / EU, Mercosur, BRICS, ASEAN, G7, G20, OECD...), for any
#' indicator: GDP, GDP growth, GDP per capita, PPP, share of world GDP, inflation, unemployment,
#' population, trade, exports, FDI, current account, government debt, net debt, budget deficit or
#' surplus, interest, private, household or corporate debt, or the Gini index (inequality). For the
#' largest deficits rank gov_balance_pct_gdp or current_account_pct_gdp with lowest_first (deficits
#' are negative). Without a year, uses the latest year
#' reported by at least half as many countries as the best-covered year, so a ranking isn't based on
#' the few early reporters.
#'
#' @param indicator `string` Indicator column, e.g. gdp_usd, gdp_per_capita_ppp, inflation_pct
#'   (same columns as latest_values).
#' @param year `integer` Year to rank. Default: the latest year most countries report.
#' @param top_n `integer` How many countries to return. Default 10.
#' @param lowest_first `boolean` Rank from the lowest value instead of the highest.
#' @param region `string` Limit to one World Bank region, e.g. 'Latin America & Caribbean',
#'   'Sub-Saharan Africa', 'East Asia & Pacific', 'Europe & Central Asia'.
#' @param group `string` Limit to the member countries of a group in the `groups` table, e.g.
#'   'Euro area', 'European Union', 'Mercosur', 'G7'.
#' @return rank, country, iso3c, region, year, and the indicator.
#' @measure
indicator_ranking <- function(
  wdi,
  indicator,
  year = NULL,
  top_n = 10,
  lowest_first = FALSE,
  region = NULL,
  group = NULL
) {
  wdi_check_indicator(wdi, indicator)
  values <- dbReadTable(wdi, "countries") |>
    select(country, iso3c, region, year, value = all_of(indicator)) |>
    filter(!is.na(value))
  if (!test_null(group)) {
    members <- wdi_group_members(wdi, group)
    values <- values |>
      filter(iso3c %in% members)
  }
  # The arguments region and year have the names of columns: .env and .data tell them apart
  if (!test_null(region)) {
    regions <- sort(unique(values$region))
    if (!test_choice(region, regions)) {
      abort(glue("Unknown region '{region}'. Use one of: {str_flatten_comma(regions)}."), call = NULL)
    }
    values <- values |>
      filter(.data$region == .env$region)
  }
  if (!test_data_frame(values, min.rows = 1)) {
    abort(glue("No data for {indicator}."), call = NULL)
  }
  if (test_null(year)) {
    year <- values |>
      count(year) |>
      filter(n >= max(n) / 2) |>
      pull(year) |>
      max()
  }
  values <- values |>
    filter(.data$year == .env$year)
  if (!test_data_frame(values, min.rows = 1)) {
    abort(glue("No countries report {indicator} for {year}."), call = NULL)
  }
  values |>
    arrange(if (lowest_first) value else desc(value)) |>
    mutate(rank = row_number()) |>
    select(rank, country, iso3c, region, year, value) |>
    rename_with(\(name) indicator, value) |>
    slice_head(n = top_n)
}

#' Total and average annual growth of an indicator between two years
#'
#' Use for "how much did it grow", "which countries grew the most / fastest since", "growth from
#' 2020 to now" and "average annual growth between two years" questions, e.g. "How fast did China's
#' GDP grow per year from 2000 to 2020?", "Which eurozone countries grew the most since 2020?". For
#' real economic growth use gdp_constant_usd (GDP at constant prices). Compares an amount (GDP, GDP
#' per capita or population) in start_year with end_year, for countries, regions or the members of
#' a group (euro area / eurozone, EU, Mercosur, BRICS, ASEAN, G7, G20, OECD...), highest growth
#' first. Not for a single year's GDP growth (use latest_values or indicator_series with
#' gdp_growth_pct), nor for rates or shares (inflation, unemployment, % of GDP).
#'
#' @param indicator `string` Amount column: gdp_constant_usd (real GDP), gdp_usd, gdp_per_capita_usd,
#'   gdp_per_capita_ppp or population.
#' @param economies `string` Countries or aggregates separated by semicolons, as World Bank names or
#'   ISO3 codes, e.g. 'Brazil; Chile'.
#' @param start_year `integer` Start year: growth is measured from this year's level, so "since 2020"
#'   is 2020.
#' @param end_year `integer` End year, after start_year.
#' @param group `string` Also include every member country of a group in the `groups` table, e.g.
#'   'Euro area', 'European Union', 'Mercosur', 'G7'.
#' @return economy, iso3c, start and end years and values, total_growth_pct (whole period) and
#'   annual_growth_pct (compound average per year), highest first; NA when either year is missing.
#' @measure
compound_growth <- function(wdi, indicator, economies = NULL, start_year, end_year, group = NULL) {
  if (!test_true(end_year > start_year)) {
    abort("end_year must be after start_year.", call = NULL)
  }
  if (!test_choice(indicator, wdi_amount_indicators)) {
    abort(
      glue(
        "compound_growth needs an amount: {str_flatten_comma(wdi_amount_indicators)}. For real GDP ",
        "growth over several years use gdp_constant_usd, not the yearly gdp_growth_pct rates."
      ),
      call = NULL
    )
  }
  years <- end_year - start_year
  wdi_economy_values(wdi, indicator, economies, group) |>
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
      total_growth_pct = if_else(start_value > 0, 100 * (end_value / start_value - 1), NA_real_),
      annual_growth_pct = if_else(start_value > 0, 100 * ((end_value / start_value)^(1 / years) - 1), NA_real_)
    ) |>
    arrange(desc(total_growth_pct)) |>
    select(economy, iso3c, start_year, start_value, end_year, end_value, total_growth_pct, annual_growth_pct)
}

# Chart text and grid colours readable on both the light and the dark theme
chart_text_colour <- "#8a8a8a"
chart_grid_colour <- "rgba(128, 128, 128, 0.25)"

# Divisor and axis note that keep large amounts (GDP in US$, population) readable on the axis
chart_unit <- function(values) {
  largest <- max(abs(values))
  if (largest >= 1e12) {
    list(divisor = 1e12, note = " (trillions)")
  } else if (largest >= 1e9) {
    list(divisor = 1e9, note = " (billions)")
  } else if (largest >= 1e6) {
    list(divisor = 1e6, note = " (millions)")
  } else {
    list(divisor = 1, note = "")
  }
}

chart_number <- function(x) format(signif(x, 4), big.mark = ",", scientific = FALSE, trim = TRUE)

# How the chart writes values (JavaScript, run by echarts in the browser): percentages with two
# decimals and a % sign ("5.02%"), amounts with thousands separators and at most two decimals
chart_formats <- function(percent) {
  if (percent) {
    list(tooltip = JS("(value) => Number(value).toFixed(2) + '%'"), axis = "{value}%")
  } else {
    list(
      tooltip = JS("(value) => Number(value).toLocaleString('en-US', { maximumFractionDigits: 2 })"),
      axis = "{value}"
    )
  }
}

#' Line chart of an indicator over the years
#'
#' Use whenever the user asks for a chart, plot, graph, line chart, figure or visualization, or to
#' draw, show or visualize a trend over time, e.g. "Plot inflation in Brazil over the years", "Chart
#' GDP of India and China since 2000", "Graph unemployment in Spain", "Plot GDP growth for the G7
#' countries". Shows the user an interactive line chart with one line per country or region, or per
#' member of a group (euro area / eurozone, EU, Mercosur, BRICS, G7...), for any indicator: GDP, GDP growth,
#' GDP per capita, PPP, share of world GDP, inflation, unemployment, population, trade, exports, FDI,
#' current account, government debt, net debt, budget deficit, interest, private, household and
#' corporate debt, and the Gini index (inequality).
#'
#' @param indicator `string` Indicator column, e.g. gdp_usd, gdp_growth_pct, inflation_pct, population
#'   (same columns as latest_values).
#' @param economies `string` Countries or aggregates separated by semicolons, as World Bank names or
#'   ISO3 codes, e.g. 'Brazil; Chile' or 'BRA; CHL; WLD'.
#' @param start_year `integer` First year to include.
#' @param end_year `integer` Last year to include.
#' @param group `string` Also plot every member country of a group in the `groups` table, e.g.
#'   'Euro area', 'European Union', 'Mercosur', 'G7'.
#' @return The chart is shown to the user. You get a text summary per economy: years covered, first
#'   and last value, minimum and maximum with their years.
#' @measure
indicator_chart <- function(wdi, indicator, economies = NULL, start_year = NULL, end_year = NULL, group = NULL) {
  first_year <- if (test_null(start_year)) -Inf else start_year
  last_year <- if (test_null(end_year)) Inf else end_year
  rows <- wdi_economy_values(wdi, indicator, economies, group) |>
    rename(value = all_of(indicator)) |>
    filter(year >= first_year, year <= last_year) |>
    arrange(year, economy)
  if (!test_data_frame(rows, min.rows = 1)) {
    abort(glue("No {indicator} data for {str_flatten_comma(c(economies, group))} in those years."), call = NULL)
  }
  unit <- chart_unit(rows$value)
  # Indicators in percent are named *_pct or *_pct_gdp
  formats <- chart_formats(str_detect(indicator, "_pct"))

  chart <- rows |>
    mutate(year = as.character(year), value = value / unit$divisor) |>
    group_by(economy) |>
    e_charts(year, height = "320px", width = "100%") |>
    e_line(value, symbolSize = 5) |>
    e_axis_labels(y = glue("{indicator}{unit$note}")) |>
    e_y_axis(
      scale = TRUE,
      axisLabel = list(formatter = formats$axis),
      splitLine = list(lineStyle = list(color = chart_grid_colour))
    ) |>
    e_grid(left = 60, right = 40, top = 70, bottom = 30) |>
    e_legend(top = 0, left = "center", textStyle = list(color = chart_text_colour)) |>
    e_text_style(color = chart_text_colour) |>
    e_tooltip(trigger = "axis", valueFormatter = formats$tooltip) |>
    e_toolbox_feature("saveAsImage")

  summary <- rows |>
    summarise(
      text = glue(
        "{economy[1]}: {min(year)}-{max(year)}, first {chart_number(value[which.min(year)])}, ",
        "last {chart_number(value[which.max(year)])}, min {chart_number(min(value))} ({year[which.min(value)]}), ",
        "max {chart_number(max(value))} ({year[which.max(value)]})"
      ),
      .by = economy
    )
  # The chart goes in the reply (chart_stream() in grounding.R); the model only gets the summary.
  # No `extra$data`: commons would offer the rows to run_r, and re-analysing them there marks the
  # answer untrusted. The div keeps the widget's JavaScript, which shinychat drops from a bare widget.
  ContentToolResult(
    value = str_flatten(c(glue("Line chart of {indicator} by year, shown to the user."), summary$text), "\n"),
    extra = list(chart = div(as.tags(chart)))
  )
}
