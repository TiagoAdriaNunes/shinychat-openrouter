# Chart measures for the World Bank agent. Runs in commons' own R session: import packages with
# box::use(), but not app modules or other measure files (hence its own economy lookup).
#
# A chart measure returns an ellmer::ContentToolResult: `extra$chart` is the interactive chart, which
# chart_stream() (app/logic/agent.R) puts in the reply itself (as `extra$display` it would sit inside
# the collapsed "Ran a trusted calculation" card), and `value` a text summary for the model. No
# `extra$data`: commons would offer the rows to run_r ("Available to `run_r` as `r1`"), and a model
# then re-analysed them there, which marks the whole answer untrusted. Never return the plot itself
# or an image: commons would send it to the model, and free models that can't read images fail with
# "No endpoints found that support image input".

box::use(
  checkmate[test_character, test_choice, test_data_frame, test_null, test_subset],
  DBI[dbListFields, dbReadTable],
  dplyr[all_of, arrange, bind_rows, filter, group_by, mutate, pull, select, summarise],
  duckplyr[methods_overwrite],
  echarts4r[e_axis_labels, e_charts, e_grid, e_legend, e_line, e_text_style, e_toolbox_feature, e_tooltip, e_y_axis],
  ellmer[ContentToolResult],
  glue[glue],
  htmltools[as.tags, div],
  htmlwidgets[JS],
  rlang[abort],
  stringr[fixed, str_detect, str_flatten, str_flatten_comma, str_split_1, str_subset, str_to_lower, str_trim],
)

# Run the dplyr verbs on duckdb, as in world-bank.R
Sys.setenv(DUCKPLYR_FALLBACK_COLLECT = 0)
suppressMessages(methods_overwrite())

# Columns that aren't indicators
chart_id_columns <- c("country", "aggregate", "iso3c", "region", "income", "year")

# Text and grid colours readable on both the light and the dark theme
chart_text_colour <- "#8a8a8a"
chart_grid_colour <- "rgba(128, 128, 128, 0.25)"

# ISO3 codes of the members of a country group in the `groups` table (name in any case), or none
# without a group
chart_group_members <- function(wdi, group) {
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

# Non-missing values of `indicator` by year for the named countries or aggregates (World Bank names
# or ISO3 codes, any case) and the members of `group`, as columns economy, iso3c, year and value.
# Errors on unknown names so the model can correct them.
chart_values <- function(wdi, indicator, economies, start_year, end_year, group = NULL) {
  indicators <- setdiff(dbListFields(wdi, "countries"), chart_id_columns)
  if (!test_choice(indicator, indicators)) {
    abort(glue("Unknown indicator '{indicator}'. Use one of: {str_flatten_comma(indicators)}."), call = NULL)
  }
  named <- if (test_null(economies)) character() else str_subset(str_trim(str_split_1(economies, fixed(";"))), ".")
  wanted <- c(named, chart_group_members(wdi, group))
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
  first_year <- if (test_null(start_year)) -Inf else start_year
  last_year <- if (test_null(end_year)) Inf else end_year
  rows <- rows |>
    filter(
      str_to_lower(economy) %in% str_to_lower(wanted) | str_to_lower(iso3c) %in% str_to_lower(wanted),
      !is.na(value),
      year >= first_year,
      year <= last_year
    ) |>
    arrange(year, economy)
  if (!test_data_frame(rows, min.rows = 1)) {
    abort(glue("No {indicator} data for {str_flatten_comma(wanted)} in those years."), call = NULL)
  }
  as.data.frame(rows)
}

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
  rows <- chart_values(wdi, indicator, economies, start_year, end_year, group)
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
  ContentToolResult(
    value = str_flatten(c(glue("Line chart of {indicator} by year, shown to the user."), summary$text), "\n"),
    # In a div: shinychat drops the JavaScript dependencies of a bare widget or tag list in a
    # streamed reply (the chart's space stays blank)
    extra = list(chart = div(as.tags(chart)))
  )
}
