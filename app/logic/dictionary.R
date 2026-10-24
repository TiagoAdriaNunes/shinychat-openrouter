box::use(
  commons[data_source],
  glue[glue],
  purrr[list_rbind, map, map_chr],
  stringr[str_c, str_flatten_comma],
  yaml[write_yaml],
)

box::use(
  . / config[world_bank],
  . / indicators[is_imf],
)

#' Names of the configured country groups.
#' @export
group_names <- function() map_chr(world_bank$groups, "name")

#' The `world_bank$groups` memberships as a table: one row per group and member (columns group, iso3c).
#' @export
groups_table <- function() {
  map(world_bank$groups, \(g) data.frame(group = g$name, iso3c = as.character(unlist(g$members)))) |>
    list_rbind()
}

# Data dictionary (data-dict.yaml) describing the agent's tables, written to a temporary file
dictionary_file <- function(panel) {
  indicator_columns <- map(world_bank$indicators, function(i) {
    source <- glue("World Bank series {i$code}.")
    if (is_imf(i)) {
      source <- glue("IMF indicator {i$code}; countries only, empty in `aggregates`.")
    }
    list(name = i$name, description = glue("{i$description}. {source}"))
  })
  dictionary <- list(
    name = "Economic indicators",
    description = glue(
      "Economic indicators from the {str_flatten_comma(map_chr(world_bank$sources, 'name'), ' and ')}, ",
      "{world_bank$start_year} onward, downloaded on {panel$downloaded}. Each indicator column names its source."
    ),
    details = str_c(
      "Values are missing (NULL) when the World Bank has no estimate; the latest year with data differs by ",
      "country and indicator. Country names follow World Bank spelling (e.g. 'Korea, Rep.', 'Turkiye')."
    ),
    tables = list(
      list(
        name = "countries",
        description = "One row per country (economy) and year.",
        details = "Only individual economies; never sum them to get regional or world totals, use `aggregates`.",
        columns = c(
          list(
            list(name = "country", description = "Economy name, World Bank spelling."),
            list(name = "iso3c", description = "ISO 3166-1 alpha-3 code."),
            list(name = "region", description = "World Bank geographic region."),
            list(name = "income", description = "World Bank income group (e.g. 'High income', 'Low income')."),
            list(name = "year", description = "Calendar year.")
          ),
          indicator_columns
        )
      ),
      list(
        name = "aggregates",
        description = str_c(
          "One row per World Bank aggregate and year: World, regions, income groups, Euro area, OECD members ",
          "and other groupings."
        ),
        details = str_c(
          "Computed by the World Bank (weighted where appropriate), so use these instead of combining ",
          "countries."
        ),
        columns = c(
          list(
            list(name = "aggregate", description = "Aggregate name, e.g. 'World', 'Euro area', 'High income'."),
            list(name = "iso3c", description = "World Bank aggregate code, e.g. 'WLD', 'EMU', 'HIC'."),
            list(name = "year", description = "Calendar year.")
          ),
          indicator_columns
        )
      ),
      list(
        name = "groups",
        description = glue("Member countries of country groups: {str_flatten_comma(group_names())}."),
        details = str_c(
          "Current membership, applied to every year. Join to `countries` on iso3c to list or rank a ",
          "group's countries; the group's total is in `aggregates`."
        ),
        columns = list(
          list(name = "group", description = "Group name, e.g. 'Euro area', 'European Union', 'Mercosur'."),
          list(name = "iso3c", description = "ISO3 code of a member country.")
        )
      )
    )
  )
  path <- tempfile("wdi-", fileext = ".yaml")
  write_yaml(dictionary, path)
  path
}

#' commons data source with the panel's `countries` and `aggregates` tables and the configured
#' `groups` (an in-memory duckdb). Create one per Shiny session and reuse it for every agent in it.
#' @export
wdi_source <- function(panel) {
  data_source(
    countries = panel$countries,
    aggregates = panel$aggregates,
    groups = groups_table(),
    dictionary = dictionary_file(panel)
  )
}
