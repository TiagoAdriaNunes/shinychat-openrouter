box::use(
  glue[glue],
  htmltools[htmlEscape],
  purrr[keep, map_chr],
  stringr[fixed, str_c, str_flatten, str_flatten_comma, str_replace_all, str_trim],
)

box::use(
  . / config[world_bank],
)

# A shinychat suggestion: clicking it fills the message box with its text
suggestion <- function(text) as.character(glue('<span class="suggestion">{htmlEscape(text)}</span>'))

# A "|" would end a Markdown table cell
table_cell <- function(text) str_replace_all(text, fixed("|"), "\\|")

# A section that opens on click. The Markdown inside needs blank lines around it to be rendered.
collapsible <- function(summary, count, body) {
  template <- "<details>\n<summary><strong>{htmlEscape(summary)}</strong> ({count})</summary>\n\n{body}\n\n</details>"
  as.character(glue(template, .trim = FALSE))
}

#' The source (an entry of `sources`) an indicator comes from: its `source` id, World Bank (`wdi`)
#' without one.
#' @export
indicator_source <- function(indicator, sources = world_bank$sources) {
  id <- indicator$source %||% "wdi"
  keep(sources, \(s) identical(s$id, id))[[1]]
}

#' The Agent mode catalog as Markdown: an example question for each kind of question, then
#' collapsible lists of the indicators (each with its source and an example question), the country
#' groups and the data sources (linked). Defaults come from `world_bank` in config.yml.
#' @export
catalog_markdown <- function(
  catalog = world_bank$catalog,
  indicators = world_bank$indicators,
  groups = world_bank$groups,
  sources = world_bank$sources
) {
  questions <- map_chr(catalog$questions, \(q) str_c("- ", q$label, ": ", suggestion(q$example)))
  indicator_rows <- map_chr(indicators, \(i) {
    cells <- c(htmlEscape(i$description), htmlEscape(indicator_source(i, sources)$short_name), suggestion(i$example))
    str_c("| ", str_flatten(table_cell(cells), " | "), " |")
  })
  header <- c(catalog$indicator_column, catalog$source_column, catalog$example_column)
  indicator_table <- str_flatten(
    c(str_c("| ", str_flatten(header, " | "), " |"), "| --- | --- | --- |", indicator_rows),
    "\n"
  )
  group_names <- htmlEscape(str_flatten_comma(map_chr(groups, "name")))
  groups_text <- str_c(group_names, ".\n\n", catalog$groups_text, " ", suggestion(catalog$group_example))
  source_list <- str_flatten(
    map_chr(sources, \(s) as.character(glue("- [{htmlEscape(s$name)}]({s$url}): {htmlEscape(s$covers)}"))),
    "\n"
  )

  str_flatten(
    c(
      str_c("**", catalog$questions_heading, "**"),
      str_flatten(questions, "\n"),
      collapsible(catalog$indicators_heading, length(indicators), indicator_table),
      collapsible(catalog$groups_heading, length(groups), groups_text),
      collapsible(catalog$sources_heading, length(sources), source_list)
    ),
    "\n\n"
  )
}

#' Greeting of Agent mode: the configured text followed by the catalog
#' @export
world_bank_greeting <- str_c(str_trim(world_bank$greeting), "\n\n", catalog_markdown())
