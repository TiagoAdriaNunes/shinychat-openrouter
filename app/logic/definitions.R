box::use(
  checkmate[test_string],
  dplyr[filter],
  glue[glue_data],
  purrr[discard, list_rbind, map, set_names],
  stringr[fixed, str_flatten, str_remove, str_split_1, str_trim],
  WDI[WDI_data],
)

box::use(
  . / catalog[indicator_source],
  . / config[world_bank],
)

# A WDI source organisation without its catalogue details (", uri: ...", ", note: ...", ...), its
# sources separated by semicolons
clean_source <- function(text) {
  if (!test_string(text, min.chars = 1)) {
    return(NA_character_)
  }
  str_split_1(text, fixed(";")) |>
    str_remove(",\\s*(uri|note|publisher|type|date accessed|date published):[\\s\\S]*$") |>
    str_trim() |>
    discard(\(source) source == "") |>
    unique() |>
    str_flatten("; ")
}

#' The official definition of every configured indicator, one row each: its `name` (column),
#' `title` (config description), `source` (short name of the data source), `code`, `definition` and
#' `organization` (who publishes it). World Bank series come from the WDI package's series metadata,
#' offline; IMF indicators from `imf_metadata` (fetch_imf_metadata()), NA without it.
#' @export
indicator_definitions <- function(imf_metadata = list()) {
  map(world_bank$indicators, function(i) {
    source <- indicator_source(i)
    if (identical(source$id, "imf")) {
      definition <- imf_metadata[[i$code]]$description %||% NA_character_
      organization <- imf_metadata[[i$code]]$source %||% source$name
    } else {
      series <- filter(WDI_data$series, indicator == i$code)
      definition <- series$description[1]
      organization <- clean_source(series$sourceOrganization[1])
    }
    if (!test_string(definition, min.chars = 1)) {
      definition <- NA_character_
    }
    data.frame(
      name = i$name,
      title = i$description,
      source = source$short_name,
      code = i$code,
      definition = str_trim(definition),
      organization = organization
    )
  }) |>
    list_rbind()
}

#' The indicator definitions (indicator_definitions()) as Markdown documents for the agent's context
#' layer, one per indicator with a definition, named by its column.
#' @export
definitions_markdown <- function(definitions, heading = world_bank$definitions$heading) {
  defined <- filter(definitions, !is.na(definition))
  documents <- glue_data(
    defined,
    "# {heading}: {title} (`{name}`)\n\n{definition}\n\nSource: {organization}. {source} code: {code}."
  )
  set_names(as.character(documents), defined$name)
}
