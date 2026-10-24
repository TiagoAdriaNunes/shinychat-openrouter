box::use(
  purrr[keep],
)

box::use(
  . / config[world_bank],
)

#' The data source (an entry of `sources`, `world_bank$sources` by default) an indicator comes from:
#' its `source` id, the World Bank (`wdi`) without one.
#' @export
indicator_source <- function(indicator, sources = world_bank$sources) {
  id <- indicator$source %||% "wdi"
  keep(sources, \(source) identical(source$id, id))[[1]]
}

#' TRUE for an indicator downloaded from the IMF (`source: imf`) rather than with WDI.
#' @export
is_imf <- function(indicator) identical(indicator$source, "imf")
