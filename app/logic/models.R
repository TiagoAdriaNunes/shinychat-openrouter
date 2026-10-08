box::use(
  cachem[cache_mem],
  httr2[req_perform, req_timeout, request, resp_body_json],
  logger[log_info, log_warn],
  memoise[memoise],
  purrr[keep, map_chr, set_names],
)

box::use(
  . / config[
    models_cache_seconds, models_url, openrouter_model, openrouter_model_label, request_timeout_seconds,
  ],
)

is_free <- function(model) {
  identical(model$pricing$prompt, "0") && identical(model$pricing$completion, "0")
}

# Chat needs text out and nothing else (excludes e.g. music generation models)
is_text_only <- function(model) {
  identical(unlist(model$architecture$output_modalities), "text")
}

#' Fetch free text models from OpenRouter, uncached. Errors if the request fails or none are found.
#' @export
fetch_free_models <- function() {
  models <- request(models_url) |>
    req_timeout(request_timeout_seconds) |>
    req_perform() |>
    resp_body_json()

  free <- keep(models$data, \(m) is_free(m) && is_text_only(m))
  if (length(free) == 0) {
    stop("No free text models in the response")
  }

  ids <- set_names(map_chr(free, "id"), map_chr(free, "name"))
  ids <- ids[order(names(ids))]
  log_info("Fetched {length(ids)} free OpenRouter models")

  # Keep the default first so it is the initial selection
  c(ids[ids == openrouter_model], ids[ids != openrouter_model])
}

# Shared by all sessions in this R process. memoise only stores returned values,
# so a failed fetch is retried on the next call instead of being cached.
cached_fetch <- memoise(fetch_free_models, cache = cache_mem(max_age = models_cache_seconds))

#' Free text models on OpenRouter as a named character vector (name = label, value = id).
#'
#' Uses `fetch` (cached for `models_cache_seconds` by default); falls back to the
#' default model if it fails.
#' @export
free_models <- function(fetch = cached_fetch) {
  tryCatch(fetch(), error = function(e) {
    log_warn("Could not fetch OpenRouter models: {conditionMessage(e)}")
    set_names(openrouter_model, openrouter_model_label)
  })
}
