box::use(
  cachem[cache_mem],
  httr2[req_perform, req_timeout, request, resp_body_json],
  logger[log_info, log_warn],
  memoise[memoise],
  purrr[keep, map_chr, map_dbl, set_names],
)

box::use(
  . / config[
    excluded_models, fallback_model, fallback_model_label, models_cache_seconds, models_url,
    request_timeout_seconds,
  ],
)

is_free <- function(model) {
  identical(model$pricing$prompt, "0") && identical(model$pricing$completion, "0")
}

# Chat needs text out and nothing else (excludes e.g. music generation models)
is_text_only <- function(model) {
  identical(unlist(model$architecture$output_modalities), "text")
}

# The World Bank agent works through tools, so it needs models that support tool calling
supports_tools <- function(model) {
  "tools" %in% unlist(model$supported_parameters)
}

# Artificial Analysis intelligence index, or NA when missing or malformed (many free models have none)
intelligence <- function(model) {
  score <- model$benchmarks$artificial_analysis$intelligence_index
  if (is.numeric(score) && length(score) == 1) score else NA_real_
}

#' Fetch free text models from OpenRouter, uncached (only those supporting tool calling if
#' `tools_only`). Errors if the request fails or none are found.
#' @export
fetch_free_models <- function(tools_only = FALSE) {
  models <- request(models_url) |>
    req_timeout(request_timeout_seconds) |>
    req_perform() |>
    resp_body_json()

  free <- keep(models$data, \(m) {
    is_free(m) && is_text_only(m) && !m$id %in% excluded_models && (!tools_only || supports_tools(m))
  })
  if (length(free) == 0) {
    stop("No free text models in the response")
  }

  # Most intelligent first (the chat starts with the first one); unscored models after, newest first
  score <- map_dbl(free, intelligence)
  created <- map_dbl(free, \(m) if (is.numeric(m$created) && length(m$created) == 1) m$created else 0)
  ids <- set_names(map_chr(free, "id"), map_chr(free, "name"))
  log_info("Fetched {length(ids)} free OpenRouter models{if (tools_only) ' with tool calling' else ''}")
  ids[order(-score, -created)]
}

# Shared by all sessions in this R process. memoise only stores returned values,
# so a failed fetch is retried on the next call instead of being cached.
cached_fetch <- memoise(fetch_free_models, cache = cache_mem(max_age = models_cache_seconds))

#' Free text models on OpenRouter as a named character vector (name = label, value = id),
#' only those supporting tool calling if `tools_only`.
#'
#' Uses `fetch` (cached for `models_cache_seconds` by default); falls back to the
#' fallback model (a router that supports tools) if it fails.
#' @export
free_models <- function(fetch = cached_fetch, tools_only = FALSE) {
  tryCatch(fetch(tools_only = tools_only), error = function(e) {
    log_warn("Could not fetch OpenRouter models: {conditionMessage(e)}")
    set_names(fallback_model, fallback_model_label)
  })
}
