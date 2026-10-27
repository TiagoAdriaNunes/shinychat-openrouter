box::use(
  cachem[cache_mem],
  checkmate[test_null, test_number, test_string],
  httr2[req_auth_bearer_token, req_perform, req_perform_parallel, req_timeout, request, resp_body_json],
  logger[log_info, log_warn],
  memoise[memoise],
  purrr[compact, keep, map, map_chr, map_dbl, map2_dbl, set_names],
  rlang[abort, is_condition],
  stringr[str_c, str_flatten],
)

box::use(
  . / config[
    excluded_models, fallback_model, fallback_model_label, low_priority_models, min_uptime_30m,
    models_cache_seconds, models_url, request_timeout_seconds, user_models_url,
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

get_json <- function(url, req = request(url)) {
  req |>
    req_timeout(request_timeout_seconds) |>
    req_perform() |>
    resp_body_json()
}

# Ids of the models the API key's account can use (OpenRouter applies its provider and privacy
# settings), or NULL if there's no key or the request fails. The full catalogue still comes from
# models_url: this list lacks the benchmarks and reasoning fields.
usable_ids <- function() {
  key <- Sys.getenv("OPENROUTER_API_KEY")
  if (!test_string(key, min.chars = 1)) {
    return(NULL)
  }
  tryCatch(
    map_chr(get_json(user_models_url, req_auth_bearer_token(request(user_models_url), key))$data, "id"),
    error = function(e) {
      log_warn("Could not fetch the models this account can use: {conditionMessage(e)}")
      NULL
    }
  )
}

# Drop the models the account can't use (see usable_ids()); keeps them all when that list is missing
keep_usable <- function(models) {
  usable <- usable_ids()
  if (test_null(usable)) {
    return(models)
  }
  dropped <- setdiff(map_chr(models, "id"), usable)
  if (length(dropped) > 0) {
    log_info("Left out as unusable with this account's provider settings: {str_flatten(dropped, ', ')}")
  }
  keep(models, \(m) m$id %in% usable)
}

# Share (%) of requests the model's best provider answered in the last 30 minutes, from the model's
# <models_url>/<id>/endpoints response (`resp`, or the error its request ended with); NA when
# OpenRouter has no figure (no requests in that time) or the lookup failed
uptime_from <- function(resp, id) {
  failed <- function(cnd) {
    log_warn("Could not fetch the uptime of {id}: {conditionMessage(cnd)}")
    NA_real_
  }
  if (is_condition(resp)) {
    return(failed(resp))
  }
  tryCatch(
    {
      uptime <- map_dbl(resp_body_json(resp)$data$endpoints, function(e) {
        if (test_number(e$uptime_last_30m)) e$uptime_last_30m else NA_real_
      })
      if (all(is.na(uptime))) NA_real_ else max(uptime, na.rm = TRUE)
    },
    error = failed
  )
}

# Uptimes (see uptime_from()) of the models `ids`, named by id: one request per model, sent in
# parallel (one after another, they took several seconds)
uptimes_30m <- function(ids) {
  requests <- map(ids, \(id) req_timeout(request(str_c(models_url, "/", id, "/endpoints")), request_timeout_seconds))
  responses <- req_perform_parallel(requests, on_error = "continue", progress = FALSE)
  set_names(map2_dbl(responses, ids, uptime_from), ids)
}

# Models that were up for at least `min_uptime_30m` percent of the last 30 minutes (`uptime`, from
# uptimes_30m()). All of them if none passes (e.g. the uptime lookups failed): better a list to try
# than the fallback router.
keep_reliable <- function(models, uptime) {
  if (!test_number(min_uptime_30m) || length(models) == 0) {
    return(models)
  }
  reliable <- keep(models, \(m) isTRUE(unname(uptime[m$id]) >= min_uptime_30m))
  if (length(reliable) == 0) {
    log_warn("No free model has {min_uptime_30m}% uptime in the last 30 minutes; listing all of them")
    return(models)
  }
  dropped <- setdiff(map_chr(models, "id"), map_chr(reliable, "id"))
  if (length(dropped) > 0) {
    log_info("Left out for uptime below {min_uptime_30m}% (or unknown): {str_flatten(dropped, ', ')}")
  }
  reliable
}

# The reliable models among `free` (only those supporting tool calling if `tools_only`) as a named
# character vector (name = label, value = id): most intelligent first (the chat starts with the
# first one); unscored models after, newest first; low priority models last
pick_models <- function(free, uptime, tools_only) {
  if (tools_only) {
    free <- keep(free, supports_tools)
  }
  free <- keep_reliable(free, uptime)
  score <- map_dbl(free, intelligence)
  created <- map_dbl(free, \(m) if (is.numeric(m$created) && length(m$created) == 1) m$created else 0)
  ids <- set_names(map_chr(free, "id"), map_chr(free, "name"))
  log_info("Fetched {length(ids)} free OpenRouter models{if (tools_only) ' with tool calling' else ''}")
  ids[order(ids %in% low_priority_models, -score, -created)]
}

# Reasoning effort levels the model offers (OpenRouter's `reasoning.supported_efforts`, highest first)
# and its default effort, or NULL when it has no effort selection. "none" is left out when the model
# always reasons (`mandatory`), since it rejects that effort.
model_reasoning <- function(model) {
  reasoning <- model$reasoning
  efforts <- as.character(unlist(reasoning$supported_efforts))
  if (isTRUE(reasoning$mandatory)) {
    efforts <- setdiff(efforts, "none")
  }
  if (length(efforts) == 0) {
    return(NULL)
  }
  default <- reasoning$default_effort
  list(efforts = efforts, default = if (test_string(default) && default %in% efforts) default else efforts[[1]])
}

#' Fetch OpenRouter's catalogue once, uncached: free text models (`all`), tool-calling ones (`tools`)
#' and every model's `efforts`. Errors on failure or no free model, so memoise never keeps an empty list.
#' @export
fetch_free_models <- function() {
  models <- get_json(models_url)$data
  free <- keep(models, \(m) is_free(m) && is_text_only(m) && !m$id %in% excluded_models)
  free <- keep_usable(free)
  if (length(free) == 0) {
    abort("No free text models in the response.")
  }
  uptime <- if (test_number(min_uptime_30m)) uptimes_30m(map_chr(free, "id"))
  list(
    all = pick_models(free, uptime, tools_only = FALSE),
    tools = pick_models(free, uptime, tools_only = TRUE),
    efforts = compact(set_names(map(models, model_reasoning), map_chr(models, "id")))
  )
}

# Shared by all sessions in this R process, and by the model lists and effort options. memoise only
# stores returned values, so a failed fetch is retried on the next call instead of being cached.
cached_fetch <- memoise(fetch_free_models, cache = cache_mem(max_age = models_cache_seconds))

#' Free text models on OpenRouter as a named character vector (name = label, value = id),
#' only those supporting tool calling if `tools_only`.
#'
#' Uses `fetch` (cached for `models_cache_seconds` by default; see fetch_free_models()); falls back
#' to the fallback model (a router that supports tools) if it fails or lists no model.
#' @export
free_models <- function(fetch = cached_fetch, tools_only = FALSE) {
  tryCatch(
    {
      ids <- fetch()[[if (tools_only) "tools" else "all"]]
      if (length(ids) == 0) {
        abort(str_c("No free text models", if (tools_only) " with tool calling", " in the response."))
      }
      ids
    },
    error = function(e) {
      log_warn("Could not fetch OpenRouter models: {conditionMessage(e)}")
      set_names(fallback_model, fallback_model_label)
    }
  )
}

# OpenRouter's effort levels, highest first
effort_levels <- c("max", "xhigh", "high", "medium", "low", "minimal", "none")

#' The level among `efforts` closest to `effort`, as OpenRouter maps an effort a model doesn't offer
#' (the higher one on a tie).
#' @export
nearest_effort <- function(effort, efforts) {
  distance <- abs(match(efforts, effort_levels) - match(effort, effort_levels))
  efforts[[which.min(distance)]]
}

#' Reasoning effort options by model id (`efforts` of fetch_free_models(), from the same cached
#' fetch as free_models()); an empty list if they can't be fetched, so every model keeps its default.
#' @export
reasoning_options <- function(fetch = cached_fetch) {
  tryCatch(fetch()$efforts, error = function(e) {
    log_warn("Could not fetch OpenRouter reasoning options: {conditionMessage(e)}")
    list()
  })
}
