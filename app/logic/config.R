box::use(
  config[get_config = get],
)

# Settings live in config.yml at the project root; R_CONFIG_ACTIVE picks the section (default: "default").
# Resolved relative to this file so it works from any working directory (e.g. tests).
settings <- get_config(file = box::file("..", "..", "config.yml"))

#' @export
app_title <- settings$app$title

#' @export
greeting <- settings$app$greeting

#' @export
placeholder <- settings$app$placeholder

#' @export
disclaimer <- settings$app$disclaimer

#' @export
source_url <- settings$app$source_url

#' @export
show_response_stats <- isTRUE(settings$app$show_response_stats)

#' @export
system_prompt <- settings$chat$system_prompt

#' @export
fallback_model <- settings$openrouter$fallback_model

#' @export
fallback_model_label <- settings$openrouter$fallback_model_label

#' @export
models_url <- settings$openrouter$models_url

#' @export
models_page_url <- settings$openrouter$models_page_url

#' @export
models_cache_seconds <- settings$openrouter$models_cache_seconds

#' @export
request_timeout_seconds <- settings$openrouter$request_timeout_seconds

#' @export
excluded_models <- as.character(unlist(settings$openrouter$excluded_models))

#' @export
error_messages <- settings$errors

#' @export
theme_settings <- settings$theme
