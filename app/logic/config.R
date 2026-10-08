# Default model: a router that picks a random free model per request. Users can
# switch to any other free model from the dropdown below the chat.
#' @export
openrouter_model <- "openrouter/free"

# Public model catalogue (no API key needed); free models have zero prompt and completion pricing.
#' @export
models_url <- "https://openrouter.ai/api/v1/models"

# How long the fetched model list is reused before asking OpenRouter again.
#' @export
models_cache_seconds <- 3600

#' @export
system_prompt <- "You are a helpful assistant."

#' @export
disclaimer <- "This is a personal demo project. Please don't share sensitive or personal information."

#' @export
greeting <-"**Hello!** How can I help you today?"

#' @export
app_title <- "shinychat + OpenRouter"
