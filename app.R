# shinychat + OpenRouter - Entry Point

# Set box module search path to the project root so `app/...` modules resolve
options(box.path = getwd())

box::use(
  checkmate[test_null],
  logger[log_info, log_threshold],
  shiny[shinyApp],
)

box::use(
  app / logic / checks[check_api_key, check_model],
  app / logic / agent[prewarm_agent],
  app / logic / config[fallback_model, world_bank],
  app / logic / models[free_models, reasoning_options],
  app / logic / wdi[start_wdi_download, wdi_panel],
  app / main,
)

# Set LOG_LEVEL (e.g. DEBUG, INFO, WARN, ERROR) in the environment to change verbosity
log_threshold(Sys.getenv("LOG_LEVEL", "INFO"))
log_info("Starting shinychat-openrouter app")

# Fail fast with a clear message instead of an error inside the first chat session
check_model(fallback_model)
check_api_key()

# Fetch the model lists (chat and Agent mode) and effort levels now; every session reuses them for
# models_cache_seconds, so neither the first visitor nor the first switch to Agent mode waits for them
invisible(free_models())
invisible(reasoning_options())

# Load the World Bank data from the disk cache and build the agent's context index now, so turning
# on Agent mode and the first question are quick. A missing or old cache is downloaded
# (~30 s) in the background instead of delaying the start: an old copy is used meanwhile, and with
# no copy World Bank data mode shows that it is downloading and turns on when it is done.
options(commons.context_cache = world_bank$context_cache)
start_wdi_download()
panel <- wdi_panel()
if (!test_null(panel)) {
  prewarm_agent(panel)
}

shinyApp(main$ui(), main$server)
