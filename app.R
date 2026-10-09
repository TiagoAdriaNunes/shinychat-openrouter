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
  app / logic / wdi[wdi_panel],
  app / main,
)

# Set LOG_LEVEL (e.g. DEBUG, INFO, WARN, ERROR) in the environment to change verbosity
log_threshold(Sys.getenv("LOG_LEVEL", "INFO"))
log_info("Starting shinychat-openrouter app")

# Fail fast with a clear message instead of an error inside the first chat session
check_model(fallback_model)
check_api_key()

# Load the World Bank data (from the disk cache, or a ~30 s download) and build the agent's context
# index now, so turning on World Bank data mode and the first question are quick. If loading fails,
# the mode retries when it is turned on.
options(commons.context_cache = world_bank$context_cache)
panel <- wdi_panel()
if (!test_null(panel)) prewarm_agent(panel)

shinyApp(main$ui(), main$server)
