# shinychat + OpenRouter - Entry Point

# Set box module search path to the project root so `app/...` modules resolve
options(box.path = getwd())

box::use(
  checkmate[test_null],
  later[later],
  logger[log_error, log_info, log_threshold],
  shiny[shinyApp],
)

box::use(
  app / logic / checks[check_api_key, check_model],
  app / logic / agent[prewarm_agent],
  app / logic / config[fallback_model, response_timeout_seconds, world_bank],
  app / logic / models[free_models, reasoning_options],
  app / logic / wdi[wdi_panel],
  app / main,
)

# Set LOG_LEVEL (e.g. DEBUG, INFO, WARN, ERROR) in the environment to change verbosity
log_threshold(Sys.getenv("LOG_LEVEL", "INFO"))
log_info("Starting shinychat-openrouter app")

# Fail fast with a clear message instead of an error inside the first chat session
check_model(fallback_model)
check_api_key()

# Safety limit on any model request (ellmer reads it as each request is built); stalls are caught
# much sooner by watch_stream() (app/logic/client.R)
options(ellmer_timeout_s = response_timeout_seconds)

options(commons.context_cache = world_bank$context_cache)

# Warm-ups run once runApp() has opened the port: before it, a ~5 s cold start made Fly's proxy give
# up and show the first visitor an error. Requests meanwhile wait; a failure is only logged.
later(function() {
  tryCatch(
    {
      free_models()
      reasoning_options()
      # Also starts a background download when the cache is missing or old
      panel <- wdi_panel()
      if (!test_null(panel)) {
        prewarm_agent(panel)
      }
    },
    error = function(e) log_error("Warm-up failed: {conditionMessage(e)}")
  )
})

shinyApp(main$ui(), main$server)
