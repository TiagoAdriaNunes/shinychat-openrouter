# shinychat + OpenRouter - Entry Point

# Set box module search path to the project root so `app/...` modules resolve
options(box.path = getwd())

box::use(
  logger[log_info, log_threshold],
  shiny[shinyApp],
)

box::use(
  app / logic / checks[check_api_key, check_model],
  app / logic / config[fallback_model],
  app / main,
)

# Set LOG_LEVEL (e.g. DEBUG, INFO, WARN, ERROR) in the environment to change verbosity
log_threshold(Sys.getenv("LOG_LEVEL", "INFO"))
log_info("Starting shinychat-openrouter app")

# Fail fast with a clear message instead of an error inside the first chat session
check_model(fallback_model)
check_api_key()

shinyApp(main$ui(), main$server)
