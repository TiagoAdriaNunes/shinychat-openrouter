# shinychat + OpenRouter - Entry Point

# Set box module search path to the project root so `app/...` modules resolve
options(box.path = getwd())

box::use(
  shiny[shinyApp],
)

box::use(
  app / main,
)

shinyApp(main$ui(), main$server)
