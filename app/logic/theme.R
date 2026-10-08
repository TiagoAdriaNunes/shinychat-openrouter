box::use(
  shinychat[page_chat_theme],
)

box::use(
  . / config[theme_settings],
)

# Theme variables come from the `theme` section of config.yml
#' @export
chat_theme <- do.call(page_chat_theme, theme_settings)
