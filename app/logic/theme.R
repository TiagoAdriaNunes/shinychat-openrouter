box::use(
  bslib[bs_add_rules],
  shinychat[page_chat_theme],
)

box::use(
  . / config[theme_settings],
)

# Theme variables come from the `theme` section of config.yml.
# Top bar icons (GitHub link and dark mode toggle): bslib sets every element inside a toolbar to
# font-size 0.9rem, so these selectors need to be more specific than `.bslib-toolbar :not(...)`.
# Both icons get the same centred square so they line up; the toggle is drawn at 1.3em of its font-size.
#' @export
chat_theme <- do.call(page_chat_theme, theme_settings) |>
  bs_add_rules("
    .shiny-chat-page-toolbar-global .bslib-toolbar > .btn-link,
    .shiny-chat-page-toolbar-global .bslib-toolbar > bslib-input-dark-mode {
      display: inline-flex;
      align-items: center;
      justify-content: center;
      width: 2.25rem;
      height: 2.25rem;
      padding: 0;
      line-height: 1;
    }
    .shiny-chat-page-toolbar-global .bslib-toolbar > .btn-link > i {
      font-size: 1.35rem;
      /* Font Awesome glyphs sit slightly high in their line box; centre it with the toggle */
      translate: 0 0.08em;
    }
    .shiny-chat-page-toolbar-global .bslib-toolbar > bslib-input-dark-mode {
      font-size: 1.1rem;
    }
  ")
