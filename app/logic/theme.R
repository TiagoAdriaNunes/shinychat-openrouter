box::use(
  bslib[bs_add_rules],
  commons[commons_theme],
  rlang[exec],
)

box::use(
  . / config[theme_settings],
)

# Theme from the `theme` section of config.yml: shinychat's page_chat_theme() plus commons' chat assets
# (provenance markers). The top bar icons' rules beat bslib's toolbar font-size with more specific
# selectors and give both icons the same centred square.
#' @export
chat_theme <- exec(commons_theme, !!!theme_settings) |>
  bs_add_rules(
    "
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
  "
  )
