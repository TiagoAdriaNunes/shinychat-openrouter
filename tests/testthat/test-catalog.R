box::use(
  htmltools[htmlEscape],
  purrr[map_chr],
  stringr[fixed, str_c, str_count, str_detect, str_starts, str_trim],
  testthat[expect_equal, expect_false, expect_true, test_that],
)

box::use(
  app / logic / catalog[catalog_markdown, world_bank_greeting],
  app / logic / config[world_bank],
)

suggestion <- function(text) str_c('<span class="suggestion">', text, "</span>")

test_that("the catalog has a clickable example for every kind of question and indicator, and lists every group", {
  catalog <- catalog_markdown()
  examples <- c(
    map_chr(world_bank$catalog$questions, "example"),
    map_chr(world_bank$indicators, "example"),
    world_bank$catalog$group_example
  )
  for (example in examples) {
    expect_true(str_detect(catalog, fixed(suggestion(htmlEscape(example)))), info = example)
  }
  for (indicator in world_bank$indicators) {
    expect_true(str_detect(catalog, fixed(htmlEscape(indicator$description))), info = indicator$name)
  }
  for (group in map_chr(world_bank$groups, "name")) {
    expect_true(str_detect(catalog, fixed(group)), info = group)
  }
  # Indicators and groups are collapsible sections
  expect_equal(str_count(catalog, fixed("<details>")), 2)
  expect_equal(str_count(catalog, fixed("</details>")), 2)
})

test_that("catalog text can't break the Markdown table or inject HTML", {
  catalog <- catalog_markdown(
    catalog = world_bank$catalog,
    indicators = list(list(name = "x", description = "A | B <b>", example = "Is <i>this</i> & that?")),
    groups = list(list(name = "G<1>"))
  )
  expect_true(str_detect(catalog, fixed("| A \\| B &lt;b&gt; |")))
  expect_true(str_detect(catalog, fixed(suggestion("Is &lt;i&gt;this&lt;/i&gt; &amp; that?"))))
  expect_true(str_detect(catalog, fixed("G&lt;1&gt;")))
  expect_false(str_detect(catalog, fixed("<i>")))
})

test_that("World Bank data mode's greeting is the configured text followed by the catalog", {
  expect_true(str_starts(world_bank_greeting, fixed(str_trim(world_bank$greeting))))
  expect_true(str_detect(world_bank_greeting, fixed(catalog_markdown())))
})
