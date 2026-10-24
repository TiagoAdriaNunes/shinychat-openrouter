box::use(
  purrr[map_chr],
  stringr[fixed, str_c, str_detect, str_starts],
  testthat[expect_equal, expect_false, expect_match, expect_true, test_that],
)

box::use(
  app / logic / config[world_bank],
  app / logic / definitions[definitions_markdown, indicator_definitions],
)

imf_metadata <- list(
  GGXWDG_NGDP = list(
    description = "Gross debt consists of all liabilities that require payment.",
    source = "World Economic Outlook (October 2026)"
  )
)

test_that("indicator_definitions() has the WDI definition of every World Bank indicator, offline", {
  definitions <- indicator_definitions()
  expect_equal(definitions$name, map_chr(world_bank$indicators, "name"))

  wdi <- definitions[definitions$source == "World Bank", ]
  expect_false(anyNA(wdi$definition))
  inflation <- definitions[definitions$name == "inflation_pct", ]
  expect_match(inflation$definition, "consumer price index", fixed = TRUE)
  expect_match(inflation$organization, "International Monetary Fund", fixed = TRUE)
  # The source without its catalogue details (links, access dates)
  expect_false(any(str_detect(wdi$organization, fixed("uri:"))))
  # The IMF's definitions come from its metadata
  expect_true(all(is.na(definitions$definition[definitions$source == "IMF"])))
})

test_that("indicator_definitions() takes an IMF indicator's description and source from its metadata", {
  definitions <- indicator_definitions(imf_metadata)
  debt <- definitions[definitions$name == "gov_debt_pct_gdp", ]

  expect_equal(debt$definition, "Gross debt consists of all liabilities that require payment.")
  expect_equal(debt$organization, "World Economic Outlook (October 2026)")
  expect_equal(debt$code, "GGXWDG_NGDP")
})

test_that("definitions_markdown() makes one document per defined indicator, under the configured heading", {
  documents <- definitions_markdown(indicator_definitions(imf_metadata))
  heading <- world_bank$definitions$heading

  expect_true(all(str_starts(documents, fixed(str_c("# ", heading, ": ")))))
  expect_true(str_starts(documents[["inflation_pct"]], fixed(str_c("# ", heading, ": Inflation, consumer prices"))))
  expect_match(
    documents[["gov_debt_pct_gdp"]],
    "Source: World Economic Outlook (October 2026). IMF code: GGXWDG_NGDP.",
    fixed = TRUE
  )
  # An indicator without a definition gets no document
  expect_false("gov_net_debt_pct_gdp" %in% names(documents))
})
