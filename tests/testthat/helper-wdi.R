# Minimal World Bank panel shaped like app/logic/wdi.R's split_panel() output (two indicators
# set, the others missing)
fake_wdi_raw <- function() {
  indicators <- c(
    "gdp_usd", "gdp_growth_pct", "gdp_per_capita_usd", "gdp_per_capita_ppp", "inflation_pct",
    "unemployment_pct", "population", "trade_pct_gdp", "exports_pct_gdp", "fdi_inflows_pct_gdp",
    "gov_debt_pct_gdp", "gini"
  )
  raw <- data.frame(
    country = rep(c("Brazil", "Chile", "Korea, Rep.", "World", "High income"), each = 3),
    iso2c = rep(c("BR", "CL", "KR", "1W", "XD"), each = 3),
    iso3c = rep(c("BRA", "CHL", "KOR", "WLD", "HIC"), each = 3),
    year = rep(2020:2022, 5),
    # The aggregates: WDI labels one "Aggregates" and leaves the other without a region
    region = rep(c("Latin America & Caribbean", "Latin America & Caribbean", "East Asia & Pacific", "Aggregates", NA), each = 3),
    income = rep(c("Upper middle income", "High income", "High income", "Aggregates", NA), each = 3)
  )
  for (name in indicators) raw[[name]] <- NA_real_
  raw$gdp_usd <- c(100, 121, NA, 50, 55, 60.5, 200, 210, 220, 1000, 1100, 1210, 600, 630, 660)
  raw$inflation_pct <- c(3, 8, 9, 3, 7, NA, 0.5, 2.5, 5, 2, 3, 8, 1, 3, 7)
  attr(raw$gdp_usd, "label") <- "GDP (current US$)"
  raw[sample(nrow(raw)), ]
}
