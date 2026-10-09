# Reading World Bank indicators

## Units and prices

- GDP in current US$ (`gdp_usd`, `gdp_per_capita_usd`) converts each year's local-currency GDP at that year's official exchange rate. It mixes real growth, inflation and exchange-rate swings, so a fall in US$ GDP can come from a weaker currency rather than a shrinking economy.
- GDP growth (`gdp_growth_pct`) is real growth at constant local prices. Use it, not changes in current US$ GDP, to describe how fast an economy grew.
- GDP per capita, PPP (`gdp_per_capita_ppp`) is in constant 2021 international dollars, which adjust for price levels across countries and for inflation over time. Prefer it for comparing living standards between countries or over long periods.
- Inflation (`inflation_pct`) is the annual change in consumer prices. Values above 50% are hyperinflation episodes, not data errors.

## Coverage and gaps

- Recent years are often missing for small or fragile economies.
- Unemployment (`unemployment_pct`) is a modeled ILO estimate, available for most countries every year even where no survey was run.
- The Gini index (`gini`) comes from household surveys run every few years, so most countries have gaps; use each country's latest survey year and say how old it is.
- Central government debt (`gov_debt_pct_gdp`) is reported by few countries and covers central government only, not total public debt.
- Taiwan has no World Bank data. Kosovo, West Bank and Gaza and Hong Kong SAR, China are listed separately.

## Countries, regions and groups

- World Bank spellings of common names: "Korea, Rep." (South Korea), "Korea, Dem. People's Rep." (North Korea), "Russian Federation", "Turkiye", "Egypt, Arab Rep.", "Iran, Islamic Rep.", "Venezuela, RB", "Viet Nam", "Czechia", "Slovak Republic", "Congo, Dem. Rep." (Kinshasa), "Congo, Rep." (Brazzaville), "Cote d'Ivoire", "Lao PDR", "Gambia, The", "Bahamas, The", "Yemen, Rep.", "Syrian Arab Republic". ISO3 codes (e.g. KOR, RUS) avoid spelling problems.
- Regions and income groups are the World Bank's current classification, applied to every year in the data: a country that is high income today shows as high income in 1990 too.
- Income groups are set every July from GNI per capita (Atlas method); they are not based on the GDP figures in this data.
- Aggregates include World Bank estimates for countries with missing data, so an aggregate can have a value for a year in which some of its countries don't.
