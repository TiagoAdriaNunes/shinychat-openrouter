# Reading IMF indicators

## Government debt

- Government debt (`gov_debt_pct_gdp`) is general government gross debt as a share of GDP, from the IMF World Economic Outlook, not the World Bank. General government covers central, state and local government and social security funds.
- Gross debt is every liability that requires future payments of interest or principal: debt securities, loans, currency and deposits, insurance and pension liabilities, and other accounts payable. Equity, investment fund shares and financial derivatives are excluded.
- It is gross: the government's financial assets are not subtracted. A country that borrows to invest, such as Singapore, can show high gross debt while holding larger assets.
- Net debt (`gov_net_debt_pct_gdp`, IMF Fiscal Monitor) is gross debt minus the government's financial assets that match those debt instruments. It is negative when assets exceed debt, as in Norway. Fewer countries report it (about 85), so compare it with gross debt only where both exist.
- Valuation (market, nominal or face value) follows each country's reporting, and the figures can differ from those national governments or Eurostat publish under other definitions (the EU's Maastricht debt is measured at face value and leaves out some liabilities). Say the figure is the IMF's.

## Deficits, interest and the current account

- Net lending/borrowing (`gov_balance_pct_gdp`) is the general government budget balance: revenue minus spending. Positive is a surplus, negative a deficit, so the largest deficits are the lowest values; say "deficit of 5% of GDP" rather than "-5%".
- Interest paid on public debt (`gov_interest_pct_gdp`) comes from the IMF's Public Finances in Modern History database. Its latest year is often a year or two behind the World Economic Outlook.
- The current account balance (`current_account_pct_gdp`) is a country's exports minus imports of goods and services plus net income and transfers from abroad. Positive is a surplus (lending to the rest of the world), negative a deficit (borrowing from it).

## Share of world GDP

- `world_gdp_share_ppp_pct` is a country's GDP at purchasing power parity as a percent of world GDP. PPP values each country's output at common prices, so emerging economies such as China and India weigh more than at market exchange rates. The shares of all countries add up to about 100.

## Private, household and corporate debt

- `private_debt_pct_gdp`, `household_debt_pct_gdp` and `corporate_debt_pct_gdp` come from the IMF Global Debt Database: loans and debt securities owed by households and by nonfinancial companies. Private debt is the sum of household and corporate debt; government debt is separate.
- They cover fewer countries (about 160 for private debt, 86 for household and corporate debt) and end a year or two earlier than the World Economic Outlook. The most recent year often has only some countries, so a ranking defaults to the latest year most countries report.

## Editions and estimates

- The World Economic Outlook and the Fiscal Monitor are published every April and October; the data holds the latest editions. Their figures for the most recent year are often IMF estimates, and later editions revise them.
- Their forecasts for the current and coming years are not in the data.

## Coverage

- IMF indicators are in the `countries` table only: there are no world, regional, income group or euro area totals. For a group, list or rank its member countries (`group`).
- There are no IMF government debt figures for Cuba, North Korea, Somalia, Venezuela, Yemen or most small territories. Kosovo and West Bank and Gaza are included. Taiwan has IMF figures but is not in the data, which lists only World Bank economies.
