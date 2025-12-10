# US Border Crossings Shiny Dashboard

This dashboard is a Shiny web app on legal U.S.-Canada and U.S.-Mexico land border crossings using public data from the Department of Transportation (data.gov, 2025). 

<B> App URL: https://rkdngc-d-m.shinyapps.io/project/ </B> (May take a few seconds to fully load)

The app was created to visualize activity by year, state, border, and transport type in an easy to follow UI. It was built in R with Shiny for the web layout, ggplot for the charts, and Plotly for interaction. The goal was set to take a large federal dataset and turn it into something that can provide value and depth without raw tables or static plots. It focuses on the trends over time, differences between borders, how traffic varies by ports and states, and overall volume.

## Dashboard Tabs & Content

<B> Overview </B>
- Total records, total border states, and total ports
- Year range and transport filter
- Annual totals by border and transport

<B> State Trends </B>
- Single state and transport selection
- Time series of crossings over decades

<B> Port Insights </B>
- State based port ranking
- Top ports by total traffic

## Data Source

The dataset couldnt be stored in this repository because of the file size (47MB) limit. It can be found on data.gov as "Border Crossing Entry Data", published by the Bureau of Transportation Statistics.
- Data.gov. (2025, November 22). Department of Transportation - Border Crossing Entry data. https://catalog.data.gov/dataset/border-crossing-entry-data-683ae
