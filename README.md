# Air Quality Monitoring Dashboard

## Overview
An interactive R Shiny dashboard for exploring synthetic air-quality observations across eight cities.

## Problem statement
Air-quality indicators are easier to understand when pollutant levels, city comparisons, and historical patterns are presented together in an accessible interface.

## Objectives
- Monitor AQI and key pollutant measurements.
- Compare city-level air quality.
- Explore historical patterns and pollutant relationships.
- Provide a clearly labeled educational AQI estimate.

## Dataset
`data/air_quality.csv` contains 500 reproducible synthetic records spanning 2025. Values are illustrative, not live or regulatory monitoring data. Run `generate_data.R` to recreate it.

## Features
Overview KPIs and gauge, AQI category distribution, interactive pollutant charts, city comparison, date trends, correlation insights, educational AQI estimator, and a filterable records table.

## Technologies
R, Shiny, bslib, ggplot2, dplyr, Plotly, and DT.

## Project structure
```text
air-quality-dashboard/
├── data/air_quality.csv
├── app.R
├── generate_data.R
├── requirements.R
└── README.md
```

## Installation
From the project directory, run:
```r
source("requirements.R")
source("generate_data.R")
```

## How to run
```r
shiny::runApp()
```

## Future improvements
Connect to validated live monitoring sources, add pollutant-specific regulatory guidance, and support downloadable filtered reports.
