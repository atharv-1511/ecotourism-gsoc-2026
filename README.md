# Ecotourism GSoC 2026 — Contributor Test Tasks

Three test tasks for the **ecotourism: update data package and create shiny app**
project under the [R Project for Statistical Computing](https://github.com/rstats-gsoc/gsoc2026/wiki).

**Contributor:** Atharv Raskar
([GitHub](https://github.com/atharv-1511) |
[LinkedIn](https://www.linkedin.com/in/atharvraskar/))

## Tasks

### Easy Task — Tutorial

A Quarto tutorial (`easy-task/ecotourism_tutorial.qmd`) built with the
[unilur](https://github.com/ginolhac/unilur) extension. Renders two
versions from a single source:

- **Student version** — questions only, solutions hidden
- **Solution version** — full code and output visible

**Focus:** Gouldian Finch sightings in the Northern Territory during
September (end of dry season, peak birdwatching window).

**Three exercises using different join types:**

| Exercise | Join | What it teaches |
|----------|------|-----------------|
| 1 | `inner_join` | Match sightings with weather — understand data loss |
| 2 | `left_join` | Add tourism data + region names via `tourism_region` |
| 3 | `anti_join` | Find stations with sightings but no weather records |

Includes interactive exploration (DT datatable, Leaflet map, plotly chart)
and a faceted ggplot2 visualisation.

### Medium Task — Shiny App

An interactive wildlife explorer (`medium-task/app.R`) built with plain
Shiny and custom CSS. **Enhanced with 4-tab interface and GLM predictions.**

**Features:**

- **Explorer Tab:** Leaflet map with clustering, weather analysis, monthly/annual charts
- **Predictions Tab:** GLM-based sighting forecasts with confidence intervals
- **Tourism Tab:** State/region rankings and tourism data integration
- **Time Analysis Tab:** Hour distribution, weekday patterns, hour×month heatmap
- Organism selector (4 species) with state, month, and record type filters
- KPI summary cards with icons (sightings, species, year range, stations, avg temp)
- Download handlers for filtered data (CSV) and charts (PNG)
- Responsive design with mobile support

**Architecture:**

```
base_data()  -->  filtered_data()  -->  with_weather()
(organism)       (state+month+type)     (left_join weather)
                        |
                        v
              predict_sighting() --> GLM predictions
```

Data lookup uses a named list (not `get()`). Charts are built by reusable
`build_*()` functions shared between `renderPlot` and `downloadHandler`.
The prediction function from the hard task is embedded in the app.

### Hard Task — Prediction Function

A GLM-based prediction function (`hard-task/predict_sighting.R`) that
identifies the best upcoming days to spot an organism.

**Approach:**

1. Aggregate daily sighting counts at the busiest weather station
2. Join with weather data (explicit zeros for days with no sightings)
3. Fit a Poisson GLM; if overdispersed, refit as negative binomial
4. Project forward using climatological normals (historical weather averages)
5. Rank future dates by predicted sighting count with 95% confidence intervals

**Function signature:**

```r
predict_sighting(occurrence, weather_data, n_days = 5,
                 forecast_horizon = 365, station_id = NULL)
```

Both occurrence and weather are explicit parameters. Returns a tibble with
ranked dates, predicted counts, confidence bounds, and expected weather.

A demo document (`hard-task/hard_task_demo.qmd`) runs the function across
all four organisms with ecological interpretation.

## Setup

```r
# install the ecotourism package
# install.packages("pak")
pak::pak("vahdatjavad/ecotourism")
```

**Rendering the tutorials** requires [Quarto](https://quarto.org/)
(>= 1.4). The unilur extension is bundled in `_extensions/`.

**Running the Shiny app:**

```r
shiny::runApp("medium-task")
```

For deployment and testing instructions, see:
- `medium-task/TESTING_CHECKLIST.md` - Comprehensive test matrix
- `medium-task/CHANGES_SUMMARY.md` - Complete rewrite documentation
- `medium-task/deploy.R` - Interactive deployment script

## Repository Structure

```
.
├── easy-task/
│   ├── ecotourism_tutorial.qmd      # Quarto tutorial with join exercises
│   └── test_results.html            # Detailed Q&A documentation
├── medium-task/
│   ├── app.R                        # Shiny app (850 lines, 4 tabs)
│   ├── www/styles.css               # Professional CSS design
│   ├── data/                        # Local RDS files for deployment
│   ├── deploy.R                     # Deployment script
│   ├── TESTING_CHECKLIST.md         # Comprehensive test matrix
│   └── CHANGES_SUMMARY.md           # Complete rewrite documentation
├── hard-task/
│   ├── predict_sighting.R           # GLM prediction function
│   ├── test_predict_sighting.R      # 27 unit tests
│   ├── hard_task_demo.qmd           # Demo document
│   └── test_results.html            # Detailed Q&A documentation
├── _extensions/unilur/              # Quarto extension for tutorials
├── LICENSE
└── README.md
```

## Documentation

For comprehensive testing and deployment instructions, see:
- `medium-task/TESTING_CHECKLIST.md` - Complete test matrix (16 organism-tab combinations)
- `medium-task/CHANGES_SUMMARY.md` - App rewrite documentation
- `medium-task/deploy.R` - Interactive deployment script

Each task folder also contains `test_results.html` with Q&A format documentation.

## Hard Task Unit Tests

```r
# Run 27 unit tests for the prediction function
setwd("hard-task")
source("predict_sighting.R")
testthat::test_file("test_predict_sighting.R")
```

## Dependencies

| Task | Packages |
|------|----------|
| Easy | tidyverse, lubridate, ecotourism, conflicted, kableExtra, DT, plotly, leaflet |
| Medium | shiny, leaflet, shinycssloaders, ecotourism, dplyr, tidyr, ggplot2, lubridate |
| Hard | MASS, dplyr, tidyr, ggplot2, lubridate, ecotourism |
