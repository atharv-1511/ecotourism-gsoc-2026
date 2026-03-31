# GSoC 2026 Ecotourism Project Roadmap

**Project:** ecotourism: update data package and create shiny app
**Contributor:** Atharv Raskar
**Organization:** R Project for Statistical Computing

---

## Project Goals (from GSoC Wiki)

1. **Update the ecotourism data package** — fill weather data gaps
2. **Create a Shiny app** — interactive exploration of wildlife sightings

---

## What I Have Already Built (Test Tasks)

### Easy Task: Tutorial
- Quarto tutorial using **all 3 join types** (inner, left, anti)
- Interactive elements: DT datatable, Leaflet map, Plotly charts
- Proper unilur format with student/solution versions
- Identifies data gaps using `anti_join()` — directly relevant to project goal

### Medium Task: Shiny App
- 4-tab interface: Explorer, Predictions, Tourism, Time Analysis
- **Weather Coverage KPI** — surfaces the core data problem
- Hour × Month heatmap for optimal viewing times
- GLM predictions integrated from hard task
- Download handlers for data and charts
- Professional CSS with responsive design

### Hard Task: Prediction Function
- `predict_sighting()` with future date projection
- Overdispersion detection: auto-switches Poisson → Negative Binomial
- 95% confidence intervals on predictions
- **27 unit tests** with edge case coverage
- Helper functions: `plot_predictions()`, `plot_weather_effect()`

---

## What Remains for Full Project

### Phase 1: Data Gap Analysis (Week 1-2)

| Task | Description | Output |
|------|-------------|--------|
| Quantify gaps | Which stations/dates have missing weather? | Gap analysis report |
| Source options | BOM API, SILO data, interpolation methods | Technical memo |
| Priority ranking | Which gaps affect most sightings? | Prioritized station list |

**My anti_join exercise already identifies these gaps. Next step: quantify and prioritize.**

### Phase 2: Weather Data Completion (Week 3-6)

| Task | Description | Approach |
|------|-------------|----------|
| BOM data ingestion | Pull historical data for gap stations | `bomrang` or direct API |
| Interpolation | For stations with no direct data | IDW or kriging from nearby |
| Quality checks | Validate filled data against existing | Statistical tests |
| Documentation | Update data dictionary | roxygen2 + vignette |

### Phase 3: Package Functions (Week 7-9)

```r
# Proposed new functions for ecotourism package:

get_weather_coverage()
# Returns % coverage by organism, station, year

fill_weather_gaps()
# Interpolates missing weather from nearby stations

predict_sighting()
# Already built — needs integration into package

validate_occurrence()
# Quality checks for citizen science data
```

### Phase 4: Shiny App Finalization (Week 10-11)

| Enhancement | Status | Priority |
|-------------|--------|----------|
| Core functionality | Done | — |
| Weather coverage display | Done | — |
| Species-level drilling | Planned | Medium |
| Data quality dashboard | Planned | High |
| Export formats (Excel, GeoJSON) | Planned | Medium |
| Deployment to shinyapps.io | Ready | High |

### Phase 5: Documentation & Submission (Week 12)

- [ ] Package vignettes (tutorial, prediction workflow)
- [ ] Updated README with new functions
- [ ] CRAN submission preparation (R CMD check)
- [ ] Final mentor review

---

## Technical Approach

### Filling Weather Gaps

The current weather dataset has incomplete coverage. My tutorial exercise with `anti_join()` reveals stations where sightings exist but no weather was recorded.

**Proposed solution:**

1. **Direct fill**: Use BOM Climate Data Online for stations with data
2. **Interpolation**: For remaining gaps, use Inverse Distance Weighting (IDW) from the 3 nearest stations with data
3. **Validation**: Compare interpolated values against actual for a holdout set

```r
# Pseudocode for interpolation approach
fill_weather_gaps <- function(occurrence, weather, weather_stations) {
  # Find dates/stations with sightings but no weather
  gaps <- occurrence |>
    anti_join(weather, by = c("ws_id", "date"))
  
  # For each gap, find 3 nearest stations with data
  # Apply IDW interpolation
  # Return filled weather tibble
}
```

### Package Integration

The `predict_sighting()` function I built can be added to the ecotourism package:

1. Move to `R/predict_sighting.R`
2. Add roxygen2 documentation
3. Export in NAMESPACE
4. Add tests to `tests/testthat/`
5. Create vignette: "Predicting Wildlife Sightings"

---

## Timeline Summary

| Week | Focus | Deliverable |
|------|-------|-------------|
| 1-2 | Gap analysis | Report + prioritized list |
| 3-6 | Data filling | Completed weather dataset |
| 7-9 | Package functions | 4 new exported functions |
| 10-11 | App finalization | Production deployment |
| 12 | Documentation | CRAN-ready package |

---

## Why I Can Deliver This

1. **Already built the hard parts** — prediction function, Shiny app, tests
2. **Understand the data** — identified gaps, coverage issues, artifacts
3. **Previous GSoC experience** — completed grepreaper package (2025)
4. **Production mindset** — unit tests, CI, documentation from day one

---

## Questions for Mentors

1. Is BOM API access available, or should I use SILO/other sources?
2. Should interpolation use kriging (accounts for elevation) or simpler IDW?
3. Target deployment: shinyapps.io free tier or Posit Connect?
4. Should `predict_sighting()` also support ensemble methods?

---

*This roadmap demonstrates my understanding of the full project scope and my readiness to execute it.*
