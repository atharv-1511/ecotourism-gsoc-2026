# data_gap_analysis.R
# -------------------
# Quantifies weather data coverage gaps in the ecotourism package.
# This is the core problem the GSoC 2026 project aims to solve.
#
# Author: Atharv Raskar

library(dplyr)
library(ecotourism)

# Load all datasets
data(gouldian_finch)
data(manta_rays)
data(glowworms)
data(orchids)
data(weather)

# Function to analyze coverage for one organism
analyze_coverage <- function(occurrence, name) {
  
  # Total sightings
  total <- nrow(occurrence)
  
  # Sightings with weather data (inner join)
  with_weather <- occurrence |>
    inner_join(weather, by = c("ws_id", "date")) |>
    nrow()
  
  # Unique stations in occurrence
  occ_stations <- n_distinct(occurrence$ws_id)
  
  # Stations with NO weather data at all (anti_join)
  unmonitored <- occurrence |>
    distinct(ws_id) |>
    anti_join(weather |> distinct(ws_id), by = "ws_id") |>
    nrow()
  
  tibble(
    organism = name,
    total_sightings = total,
    with_weather = with_weather,
    coverage_pct = round(100 * with_weather / total, 1),
    stations_used = occ_stations,
    stations_no_weather = unmonitored,
    gap_pct = round(100 * unmonitored / occ_stations, 1)
  )
}

# Analyze all organisms
coverage_report <- bind_rows(
  analyze_coverage(gouldian_finch, "Gouldian Finch"),
  analyze_coverage(manta_rays, "Manta Rays"),
  analyze_coverage(glowworms, "Glowworms"),
  analyze_coverage(orchids, "Orchids")
)

# Print report
cat("\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("ECOTOURISM WEATHER DATA GAP ANALYSIS\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n\n")

print(coverage_report, n = Inf)

cat("\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")
cat("SUMMARY:\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")

total_sightings <- sum(coverage_report$total_sightings)
total_with_weather <- sum(coverage_report$with_weather)
overall_coverage <- round(100 * total_with_weather / total_sightings, 1)

cat("Total sightings across all organisms:", format(total_sightings, big.mark = ","), "\n")
cat("Sightings with weather data:", format(total_with_weather, big.mark = ","), "\n")
cat("Overall coverage:", overall_coverage, "%\n")
cat("Data gap to fill:", 100 - overall_coverage, "%\n")

cat("\n")
cat("GSoC 2026 PROJECT GOAL: Fill these gaps to reach ~95% coverage\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
