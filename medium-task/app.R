# app.R
# -----
# Australian Wildlife Explorer — Shiny app for the ecotourism package.
# Simplified, robust version with native Shiny tabs
#
# Author: Atharv Raskar
# GSoC 2026 — ecotourism medium task

library(shiny)
library(leaflet)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(MASS)

# ---- Load data from local RDS files -----------------------------------------
gouldian_finch <- readRDS("data/gouldian_finch.rds")
manta_rays     <- readRDS("data/manta_rays.rds")
glowworms      <- readRDS("data/glowworms.rds")
orchids        <- readRDS("data/orchids.rds")
weather        <- readRDS("data/weather.rds")
tourism_region <- if (file.exists("data/tourism_region.rds")) {
  readRDS("data/tourism_region.rds")
} else {
  NULL
}

# ---- Data registry ----------------------------------------------------------
organism_data <- list(
  gouldian_finch = gouldian_finch,
  manta_rays     = manta_rays,
  glowworms      = glowworms,
  orchids        = orchids
)

organism_labels <- c(

  "Gouldian Finch" = "gouldian_finch",
  "Manta Rays"     = "manta_rays",
  "Glowworms"      = "glowworms",
  "Orchids"        = "orchids"
)

organism_info <- list(
  gouldian_finch = "Endangered bird found in the Top End. Best spotted at waterholes during dry season (June-October).",
  manta_rays     = "Reef Manta Rays observed along Queensland coastline. Warm water and calm conditions ideal.",
  glowworms      = "Bioluminescent insects in cool, humid caves and rainforest gullies. Night-time viewing only.",
  orchids        = "Australia's largest plant dataset. Peak flowering season is September-November."
)

# ---- ggplot theme -----------------------------------------------------------
theme_eco <- theme_minimal(base_size = 18) +
  theme(
    plot.title       = element_text(face = "bold", size = 22, colour = "#1b4332"),
    plot.subtitle    = element_text(colour = "#6c757d", size = 16, margin = margin(b = 15)),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#e9ecef", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 14),
    legend.text      = element_text(size = 13),
    axis.text        = element_text(size = 14, colour = "#495057"),
    axis.title       = element_text(size = 16, face = "bold", colour = "#495057"),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# ---- Prediction function (from hard-task) -----------------------------------
predict_sighting <- function(occurrence, weather_data, n_days = 5L,
                              forecast_horizon = 365L, station_id = NULL) {

  # Validation
  req_occ <- c("ws_id", "date", "hour")
  req_wx  <- c("ws_id", "date", "temp", "max", "prcp", "rh", "wind_speed")

  if (any(!req_occ %in% names(occurrence))) {
    stop("Missing columns in occurrence data")
  }
  if (any(!req_wx %in% names(weather_data))) {
    stop("Missing columns in weather data")
  }
  if (nrow(occurrence) < 10) stop("Not enough occurrence records")

  # Find best station
  if (is.null(station_id)) {
    station_id <- occurrence |>
      count(ws_id, sort = TRUE) |>
      slice(1) |>
      pull(ws_id)
  }

  occ_stn <- occurrence |> filter(ws_id == station_id)
  if (nrow(occ_stn) < 5) stop("Not enough sightings at selected station")

  # Modal hour
  modal_hour <- occ_stn |>
    filter(!is.na(hour)) |>
    count(hour, sort = TRUE) |>
    slice(1) |>
    pull(hour)
  if (length(modal_hour) == 0) modal_hour <- 12

  # Daily counts
 daily_counts <- occ_stn |>
    count(date, name = "sighting_count") |>
    mutate(month = month(date), dayofyear = yday(date))

  # Weather at station
  wx_stn <- weather_data |>
    filter(ws_id == station_id) |>
    dplyr::select(date, temp, max, prcp, rh, wind_speed)

  if (nrow(wx_stn) < 30) stop("Insufficient weather data")

  # Model data
  model_df <- wx_stn |>
    left_join(daily_counts, by = "date") |>
    mutate(
      sighting_count = replace_na(sighting_count, 0L),
      month = month(date),
      month_f = factor(month),
      dayofyear = yday(date)
    ) |>
    filter(!is.na(temp), !is.na(prcp))

  if (nrow(model_df) < 30) stop("Not enough complete records for modeling")

  # Fit GLM
  fmla <- sighting_count ~ temp + max + prcp + rh + wind_speed + month_f
  fit_pois <- glm(fmla, data = model_df, family = poisson())
  disp_ratio <- sum(residuals(fit_pois, type = "pearson")^2) / fit_pois$df.residual

  if (disp_ratio > 1.5) {
    fit <- MASS::glm.nb(fmla, data = model_df)
    model_type <- "Negative Binomial"
  } else {
    fit <- fit_pois
    model_type <- "Poisson"
  }

  # Future predictions
  last_date <- max(model_df$date)
  future_dates <- seq.Date(last_date + 1, last_date + forecast_horizon, by = "day")

  climate_normals <- wx_stn |>
    mutate(m = month(date), d = day(date)) |>
    group_by(m, d) |>
    summarise(
      temp = mean(temp, na.rm = TRUE),
      max = mean(max, na.rm = TRUE),
      prcp = mean(prcp, na.rm = TRUE),
      rh = mean(rh, na.rm = TRUE),
      wind_speed = mean(wind_speed, na.rm = TRUE),
      .groups = "drop"
    )

  future_grid <- tibble(date = future_dates) |>
    mutate(m = month(date), d = day(date), month_f = factor(m)) |>
    left_join(climate_normals, by = c("m", "d")) |>
    filter(!is.na(temp)) |>
    dplyr::select(-m, -d)

  if (nrow(future_grid) == 0) stop("Cannot generate future predictions")

  preds <- predict(fit, newdata = future_grid, type = "response", se.fit = TRUE)

  results <- future_grid |>
    mutate(
      predicted_count = preds$fit,
      se = preds$se.fit,
      confidence_lower = pmax(0, predicted_count - 1.96 * se),
      confidence_upper = predicted_count + 1.96 * se
    ) |>
    arrange(desc(predicted_count)) |>
    slice_head(n = n_days) |>
    mutate(
      rank = row_number(),
      ws_id = station_id,
      best_hour = modal_hour,
      day_of_week = wday(date, label = TRUE)
    ) |>
    dplyr::select(rank, date, day_of_week, best_hour, predicted_count,
                  confidence_lower, confidence_upper, temp, prcp, ws_id)

  attr(results, "model_type") <- model_type
  attr(results, "dispersion") <- disp_ratio
  attr(results, "n_obs") <- nrow(model_df)
  attr(results, "aic") <- AIC(fit)

  results
}


# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    )
  ),

  # Header
  div(class = "app-header",
    h1(tags$i(class = "fa-solid fa-leaf"), "Australian Wildlife Explorer"),
    p("Ecotourism Sightings, Weather & Tourism Analysis")
  ),

  # Main layout
  div(class = "app-container",

    # Sidebar
    div(class = "sidebar-panel",
      h3(tags$i(class = "fa-solid fa-filter"), " Filters"),

      selectInput("organism", "Select Organism",
                  choices = organism_labels,
                  selected = "gouldian_finch"),

      div(class = "organism-info", textOutput("organism_desc")),

      hr(),

      selectInput("state_filter", "State/Territory",
                  choices = c("All States" = "all"),
                  selected = "all"),

      selectInput("month_filter", "Month",
                  choices = c("All Months" = "all", setNames(1:12, month.name)),
                  selected = "all"),

      checkboxGroupInput("record_type", "Record Type",
                         choices = NULL, selected = NULL),

      hr(),

      h3(tags$i(class = "fa-solid fa-download"), " Downloads"),
      downloadButton("dl_data", "Download Data (CSV)", class = "dl-btn"),
      downloadButton("dl_monthly", "Download Monthly Chart", class = "dl-btn"),

      hr(),

      div(class = "sidebar-footer",
        p("Data: ecotourism package"),
        p("GSoC 2026 Test Task")
      )
    ),

    # Main content with tabs
    div(class = "main-content",

      # KPI cards
      uiOutput("kpi_cards"),

      # Native Shiny tabset
      tabsetPanel(
        id = "main_tabs",
        type = "pills",

        # Explorer Tab
        tabPanel(
          title = tagList(tags$i(class = "fa-solid fa-map"), " Explorer"),
          value = "explorer",

          div(class = "tab-content-wrapper",
            div(class = "content-row",
              div(class = "panel panel-map",
                h3(tags$i(class = "fa-solid fa-location-dot"), " Sighting Locations"),
                withSpinner(leafletOutput("map", height = "450px"), type = 6, color = "#2d6a4f")
              ),
              div(class = "panel panel-weather",
                h3(tags$i(class = "fa-solid fa-cloud-sun"), " Weather Conditions"),
                withSpinner(plotOutput("weather_plot", height = "450px"), type = 6, color = "#2d6a4f")
              )
            ),

            div(class = "content-row",
              div(class = "panel panel-half",
                h3(tags$i(class = "fa-solid fa-calendar"), " Monthly Distribution"),
                withSpinner(plotOutput("monthly_bar", height = "380px"), type = 6, color = "#2d6a4f")
              ),
              div(class = "panel panel-half",
                h3(tags$i(class = "fa-solid fa-chart-line"), " Annual Trend"),
                withSpinner(plotOutput("annual_trend", height = "380px"), type = 6, color = "#2d6a4f")
              )
            )
          )
        ),

        # Predictions Tab
        tabPanel(
          title = tagList(tags$i(class = "fa-solid fa-wand-magic-sparkles"), " Predictions"),
          value = "predictions",

          div(class = "tab-content-wrapper",
            div(class = "panel",
              h3(tags$i(class = "fa-solid fa-crystal-ball"), " Best Days for Wildlife Viewing"),

              div(class = "prediction-controls",
                numericInput("pred_n_days", "Number of days to predict:",
                            value = 5, min = 1, max = 20, width = "200px"),
                actionButton("run_prediction", "Generate Predictions",
                            class = "action-btn", icon = icon("play"))
              ),

              div(class = "info-box",
                tags$i(class = "fa-solid fa-info-circle"),
                "Uses GLM (Poisson/Negative Binomial) trained on historical sighting-weather relationships."
              ),

              uiOutput("prediction_output")
            )
          )
        ),

        # Tourism Tab
        tabPanel(
          title = tagList(tags$i(class = "fa-solid fa-plane"), " Tourism"),
          value = "tourism",

          div(class = "tab-content-wrapper",
            div(class = "content-row",
              div(class = "panel panel-half",
                h3(tags$i(class = "fa-solid fa-chart-bar"), " Sightings by Region"),
                withSpinner(plotOutput("tourism_chart", height = "450px"), type = 6, color = "#2d6a4f")
              ),
              div(class = "panel panel-half",
                h3(tags$i(class = "fa-solid fa-table"), " Regional Summary"),
                withSpinner(tableOutput("tourism_table"), type = 6, color = "#2d6a4f")
              )
            )
          )
        ),

        # Time Analysis Tab
        tabPanel(
          title = tagList(tags$i(class = "fa-solid fa-clock"), " Time Analysis"),
          value = "time",

          div(class = "tab-content-wrapper",
            div(class = "content-row",
              div(class = "panel panel-half",
                h3(tags$i(class = "fa-solid fa-sun"), " Sightings by Hour"),
                withSpinner(plotOutput("hour_chart", height = "400px"), type = 6, color = "#2d6a4f")
              ),
              div(class = "panel panel-half",
                h3(tags$i(class = "fa-solid fa-calendar-week"), " Sightings by Weekday"),
                withSpinner(plotOutput("weekday_chart", height = "400px"), type = 6, color = "#2d6a4f")
              )
            ),

            div(class = "panel",
              h3(tags$i(class = "fa-solid fa-fire"), " Activity Heatmap (Hour x Month)"),
              withSpinner(plotOutput("heatmap", height = "400px"), type = 6, color = "#2d6a4f")
            )
          )
        )
      )
    )
  )
)


# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # ---- Reactive: base data for selected organism -----------------------------
  base_data <- reactive({
    req(input$organism)
    organism_data[[input$organism]]
  })

  # ---- Update filters when organism changes ----------------------------------
  observeEvent(base_data(), {
    d <- base_data()

    # Update states
    states <- sort(unique(d$obs_state))
    updateSelectInput(session, "state_filter",
                      choices = c("All States" = "all", setNames(states, states)),
                      selected = "all")

    # Update record types
    types <- sort(unique(d$record_type))
    updateCheckboxGroupInput(session, "record_type",
                             choices = types, selected = types)
  })

  # ---- Organism description --------------------------------------------------
  output$organism_desc <- renderText({
    organism_info[[input$organism]]
  })

  # ---- Reactive: filtered data -----------------------------------------------
  filtered_data <- reactive({
    d <- base_data()

    if (!is.null(input$state_filter) && input$state_filter != "all") {
      d <- d |> filter(obs_state == input$state_filter)
    }

    if (!is.null(input$month_filter) && input$month_filter != "all") {
      d <- d |> filter(month == as.integer(input$month_filter))
    }

    if (!is.null(input$record_type) && length(input$record_type) > 0) {
      d <- d |> filter(record_type %in% input$record_type)
    }

    validate(need(nrow(d) > 0, "No sightings match filters. Try widening your selection."))
    d
  })

  # ---- Reactive: with weather ------------------------------------------------
  with_weather <- reactive({
    filtered_data() |>
      left_join(
        weather |> dplyr::select(ws_id, date, temp, prcp, rh, wind_speed),
        by = c("ws_id", "date")
      )
  })

  # ---- KPI Cards -------------------------------------------------------------
  output$kpi_cards <- renderUI({
    d <- filtered_data()
    w <- with_weather()

    n_sightings <- format(nrow(d), big.mark = ",")
    n_species <- n_distinct(d$sci_name)
    yr_range <- paste(range(d$year), collapse = " - ")
    n_stations <- n_distinct(d$ws_id)
    avg_temp <- if (sum(!is.na(w$temp)) > 0) {
      paste0(round(mean(w$temp, na.rm = TRUE), 1), "\u00B0C")
    } else "N/A"

    div(class = "kpi-row",
      div(class = "kpi-card",
        span(class = "kpi-value", n_sightings),
        span(class = "kpi-label", "Total Sightings")
      ),
      div(class = "kpi-card",
        span(class = "kpi-value", n_species),
        span(class = "kpi-label", "Species")
      ),
      div(class = "kpi-card",
        span(class = "kpi-value", yr_range),
        span(class = "kpi-label", "Year Range")
      ),
      div(class = "kpi-card",
        span(class = "kpi-value", n_stations),
        span(class = "kpi-label", "Stations")
      ),
      div(class = "kpi-card",
        span(class = "kpi-value", avg_temp),
        span(class = "kpi-label", "Avg Temp")
      )
    )
  })

  # ---- Map -------------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = 134, lat = -25, zoom = 4)
  })

  observe({
    d <- filtered_data() |> filter(!is.na(obs_lat), !is.na(obs_lon))

    leafletProxy("map") |>
      clearMarkers() |>
      clearMarkerClusters()

    if (nrow(d) > 0) {
      leafletProxy("map") |>
        addCircleMarkers(
          data = d,
          lng = ~obs_lon, lat = ~obs_lat,
          radius = 6,
          color = "#2d6a4f",
          fillColor = "#52b788",
          fillOpacity = 0.7,
          stroke = TRUE, weight = 2,
          popup = ~paste0(
            "<strong>", sci_name, "</strong><br>",
            "Date: ", date, "<br>",
            "State: ", obs_state, "<br>",
            "Hour: ", ifelse(is.na(hour), "Unknown", paste0(hour, ":00"))
          ),
          clusterOptions = markerClusterOptions()
        )
    }
  })

  # ---- Weather Plot ----------------------------------------------------------
  output$weather_plot <- renderPlot({
    w <- with_weather() |> filter(!is.na(temp))
    validate(need(nrow(w) > 5, "Not enough weather data for visualization."))

    w |>
      mutate(month_name = factor(month.abb[month], levels = month.abb)) |>
      ggplot(aes(x = month_name, y = temp, fill = month_name)) +
      geom_boxplot(alpha = 0.8, show.legend = FALSE) +
      scale_fill_viridis_d(option = "G") +
      labs(
        title = "Temperature at Sighting Times",
        x = NULL, y = "Temperature (\u00B0C)"
      ) +
      theme_eco
  })

  # ---- Monthly Bar Chart -----------------------------------------------------
  output$monthly_bar <- renderPlot({
    d <- filtered_data()

    counts <- d |>
      count(month) |>
      mutate(month_name = factor(month.abb[month], levels = month.abb))

    peak <- counts |> slice_max(n, n = 1)

    ggplot(counts, aes(x = month_name, y = n, fill = n)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      geom_text(aes(label = format(n, big.mark = ",")),
                vjust = -0.5, size = 5, fontface = "bold") +
      scale_fill_gradient(low = "#74c69d", high = "#1b4332") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = "Monthly Sighting Patterns",
        subtitle = paste0("Peak: ", peak$month_name, " (", format(peak$n, big.mark = ","), " sightings)"),
        x = NULL, y = "Number of Sightings"
      ) +
      theme_eco
  })

  # ---- Annual Trend ----------------------------------------------------------
  output$annual_trend <- renderPlot({
    d <- filtered_data()
    annual <- d |> count(year)

    ggplot(annual, aes(x = year, y = n)) +
      geom_area(fill = "#52b788", alpha = 0.3) +
      geom_line(colour = "#2d6a4f", linewidth = 2) +
      geom_point(colour = "#1b4332", size = 4) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
        title = "Sighting Trends Over Time",
        x = "Year", y = "Annual Sightings"
      ) +
      theme_eco
  })

  # ---- Tourism Chart ---------------------------------------------------------
  output$tourism_chart <- renderPlot({
    d <- filtered_data()

    # Use state-level analysis (more reliable)
    state_counts <- d |>
      count(obs_state, sort = TRUE) |>
      mutate(pct = round(n / sum(n) * 100, 1))

    validate(need(nrow(state_counts) > 0, "No state data available."))

    ggplot(state_counts, aes(x = reorder(obs_state, n), y = n, fill = n)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      geom_text(aes(label = paste0(format(n, big.mark = ","), " (", pct, "%)")),
                hjust = -0.1, size = 5, fontface = "bold") +
      scale_fill_gradient(low = "#74c69d", high = "#1b4332") +
      coord_flip(clip = "off") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
      labs(
        title = "Sightings by State/Territory",
        x = NULL, y = "Number of Sightings"
      ) +
      theme_eco +
      theme(plot.margin = margin(10, 80, 10, 10))
  })

  # ---- Tourism Table ---------------------------------------------------------
  output$tourism_table <- renderTable({
    d <- filtered_data()

    d |>
      group_by(State = obs_state) |>
      summarise(
        Sightings = n(),
        Species = n_distinct(sci_name),
        `First Year` = min(year),
        `Last Year` = max(year),
        .groups = "drop"
      ) |>
      arrange(desc(Sightings)) |>
      mutate(
        `% of Total` = paste0(round(Sightings / sum(Sightings) * 100, 1), "%"),
        Sightings = format(Sightings, big.mark = ",")
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

  # ---- Hour Chart ------------------------------------------------------------
  output$hour_chart <- renderPlot({
    d <- filtered_data() |> filter(!is.na(hour))
    validate(need(nrow(d) > 0, "No hour data available for this selection."))

    hour_counts <- d |>
      count(hour) |>
      complete(hour = 0:23, fill = list(n = 0))

    peak <- hour_counts |> slice_max(n, n = 1)

    ggplot(hour_counts, aes(x = hour, y = n, fill = n)) +
      geom_col(width = 0.8, show.legend = FALSE) +
      scale_fill_gradient(low = "#74c69d", high = "#1b4332") +
      scale_x_continuous(breaks = seq(0, 23, 3),
                         labels = sprintf("%02d:00", seq(0, 23, 3))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
        title = "Daily Activity Pattern",
        subtitle = paste0("Peak hour: ", sprintf("%02d:00", peak$hour)),
        x = "Hour of Day", y = "Number of Sightings"
      ) +
      theme_eco +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # ---- Weekday Chart ---------------------------------------------------------
  output$weekday_chart <- renderPlot({
    d <- filtered_data()

    weekday_counts <- d |>
      mutate(weekday = wday(date, label = TRUE, abbr = FALSE)) |>
      count(weekday) |>
      mutate(is_weekend = weekday %in% c("Saturday", "Sunday"))

    ggplot(weekday_counts, aes(x = weekday, y = n, fill = is_weekend)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      geom_text(aes(label = format(n, big.mark = ",")),
                vjust = -0.5, size = 5, fontface = "bold") +
      scale_fill_manual(values = c("TRUE" = "#e76f51", "FALSE" = "#2d6a4f")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = "Sightings by Day of Week",
        subtitle = "Orange = weekends, Green = weekdays",
        x = NULL, y = "Number of Sightings"
      ) +
      theme_eco
  })

  # ---- Heatmap ---------------------------------------------------------------
  output$heatmap <- renderPlot({
    d <- filtered_data() |> filter(!is.na(hour))
    validate(need(nrow(d) > 10, "Not enough hour data for heatmap."))

    heatmap_data <- d |>
      count(month, hour) |>
      complete(month = 1:12, hour = 0:23, fill = list(n = 0)) |>
      mutate(month_name = factor(month.abb[month], levels = month.abb))

    ggplot(heatmap_data, aes(x = hour, y = month_name, fill = n)) +
      geom_tile(colour = "white", linewidth = 0.3) +
      scale_fill_viridis_c(option = "G", direction = -1, name = "Sightings") +
      scale_x_continuous(breaks = seq(0, 23, 3),
                         labels = sprintf("%02d:00", seq(0, 23, 3)),
                         expand = c(0, 0)) +
      labs(
        title = "Best Times for Wildlife Viewing",
        subtitle = "Activity by hour and month",
        x = "Hour of Day", y = NULL
      ) +
      theme_eco +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
      )
  })

  # ---- Predictions -----------------------------------------------------------
  prediction_data <- eventReactive(input$run_prediction, {
    d <- filtered_data()
    n <- input$pred_n_days

    tryCatch({
      predict_sighting(d, weather, n_days = n)
    }, error = function(e) {
      NULL
    })
  })

  output$prediction_output <- renderUI({
    # Show message if button not clicked
    if (is.null(input$run_prediction) || input$run_prediction == 0) {
      return(div(class = "info-box info-primary",
        tags$i(class = "fa-solid fa-hand-pointer"),
        " Click 'Generate Predictions' to see the best upcoming days."
      ))
    }

    preds <- prediction_data()

    if (is.null(preds)) {
      return(div(class = "info-box info-warning",
        tags$i(class = "fa-solid fa-exclamation-triangle"),
        " Not enough data for predictions. Try selecting 'All Months' or 'All States'."
      ))
    }

    model_type <- attr(preds, "model_type")
    n_obs <- attr(preds, "n_obs")
    aic <- round(attr(preds, "aic"), 1)

    tagList(
      div(class = "model-stats",
        span(class = "stat", paste("Model:", model_type)),
        span(class = "stat", paste("Observations:", format(n_obs, big.mark = ","))),
        span(class = "stat", paste("AIC:", format(aic, big.mark = ",")))
      ),

      div(class = "content-row",
        div(class = "panel panel-half",
          h4("Top Predicted Days"),
          tableOutput("pred_table")
        ),
        div(class = "panel panel-half",
          h4("Prediction Chart"),
          plotOutput("pred_plot", height = "350px")
        )
      )
    )
  })

  output$pred_table <- renderTable({
    preds <- prediction_data()
    req(preds)

    preds |>
      mutate(
        Date = format(date, "%b %d, %Y"),
        Day = as.character(day_of_week),
        `Best Hour` = paste0(best_hour, ":00"),
        `Predicted` = round(predicted_count, 2),
        `95% CI` = paste0("[", round(confidence_lower, 2), "-", round(confidence_upper, 2), "]"),
        `Temp` = paste0(round(temp, 1), "\u00B0C")
      ) |>
      dplyr::select(`#` = rank, Date, Day, `Best Hour`, Predicted, `95% CI`, Temp)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

  output$pred_plot <- renderPlot({
    preds <- prediction_data()
    req(preds)

    preds |>
      mutate(date_label = format(date, "%b %d")) |>
      ggplot(aes(x = reorder(date_label, date), y = predicted_count)) +
      geom_segment(aes(xend = date_label, y = confidence_lower, yend = confidence_upper),
                   linewidth = 6, colour = "#95d5b2", lineend = "round") +
      geom_point(size = 6, colour = "#1b4332") +
      geom_text(aes(label = round(predicted_count, 1)),
                vjust = -1.2, size = 5, fontface = "bold") +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
      labs(
        title = paste("Top", nrow(preds), "Predicted Sighting Days"),
        x = NULL, y = "Expected Daily Count"
      ) +
      theme_eco +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))
  })

  # ---- Downloads -------------------------------------------------------------
  output$dl_data <- downloadHandler(
    filename = function() paste0("ecotourism_", input$organism, "_", Sys.Date(), ".csv"),
    content = function(file) write.csv(with_weather(), file, row.names = FALSE)
  )

  output$dl_monthly <- downloadHandler(
    filename = function() paste0("monthly_", input$organism, "_", Sys.Date(), ".png"),
    content = function(file) {
      d <- filtered_data()
      counts <- d |>
        count(month) |>
        mutate(month_name = factor(month.abb[month], levels = month.abb))

      p <- ggplot(counts, aes(x = month_name, y = n, fill = n)) +
        geom_col(width = 0.7, show.legend = FALSE) +
        scale_fill_gradient(low = "#74c69d", high = "#1b4332") +
        labs(title = "Monthly Sighting Patterns", x = NULL, y = "Sightings") +
        theme_eco

      ggsave(file, plot = p, width = 10, height = 6, dpi = 150, bg = "white")
    }
  )
}


# ---- Run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
