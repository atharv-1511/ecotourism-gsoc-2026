# predict_sighting.R
# ------------------
# Simplified prediction function for wildlife sighting forecasting.
#
# Purpose: Fit a count regression model on daily sighting totals,
# then predict the top days for wildlife sightings in the next year
# using historical weather patterns.
#
# Author: Atharv Raskar
# GSoC 2026 — ecotourism hard task

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(MASS)

# ==============================================================================
# MAIN FUNCTION: predict_sighting
# ==============================================================================

predict_sighting <- function(occurrence, weather_data,
                              n_days = 5L,
                              forecast_horizon = 365L,
                              station_id = NULL,
                              model_type = c("auto", "poisson", "negbin")) {

  # Validate inputs first
  model_type <- match.arg(model_type)

  # Check required columns
  req_occ <- c("ws_id", "date", "hour")
  req_wx  <- c("ws_id", "date", "temp", "max", "prcp", "rh", "wind_speed")

  missing_occ <- setdiff(req_occ, names(occurrence))
  missing_wx  <- setdiff(req_wx, names(weather_data))

  if (length(missing_occ) > 0)
    stop("occurrence is missing columns: ", paste(missing_occ, collapse = ", "))
  if (length(missing_wx) > 0)
    stop("weather_data is missing columns: ", paste(missing_wx, collapse = ", "))

  # Validate parameter values
  if (!is.numeric(n_days) || n_days < 1)
    stop("n_days must be a positive integer")
  if (!is.numeric(forecast_horizon) || forecast_horizon < 1)
    stop("forecast_horizon must be a positive integer")

  # Check data is non-empty
  if (nrow(occurrence) == 0)
    stop("occurrence has no rows")
  if (nrow(weather_data) == 0)
    stop("weather_data has no rows")

  # STEP 1: Select weather station
  if (is.null(station_id)) {
    # Auto-select station with most sightings
    station_id <- occurrence %>%
      count(ws_id, sort = TRUE) %>%
      slice(1) %>%
      pull(ws_id)
    message("Auto-selected station: ", station_id, " (most sightings)")
  }

  # Filter occurrence data to selected station
  occ_stn <- occurrence %>% filter(ws_id == station_id)
  if (nrow(occ_stn) == 0)
    stop("No sightings found at station: ", station_id)

  # Get the most common sighting hour for reporting
  modal_hour <- occ_stn %>%
    filter(!is.na(hour)) %>%
    count(hour, sort = TRUE) %>%
    slice(1) %>%
    pull(hour)
  if (is.na(modal_hour)) modal_hour <- 12  # Default if no hours available

  # STEP 2: Aggregate daily sighting counts
  daily_counts <- occ_stn %>%
    count(date, name = "sighting_count") %>%
    mutate(
      month    = month(date),
      dayofyear = yday(date)
    )

  # STEP 3: Join with weather data
  # Important: LEFT JOIN weather to counts preserves zeros for no-sighting days
  wx_stn <- weather_data %>%
    filter(ws_id == station_id) %>%
    dplyr::select(date, temp, max, prcp, rh, wind_speed)

  if (nrow(wx_stn) < 30)
    stop("Fewer than 30 weather records at station ", station_id,
         " — not enough data to fit a model")

  # Build model dataframe
  model_df <- wx_stn %>%
    left_join(daily_counts, by = "date") %>%
    mutate(
      sighting_count = replace_na(sighting_count, 0L),
      month    = month(date),
      month_f  = factor(month),
      dayofyear = yday(date)
    ) %>%
    filter(!is.na(temp), !is.na(prcp))

  if (nrow(model_df) < 30)
    stop("After joining and dropping NAs, fewer than 30 rows remain")

  # STEP 4: Model selection
  # Simple strategy: Poisson by default, Negative Binomial if overdispersed

  fmla <- sighting_count ~ temp + max + prcp + rh + wind_speed + month_f

  # Always fit Poisson first for diagnostics
  fit_pois <- glm(fmla, data = model_df, family = poisson())

  # Calculate dispersion ratio (Pearson chi-square / df)
  pearson_resid <- residuals(fit_pois, type = "pearson")
  dispersion_ratio <- sum(pearson_resid^2) / fit_pois$df.residual

  zero_prop <- mean(model_df$sighting_count == 0)

  # Select model based on dispersion
  if (model_type == "auto") {
    if (dispersion_ratio > 1.5) {
      model_type <- "negbin"
    } else {
      model_type <- "poisson"
    }
  }

  # Fit selected model
  if (model_type == "negbin") {
    fit <- MASS::glm.nb(fmla, data = model_df)
    model_label <- "Negative Binomial"
  } else {
    fit <- fit_pois
    model_label <- "Poisson"
  }

  # STEP 5: Build future date grid with climatological weather
  # Use historical monthly averages as proxy for future weather

  last_date <- max(model_df$date)
  future_dates <- seq.Date(last_date + 1,
                           last_date + forecast_horizon,
                           by = "day")

  # Calculate climatological normals (monthly averages)
  climate_normals <- wx_stn %>%
    mutate(
      month_only = month(date)
    ) %>%
    group_by(month_only) %>%
    summarise(
      temp       = mean(temp, na.rm = TRUE),
      max        = mean(max, na.rm = TRUE),
      prcp       = mean(prcp, na.rm = TRUE),
      rh         = mean(rh, na.rm = TRUE),
      wind_speed = mean(wind_speed, na.rm = TRUE),
      .groups    = "drop"
    )

  # Build future grid
  future_grid <- tibble(date = future_dates) %>%
    mutate(
      month    = month(date),
      month_f  = factor(month),
      dayofyear = yday(date)
    ) %>%
    left_join(climate_normals, by = c("month" = "month_only")) %>%
    filter(!is.na(temp))  # Drop if month has no data

  if (nrow(future_grid) == 0)
    stop("After climatological join, no future dates remain")

  # STEP 6: Make predictions
  preds <- predict(fit, newdata = future_grid, type = "response",
                   se.fit = TRUE)

  # Build results table
  results <- future_grid %>%
    mutate(
      predicted_count  = preds$fit,
      se               = preds$se.fit,
      confidence_lower = pmax(0, predicted_count - 1.96 * se),
      confidence_upper = predicted_count + 1.96 * se
    ) %>%
    arrange(desc(predicted_count)) %>%
    slice_head(n = n_days) %>%
    mutate(
      rank       = row_number(),
      day_of_week = wday(date, label = TRUE),
      best_hour   = modal_hour,
      ws_id       = station_id
    ) %>%
    dplyr::select(rank, date, day_of_week, best_hour,
           predicted_count, confidence_lower, confidence_upper,
           temp, prcp, rh, wind_speed, ws_id)

  # Attach model metadata
  attr(results, "model_summary")   <- summary(fit)
  attr(results, "model_type")      <- model_label
  attr(results, "dispersion")      <- dispersion_ratio
  attr(results, "zero_proportion") <- zero_prop
  attr(results, "station_id")      <- station_id
  attr(results, "n_obs")           <- nrow(model_df)
  attr(results, "aic")             <- AIC(fit)

  results
}


# ==============================================================================
# PLOTTING FUNCTIONS
# ==============================================================================

plot_predictions <- function(preds) {
  # Bar chart of predicted sighting days with confidence intervals

  if (nrow(preds) == 0) {
    message("No predictions to plot")
    return(invisible(NULL))
  }

  ggplot(preds, aes(x = reorder(format(date, "%b %d"), date),
                    y = predicted_count)) +
    geom_col(fill = "#2d6a4f", alpha = 0.85, width = 0.6) +
    geom_errorbar(
      aes(ymin = confidence_lower, ymax = confidence_upper),
      width = 0.25, colour = "#555555", linewidth = 1
    ) +
    geom_text(aes(label = round(predicted_count, 1)),
              vjust = -0.5, size = 3, colour = "#333333") +
    labs(
      title = paste("Top", nrow(preds), "Predicted Sighting Days"),
      subtitle = paste("Station:", preds$ws_id[1],
                       "| Model:", attr(preds, "model_type")),
      x = "Date",
      y = "Expected daily sighting count"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}


plot_weather_effect <- function(preds, var = "temp") {
  # Show relationship between weather variable and predicted counts

  if (!var %in% names(preds))
    stop("Variable '", var, "' not found in predictions")

  ggplot(preds, aes(x = .data[[var]], y = predicted_count)) +
    geom_point(colour = "#2d6a4f", size = 3, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, colour = "#d4a373",
                linewidth = 0.8, alpha = 0.2) +
    labs(
      title = paste("Predicted sightings vs", var),
      x = var,
      y = "Predicted count"
    ) +
    theme_minimal(base_size = 11)
}


plot_model_diagnostics <- function(preds) {
  # Residual plot for model diagnostics

  model_obj <- attr(preds, "model_summary")
  if (is.null(model_obj)) {
    message("No model summary attached to predictions")
    return(invisible(NULL))
  }

  resid_df <- tibble(
    fitted = model_obj$fitted.values,
    residual = model_obj$deviance.resid
  )

  ggplot(resid_df, aes(x = fitted, y = residual)) +
    geom_point(alpha = 0.4, colour = "#40916c", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_smooth(method = "loess", se = FALSE, colour = "#d4a373",
                linewidth = 0.8) +
    labs(
      title = "Deviance Residuals vs Fitted Values",
      x = "Fitted values",
      y = "Deviance residuals"
    ) +
    theme_minimal(base_size = 11)
}
