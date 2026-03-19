# predict_sighting.R
# ------------------
# Predicts the best upcoming days to spot a given organism based on
# historical sighting-weather relationships. Uses a negative binomial
# (or Poisson) GLM fitted on daily sighting counts with weather covariates.
#
# Author: Atharv Raskar
# GSoC 2026 — ecotourism hard task

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)


# ---- Main prediction function ------------------------------------------------

predict_sighting <- function(occurrence, weather_data,
                              n_days = 5L, forecast_horizon = 365L,
                              station_id = NULL) {
  #
  # Fits a count GLM on daily sighting data joined with weather records,

  # then projects forward using climatological normals to rank the best
  # upcoming days for spotting the organism.
  #
  # Args:
  #   occurrence    – data frame of sighting records. Needs columns:

  #                   ws_id, date, hour
  #   weather_data  – data frame of daily weather. Needs columns:
  #                   ws_id, date, temp, max, prcp, rh, wind_speed
  #   n_days        – how many top days to return (default 5)
  #   forecast_horizon – how far ahead in days to project (default 365)
  #   station_id    – specific weather station to model; if NULL we pick
  #                   the station with the most sightings
  #
  # Returns:
  #   A tibble of top-ranked future dates with predicted sighting counts,
  #   confidence bounds, and expected weather. Model details are attached
  #   as attributes.
  #

  # -- 1. Validate inputs -----------------------------------------------------

  req_occ <- c("ws_id", "date", "hour")
  req_wx  <- c("ws_id", "date", "temp", "max", "prcp", "rh", "wind_speed")

  missing_occ <- setdiff(req_occ, names(occurrence))
  missing_wx  <- setdiff(req_wx, names(weather_data))

  if (length(missing_occ) > 0)
    stop("occurrence is missing columns: ", paste(missing_occ, collapse = ", "))
  if (length(missing_wx) > 0)
    stop("weather_data is missing columns: ", paste(missing_wx, collapse = ", "))

  if (!is.numeric(n_days) || n_days < 1)
    stop("n_days must be a positive integer")
  if (!is.numeric(forecast_horizon) || forecast_horizon < 1)
    stop("forecast_horizon must be a positive integer")

  if (nrow(occurrence) == 0) stop("occurrence has no rows")
  if (nrow(weather_data) == 0) stop("weather_data has no rows")


  # -- 2. Pick station and aggregate daily counts ----------------------------

  if (is.null(station_id)) {
    station_id <- occurrence |>
      count(ws_id, sort = TRUE) |>
      slice(1) |>
      pull(ws_id)
    message("Auto-selected station: ", station_id,
            " (most sightings)")
  }

  occ_stn <- occurrence |> filter(ws_id == station_id)
  if (nrow(occ_stn) == 0)
    stop("No sightings found at station: ", station_id)

  # figure out the most common sighting hour — we'll use this later
  modal_hour <- occ_stn |>
    filter(!is.na(hour)) |>
    count(hour, sort = TRUE) |>
    slice(1) |>
    pull(hour)

  daily_counts <- occ_stn |>
    count(date, name = "sighting_count") |>
    mutate(month = month(date),
           dayofyear = yday(date))


  # -- 3. Join weather onto the date grid ------------------------------------
  #
  # Important: we LEFT JOIN weather dates onto sighting counts so that days

  # with weather data but zero sightings show up as count = 0. The model
  # needs those zeros to understand what conditions produce no sightings.

  wx_stn <- weather_data |>
    filter(ws_id == station_id) |>
    dplyr::select(date, temp, max, prcp, rh, wind_speed)

  if (nrow(wx_stn) < 30)
    stop("Fewer than 30 weather records at station ", station_id,
         " — not enough data to fit a model")

  model_df <- wx_stn |>
    left_join(daily_counts, by = "date") |>
    mutate(
      sighting_count = replace_na(sighting_count, 0L),
      month    = month(date),
      month_f  = factor(month),
      dayofyear = yday(date)
    ) |>
    filter(!is.na(temp), !is.na(prcp))

  if (nrow(model_df) < 30)
    stop("After joining and dropping NAs, fewer than 30 rows remain")


  # -- 4. Fit the GLM -------------------------------------------------------
  #
  # Start with Poisson, then check dispersion. Wildlife count data is
  # typically overdispersed (lots of zeros, occasional bursts) so we
  # expect to switch to negative binomial most of the time.

  fmla <- sighting_count ~ temp + max + prcp + rh + wind_speed + month_f

  fit_pois <- glm(fmla, data = model_df, family = poisson())
  disp_ratio <- sum(residuals(fit_pois, type = "pearson")^2) / fit_pois$df.residual

  if (disp_ratio > 1.5) {
    fit <- MASS::glm.nb(fmla, data = model_df)
    model_type <- "Negative Binomial"
    message("Overdispersion detected (ratio = ", round(disp_ratio, 2),
            "), using negative binomial")
  } else {
    fit <- fit_pois
    model_type <- "Poisson"
    message("Dispersion ratio = ", round(disp_ratio, 2),
            ", Poisson model is adequate")
  }


  # -- 5. Build future date grid with climatological weather ----------------
  #
  # Since we don't have a weather forecast API, we approximate future
  # weather using historical averages for each calendar day (month+day).
  # This is a standard climatological baseline approach.

  last_date <- max(model_df$date)
  future_dates <- seq.Date(last_date + 1,
                           last_date + forecast_horizon,
                           by = "day")

  # compute historical averages by month-day across all years
  climate_normals <- wx_stn |>
    mutate(m = month(date), d = day(date)) |>
    group_by(m, d) |>
    summarise(
      temp       = mean(temp, na.rm = TRUE),
      max        = mean(max, na.rm = TRUE),
      prcp       = mean(prcp, na.rm = TRUE),
      rh         = mean(rh, na.rm = TRUE),
      wind_speed = mean(wind_speed, na.rm = TRUE),
      .groups = "drop"
    )

  future_grid <- tibble(date = future_dates) |>
    mutate(m = month(date),
           d = day(date),
           dayofyear = yday(date),
           month_f = factor(m)) |>
    left_join(climate_normals, by = c("m", "d")) |>
    filter(!is.na(temp)) |>   # drops Feb 29 if no historical data
    dplyr::select(-m, -d)


  # -- 6. Predict and rank --------------------------------------------------

  preds <- predict(fit, newdata = future_grid, type = "response", se.fit = TRUE)

  results <- future_grid |>
    mutate(
      predicted_count  = preds$fit,
      se               = preds$se.fit,
      confidence_lower = pmax(0, predicted_count - 1.96 * se),
      confidence_upper = predicted_count + 1.96 * se
    ) |>
    arrange(desc(predicted_count)) |>
    slice_head(n = n_days) |>
    mutate(
      rank        = row_number(),
      ws_id       = station_id,
      best_hour   = modal_hour,
      day_of_week = wday(date, label = TRUE)
    ) |>
    dplyr::select(rank, date, day_of_week, best_hour,
           predicted_count, confidence_lower, confidence_upper,
           temp, prcp, rh, wind_speed, ws_id)

  # attach model metadata so the user can inspect later
  attr(results, "model_summary") <- summary(fit)
  attr(results, "model_type")    <- model_type
  attr(results, "dispersion")    <- disp_ratio
  attr(results, "station_id")    <- station_id
  attr(results, "n_obs")         <- nrow(model_df)
  attr(results, "aic")           <- AIC(fit)

  results
}


# ---- Plotting helpers --------------------------------------------------------

plot_predictions <- function(preds) {
  #
  # Bar chart of the top predicted sighting days with confidence intervals.
  #
  ggplot(preds, aes(x = reorder(format(date, "%b %d\n%Y"), date),
                     y = predicted_count)) +
    geom_col(fill = "#2d6a4f", alpha = 0.85, width = 0.6) +
    geom_errorbar(
      aes(ymin = confidence_lower, ymax = confidence_upper),
      width = 0.25, colour = "#555555"
    ) +
    geom_text(aes(label = round(predicted_count, 2)),
              vjust = -0.8, size = 3.5, colour = "#333333") +
    labs(
      title    = paste("Top", nrow(preds), "Predicted Sighting Days"),
      subtitle = paste("Station:", preds$ws_id[1],
                       " | Model:", attr(preds, "model_type")),
      x = NULL, y = "Expected daily sighting count"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank()
    )
}


plot_weather_effect <- function(preds, var = "temp") {
  #
  # Shows how a particular weather variable relates to predicted counts.
  # Useful for understanding what the model learned.
  #
  if (!var %in% names(preds))
    stop("Variable '", var, "' not found in predictions")

  ggplot(preds, aes(x = .data[[var]], y = predicted_count)) +
    geom_point(colour = "#2d6a4f", size = 3.5, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, colour = "#d4a373",
                linewidth = 0.9) +
    labs(
      title = paste("Predicted sightings vs", var),
      x = var, y = "Predicted count"
    ) +
    theme_minimal(base_size = 12)
}


# ---- Diagnostic plot ---------------------------------------------------------

plot_model_diagnostics <- function(preds) {
  #
  # Quick residual check for the fitted model.
  #
  model_obj <- attr(preds, "model_summary")
  if (is.null(model_obj)) {
    message("No model summary attached — can't plot diagnostics")
    return(invisible(NULL))
  }

  resid_df <- tibble(
    fitted  = model_obj$fitted.values,
    residual = model_obj$deviance.resid
  )

  ggplot(resid_df, aes(x = fitted, y = residual)) +
    geom_point(alpha = 0.3, colour = "#40916c") +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_smooth(method = "loess", se = FALSE, colour = "#d4a373") +
    labs(
      title = "Deviance Residuals vs Fitted Values",
      x = "Fitted values", y = "Deviance residuals"
    ) +
    theme_minimal(base_size = 12)
}
