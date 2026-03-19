# test_predict_sighting.R
# ----------------------
# Edge-case tests for predict_sighting().
# Uses testthat + synthetic data so no external packages beyond MASS are needed.
#
# Run:
#   source("predict_sighting.R")
#   testthat::test_file("test_predict_sighting.R")

library(testthat)

source("predict_sighting.R")   # loads dplyr, lubridate, tidyr, ggplot2

# ---- Helpers: build synthetic occurrence & weather data ----------------------

make_weather <- function(station = "WS001",
                         start = as.Date("2020-01-01"),
                         n_days = 365) {
  dates <- seq.Date(start, by = "day", length.out = n_days)
  tibble(
    ws_id      = station,
    date       = dates,
    temp       = runif(n_days, 10, 35),
    min        = runif(n_days, 5, 15),
    max        = runif(n_days, 30, 45),
    prcp       = runif(n_days, 0, 20),
    rh         = runif(n_days, 30, 90),
    wind_speed = runif(n_days, 0, 30)
  )
}

make_occurrence <- function(station = "WS001",
                            start = as.Date("2020-01-01"),
                            n_records = 200) {
  dates <- sample(seq.Date(start, by = "day", length.out = 365),
                  n_records, replace = TRUE)
  tibble(
    ws_id     = station,
    date      = dates,
    month     = lubridate::month(dates),
    hour      = sample(6:18, n_records, replace = TRUE),
    dayofyear = lubridate::yday(dates),
    sci_name  = "Testus edgecasus"
  )
}

set.seed(42)
wx  <- make_weather()
occ <- make_occurrence()


# ==== 1. Input validation ====================================================

test_that("errors when occurrence is missing required columns", {
  bad_occ <- occ |> dplyr::select(-hour)
  expect_error(
    predict_sighting(bad_occ, wx),
    "occurrence is missing columns:.*hour"
  )
})

test_that("errors when weather_data is missing required columns", {
  bad_wx <- wx |> dplyr::select(-rh, -wind_speed)
  expect_error(
    predict_sighting(occ, bad_wx),
    "weather_data is missing columns:.*rh.*wind_speed"
  )
})

test_that("works with minimal occurrence columns (ws_id, date, hour only)", {
  # month and dayofyear should NOT be required — they are recomputed from date
  minimal_occ <- occ |> dplyr::select(ws_id, date, hour)
  result <- suppressMessages(predict_sighting(minimal_occ, wx, n_days = 3))
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
})

test_that("errors when occurrence has zero rows", {
  expect_error(
    predict_sighting(occ |> dplyr::filter(FALSE), wx),
    "occurrence has no rows"
  )
})

test_that("errors when weather_data has zero rows", {
  expect_error(
    predict_sighting(occ, wx |> dplyr::filter(FALSE)),
    "weather_data has no rows"
  )
})

test_that("errors for invalid n_days values", {
  expect_error(predict_sighting(occ, wx, n_days = 0),
               "n_days must be a positive integer")
  expect_error(predict_sighting(occ, wx, n_days = -3),
               "n_days must be a positive integer")
  expect_error(predict_sighting(occ, wx, n_days = "five"),
               "n_days must be a positive integer")
})

test_that("errors for invalid forecast_horizon values", {
  expect_error(predict_sighting(occ, wx, forecast_horizon = 0),
               "forecast_horizon must be a positive integer")
  expect_error(predict_sighting(occ, wx, forecast_horizon = -1),
               "forecast_horizon must be a positive integer")
})


# ==== 2. Station handling =====================================================

test_that("errors when specified station_id has no sightings", {
  expect_error(
    predict_sighting(occ, wx, station_id = "NONEXISTENT"),
    "No sightings found at station: NONEXISTENT"
  )
})

test_that("errors when station has too few weather records", {
  tiny_wx <- wx |> dplyr::slice_head(n = 10)
  expect_error(
    predict_sighting(occ, tiny_wx),
    "Fewer than 30 weather records"
  )
})

test_that("auto-selects station with the most sightings", {
  occ_multi <- dplyr::bind_rows(
    make_occurrence("WS001", n_records = 50),
    make_occurrence("WS002", n_records = 150)
  )
  wx_multi <- dplyr::bind_rows(
    make_weather("WS001"),
    make_weather("WS002")
  )
  result <- expect_message(
    predict_sighting(occ_multi, wx_multi, n_days = 3),
    "Auto-selected station: WS002"
  )
  expect_equal(unique(result$ws_id), "WS002")
})


# ==== 3. Output structure =====================================================

test_that("returns a tibble with expected columns and n_days rows", {
  result <- suppressMessages(predict_sighting(occ, wx, n_days = 3))

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)

  expected_cols <- c("rank", "date", "day_of_week", "best_hour",
                     "predicted_count", "confidence_lower",
                     "confidence_upper", "temp", "prcp", "rh",
                     "wind_speed", "ws_id")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("predictions are non-negative (count model)", {
  result <- suppressMessages(predict_sighting(occ, wx, n_days = 5))
  expect_true(all(result$predicted_count >= 0))
  expect_true(all(result$confidence_lower >= 0))
})

test_that("ranks are sequential from 1 to n_days", {
  result <- suppressMessages(predict_sighting(occ, wx, n_days = 4))
  expect_equal(result$rank, 1:4)
})

test_that("results are sorted by predicted_count descending", {
  result <- suppressMessages(predict_sighting(occ, wx, n_days = 5))
  diffs <- diff(result$predicted_count)
  expect_true(all(diffs <= 0))
})

test_that("model metadata attributes are attached", {
  result <- suppressMessages(predict_sighting(occ, wx))
  expect_false(is.null(attr(result, "model_type")))
  expect_true(attr(result, "model_type") %in% c("Poisson", "Negative Binomial"))
  expect_false(is.null(attr(result, "dispersion")))
  expect_false(is.null(attr(result, "station_id")))
  expect_false(is.null(attr(result, "n_obs")))
  expect_false(is.null(attr(result, "aic")))
  expect_true(is.numeric(attr(result, "aic")))
})


# ==== 4. Boundary / edge values ==============================================

test_that("n_days = 1 returns exactly one row", {
  result <- suppressMessages(predict_sighting(occ, wx, n_days = 1))
  expect_equal(nrow(result), 1)
  expect_equal(result$rank, 1L)
})

test_that("works with a short forecast_horizon", {
  result <- suppressMessages(
    predict_sighting(occ, wx, n_days = 2, forecast_horizon = 30)
  )
  expect_equal(nrow(result), 2)
  # all predicted dates should be within 30 days of the data's last date
  last_data_date <- max(wx$date)
  expect_true(all(result$date <= last_data_date + 30))
})

test_that("n_days larger than forecast_horizon returns fewer rows", {
  # With a 7-day horizon, requesting 100 days should cap at ~7
  result <- suppressMessages(
    predict_sighting(occ, wx, n_days = 100, forecast_horizon = 7)
  )
  expect_true(nrow(result) <= 7)
  expect_true(nrow(result) >= 1)
})

test_that("handles occurrence data concentrated on a single day", {
  single_day <- as.Date("2020-06-15")
  occ_single <- tibble(
    ws_id     = "WS001",
    date      = rep(single_day, 50),
    hour      = sample(6:18, 50, replace = TRUE)
  )
  result <- suppressMessages(predict_sighting(occ_single, wx, n_days = 3))
  expect_equal(nrow(result), 3)
})


# ==== 5. MASS / dplyr conflict is resolved ====================================

test_that("select() works correctly even if MASS is loaded", {
  # This is the exact scenario that caused the original bug:
  # loading MASS after dplyr would mask dplyr::select
  library(MASS)
  result <- suppressMessages(predict_sighting(occ, wx, n_days = 3))
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_true("rank" %in% names(result))
})


# ==== 6. Weather data edge cases ==============================================

test_that("handles weather data with some NA values", {
  wx_na <- wx
  wx_na$temp[1:5] <- NA
  wx_na$prcp[10:15] <- NA
  # Should still work—NAs are filtered out in step 3
  result <- suppressMessages(predict_sighting(occ, wx_na, n_days = 3))
  expect_equal(nrow(result), 3)
})

test_that("handles occurrence data with NA hours", {
  occ_na_hour <- occ
  occ_na_hour$hour[1:50] <- NA
  # modal_hour computation filters NAs, so this should work
  result <- suppressMessages(predict_sighting(occ_na_hour, wx, n_days = 3))
  expect_equal(nrow(result), 3)
  expect_true(!is.na(result$best_hour[1]))
})

test_that("errors when ALL weather rows become NA after filtering", {
  wx_all_na <- wx
  wx_all_na$temp <- NA
  expect_error(
    suppressMessages(predict_sighting(occ, wx_all_na)),
    "After joining and dropping NAs, fewer than 30 rows remain"
  )
})


# ==== 7. Plotting functions ===================================================

test_that("plot_predictions returns a ggplot object", {
  result <- suppressMessages(predict_sighting(occ, wx, n_days = 5))
  p <- plot_predictions(result)
  expect_s3_class(p, "ggplot")
})

test_that("plot_weather_effect errors on unknown variable", {
  result <- suppressMessages(predict_sighting(occ, wx, n_days = 5))
  expect_error(
    plot_weather_effect(result, var = "nonexistent_var"),
    "Variable 'nonexistent_var' not found"
  )
})

test_that("plot_weather_effect returns ggplot for valid variable", {
  result <- suppressMessages(predict_sighting(occ, wx, n_days = 5))
  p <- plot_weather_effect(result, var = "temp")
  expect_s3_class(p, "ggplot")
})

test_that("plot_model_diagnostics returns NULL with message when no model", {
  fake_preds <- tibble(date = Sys.Date(), predicted_count = 1)
  expect_message(
    result <- plot_model_diagnostics(fake_preds),
    "No model summary attached"
  )
  expect_null(result)
})
