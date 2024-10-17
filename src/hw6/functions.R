library(httr)
library(jsonlite)
library(glue)
library(purrr)
library(lubridate)
library(hms)
library(dplyr)

getRMSE <- function(responses, predictions, ...) {
  # takes in a vector of responses and predictions and returns RMSE
  if (!is.vector(responses) | !is.vector(predictions)) {
    stop("Both inputs must be vectors")
  }
  RMSE <- sqrt(mean((responses - predictions)^2, ...))
  return (RMSE)
}

getMAE <- function(responses, predictions, ...) {
  # takes in a vector of responses and predictions and returns MAE
  if (!is.vector(responses) | !is.vector(predictions)) {
    stop("Both inputs must be vectors")
  }

  MAE <- mean(abs(responses - predictions), ...)
  return (MAE)
}

get_metrics <- function(responses, predictions, metrics="rmse,mae", ...) {
  # takes in a numeric atomic vector of responses and predictions and returns a list of metrics (RMSE, MAE) as optionsgit
  if (!all(
    is.numeric(responses), is.numeric(predictions),
    is.atomic(responses), is.atomic(predictions),
    is.vector(responses), is.vector(predictions)
  )) {
    stop("Both inputs must be atomic numeric vectors")
  }

  ret <- list()
  # RMSE
  if (grepl("rmse", metrics, ignore.case=TRUE)) {
    RMSE <- getRMSE(responses, predictions, ...)
    ret$RMSE <- RMSE
  }
  # MAE
  if (grepl("mae", metrics, ignore.case=TRUE)) {
    MAE <- getMAE(responses, predictions, ...)
    ret$MAE <- MAE
  }

  return (ret)
}

get_news_api_data <- function(api_key, subject="esports", time_period="2024-10-01") {
  BASE_URL <- glue("https://newsapi.org/v2/everything?q={subject}&from={time_period}&sortBy=publishedAt&apiKey={api_key}")

  data <- httr::GET(BASE_URL) |>
    content("text") |>
    fromJSON(flatten = TRUE, simplifyDataFrame = TRUE) |>
    pluck("articles")

    return (data)
}

get_dates_publish_diff <- function(data) {
  data_with_dates_and_diff <- data |>
    mutate(publishedAt = ymd_hms(publishedAt, tz="UTC")) |>
    arrange(desc(publishedAt)) |>
    mutate(pub_diff = lag(publishedAt, 1) - publishedAt) |>
    mutate(publishedAt_str = as_datetime(publishedAt, tz="EST"), pub_diff_str = as.character(as_hms(pub_diff)))

  return (data_with_dates_and_diff)
}
