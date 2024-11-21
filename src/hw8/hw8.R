library(dplyr)
library(lubridate)
library(tidymodels)

URL <- "https://www4.stat.ncsu.edu/~online/datasets/SeoulBikeData.csv"
# URL_repo_path <- "data/hw8/seoul_bike_data.csv"

# download the data
data <- read.csv(URL, header = TRUE, fileEncoding="latin1")
# data_local <- read.csv(URL_repo_path, header=TRUE) # downloading and running works fine vs the URL itself

# sum(is.na(data))
# str(data)

# everything is numeric except for Date (Date), Seasons (factor), Holiday (factor), Functioning.Day (factor)
data_fixed <- data |>
  mutate(
    Date = lubridate::dmy(Date),
    across(where(is.character), as.factor)
  ) |>
  rename(
    date = Date,
    rented_bike_count = Rented.Bike.Count,
    hour = Hour,
    temp_c = Temperature..C.,
    humidity = Humidity...,
    wind_speed_m_s = Wind.speed..m.s.,
    visibility_10m = Visibility..10m.,
    dew_point_temp_c = Dew.point.temperature..C.,
    solar_radiation_mj_m2 = Solar.Radiation..MJ.m2.,
    rainfall_mm = Rainfall.mm.,
    snowfall_cm = Snowfall..cm.,
    seasons = Seasons,
    holiday = Holiday,
    functioning_day = Functioning.Day
  ) |>
  filter(functioning_day == "Yes") |>
  select(-functioning_day)


# str(data_fixed)
# summary(data_fixed)

bike_data <- data_fixed |>
  group_by(date, seasons, holiday) |>
  summarize(
    rented_bike_count = sum(rented_bike_count),
    temp_c = mean(temp_c),
    humidity = mean(humidity),
    wind_speed_m_s = mean(wind_speed_m_s),
    visibility_10m = mean(visibility_10m),
    dew_point_temp_c = mean(dew_point_temp_c),
    solar_radiation_mj_m2 = mean(solar_radiation_mj_m2),
    rainfall_mm = sum(rainfall_mm),
    snowfall_cm = sum(snowfall_cm)
  ) |>
  ungroup()

# data split
set.seed(11)
bike_split <- initial_split(bike_data, prop = 0.75, strata = seasons)
bike_train <- training(bike_split)
bike_test <- testing(bike_split)
bike_10_fold <- vfold_cv(bike_train, v = 10, strata = seasons)

MLR_1 <- recipe(rented_bike_count ~ ., data = bike_train) |>
  step_date(date, features = "dow") |>
  step_mutate(day_type = factor(ifelse(date_dow %in% c("Sat", "Sun"), "Weekend", "Weekday"))) |>
  step_rm(date, date_dow) |>
  step_dummy(seasons, holiday, day_type) |>
  step_normalize(all_numeric(), -rented_bike_count)

MLR_2 <- MLR_1 |>
  step_interact(
    terms = ~starts_with("seasons") * starts_with("holiday") +
    starts_with("seasons") * temp_c + rainfall_mm * temp_c)

MLR_3 <- MLR_2 |>
  step_poly(temp_c, wind_speed_m_s, visibility_10m, dew_point_temp_c, solar_radiation_mj_m2, rainfall_mm, snowfall_cm, degree = 2)

MLR_spec <- linear_reg() |>
  set_engine("lm")

MLR_CV_fit1 <- workflow() |>
  add_recipe(MLR_1) |>
  add_model(MLR_spec) |>
  fit_resamples(resamples = bike_10_fold)

MLR_CV_fit2 <- workflow() |>
  add_recipe(MLR_2) |>
  add_model(MLR_spec) |>
  fit_resamples(resamples = bike_10_fold)
MLR_CV_fit3 <- workflow() |>
  add_recipe(MLR_3) |>
  add_model(MLR_spec) |>
  fit_resamples(resamples = bike_10_fold)

rbind(
  MLR_CV_fit1 |> collect_metrics(),
  MLR_CV_fit2 |> collect_metrics(),
  MLR_CV_fit3 |> collect_metrics()
)

final_fit <- workflow() |>
  add_recipe(MLR_3) |>
  add_model(MLR_spec) |>
  last_fit(bike_split)
final_fit |> collect_metrics()

final_fit |>
  extract_fit_parsnip() |>
  tidy()
