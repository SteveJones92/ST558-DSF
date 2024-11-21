source("src/hw8/hw8.R")

library(tidyverse)
library(rpart.plot)
library(baguette)

# ------------ HW8 ------------
# MLR_1 <- recipe(rented_bike_count ~ ., data = bike_train) |>
#   step_date(date, features = "dow") |>
#   step_mutate(day_type = factor(ifelse(date_dow %in% c("Sat", "Sun"), "Weekend", "Weekday"))) |>
#   step_rm(date, date_dow) |>
#   step_dummy(seasons, holiday, day_type) |>
#   step_normalize(all_numeric(), -rented_bike_count)
# ------------ HW8 ------------

MLR_1 |> prep() |> bake(new_data = bike_train) |> str()

# ------------ HW9 ------------
# 1 for LASSO, penalty() for resampling
LASSO_spec <- linear_reg(penalty = tune(), mixture=1) |>
  set_engine("glmnet")

LASSO_wkf <- workflow() |>
  add_recipe(MLR_1) |>
  add_model(LASSO_spec)

# LASSO_wkf
LASSO_grid <- LASSO_wkf |>
  tune_grid(resamples=bike_10_fold, grid=grid_regular(penalty(range=c(-5, 2)), levels=200))

LASSO_grid |>
  collect_metrics() |>
  filter(.metric == "rmse") |>
  ggplot(aes(penalty, mean, color= .metric)) +
  geom_line()

lowest_rmse <- LASSO_grid |>
  select_best(metric = "rmse")
lowest_rmse

LASSO_final_fit <- LASSO_wkf |>
  finalize_workflow(lowest_rmse) |>
  last_fit(bike_split, metrics = metric_set(rmse, mae, rsq))

LASSO_final_fit |>
  extract_fit_parsnip() |>
  tidy()

LASSO_final_fit |>
  collect_metrics()




# need tuned regression tree

REG_TREE_spec <- decision_tree(tree_depth = tune(), min_n = 20, cost_complexity = tune()) |>
  set_engine("rpart") |>
  set_mode ("regression")

REG_TREE_wkf <- workflow() |>
  add_recipe(MLR_1) |>
  add_model(REG_TREE_spec)

REG_TREE_grid <- REG_TREE_wkf |>
  tune_grid(resamples=bike_10_fold, grid=grid_regular(cost_complexity(), tree_depth(), levels=c(10, 10)))

REG_TREE_grid |>
  collect_metrics() |>
  filter(.metric == "rmse") |>
  ggplot(aes(x=tree_depth, y=mean, color=.metric)) +
  geom_line()

REG_TREE_grid |>
  collect_metrics() |>
  filter(.metric == "rmse") |>
  ggplot(aes(x=cost_complexity, y=mean, color=.metric)) +
  geom_line()


lowest_rmse <- REG_TREE_grid |>
    select_best(metric = "rmse")
lowest_rmse

REG_TREE_wkf |>
  finalize_workflow(lowest_rmse)

REG_TREE_final <- REG_TREE_wkf |>
    finalize_workflow(lowest_rmse) |>
    fit(bike_train)

REG_TREE_final_fit <- REG_TREE_wkf |>
    finalize_workflow(lowest_rmse) |>
    last_fit(bike_split, metrics = metric_set(rmse, mae, rsq))


REG_TREE_final_fit |>
  collect_metrics()


tree_final_model <- extract_workflow(REG_TREE_final_fit)

tree_final_model |>
  extract_fit_engine() |>
  rpart.plot::rpart.plot(roundint = FALSE)


# need tuned bagged tree

BAG_TREE_spec <- bag_tree(tree_depth = 5, min_n = 10, cost_complexity = tune()) |>
  set_engine("rpart") |>
  set_mode("regression")

BAG_TREE_wkf <- workflow() |>
  add_recipe(MLR_1) |>
  add_model(BAG_TREE_spec)

# RMSE
BAG_TREE_grid <- BAG_TREE_wkf |>
  tune_grid(resamples = bike_10_fold, grid=grid_regular(cost_complexity(range = c(-5, -1)), levels=15), metrics = metric_set(rmse))

BAG_TREE_grid |>
  collect_metrics() |>
  filter(.metric == "rmse") |>
  ggplot(aes(x=cost_complexity, y=mean, color=.metric)) +
  geom_line()

lowest_rmse <- BAG_TREE_grid |>
  select_best(metric = "rmse")

lowest_rmse

BAG_TREE_final_fit <- BAG_TREE_wkf |>
    finalize_workflow(lowest_rmse) |>
    last_fit(bike_split, metrics = metric_set(rmse, mae, rsq))

# extract fit engine and $imp for variable importance
# plot


BAG_TREE_final_fit |>
    collect_metrics()


# need tuned random forest

RF_spec <- rand_forest(mtry = tune()) |>
  set_engine("ranger") |>
  set_mode("regression")

RF_wkf <- workflow() |>
  add_recipe(MLR_1) |>
  add_model(RF_spec)

RF_grid <- RF_wkf |>
    tune_grid(resamples = bike_10_fold, grid = 10, metrics = metric_set(rmse))

RF_grid |>
  collect_metrics() |>
  filter(.metric == "rmse") |>
  ggplot(aes(x=mtry, y=mean, color=.metric)) +
  geom_line()

lowest_rmse <- RF_grid |>
    select_best(metric = "rmse")
lowest_rmse

RF_final_fit <- RF_wkf |>
    finalize_workflow(lowest_rmse) |>
    last_fit(bike_split, metrics = metric_set(rmse, mae, rsq))

RF_final_fit |>
    collect_metrics()


