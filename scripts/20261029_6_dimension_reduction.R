library(ISLR)
library(tidyverse) 
library(tidymodels) 

# data
str(Hitters)

## Reproducibility
set.seed(445)

## Data Processing
# 1.  Remove records with missing data. Create a new (complete) version of your data set. (Hint: `drop_na` in `tidyr` could be helpful.)

Hitters_complete <- Hitters |>
  drop_na()

# 2. You may need to create dummy variables for categorical variables in your recipes. `step_dummy(all_nominal_predictors())` is a good way to do this.
# 3. You may need to standardize all variables in your recipes. `step_normalize(all_predictors())` is a good way to do this.


## Principal Components Regression
# 1. Fit the PCR model using 10-fold cross validation for values of $M$. Be sure to normalize your predictors.
tune_df <- data.frame(M = seq_len(ncol(Hitters_complete) - 1))

prep_data_pca <- prep_data |>
  step_pca(all_predictors(), num_comp = tune("M"))

linear_spec <- linear_reg()

workflow() |>
  add_model(linear_spec) |>
  add_recipe(prep_data_pca) -> pca_workflow

pca_workflow |>
  tune_grid(resamples = Hitters_10foldcv, grid = tune_df) -> pca_tune

# 2. Create a plot of the CV MSE vs. $M$.
pca_tune |>
  collect_metrics() |>
  select(M, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  ggplot() +
  geom_line(aes(M, rmse^2)) +
  geom_point(aes(M, rmse^2))

# 3. When does the smallest cross-validation error occur? Which $M$ would you choose for your final model?
show_best(pca_tune, metric = "rmse", n = 1)

## final model
pca_final <- finalize_workflow(pca_workflow, select_best(pca_tune, metric = "rmse"))
pca_final_fit <- fit(pca_final, data = Hitters_complete)

# 4. How much variability in $Y$ is explained for your chosen value of $M$?
glance(pca_final_fit)$r.squared

## Partial Least Squares
# 1. Fit the PLS model using 10-fold cross validation for values of $M$. Be sure to normalize your predictors.
prep_data_pls <- prep_data |>
  step_pls(all_predictors(), outcome = "Salary", num_comp = tune("M"))

workflow() |>
  add_model(linear_spec) |>
  add_recipe(prep_data_pls) -> pls_workflow

pls_workflow |>
  tune_grid(resamples = Hitters_10foldcv, grid = tune_df) -> pls_tune

# 2. Create a plot of the CV MSE vs. $M$.
pls_tune |>
  collect_metrics() |>
  dplyr::select(M, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  ggplot() +
  geom_line(aes(M, rmse^2)) +
  geom_point(aes(M, rmse^2))

# 3. When does the smallest cross-validation error occur? Which $M$ would you choose for your final model?
show_best(pls_tune, metric = "rmse", n = 1)

## final model
pls_final <- finalize_workflow(pls_workflow, select_best(pls_tune, metric = "rmse"))
pls_final_fit <- fit(pls_final, data = Hitters_complete)

# 4. How much variability in $Y$ is explained for your chosen value of $M$?
glance(pls_final_fit)$r.squared

# 5. Discuss the difference between PCR and PLS results. Which would you prefer?
## It does seem that the variability in Y is similarly explained with less components using
## PLS vs PCR.
pca_tune |>
  collect_metrics() |>
  mutate(method = "pcr") |>
  bind_rows(pls_tune |> collect_metrics() |> mutate(method = "pls")) |>
  dplyr::select(M, .metric, mean, method) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  ggplot() +
  geom_line(aes(M, rmse^2, colour = method)) +
  geom_point(aes(M, rmse^2, colour = method))

## It looks like PCR has lower CV MSE values for our chosen Ms, but uses almost all possible components to get there. 
## This would lead me to preferring PCR, but neither is great.