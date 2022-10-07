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

## Ridge Regression
# 1. Create a vector of $\lambda$ values from $\lambda = .01$ to  $\lambda = 10^10$ of length $100$.
lambda <- lambda <- 10^seq(-2, 10, length.out = 100)
tune_df <- data.frame(lambda = lambda)

# 2. Fit a ridge regression model for each $\lambda$ in your grid. Be sure to normalize your predictors.
prep_data <- recipe(Salary ~ ., data = Hitters_complete) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_predictors())

ridge_ests <- data.frame()
for(lam in lambda) {
  ridge_spec <- linear_reg(mixture = 0, penalty = lam) |>
    set_mode("regression") |>
    set_engine("glmnet")
  
  workflow() |>
    add_model(ridge_spec) |>
    add_recipe(prep_data) |>
    fit(Hitters_complete) |>
    tidy() |>
    bind_rows(ridge_ests) -> ridge_ests
}

# 3. Make a line plot of coefficient corresponding to each $\lambda$. You should have an individual line for each variable with coefficient value on the $y$-axis and $\lambda$ on the $x$ axis. What happens to your coefficients as $\lambda$ increases?
ridge_ests |>
  filter(term != "(Intercept)") |>
  ggplot() +
  geom_line(aes(penalty, estimate, group = term, colour = term)) +
  coord_trans(x = "log10")

## as lambda increases you can see the overall magnitudes of the coefficients shrinking towards zero

# 4. Perform $10$-fold cross validation and get an estimate of the test MSE for each $\lambda$ in your grid. Which $\lambda$ would you choose and why? (Hint: look at the `tune` package for a fast way to do this.)
Hitters_10foldcv <- vfold_cv(Hitters_complete, v = 10)

ridge_spec <- linear_reg(mixture = 0, penalty = tune("lambda")) |>
  set_mode("regression") |>
  set_engine("glmnet")

workflow() |>
  add_model(ridge_spec) |>
  add_recipe(prep_data) |>
  tune_grid(resamples = Hitters_10foldcv, grid = tune_df) -> ridge_tune

ridge_tune |>
  collect_metrics() |>
  select(lambda, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  ggplot() +
  geom_line(aes(lambda, rmse^2)) +
  geom_point(aes(lambda, rmse^2)) +
  coord_trans(x = "log10")

## the penalty I would choose is
show_best(ridge_tune, metric = "rmse", n = 1)
    
  
## Lasso
# 1. Fit the lasso model for each $\lambda$ in your grid.
lasso_ests <- data.frame()
for(lam in lambda) {
  lasso_spec <- linear_reg(mixture = 1, penalty = lam) |>
    set_mode("regression") |>
    set_engine("glmnet")
  
  workflow() |>
    add_model(lasso_spec) |>
    add_recipe(prep_data) |>
    fit(Hitters_complete) |>
    tidy() |>
    bind_rows(lasso_ests) -> lasso_ests
}

# 2. Make a line plot of coefficient corresponding to each $\lambda$. You should have an individual line for each variable with coefficient value on the $y$-axis and $\lambda$ on the $x$ axis. (Hint: `coef` may be a useful function). What happens to your coefficients as $\lambda$ increases?
lasso_ests |>
  filter(term != "(Intercept)") |>
  ggplot() +
  geom_line(aes(penalty, estimate, group = term, colour = term)) +
  coord_trans(x = "log10")

## again as lambda increases the magnitudes of the coefficients go to zero
## but also we can see certain coefficients get set exactly to zero

# 3. Perform $10$-fold cross validation and get an estimate of the test MSE for each $\lambda$ in your grid. Which $\lambda$ would you choose and why?
lasso_spec <- linear_reg(mixture = 1, penalty = tune("lambda")) |>
  set_mode("regression") |>
  set_engine("glmnet")

workflow() |>
  add_model(lasso_spec) |>
  add_recipe(prep_data) |>
  tune_grid(resamples = Hitters_10foldcv, grid = tune_df) -> lasso_tune

lasso_tune |>
  collect_metrics() |>
  select(lambda, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  ggplot() +
  geom_line(aes(lambda, rmse^2)) +
  geom_point(aes(lambda, rmse^2)) +
  coord_trans(x = "log10")

## the penalty I would choose is
show_best(lasso_tune, metric = "rmse", n = 1)

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
## It does seem that the variability in Y is more explained with less components using
## PLS vs PCR, which is to be expected.
pca_tune |>
  collect_metrics() |>
  mutate(method = "pcr") |>
  bind_rows(pls_tune |> collect_metrics() |> mutate(method = "pls")) |>
  dplyr::select(M, .metric, mean, method) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  ggplot() +
  geom_line(aes(M, rmse^2, colour = method)) +
  geom_point(aes(M, rmse^2, colour = method))

## It also looks like PLS has lower CV MSE values for our chosen Ms. This would lead me to preferring
## PLS for this problem over PCR.
  

