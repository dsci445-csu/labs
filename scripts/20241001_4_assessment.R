## Libraries
library(ISLR) ## data
library(tidyverse) ## data manipulation & plots
library(tidymodels) ## tidy models

## Data
head(Auto)

## Reproducibility
set.seed(445)

## Validation Set Approach
# 1. Split the data into 50% training and 50% test data.
Auto.val <- validation_split(Auto, prop = 0.5)

# 2. Fit a linear model of `mpg` on `horsepower` using your training data.
linear_spec <- linear_reg()
linear_rec <- recipe(mpg ~ horsepower, data = Auto)

linear_model <- workflow() |>
  add_model(linear_spec) |>
  add_recipe(linear_rec) |>
  fit_resamples(resamples = Auto.val)


# 3. Estimate the test error by using test MSE.
linear_model |>
  collect_metrics() |>
  filter(.metric == "rmse") |>
  mutate(mean = mean^2) |>
  pull(mean)

# 4. Repeat steps 2-3 for a cubic and quadratic model. Which model would you pick?
quad_rec <- linear_rec |> step_mutate(horsepower2 = horsepower^2)

workflow() |>
  add_model(linear_spec) |>
  add_recipe(quad_rec) |>
  fit_resamples(resamples = Auto.val) |>
  collect_metrics() |>
  filter(.metric == "rmse") |>
  mutate(mean = mean^2) |>
  pull(mean)

cubic_rec <- quad_rec |> step_mutate(horsepower3 = horsepower^3)

workflow() |>
  add_model(linear_spec) |>
  add_recipe(cubic_rec) |>
  fit_resamples(resamples = Auto.val) |>
  collect_metrics() |>
  filter(.metric == "rmse") |>
  mutate(mean = mean^2) |>
  pull(mean)

## I would pick the quadratic model, although the cubic is very close to the same test MSE.

# 5. Repeat steps 1-4 after reseting the seed
set.seed(42)

## split data
Auto.val2 <- validation_split(Auto, prop = 0.5)

## linear
workflow() |>
  add_model(linear_spec) |>
  add_recipe(linear_rec) |>
  fit_resamples(resamples = Auto.val2) |>
  collect_metrics() |>
  filter(.metric == "rmse") |>
  mutate(mean = mean^2) |>
  pull(mean)

## quadratic
workflow() |>
  add_model(linear_spec) |>
  add_recipe(quad_rec) |>
  fit_resamples(resamples = Auto.val2) |>
  collect_metrics() |>
  filter(.metric == "rmse") |>
  mutate(mean = mean^2) |>
  pull(mean)

## cubic
workflow() |>
  add_model(linear_spec) |>
  add_recipe(cubic_rec) |>
  fit_resamples(resamples = Auto.val2) |>
  collect_metrics() |>
  filter(.metric == "rmse") |>
  mutate(mean = mean^2) |>
  pull(mean)


# 6. Did you get the same results? Is this what you expected to happen?

## yes, same results (quadratic is the best model), although the estimates of error are not exactly the same.
## I would expect the results to be slightly different because the validation approach has a lot of variance 
## => the estimates depend heavily on the exact split.

## LOOCV

# 1. Get the estimate of test MSE for the linear model using LOOCV.

## split data
Auto.loo <- vfold_cv(Auto, v = nrow(Auto))

## linear
workflow() |>
  add_model(linear_spec) |>
  add_recipe(linear_rec) |>
  fit_resamples(resamples = Auto.loo) |>
  collect_metrics() |>
  filter(.metric == "rmse") |>
  mutate(mean = mean^2) |>
  pull(mean)

## 14.81286

# 2. Repeat steps 2-3 for a cubic and quadratic model. Which model would you pick?

## quadratic
workflow() |>
  add_model(linear_spec) |>
  add_recipe(quad_rec) |>
  fit_resamples(resamples = Auto.loo) |>
  collect_metrics() |>
  filter(.metric == "rmse") |>
  mutate(mean = mean^2) |>
  pull(mean)

## 10.70625

## cubic
workflow() |>
  add_model(linear_spec) |>
  add_recipe(cubic_rec) |>
  fit_resamples(resamples = Auto.loo) |>
  collect_metrics() |>
  filter(.metric == "rmse") |>
  mutate(mean = mean^2) |>
  pull(mean)

## 10.73746

## I would still pick the quadratic model as it has the lowest estimates test MSE

## k-Fold CV
# 1. Using $k = 10$-fold CV, compute the $k$-fold CV estimate of the test MSE for polynomial models of order $i = 1, \dots, 10$. (Hint: you can use the `poly` function in your formula to specify a polynomial model.)
Auto.kfold <- vfold_cv(Auto, v = 10) ## split into k folds
degrees <- data.frame(degree = 1:10) ## tuning values

## setup polynomial regressions
poly_rec <- recipe(mpg ~ horsepower, data = Auto) |>
  step_poly(horsepower, degree = tune("degree"))

workflow() |>
  add_model(linear_spec) |>
  add_recipe(poly_rec) |>
  tune_grid(resamples = Auto.kfold, grid = degrees) -> tune.fit

# 2. Plot the estimated test MSE vs. the polynomial order.
collect_metrics(tune.fit) |>
  select(degree, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  mutate(mse = rmse^2) |>
  ggplot() +
  geom_line(aes(degree, mse)) +
  geom_point(aes(degree, mse))

# 3. Which of these models would you choose?
# I would pick the model with polynomial of degree 7
show_best(tune.fit, metric = "rmse")

## Bonus
# 1. Write your own $k$-fold CV function that will calculate CV for the $KNN$ Regression model. You function should take as parameters
#    - CV $k$ value
#    - KNN $K$ value
#    - Data
#    - A vector of names (character) of predictor columns
#    - A character string of the response column
# And return the estimated test MSE.
# 2. Use your function to estimate the test MSE using 10-fold CV for KNN models with $K = 1, 5, 10, 20, 100$ of a model predicting `mpg` using the `horsepower` predictor variable in the `Auto` data set.
# 3. Compare your results to the previous $k$-Fold CV method.
k_fold_cv_err_knn <- function(k_fold = 10, knn, data, formula) {
  data.kfold <- vfold_cv(data, v = k_fold)
  
  knn_spec <- nearest_neighbor(mode = "regression", neighbors = knn)
  knn_res <- recipe(formula, data)
  
  workflow() |>
    add_model(knn_spec) |>
    add_recipe(knn_res) |>
    fit_resamples(data.kfold) |>
    collect_metrics() |>
    select(.metric, mean) |>
    pivot_wider(names_from = .metric, values_from = mean) |>
    mutate(mse = rmse^2) |>
    pull(mse)
}

res <- data.frame(knn = c(1, 5, 10, 20, 100))
for(i in seq_len(nrow(res))) {
  res[i, "mse"] <- k_fold_cv_err_knn(10, res[i, "knn"], Auto, mpg ~ horsepower)
}  

collect_metrics(tune.fit) |>
  select(degree, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  mutate(mse = rmse^2) |>
  mutate(model = "lm") |>
  select(model, degree, mse) |>
  bind_rows(res |> rename(degree = knn) |> mutate(model = "knn")) |>
  ggplot() +
  geom_line(aes(degree, mse)) +
  geom_point(aes(degree, mse)) +
  facet_grid(. ~ model, scales = "free_x")

res[which.min(res$mse),]

show_best(tune.fit, metric = "rmse") |> mutate(mse = mean^2)

## knn with 100 neighbors looks better than the best polynomial model.
## based on the above I would choose the model with neighbor size of 20 or 100. We know as K gets 
## larger there is more of a linear decision boundary. This is at odds with our polynomial regression
## results, which pointed to needing a highly flexible model. However, whether KNN or polynomial regression,
## it appears we achieve similar estimated test MSE.