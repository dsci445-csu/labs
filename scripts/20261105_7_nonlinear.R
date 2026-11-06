library(ISLR)
library(tidyverse) 
library(tidymodels)
library(knitr)

# data
str(Wage)
Wage <- as_tibble(Wage)

## Reproducibility
set.seed(445)

## Polynomial Regression and Step Functions
# 1. Fit a degree-4 polynomial regression model predicting `wage` based on `age`. Inspect your model and describe the fit. [**Hint**: you can use the `step_poly` function to create your polynomials.]
lm_spec <- linear_reg()

poly_4_rec <- recipe(wage ~ age, data = Wage) |>
  step_poly(age, degree = 4)

poly_4_wf <- workflow() |>
  add_model(lm_spec) |>
  add_recipe(poly_4_rec)

poly_4_fit <- poly_4_wf |>
  fit(data = Wage)

poly_4_fit |> tidy()
poly_4_fit |> glance()

age_grid <- tibble(age = seq(min(Wage$age), max(Wage$age)))
bind_cols(
  augment(poly_4_fit, new_data = age_grid),
  predict(poly_4_fit, new_data = age_grid, type = "conf_int")
) -> wage_pred_poly_4

Wage |>
  ggplot() +
  geom_point(aes(age, wage), alpha = .3) +
  geom_line(aes(age, .pred), data = wage_pred_poly_4) +
  geom_line(aes(age, .pred_lower), data = wage_pred_poly_4, colour = "blue", lty = 2) +
  geom_line(aes(age, .pred_upper), data = wage_pred_poly_4, colour = "blue", lty = 2)

## Each power of the polynomial is significant in the model and the model is explaining something in the relationship
## because of the p-value of the F-test being so low. This is probably not good predictive model because we are only 
## explaining ~8% of the variability in the training Y with this model (R^2).

# 2. Choose your degree of polynomial using a cross validation approach. What degree model would you pick?
Wage_10foldcv <- vfold_cv(Wage, v = 10)
degree_df <- data.frame(degree = seq_len(12))

poly_rec <- recipe(wage ~ age, data = Wage) |>
  step_poly(age, degree = tune("degree"))

poly_wf <- workflow() |>
  add_model(lm_spec) |>
  add_recipe(poly_rec)

poly_tune <- poly_wf |>
  tune_grid(grid = degree_df, resamples = Wage_10foldcv)

poly_tune |>
  collect_metrics() |>
  select(degree, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  mutate(mse = rmse^2) |>
  ggplot() +
  geom_line(aes(degree, mse)) +
  geom_point(aes(degree, mse))

## best predictive degree polynomial
show_best(poly_tune, metric = "rmse", n = 1)

poly_final <- finalize_workflow(poly_wf, select_best(poly_tune, metric = "rmse")) 
poly_final_fit <- fit(poly_final, data = Wage)

poly_final_fit |> tidy()
poly_final_fit |> glance()

bind_cols(
  augment(poly_final_fit, new_data = age_grid),
  predict(poly_final_fit, new_data = age_grid, type = "conf_int")
) -> wage_pred_poly_final

Wage |>
  ggplot() +
  geom_point(aes(age, wage), alpha = .3) +
  geom_line(aes(age, .pred), data = wage_pred_poly_final) +
  geom_line(aes(age, .pred_lower), data = wage_pred_poly_final, colour = "blue", lty = 2) +
  geom_line(aes(age, .pred_upper), data = wage_pred_poly_final, colour = "blue", lty = 2)

# 3. Fit a step function for `age` predicting `wage` with $4$ cut points. You can use the function `step_discretize` to change your quantitative variable into a categorical one. Let `step_discretize` automatically choose the cut locations based on your data.
step_4_rec <- recipe(wage ~ age, data = Wage) |>
  step_discretize(age, num_breaks = 4)

step_4_wf <- workflow() |>
  add_model(lm_spec) |>
  add_recipe(step_4_rec)

step_4_fit <- step_4_wf |>
  fit(data = Wage)

step_4_fit |> tidy()
step_4_fit |> glance()

bind_cols(
  augment(step_4_fit, new_data = age_grid),
  predict(step_4_fit, new_data = age_grid, type = "conf_int")
) -> wage_pred_step_4

Wage |>
  ggplot() +
  geom_point(aes(age, wage), alpha = .3) +
  geom_line(aes(age, .pred), data = wage_pred_step_4) +
  geom_line(aes(age, .pred_lower), data = wage_pred_step_4, colour = "blue", lty = 2) +
  geom_line(aes(age, .pred_upper), data = wage_pred_step_4, colour = "blue", lty = 2)


## Regression Splines
# 1. Fit `wage` on `age` using a cubic regression spline with knots at ages $25, 40, 60$. 
bs_fixed_rec <- recipe(wage ~ age, data = Wage) |>
  step_bs(age, options = list(knots = c(25, 40, 60)))

bs_fixed_wf <- workflow() |>
  add_model(lm_spec) |>
  add_recipe(bs_fixed_rec)

bs_fixed_fit <- bs_fixed_wf |>
  fit(data = Wage)

bs_fixed_fit |> tidy()
bs_fixed_fit |> glance()

bind_cols(
  augment(bs_fixed_fit, new_data = age_grid),
  predict(bs_fixed_fit, new_data = age_grid, type = "conf_int")
) -> wage_pred_bs_fixed

Wage |>
  ggplot() +
  geom_point(aes(age, wage), alpha = .3) +
  geom_line(aes(age, .pred), data = wage_pred_bs_fixed) +
  geom_line(aes(age, .pred_lower), data = wage_pred_bs_fixed, colour = "blue", lty = 2) +
  geom_line(aes(age, .pred_upper), data = wage_pred_bs_fixed, colour = "blue", lty = 2)

# 2. Fit `wage` on `age` using a cubic regression spline with 6 degrees of freedom and knots chosen uniformly on the quantiles of the data (this is how `step_bs` does it by default).
bs_rec <- recipe(wage ~ age, data = Wage) |>
  step_bs(age, degree = 3, deg_free = 6)

bs_wf <- workflow() |>
  add_model(lm_spec) |>
  add_recipe(bs_rec)

bs_fit <- bs_wf |>
  fit(data = Wage)

bs_fit |> tidy()
bs_fit |> glance()

bind_cols(
  augment(bs_fit, new_data = age_grid),
  predict(bs_fit, new_data = age_grid, type = "conf_int")
) -> wage_pred_bs

Wage |>
  ggplot() +
  geom_point(aes(age, wage), alpha = .3) +
  geom_line(aes(age, .pred), data = wage_pred_bs) +
  geom_line(aes(age, .pred_lower), data = wage_pred_bs, colour = "blue", lty = 2) +
  geom_line(aes(age, .pred_upper), data = wage_pred_bs, colour = "blue", lty = 2)

# 3. Fit `wage` on `age` using a natural cubic regression spline with 6 degrees of freedom and knots chosen uniformly on the quantiles of the data.
ns_rec <- recipe(wage ~ age, data = Wage) |>
  step_ns(age, deg_free = 6)

ns_wf <- workflow() |>
  add_model(lm_spec) |>
  add_recipe(ns_rec)

ns_fit <- ns_wf |>
  fit(data = Wage)

ns_fit |> tidy()
ns_fit |> glance()

bind_cols(
  augment(ns_fit, new_data = age_grid),
  predict(ns_fit, new_data = age_grid, type = "conf_int")
) -> wage_pred_ns

Wage |>
  ggplot() +
  geom_point(aes(age, wage), alpha = .3) +
  geom_line(aes(age, .pred), data = wage_pred_ns) +
  geom_line(aes(age, .pred_lower), data = wage_pred_ns, colour = "blue", lty = 2) +
  geom_line(aes(age, .pred_upper), data = wage_pred_ns, colour = "blue", lty = 2)

# 4. Create a scatter plot of `wage` vs `age` with all three of your fitted splines overlayed as well as your chosen polynomial model (either by anova or CV). Comment on the shapes. [**Hint:** `predict` over a grid of `age` values might be helpful.]
Wage |>
  ggplot() +
  geom_point(aes(age, wage), alpha = .3) +
  geom_line(aes(age, .pred), data = wage_pred_bs_fixed, colour = "darkgreen") +
  geom_line(aes(age, .pred_lower), data = wage_pred_bs_fixed, colour = "darkgreen", lty = 2) +
  geom_line(aes(age, .pred_upper), data = wage_pred_bs_fixed, colour = "darkgreen", lty = 2) +
  geom_line(aes(age, .pred), data = wage_pred_bs, colour = "red") +
  geom_line(aes(age, .pred_lower), data = wage_pred_bs, colour = "red", lty = 2) +
  geom_line(aes(age, .pred_upper), data = wage_pred_bs, colour = "red", lty = 2) +
  geom_line(aes(age, .pred), data = wage_pred_ns, colour = "blue") +
  geom_line(aes(age, .pred_lower), data = wage_pred_ns, colour = "blue", lty = 2) +
  geom_line(aes(age, .pred_upper), data = wage_pred_ns, colour = "blue", lty = 2)

## GAMs
# 1. Fit a GAM using natural spline functions of `year` and `age`, treating `education` as a categorical predictor. 
gam_rec <- recipe(wage ~ age + year + education, data = Wage) |>
  step_ns(age, deg_free = 6) |>
  step_ns(year, deg_free = 7)

gam_wf <- workflow() |>
  add_model(lm_spec) |>
  add_recipe(gam_rec)

gam_fit <- gam_wf |>
  fit(data = Wage)

gam_fit |> tidy()
gam_fit |> glance()

dat_grid <- tibble(expand.grid(age = seq(min(Wage$age), max(Wage$age)),
                               year = seq(min(Wage$year), max(Wage$year)),
                               education = unique(Wage$education)))
bind_cols(
  augment(gam_fit, new_data = dat_grid),
  predict(gam_fit, new_data = dat_grid, type = "conf_int")
) -> wage_pred_gam

for(pred in c("age", "year", "education")) {
  pred_dat <- wage_pred_gam[, c(pred, ".pred", ".pred_lower", ".pred_upper")] |>
    group_by_at(pred) |>
    summarise(.pred = mean(.pred), .pred_lower = mean(.pred_lower), .pred_upper = mean(.pred_upper))
  wage_dat <- Wage[, c(pred, "wage")]
  
  if(is.numeric(wage_dat |> pull(pred))) {
    p <- wage_dat |>
      ggplot() +
      geom_point(aes_string(pred, "wage"), alpha = .3) +
      geom_line(aes_string(pred, ".pred"), data = pred_dat, colour = "darkgreen") +
      geom_line(aes_string(pred, ".pred_lower"), data = pred_dat, colour = "darkgreen", lty = 2) +
      geom_line(aes_string(pred, ".pred_upper"), data = pred_dat, colour = "darkgreen", lty = 2)
  } else {
    p <- wage_dat |>
      ggplot() +
      geom_jitter(aes_string(pred, "wage"), alpha = .3) +
      geom_point(aes_string(pred, ".pred"), data = pred_dat, colour = "darkgreen") +
      geom_errorbar(aes_string(pred, ymin = ".pred_lower", ymax = ".pred_upper"), data = pred_dat, colour = "darkgreen")
  }
  print(p)
}



