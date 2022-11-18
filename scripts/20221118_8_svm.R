library(tidyverse) ## data manipulation
library(tidymodels) ## models
library(knitr) ## tables

## reproducible
set.seed(445)

## Data Preparation
# Run the following code to create two datasets.

n1 <- 20
n2 <- 200
p <- 2

## training data sets
x_small <- matrix(rnorm(n1 * p), ncol = p)
x_large <- matrix(rnorm(n2 * p), ncol = p)
y_small <- c(rep(-1, n1/2), rep(1, n1/2))
y_large <- c(rep(1, n2/4*3), rep(2, n2/4))

## shift data farther apart
x_small[y_small == 1,] <- x_small[y_small == 1,] + 1
x_large[1:100,] <- x_large[1:100,] + 2
x_large[101:150,] <- x_large[101:150,] - 2

## put data into dataframes
df_small <- data.frame(x_small, y = as.factor(y_small))
df_large <- data.frame(x_large, y = as.factor(y_large))

# 1. Make two scatterplots to inspect the small and large training data sets. Describe what you see.
ggplot() +
  geom_point(aes(X1, X2, colour = y), data = df_small)

## linear boundary could work reasonably well with this data.

ggplot() +
  geom_point(aes(X1, X2, colour = y), data = df_large)

## linear boundary will work very poorly. Either polynomial or radial basis will 
## be necessary to separate the classes.

## Support Vector Classifier
# 1. Fit a support vector classifier on the small data with $C = 10$ (use `scaled = FALSE` to indicated your data has not been scaled.)
svm_linear_spec <- svm_poly(degree = 1) %>%
  set_mode("classification") %>%
  set_engine("kernlab", scaled = FALSE)

svm_linear_fit <- svm_linear_spec %>% 
  set_args(cost = 10) %>%
  fit(y ~ ., data = df_small)
# 2. How many support vectors were used to fit your classifier?
svm_linear_fit

## 9 support vectors were used

# 3. Predict a grid of $\boldsymbol X$ values between the range of $X_1$ and $X_2$. Plot these predictions using `geom_tile()` to visualize the decision boundary and add a scatteplot of training data on top, colored by training label. Describe what you see.
x_grid <- expand.grid(X1 = seq(min(df_small$X1), max(df_small$X1), length.out = 100),
                      X2 = seq(min(df_small$X2), max(df_small$X2), length.out = 100))

svm_linear_fit |>
  augment(new_data = x_grid) |>
  ggplot() +
  geom_tile(aes(X1, X2, fill = .pred_class), alpha = .3) +
  geom_point(aes(X1, X2, colour = y), data = df_small)

# 4. Alter your plot from 2 to change the shape of the support vectors.
svm_linear_fit |>
  extract_fit_engine() -> svm_linear_fit_engine

svm_linear_fit |>
  augment(new_data = x_grid) |>
  ggplot() +
  geom_tile(aes(X1, X2, fill = .pred_class), alpha = .3) +
  geom_point(aes(X1, X2, colour = y, shape = support_vector, size = support_vector), 
             data = df_small |> mutate(support_vector = 1:n() %in% svm_linear_fit_engine@alphaindex[[1]]))

# 5. Perform CV on the cost parameter. Which value of $C$ would you choose?
df_small_10foldcv <- vfold_cv(df_small, v = 10)
df_cost <- grid_regular(cost(), levels = 10)

svm_linear_tune_spec <- svm_poly(degree = 1, cost = tune("cost")) %>%
  set_mode("classification") %>%
  set_engine("kernlab", scaled = FALSE)

svm_linear_rec <- recipe(y ~ ., data = df_small)

svm_linear_wf <- workflow() |>
  add_model(svm_linear_tune_spec) |>
  add_recipe(svm_linear_rec)

tune_fit <- svm_linear_wf |>
  tune_grid(resamples = df_small_10foldcv, grid = df_cost)

autoplot(tune_fit)
show_best(tune_fit, metric = "accuracy", n = 1)

# 6. Repeat 3. and 4. using your chosen $C$ value. Describe what you see.
svm_linear_final <- finalize_workflow(svm_linear_wf, select_best(tune_fit, metric = "accuracy"))
svm_linear_final |>
  fit(data = df_small) -> svm_linear_final_fit

svm_linear_final_fit |>
  extract_fit_engine() -> svm_linear_final_fit_engine

svm_linear_final_fit |>
  augment(new_data = x_grid) |>
  ggplot() +
  geom_tile(aes(X1, X2, fill = .pred_class), alpha = .3) +
  geom_point(aes(X1, X2, colour = y, shape = support_vector, size = support_vector), 
             data = df_small |> mutate(support_vector = 1:n() %in% svm_linear_final_fit_engine@alphaindex[[1]]))
                                                                                              
## Support Vector Machines
# 1. Split the large data frame into 50% training and 50% test.
df_large_split <- initial_split(df_large, prop = .5)
df_large_train <- training(df_large_split)
df_large_test <- testing(df_large_split)

# 2. Fit a linear SVM, radial SVM with $\gamma = 1$, and polynomial SVM with $d = 3$ using CV to choose the appropriate cost for each model.
df_large_10foldcv <- vfold_cv(df_large_train, v = 10)

svm_poly_tune_spec <- svm_poly(degree = 3, cost = tune("cost")) %>%
  set_mode("classification") %>%
  set_engine("kernlab", scaled = FALSE)

svm_rbf_tune_spec <- svm_rbf(rbf_sigma = 1, cost = tune("cost")) %>%
  set_mode("classification") %>%
  set_engine("kernlab", scaled = FALSE)

svm_rec <- recipe(y ~ ., data = df_large)

svm_linear_wf <- workflow() |>
  add_model(svm_linear_tune_spec) |>
  add_recipe(svm_rec)

svm_poly_wf <- workflow() |>
  add_model(svm_poly_tune_spec) |>
  add_recipe(svm_rec)

svm_rbf_wf <- workflow() |>
  add_model(svm_rbf_tune_spec) |>
  add_recipe(svm_rec)

linear_tune_fit <- svm_linear_wf |>
  tune_grid(resamples = df_large_10foldcv, grid = df_cost)

poly_tune_fit <- svm_poly_wf |>
  tune_grid(resamples = df_large_10foldcv, grid = df_cost)

rbf_tune_fit <- svm_rbf_wf |>
  tune_grid(resamples = df_large_10foldcv, grid = df_cost)

autoplot(linear_tune_fit)
show_best(linear_tune_fit, metric = "accuracy", n = 1)

autoplot(poly_tune_fit)
show_best(poly_tune_fit, metric = "accuracy", n = 1)

autoplot(rbf_tune_fit)
show_best(rbf_tune_fit, metric = "accuracy", n = 1)

svm_linear_final <- finalize_workflow(svm_linear_wf, select_best(linear_tune_fit, metric = "accuracy"))
svm_linear_final |> fit(data = df_large_train) -> svm_linear_final_fit

svm_poly_final <- finalize_workflow(svm_poly_wf, select_best(poly_tune_fit, metric = "accuracy"))
svm_poly_final |> fit(data = df_large_train) -> svm_poly_final_fit

svm_rbf_final <- finalize_workflow(svm_rbf_wf, select_best(rbf_tune_fit, metric = "accuracy"))
svm_rbf_final |> fit(data = df_large_train) -> svm_rbf_final_fit


# 3. Predict a grid of $\boldsymbol X$ values between the range of $X_1$ and $X_2$. Plot these predictions using `geom_tile()` to visualize the decision boundary and add a scatteplot of training data on top, colored by training label. Describe what you see.
x_grid <- expand.grid(X1 = seq(min(df_large$X1), max(df_large$X1), length.out = 100),
                      X2 = seq(min(df_large$X2), max(df_large$X2), length.out = 100))

svm_linear_final_fit |> extract_fit_engine() -> svm_linear_final_fit_engine
svm_poly_final_fit |> extract_fit_engine() -> svm_poly_final_fit_engine
svm_rbf_final_fit |> extract_fit_engine() -> svm_rbf_final_fit_engine

svm_linear_final_fit |>
  augment(new_data = x_grid) |>
  ggplot() +
  geom_tile(aes(X1, X2, fill = .pred_class), alpha = .3) +
  geom_point(aes(X1, X2, colour = y, shape = support_vector, size = support_vector), 
             data = df_large_train |> mutate(support_vector = 1:n() %in% svm_linear_final_fit_engine@alphaindex[[1]]))

svm_poly_final_fit |>
  augment(new_data = x_grid) |>
  ggplot() +
  geom_tile(aes(X1, X2, fill = .pred_class), alpha = .3) +
  geom_point(aes(X1, X2, colour = y, shape = support_vector, size = support_vector), 
             data = df_large_train |> mutate(support_vector = 1:n() %in% svm_poly_final_fit_engine@alphaindex[[1]]))

svm_rbf_final_fit |>
  augment(new_data = x_grid) |>
  ggplot() +
  geom_tile(aes(X1, X2, fill = .pred_class), alpha = .3) +
  geom_point(aes(X1, X2, colour = y, shape = support_vector, size = support_vector), 
             data = df_large_train |> mutate(support_vector = 1:n() %in% svm_rbf_final_fit_engine@alphaindex[[1]]))

## radial and polynomial kernels are producing a decent decision boundary

# 4. Predict your test data with your three models. Which model would you choose?
svm_linear_final_fit |>
  augment(new_data = df_large_test) |>
  conf_mat(truth = y, estimate = .pred_class)

svm_poly_final_fit |>
  augment(new_data = df_large_test) |>
  conf_mat(truth = y, estimate = .pred_class)

svm_rbf_final_fit |>
  augment(new_data = df_large_test) |>
  conf_mat(truth = y, estimate = .pred_class)

## while the overall test accuracy of radial and polynomial kernels are the same, 
## it looks like the radial SVM produces a more balanced result.
