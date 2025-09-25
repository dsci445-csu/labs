### libraries
library(tidymodels)
library(discrim)
library(ISLR)
library(ggplot2)
library(dplyr)

## Stock Market Data ----
# 1. Load and explore (through numerical and graphical summaries) the `Smarket` data (this is in `ISLR` package). 
head(Smarket)

## Any relationship between Direction and Lag over time?
Smarket |>
  pivot_longer(Lag1:Lag5, names_to = "Lag", values_to = "value") |>
  separate(Lag, into = c("junk", "Lag"), sep = "g") |>
  select(-junk) |>
  mutate(Lag = as.numeric(Lag)) |>
  ggplot() +
  geom_boxplot(aes(Year, value, fill = Direction)) +
  facet_wrap(~Lag)

## Doesn't look like much....

## Logistic Regression ----
# 1. Fit a logistic regression model to predict `Direction` using `Lag1` through `Lag5` and `Volume`. Describe your results.
logistic_spec <- logistic_reg()

logistic_spec |>
  fit(Direction ~ ., data = Smarket |> select(-Year, -Today), family = "binomial") -> m0.fit

m0.fit |>
  pluck("fit") |>
  summary()

# 2. Create a confusion matrix for the training data.
m0.fit |>
  augment(new_data = Smarket) |>
  conf_mat(truth = Direction, estimate = .pred_class)


# 3. What is the overall error rate of the model?
m0.fit |>
  augment(new_data = Smarket) |>
  accuracy(truth = Direction, estimate = .pred_class) |>
  mutate(error = 1 - .estimate) |>
  pull(error)

## This is barely better than flipping a coin.

# 4. Create two data sets, `train` and `test` that correspond to the observations from 2001 to 2004 (`train`) and 2005 (`test`).
train <- Smarket |> filter(Year <= 2004)
test <- Smarket |> filter(Year == 2005)

# 5. Repeat 1-3, but obtain the test confusion matrix and error rate.
logistic_spec |>
  fit(Direction ~ ., data = train |> select(-Year, -Today), family = "binomial") |>
  augment(new_data = test) -> m0.test_res

m0.test_res |>
  conf_mat(truth = Direction, estimate = .pred_class)

m0.test_res |>
  accuracy(truth = Direction, estimate = .pred_class) |>
  mutate(error = 1 - .estimate) |>
  pull(error)

## Now our error rate is worse than coin flipping.

# 6. Repeat 5, but with a model of `Direction` on `Lag1` and `Lag2` only.
logistic_spec |>
  fit(Direction ~ Lag1 + Lag2, data = train, family = "binomial") |>
  augment(new_data = test) -> m1.test_res

m1.test_res |>
  conf_mat(truth = Direction, estimate = .pred_class)

m1.test_res |>
  accuracy(truth = Direction, estimate = .pred_class) |>
  mutate(error = 1 - .estimate) |>
  pull(error)

## Similar results.

## LDA ----
# 1. Fit a linear discriminant analysis model to the `train` data set you created in the previous section with `Direction` as the response and `Lag1` and `Lag2` as the predictors.
lda_spec <- discrim_linear()

lda_spec |>
  fit(Direction ~ Lag1 + Lag2, data = train) -> m2.fit

# 2. What are the values for $\hat{\pi}_1$ and $\hat{\pi}_2$?
m2.fit$fit$prior

# 3. Create a confusion matrix for the `test` data.
m2.fit |>
  augment(new_data = test) -> m2.test_res

m2.test_res |>
  conf_mat(truth = Direction, estimate = .pred_class)

# 4. What is the test error rate?
m2.test_res |>
  accuracy(truth = Direction, estimate = .pred_class) |>
  mutate(error = 1 - .estimate) |>
  pull(error)


## QDA ----
# 1. Fit a quadratic discriminant analysis model to the `train` data set you created in the previous section with `Direction` as the response and `Lag1` and `Lag2` as the predictors.
qda_spec <- discrim_quad()

qda_spec |>
  fit(Direction ~ Lag1 + Lag2, data = train) -> m3.fit

# 2. Create a confusion matrix for the `test` data.
m3.fit |>
  augment(new_data = test) -> m3.test_res

m3.test_res |>
  conf_mat(truth = Direction, estimate = .pred_class)

# 3. What is the test error rate?
m3.test_res |>
  accuracy(truth = Direction, estimate = .pred_class) |>
  mutate(error = 1 - .estimate) |>
  pull(error)

## KNN ----
# 1. Fit a KNN model with $K = 1$ to the `train` data set you created in the previous section with `Direction` as the response and `Lag1` and `Lag2` as the predictors.
knn1_spec <- nearest_neighbor(mode = "classification", neighbors = 1)

knn1_spec |>
  fit(Direction ~ Lag1 + Lag2, data = train) -> m4.fit


# 2. Create a confusion matrix for the `test` data.
m4.fit |>
  augment(new_data = test) -> m4.test_res

m4.test_res |>
  conf_mat(truth = Direction, estimate = .pred_class)

# 3. What is the test error rate?
m4.test_res |>
  accuracy(truth = Direction, estimate = .pred_class) |>
  mutate(error = 1 - .estimate) |>
  pull(error)

# 4. Repeat 1.-3. with $K = 3$ and $K = 5$.
knn3_spec <- nearest_neighbor(mode = "classification", neighbors = 3)

knn3_spec |>
  fit(Direction ~ Lag1 + Lag2, data = train) -> m5.fit

m5.fit |>
  augment(new_data = test) -> m5.test_res

m5.test_res |>
  conf_mat(truth = Direction, estimate = .pred_class)

m5.test_res |>
  accuracy(truth = Direction, estimate = .pred_class) |>
  mutate(error = 1 - .estimate) |>
  pull(error)

knn5_spec <- nearest_neighbor(mode = "classification", neighbors = 5)

knn5_spec |>
  fit(Direction ~ Lag1 + Lag2, data = train) -> m6.fit

m6.fit |>
  augment(new_data = test) -> m6.test_res

m6.test_res |>
  conf_mat(truth = Direction, estimate = .pred_class)

m6.test_res |>
  accuracy(truth = Direction, estimate = .pred_class) |>
  mutate(error = 1 - .estimate) |>
  pull(error)

#Of all the models you fit today, which would you pick to predict values of `Direction` and why?

## QDA appears to give the "best" test error rate, indicating a good mix of flexibility.


