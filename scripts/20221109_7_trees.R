library(ISLR) ## data package
library(tidyverse) ## data manipulation
library(tidymodels) ## tidy modeling
library(knitr) ## tables
library(rpart.plot) ## tree diagrams
library(vip) ## plotting variable importance

## reproducible
set.seed(445)

## data
str(Carseats)

## Data Preparation
# 1. Make a copy of the `Carseats` data frame called `df`.
# 2. Create a variable called `high_sales` in `df` that takes the value "high" if `Sales` > 8 and "low" otherwise.
# 3. Convert your `high_sales` column to be a factor.
# 4. Remove the `Sales` column from `df`.
Carseats |>
  mutate(high_sales = factor(if_else(Sales <= 8, "low", "high"))) |>
  select(-Sales) -> df

## Decision Trees
# 1. Using the `decision_tree()` function with the `"rpart"` engine in `tidymodels`, fit a large classification tree to predict `high_sales` using every variable in `df`. [**Hint:** The syntax is very similar fitting a `linear_reg`]
tree_spec <- decision_tree() |>
  set_engine("rpart") |>
  set_mode("classification")

tree_fit <- tree_spec |>
  fit(high_sales ~ ., data = df)

# 2. Inspect your tree. How many terminal nodes do you have? What is the training error rate?
tree_fit ## 11 terminal nodes

tree_fit |>
  augment(new_data = df) |>
  accuracy(truth = high_sales, estimate = .pred_class)

## training error is 1 - 0.8475 = 0.1525

# 3. Use the `rpart.plot` function to visualize your tree. What is the most important indicator of high sales?
tree_fit |>
  extract_fit_engine() |>
  rpart.plot()

# 4. Split your observations into a training and a test set with $200$ records each. Estimate the test error rate of your tree. 
df_split <- initial_split(df, prop = 0.5)

df_train <- training(df_split)
df_test <- testing(df_split)

tree_fit <- tree_spec |>
  fit(high_sales ~ ., data = df_train)

tree_fit |>
  augment(new_data = df_test) |>
  accuracy(truth = high_sales, estimate = .pred_class)

## test error is 1 - 0.765 = 0.235

# 5. Produce a confusion matrix for your test set.

tree_fit |>
  augment(new_data = df_test) |>
  conf_mat(truth = high_sales, estimate = .pred_class)

# 6. Perform cross-validation to determine the optimal level of tree complexity on your training data set. Which $\alpha$ (corresponds to `k` in the output) should we choose?
train_cv_10fold <- vfold_cv(df_train, v = 10)

tree_tune_spec <- decision_tree(cost_complexity = tune("alpha")) |>
  set_engine("rpart") |>
  set_mode("classification")

tree_tune_rec <- recipe(high_sales ~ ., data = df)

tree_wf <- workflow() |>
  add_model(tree_tune_spec) |>
  add_recipe(tree_tune_rec)

tune_df <- data.frame(alpha = 10^seq(-3, -1, length.out = 10))

tune_fit <- tree_wf |>
  tune_grid(resamples = train_cv_10fold, grid = tune_df)

tune_fit |>
  autoplot()

# 7. Use the functions `finalize_workflow` and `select_best()` to prune your tree to the chosen complexity. Plot your final tree.
tree_final <- finalize_workflow(tree_wf, select_best(tune_fit, metric = "accuracy"))
tree_final |>
  fit(data = df_train) -> tree_final_fit

# 8. Repeat 4-5 using your pruned tree. Which performs better?
tree_final_fit |>
  augment(new_data = df_test) |>
  accuracy(truth = high_sales, estimate = .pred_class)

## test error is 1 - 0.765 = 0.235 -- same error, simpler tree

tree_final_fit |>
  augment(new_data = df_test) |>
  conf_mat(truth = high_sales, estimate = .pred_class)

tree_final_fit |>
  extract_fit_engine() |>
  rpart.plot()


## Bagging & Random Forests
# 1. Perform bagging on your training `df` to predict `high_sales`. Specify `importance = TRUE` to also obtain information on the importance of each predictor.
bagging_spec <- rand_forest(mtry = .cols()) |>
  set_engine("randomForest", importance = TRUE) |>
  set_mode("classification")

bagging_fit <- bagging_spec |>
  fit(high_sales ~ ., data = df_train)

# 2. Make a plot of the importance values for each predictor using the `vip` function. What is the predictor with the highest importance?
vip(bagging_fit)

## Price is the most important variable followed closely by shelf location.

# 3. Estimate the test error rate using your bagged tree model.
bagging_fit |>
  augment(new_data = df_test) |>
  accuracy(truth = high_sales, estimate = .pred_class)

# 4. Repeat 1-3 using a random forest with $m = \sqrt{p}$ via `mtry = sqrt(.cols())`.
rf_spec <- rand_forest(mtry = sqrt(.cols())) |>
  set_engine("randomForest", importance = TRUE) |>
  set_mode("classification")

rf_fit <- rf_spec |>
  fit(high_sales ~ ., data = df_train)

vip(rf_fit) ## still Price and Shelf location are the most important

rf_fit |>
  augment(new_data = df_test) |>
  accuracy(truth = high_sales, estimate = .pred_class)

# 5. Compare the OOB confusion matrix to your test confusion matrix. [**Hint:** The `confusion` element of the model output fit is OOB.]

## OOB:
rf_fit |>
  extract_fit_engine() %>%
  .$confusion

## test confusion
rf_fit |>
  augment(new_data = df_test) |>
  conf_mat(truth = high_sales, estimate = .pred_class)

## Boosting
# 1. Fit a boosted tree ensemble to your training `df` predicting `high_sales` with $B = 5,000$ trees, learning rate of $\lambda = 0.01$, and an interaction depth of $d = 2$. 
boost_spec <- boost_tree(trees = 5000, tree_depth = 2, learn_rate = 0.01) |>
  set_engine("xgboost") |>
  set_mode("classification")

boost_fit <- boost_spec |>
  fit(high_sales ~ ., data = df_train)

# 2. Estimate the test error rate using your boosted tree model and compare to all previously fit models.
boost_fit |>
  augment(new_data = df_test) |>
  accuracy(truth = high_sales, estimate = .pred_class)

## has the most accurate fit of all the models.
  