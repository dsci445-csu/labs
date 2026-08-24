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

## Decision Trees
# 1. Using the `decision_tree()` function with the `"rpart"` engine in `tidymodels`, fit a large classification tree to predict `high_sales` using every variable in `df`. [**Hint:** The syntax is very similar fitting a `linear_reg`]
# 2. Inspect your tree. How many terminal nodes do you have? What is the training error rate?
# 3. Use the `rpart.plot` function to visualize your tree. What is the most important indicator of high sales?
# 4. Split your observations into a training and a test set with $200$ records each. Estimate the test error rate of your tree. 
# 5. Produce a confusion matrix for your test set.
# 6. Perform cross-validation to determine the optimal level of tree complexity on your training data set. Which $\alpha$ (corresponds to `k` in the output) should we choose?
# 7. Use the functions `finalize_workflow` and `select_best()` to prune your tree to the chosen complexity. Plot your final tree.
# 8. Repeat 4-5 using your pruned tree. Which performs better?

## Bagging & Random Forests
# 1. Perform bagging on your training `df` to predict `high_sales`. Specify `importance = TRUE` to also obtain information on the importance of each predictor.
# 2. Make a plot of the importance values for each predictor using the `vip` function. What is the predictor with the highest importance?
# 3. Estimate the test error rate using your bagged tree model.
# 4. Repeat 1-3 using a random forest with $m = \sqrt{p}$ via `mtry = sqrt(.cols())`.
# 5. Compare the OOB confusion matrix to your test confusion matrix. [**Hint:** The `confusion` element of the model output fit is OOB.]

## Boosting
# 1. Fit a boosted tree ensemble to your training `df` predicting `high_sales` with $B = 5,000$ trees, learning rate of $\lambda = 0.1$, and an interaction depth of $d = 2$. 
# 2. Estimate the test error rate using your boosted tree model and compare to all previously fit models.
  