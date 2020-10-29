library(ISLR) ## data package
library(tidyverse) ## data manipulation
library(knitr) ## tables

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
library(tree) ## tree package
# 1. Using the `tree` function, fit a large classification tree to predict `high_sales` using every variable in `df`. [**Hint:** The syntax is very similar to `lm`]
# 2. Inspect your tree using `summary`. How many terminal nodes do you have? Whatt is the training error rate?
# 3. Use the `plot` function to visualize your tree. What is the most important indicator of high sales?
# **Hint:** Adding the following line after you plot the tree will add labels.
# text(tree.fit, pretty = 0)
# 4. Split your observations into a training and a test set with $200$ records each. Estimate the test error rate of your tree. [**Hint:** using `type = "class"` in your `predict` function will get you the actual class predictions.]
# 5. Produce a confusion matrix for your test set.
# 6. Use the `cv.tree` function to perform cross-validation to determine the optimal level of tree complexity. Using `FUN = prune.misclass` indicates that we want to use the classification error rate (instead of deviance) to guide the CV and pruning process. Which $\alpha$ (corresponds to `k` in the output) should we choose?
# 7. Use the function `prune.misclass` to prune your tree to the chosen complexity.
# 8. Repeat 4-5 using your pruned tree. Which performs better?

## Bagging & Random Forests
library(randomForest) # random forests & bagging
# 1. Perform bagging on your training `df` to predict `high_sales`. Specify `importance = TRUE` to also obtain information on the importance of each predictor.
# 2. Make a plot of the importance values for each predictor. What is the predictor with the highes importance?
# 3. Estimate the test error rate using your bagged tree model.
# 4. Repeat 1-3 using a random forest with $m = \sqrt{p}$.
# 5. Compare the OOB confusion matrix to your test confusion matrix. [**Hint:** The `confusion` element of the model output is OOB.]

## Boosting
library(gbm) ## boosting package
# 1. Fit a boosted tree ensemble to your training `df` predicting `high_sales` with $B = 5,000$ trees, shrinkage parameter of $\lambda = 0.1$, and an interaction depth of $d = 2$. We sure to include `distribution = "bernoulli"` to indicate a classification problem.
# 2. Estimate the test error rate using your boosted tree model and compare to all previously fit models.
  