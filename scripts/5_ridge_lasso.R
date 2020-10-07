library(ISLR)
library(tidyverse) 
library(knitr)
library(glmnet) # package for lasso and ridge regression

# data
str(Hitters)

## Reproducibility
set.seed(445)

## Data Processing
# 1. Remove records with missing values from the data (Hint: `complete.cases()` is useful)
# 2. Use `model.matrix` to create an $X$ matrix for all predictors that contains dummy variables for categorical predictors (for predicting `Salary`). You can specify this as a formula in the `model.matrix` call, e.g. 
#    eg. x <- model.matrix(y ~ ., data)[, -1] # remove the y column
# 3. Create a $Y$ vector of `Salary` information.

## Ridge Regression
# 1. Create a vector of $\lambda$ values from $\lambda = .01$ to  $\lambda = 10^10$ of length $100$.
# 2. Fit a ridge regression model for each $\lambda$ in your grid.
# 3. Make a line plot of coefficient corresponding to each $\lambda$. You should have an individual line for each variable with coefficient value on the $y$-axis and $\lambda$ on the $x$ axis. What happens to your coefficients as $\lambda$ increases?
# 4. Use `cv.glmnet` to perform $10$-fold cross validation and get an estimate of the test MSE for each $\lambda$ in your grid. Which $\lambda$ would you choose and why?

## Lasso
# 1. Fit the lasso model for each $\lambda$ in your grid.
# 2. Make a line plot of coefficient corresponding to each $\lambda$. You should have an individual line for each variable with coefficient value on the $y$-axis and $\lambda$ on the $x$ axis. (Hint: `coef` may be a useful function). What happens to your coefficients as $\lambda$ increases?
# 3. Use `cv.glmnet` to perform $10$-fold cross validation and get an estimate of the test MSE for each $\lambda$ in your grid. Which $\lambda$ would you choose and why?
  
  