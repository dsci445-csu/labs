library(ISLR)
library(tidyverse) 
library(tidymodels) 

# data
str(Hitters)

## Reproducibility
set.seed(445)

## Data Processing
# 1.  Remove records with missing data. Create a new (complete) version of your data set. (Hint: `drop_na` in `tidyr` could be helpful.)
# 2. You may need to create dummy variables for categorical variables in your recipes. `step_dummy(all_nominal_predictors())` is a good way to do this.
# 3. You may need to standardize all variables in your recipes. `step_normalize(all_predictors())` is a good way to do this.

## Ridge Regression
# 1. Create a vector of $\lambda$ values from $\lambda = .01$ to  $\lambda = 10^10$ of length $100$.
# 2. Fit a ridge regression model for each $\lambda$ in your grid. Be sure to normalize your predictors.
# 3. Make a line plot of coefficient corresponding to each $\lambda$. You should have an individual line for each variable with coefficient value on the $y$-axis and $\lambda$ on the $x$ axis. What happens to your coefficients as $\lambda$ increases?
# 4. Perform $10$-fold cross validation and get an estimate of the test MSE for each $\lambda$ in your grid. Which $\lambda$ would you choose and why? (Hint: look at the `tune` package for a fast way to do this.)

## Lasso
# 1. Fit the lasso model for each $\lambda$ in your grid.
# 2. Make a line plot of coefficient corresponding to each $\lambda$. You should have an individual line for each variable with coefficient value on the $y$-axis and $\lambda$ on the $x$ axis. (Hint: `coef` may be a useful function). What happens to your coefficients as $\lambda$ increases?
# 3. Perform $10$-fold cross validation and get an estimate of the test MSE for each $\lambda$ in your grid. Which $\lambda$ would you choose and why?
  
## Principal Components Regression
# 1. Fit the PCR model using 10-fold cross validation for values of $M$. Be sure to normalize your predictors.
# 2. Create a plot of the CV MSE vs. $M$.
# 3. When does the smallest cross-validation error occur? Which $M$ would you choose for your final model?
# 4. How much variability in $Y$ is explained for your chosen value of $M$?

## Partial Least Squares
# 1. Fit the PLS model using 10-fold cross validation for values of $M$. Be sure to normalize your predictors.
# 2. Create a plot of the CV MSE vs. $M$.
# 3. When does the smallest cross-validation error occur? Which $M$ would you choose for your final model?
# 4. How much variability in $Y$ is explained for your chosen value of $M$?
# 5. Discuss the difference between PCR and PLS results. Which would you prefer?
