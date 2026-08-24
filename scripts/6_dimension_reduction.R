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
