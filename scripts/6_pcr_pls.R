library(ISLR)
library(tidyverse) 
library(knitr)
library(pls) # package for pcr and pls

# data
str(Hitters)

## Reproducibility
set.seed(445)

## Data Processing
# 1. Remove records with missing values from the data (Hint: `complete.cases()` is useful)

## Principal Components Regression
# 1. Fit the PCR model using the `pcr` command. A couple tips: a) setting `scale = TRUE` will standardize your data prior to fitting the model, and b) setting `validation = TRUE` will perform 10-fold cross validation for each value of $M$.
# 2. Create a plot of the CV MSE (note root MSE is reported) vs. $M$.
# 3. When does the smallest cross-validation error occur? Which $M$ would you choose for your final model?
# 4. The `summary` function also provides the *percentage of variance explained* in the predictors and the response using $M$ principal components. How many principal components would we need to explain at least 80% of the variability in the predictors? 
# 5. How much variability in $Y$ is explained for your chosen value of $M$?

## Partial Least Squares
# 1. Fit the PLS model using the `pls` command. Again, a) setting `scale = TRUE` will standardize your data prior to fitting the model, and b) setting `validation = TRUE` will perform 10-fold cross validation for each value of $M$.
# 2. Create a plot of the CV MSE (note root MSE is reported) vs. $M$.
# 3. When does the smallest cross-validation error occur? Which $M$ would you choose for your final model?
# 4. How much variability in $Y$ is explained for your chosen value of $M$?
# 5. Discuss the two methods performed today. Which would you prefer?

  
