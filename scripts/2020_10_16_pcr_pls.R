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
df <- Hitters[complete.cases(Hitters),]


## Principal Components Regression
# 1. Fit the PCR model using the `pcr` command. A couple tips: a) setting `scale = TRUE` will standardize your data prior to fitting the model, and b) setting `validation = TRUE` will perform 10-fold cross validation for each value of $M$.
m0 <- pcr(Salary ~ ., data = df, scale = TRUE, validation = "CV")

# 2. Create a plot of the CV MSE (note root MSE is reported) vs. $M$.
mse <- MSEP(m0)
data.frame(M = mse$comps, mse = t(as.data.frame(mse$val))[, "CV"]) %>%
  ggplot() +
  geom_line(aes(M, mse))

# 3. When does the smallest cross-validation error occur? Which $M$ would you choose for your final model
mse$comps[which.min(as.data.frame(mse$val)[1,])]

## the smallest MSE occurs with M = 17 components. This is almost the same as just fitting
## least squares with all 19 components.

## While this is the smallest MSE, there is not much difference between M = 17 and M = 6
## I may prefer to choose M = 6 for this reason.

# 4. The `summary` function also provides the *percentage of variance explained* in the predictors and the response using $M$ principal components. How many principal components would we need to explain at least 80% of the variability in the predictors? 
summary(m0)
## We would need M = 5 to explain at least 80% of the variability in X

# 5. How much variability in $Y$ is explained for your chosen value of $M$?
## 46.48% of variability in Y is explained with M = 6 and 53.85% is explained with M = 17 


## Partial Least Squares
# 1. Fit the PLS model using the `pls` command. Again, a) setting `scale = TRUE` will standardize your data prior to fitting the model, and b) setting `validation = TRUE` will perform 10-fold cross validation for each value of $M$.
m1 <- plsr(Salary ~ ., data = df, scale = TRUE, validation = "CV")

# 2. Create a plot of the CV MSE (note root MSE is reported) vs. $M$.
mse1 <- MSEP(m1)
data.frame(M = mse1$comps, mse = t(as.data.frame(mse1$val))[, "CV"]) %>%
  ggplot() +
  geom_line(aes(M, mse))

# 3. When does the smallest cross-validation error occur? Which $M$ would you choose for your final model?
mse1$comps[which.min(as.data.frame(mse1$val)[1,])]

## the smallest MSE occurs with M = 12 components. 
## While this is the smallest MSE, there is not much difference between M = 12 and M = 8
## I may prefer to choose M = 8 for this reason.

# 4. How much variability in $Y$ is explained for your chosen value of $M$?
summary(m1)
## 53.26% of variability in Y is explained with M = 8 and 54.20% is explained with M = 12 

# 5. Discuss the two methods performed today. Which would you prefer?
## It does seem that the variability in Y is more explained with less components using
## PLS vs PCR, which is to be expected.
data.frame(M = mse1$comps, mse = t(as.data.frame(mse1$val))[, "CV"]) %>%
  ggplot() +
  geom_line(aes(M, mse), colour = "red") +
  geom_line(aes(M, mse), colour = "blue", data.frame(M = mse$comps, mse = t(as.data.frame(mse$val))[, "CV"]))

## However, PCR has lower CV MSE values for our chosen M. This would lead me to preferring
## PCR for this problem over PLS.
  
