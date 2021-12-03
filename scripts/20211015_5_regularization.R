library(ISLR)
library(tidyverse) 
library(knitr)
library(glmnet) # package for lasso and ridge regression
library(pls) # package for pcr and pls

# data
str(Hitters)

## Reproducibility
set.seed(445)

## Data Processing
# 1. Remove records with missing values from the data (Hint: `complete.cases()` is useful)
df <- Hitters |> filter(complete.cases(Hitters))

# 2. Use `model.matrix` to create an $X$ matrix for all predictors that contains dummy variables for categorical predictors (for predicting `Salary`). You can specify this as a formula in the `model.matrix` call, e.g. 
#    eg. x <- model.matrix(y ~ ., data)[, -1] # remove the y column
X <- model.matrix(Salary ~ ., data = df)[, -1]

# 3. Create a $Y$ vector of `Salary` information.
y <- df$Salary

## Ridge Regression
# 1. Create a vector of $\lambda$ values from $\lambda = .01$ to  $\lambda = 10^10$ of length $100$.
lambda <- 10^seq(-2, 10, length.out = 100)

# 2. Fit a ridge regression model for each $\lambda$ in your grid.
ridge_fit <- glmnet(X, y, alpha = 0, lambda = lambda)

# 3. Make a line plot of coefficient corresponding to each $\lambda$. You should have an individual line for each variable with coefficient value on the $y$-axis and $\lambda$ on the $x$ axis. What happens to your coefficients as $\lambda$ increases?
ridge_beta <- data.frame(as.matrix(t(coef(ridge_fit))))


## extract the betas and get ready for plotting
ridge_beta |>
  mutate(lambda = ridge_fit$lambda) |>
  pivot_longer(-lambda, names_to = "predictor", values_to = "coef") -> beta_df 

## line plot
beta_df |>
  filter(predictor != "X.Intercept.") %>%
  ggplot() +
  geom_line(aes(lambda, coef, group = predictor, colour = predictor)) +
  scale_x_log10() # log 10 for prettier plot

## as lambda increases you can see the overall magnitudes of the coefficients shrinking towards zero

# 4. Use `cv.glmnet` to perform $10$-fold cross validation and get an estimate of the test MSE for each $\lambda$ in your grid. Which $\lambda$ would you choose and why?
cv_ridge <- cv.glmnet(X, y, alpha = 0, lambda = lambda)

ggplot() +
  geom_line(aes(cv_ridge$lambda, cv_ridge$cvm)) +
  geom_point(aes(cv_ridge$lambda, cv_ridge$cvm)) +
  scale_x_log10()

# the lambda i would choose is 
cv_ridge$lambda.min

## Lasso
# 1. Fit the lasso model for each $\lambda$ in your grid.
lasso_fit <- glmnet(X, y, alpha = 1, lambda = lambda)

# 2. Make a line plot of coefficient corresponding to each $\lambda$. You should have an individual line for each variable with coefficient value on the $y$-axis and $\lambda$ on the $x$ axis. (Hint: `coef` may be a useful function). What happens to your coefficients as $\lambda$ increases?
lasso_beta <- data.frame(t(as.matrix(coef(lasso_fit))))

## extract the betas and get ready for plotting
lasso_beta |>
  mutate(lambda = lasso_fit$lambda) |>
  pivot_longer(-lambda, names_to = "predictor", values_to = "coef") -> beta_df 

## line plot
beta_df |>
  filter(predictor != "X.Intercept.") %>%
  ggplot() +
  geom_line(aes(lambda, coef, group = predictor, colour = predictor)) +
  scale_x_log10() # log 10 for prettier plot

## again as lambda increases the magnitudes of the coefficients go to zero
## but also we can see certain coefficients get set exactly to zero

# 3. Use `cv.glmnet` to perform $10$-fold cross validation and get an estimate of the test MSE for each $\lambda$ in your grid. Which $\lambda$ would you choose and why?
cv_lasso <- cv.glmnet(X, y, alpha = 1, lambda = lambda)

ggplot() +
  geom_line(aes(cv_lasso$lambda, cv_lasso$cvm)) +
  geom_point(aes(cv_lasso$lambda, cv_lasso$cvm)) +
  scale_x_log10()

# the lambda i would choose is 
cv_lasso$lambda.min

# this results in this many coefficients exactly = 0
cv_lasso$nzero[which.min(cv_lasso$cvm)]

## Principal Components Regression
# 1. Fit the PCR model using the `pcr` command. A couple tips: a) setting `scale = TRUE` will standardize your data prior to fitting the model, and b) setting `validation = TRUE` will perform 10-fold cross validation for each value of $M$.
m0 <- pcr(Salary ~ ., data = df, scale = TRUE, validation = "CV")

# 2. Create a plot of the CV MSE (note root MSE is reported) vs. $M$.
mse <- MSEP(m0)
data.frame(M = mse$comps, mse = t(as.data.frame(mse$val))[, "CV"]) %>%
  ggplot() +
  geom_line(aes(M, mse)) +
  geom_point(aes(M, mse))

# 3. When does the smallest cross-validation error occur? Which $M$ would you choose for your final model
mse$comps[which.min(as.data.frame(mse$val)[1,])]

## the smallest MSE occurs with M = 6 components.
## I prefer to choose M = 6 for this reason.

# 4. The `summary` function also provides the *percentage of variance explained* in the predictors and the response using $M$ principal components. How many principal components would we need to explain at least 80% of the variability in the predictors? 
summary(m0)
## We would need M = 5 to explain at least 80% of the variability in X

# 5. How much variability in $Y$ is explained for your chosen value of $M$?
## 46.48% of variability in Y is explained with M = 6 
