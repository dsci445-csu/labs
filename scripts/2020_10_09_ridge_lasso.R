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
df <- Hitters[complete.cases(Hitters),]

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
ridge_beta <- data.frame(as.matrix(coef(ridge_fit)))
names(ridge_beta) <- ridge_fit$lambda

## extract the betas and get ready for plotting
ridge_beta %>%
  mutate(predictor = rownames(ridge_beta)) %>%
  gather(lambda, coef, -predictor) %>%
  mutate(lambda = as.numeric(lambda)) -> beta_df 

## line plot
beta_df %>%
  filter(predictor != "(Intercept)") %>%
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
lasso_beta <- data.frame(as.matrix(coef(lasso_fit)))
names(lasso_beta) <- lasso_fit$lambda

## extract the betas and get ready for plotting
lasso_beta %>%
  mutate(predictor = rownames(lasso_beta)) %>%
  gather(lambda, coef, -predictor) %>%
  mutate(lambda = as.numeric(lambda)) -> beta_df 

## line plot
beta_df %>%
  filter(predictor != "(Intercept)") %>%
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



  