## Libraries
library(ISLR) ## data
library(tidyverse) ## data manipulation & plots
library(boot) ## cv.glm function

## Data
head(Auto) %>%
  kable()

## Reproducibility
set.seed(445)

## Validation Set Approach
# 1. Split the data into 50% training and 50% test data.
n <- nrow(Auto)
train <- seq_len(n) %in% sample(seq_len(n), round(n/2))

# 2. Fit a linear model of `mpg` on `horsepower` using your training data.
m_linear <- lm(mpg ~ horsepower, data = Auto[train,])

# 3. Estimate the test error by using test MSE (hint: see `predict()`).
mean((predict(m_linear, newdata = Auto[!train,]) - Auto[!train, "mpg"])^2)

# 4. Repeat steps 2-3 for a cubic and quadratic model. Which model would you pick?
m_quad <- lm(mpg ~ poly(horsepower, 2), data = Auto[train,])
m_cubic <- lm(mpg ~ poly(horsepower, 3), data = Auto[train,])

mean((predict(m_quad, newdata = Auto[!train,]) - Auto[!train, "mpg"])^2)
mean((predict(m_cubic, newdata = Auto[!train,]) - Auto[!train, "mpg"])^2)

## based on these results I would pick the quadratic model because it has the
## lowest estimated test MSE

# 5. Repeat steps 1-4 after reseting the seed
set.seed(42)

train <- seq_len(n) %in% sample(seq_len(n), round(n/2))
m_linear <- lm(mpg ~ horsepower, data = Auto[train,])
m_quad <- lm(mpg ~ poly(horsepower, 2), data = Auto[train,])
m_cubic <- lm(mpg ~ poly(horsepower, 3), data = Auto[train,])

mean((predict(m_linear, newdata = Auto[!train,]) - Auto[!train, "mpg"])^2)
mean((predict(m_quad, newdata = Auto[!train,]) - Auto[!train, "mpg"])^2)
mean((predict(m_cubic, newdata = Auto[!train,]) - Auto[!train, "mpg"])^2)

## I would still pick the cubic model based on these results.

# 6. Did you get the same results? Is this what you expected to happen?

## I got the same choice of model, but the estimate for test MSE is different after changing this seed.
## this is what I would expect.



## LOOCV
# 1. Fit the linear model of `mpg` on `horsepower` using your training data from the previous section to check that you get the same coefficients.
glm_linear <- glm(mpg ~ horsepower, data = Auto[train,])
summary(glm_linear)
summary(m_linear)

## these are the same coefficients.

# 2. Get the estimate of CV using the `cv.glm()` function. (Hint: check `?cv.glm` to understand the values returned from this function.)
glm_linear <- glm(mpg ~ horsepower, data = Auto)
cv.glm(Auto, glm_linear)$delta[1]

# 3. Repeat steps 2-3 for a cubic and quadratic model. Which model would you pick?
glm_quad <- glm(mpg ~ poly(horsepower, 2), data = Auto)
glm_cubic <- glm(mpg ~ poly(horsepower, 3), data = Auto)

cv.glm(Auto, glm_quad)$delta[1]
cv.glm(Auto, glm_cubic)$delta[1]

## Again I would choose the quadratic model based on the LOOCV estimate of the test MSE

## k-Fold CV
# 1. Using $k = 10$-fold CV, compute the $k$-fold CV estimate of the test MSE for polynomial models of order $i = 1, \dots, 10$. (Hint: you can use the `poly` function in your formula to specify a polynomial model.)
test_mse <- data.frame(p = seq_len(10) + 1, CV_k = numeric(10))
for(i in seq_len(10)) {
  # fit the model
  glm_fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  
  # get the test MSE estimate
  test_mse[i, 2] <- cv.glm(Auto, glm_fit, K = 10)$delta[1]
}

# 2. Plot the estimated test MSE vs. the polynomial order.
ggplot(test_mse) +
  geom_point(aes(p, CV_k)) +
  geom_line(aes(p, CV_k))

# 3. Which of these models would you choose?
test_mse[which.min(test_mse$CV_k), ]

## I would choose the model with 8 terms (polynomial of order 7)

## Bonus
# 1. Write your own $k$-fold CV function that will calculate CV for the $KNN$ Regression model. You function should take as parameters
#    - CV $k$ value
#    - KNN $K$ value
#    - Datat
#    - A vector of names (character) of predictor columns
#    - A character string of the response column
# And return the estimated test MSE.
library(caret) # knn function

k_fold_cv_knn_reg <- function(k = nrow(data), K = 1, data, predictors, response) {
  n <- nrow(data) # number of observations
  
  ## split the data into folds
  folds <- sample(seq_len(k), n, replace = k != n) # use replacement if not LOOCV
  
  ## storage for MSE_i
  mse <- rep(NA, k)
  
  ## model to fit
  model_formula <- as.formula(paste(response, "~", paste(predictors, collapse = "+")))
  
  ## for each fold
  for(i in seq_len(k)) {
    ## fit KNN model on k - 1 training folds
    knn_model <- knnreg(model_formula, data = data[folds != i, ], k = K) # fit knn model
    
    ## validate on left out fold
    mse[i] <- mean((predict(knn_model, data[folds == i, ]) - data[folds == i, response])^2)
  }
  
  ## return mean of test mse for each fold
  mean(mse)
}

# 2. Use your function to estimate the test MSE using 10-fold CV for KNN models with $K = 1, 5, 10, 20, 100$ of a model predicting `mpg` using the `horsepower` predictor variable in the `Auto` data set.
CV_k <- data.frame(K = c(1, 5, 10, 20, 100),
                   mse = rep(NA, 5)) ## storage

for(k in seq_len(nrow(CV_k))) {
  CV_k[k, 2] <- k_fold_cv_knn_reg(10, K = k, data = Auto, predictors = "horsepower", response = "mpg")
}
CV_k

# 3. Compare your results to the previous $k$-Fold CV method.

## based on the above I would choose the model with neighbor size of 20. We know as K gets 
## larger there is more of a linear decision boundary. This is at odds with our polynomial regression
## results, which pointed to needing a highly flexible model. However, whether KNN or polyomial regression,
## it appears we achieve similar estimated test MSE.