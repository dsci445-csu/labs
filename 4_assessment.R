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
# 2. Fit a linear model of `mpg` on `horsepower` using your training data.
# 3. Estimate the test error by using test MSE (hint: see `predict()`).
# 4. Repeat steps 2-3 for a cubic and quadratic model. Which model would you pick?
# 5. Repeat steps 1-4 after reseting the seed
set.seed(42)
# 6. Did you get the same results? Is this what you expected to happen?
  
## LOOCV
# 1. Fit the linear model of `mpg` on `horsepower` using your training data from the previous section to check that you get the same coefficients.
# 2. Get the estimate of CV using the `cv.glm()` function. (Hint: check `?cv.glm` to understand the values returned from this function.)
# 3. Repeat steps 2-3 for a cubic and quadratic model. Which model would you pick?

## k-Fold CV
# 1. Using $k = 10$-fold CV, compute the $k$-fold CV estimate of the test MSE for polynomial models of order $i = 1, \dots, 10$. (Hint: you can use the `poly` function in your formula to specify a polynomial model.)
# 2. Plot the estimated test MSE vs. the polynomial order.
# 3. Which of these models would you choose?

## Bonus
# 1. Write your own $k$-fold CV function that will calculate CV for the $KNN$ Regression model. You function should take as parameters
#    - CV $k$ value
#    - KNN $K$ value
#    - Data
#    - A vector of names (character) of predictor columns
#    - A character string of the response column
# And return the estimated test MSE.
# 2. Use your function to estimate the test MSE using 10-fold CV for KNN models with $K = 1, 5, 10, 20, 100$ of a model predicting `mpg` using the `horsepower` predictor variable in the `Auto` data set.
# 3. Compare your results to the previous $k$-Fold CV method.

  
  
