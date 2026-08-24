library(ISLR)
library(tidyverse) 
library(tidymodels)
library(knitr)

# data
str(Wage)
Wage <- as_tibble(Wage)

## Reproducibility
set.seed(445)

## Polynomial Regression and Step Functions
# 1. Fit a degree-4 polynomial regression model predicting `wage` based on `age`. Inspect your model and describe the fit. [**Hint**: you can use the `step_poly` function to create your polynomials.]
# 2. Choose your degree of polynomial using a cross validation approach. What degree model would you pick?
# 3. Fit a step function for `age` predicting `wage` with $4$ cut points. You can use the function `step_discretize` to change your quantitative variable into a categorical one. Let `step_discretize` automatically choose the cut locations based on your data.

## Regression Splines
# 1. Fit `wage` on `age` using a cubic regression spline with knots at ages $25, 40, 60$. 
# 2. Fit `wage` on `age` using a cubic regression spline with 6 degrees of freedom and knots chosen uniformly on the quantiles of the data (this is how `step_bs` does it by default).
# 3. Fit `wage` on `age` using a natural cubic regression spline with 6 degrees of freedom and knots chosen uniformly on the quantiles of the data.
# 4. Create a scatter plot of `wage` vs `age` with all three of your fitted splines overlayed as well as your chosen polynomial model (either by anova or CV). Comment on the shapes. [**Hint:** `predict` over a grid of `age` values might be helpful.]

## GAMs
# 1. Fit a GAM using natural spline functions of `year` and `age`, treating `education` as a categorical predictor. 
