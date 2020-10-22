library(ISLR)
library(tidyverse) 
library(knitr)
library(splines)
library(gam)

# data
str(Wage)

## Reproducibility
set.seed(445)

## Polynomial Regression and Step Functions
# 1. Fit a degree-4 polynomial regression model predicting `wage` based on `age`. Inspect your model with the `summary` function. [**Hint**: you can use the `poly` function to create your polynomials in the model.]
# 2. One way we can choose the degree of our polynomial is through hypothesis testing. Fit polynomial models from linear to degree-5 of `wage` on `age`. We wish to choose the simplest model which is sufficient to explain the relationship between `wage` and `age`.
#    To do this, we can use the `anova` function on our fitted models. This uses an $F$ statistic to test the null hypothesis that a model $\mathcal{M}_1$ is sufficient to explain the data against a more complex model $\mathcal{M}_2$ (the alternative). Because our models are nested, we can compare all at once sequentially. 
#    We will choose the simplest model that is still significantly different from the less complex model.
#    Use ANOVA (analysis of variance) to choose your polynomial regression model. Which model would you pick?
#  
# 3. Choose your degree of polynomial using a cross validation approach. Do the chosen degrees match?
#  4. Fit a step function for `age` predicting `wage` with $4$ cut points. You can use the function `cut` to change your quantitative variable into a categorical one. Let `cut` automatically choose the cut locations based on your data.

## Regression Splines
# 1. Fit `wage` on `age` using a cubic regression spline with knots at ages $25, 40, 60$.
# 2. Fit `wage` on `age` using a cubic regression spline with 6 degrees of freedom and knots chosen uniformly on the quantiles of the data (this is how `bs` does it by default).
# 3. Fit `wage` on `age` using a natural cubic regression spline with 6 degrees of freedom and knots chosen uniformly on the quantiles of the data.
# 4. Create a scatter plot of `wage` vs `age` with all three of your fitted splines overlayed as well as your chosen polynomial model (either by anova or CV). Comment on the shapes. [**Hint:** `predict` over a grid of `age` values might be helpful.]

## GAMs
#1. Fit a GAM using natural spline functions of `year` and `age`, treating `education` as a quantitative predictor. You can do this using either `lm` (least squares) or `gam` in the `gam` package (fit using back propagation).