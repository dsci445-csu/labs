library(ISLR)
library(tidyverse) 
library(knitr)
library(splines)
library(gam)

# data
str(Wage)
n <- nrow(Wage)

## Reproducibility
set.seed(445)

## Polynomial Regression and Step Functions
# 1. Fit a degree-4 polynomial regression model predicting `wage` based on `age`. Inspect your model with the `summary` function. [**Hint**: you can use the `poly` function to create your polynomials in the model.]
fit_poly_4 <- lm(wage ~ poly(age, 4), data = Wage)
summary(fit_poly_4)

# 2. One way we can choose the degree of our polynomial is through hypothesis testing. Fit polynomial models from linear to degree-5 of `wage` on `age`. We wish to choose the simplest model which is sufficient to explain the relationship between `wage` and `age`.
#    To do this, we can use the `anova` function on our fitted models. This uses an $F$ statistic to test the null hypothesis that a model $\mathcal{M}_1$ is sufficient to explain the data against a more complex model $\mathcal{M}_2$ (the alternative). Because our models are nested, we can compare all at once sequentially. 
#    We will choose the simplest model that is still significantly different from the less complex model.
#    Use ANOVA (analysis of variance) to choose your polynomial regression model. Which model would you pick?
fit_poly <- list()
d <- 7 # highest degree to check
for(i in seq_len(d)) {
  fit_poly[[i]] <- lm(bquote(wage ~ poly(age, .(i))), data = Wage)
}
do.call(anova, fit_poly)
## I would choose the model with degree 3 (cubic model).

# 3. Choose your degree of polynomial using a cross validation approach. Do the chosen degrees match?
k <- 10
cv_labels <- sample(seq_len(k), n, replace = TRUE)

cv_mse <- rep(NA, d)
for(i in seq_len(d)) {
  cv_mse_i <- rep(NA, k) 
  for(j in seq_len(k)) {
    train <- Wage[cv_labels != j,]
    test <- Wage[cv_labels == j,]
    
    fit <- lm(wage ~ poly(age, i), data = train)
    pred <- predict(fit, test)
    cv_mse_i[j] <- mean((pred - test$wage)^2)
  }
  cv_mse[[i]] <- mean(cv_mse_i)
}

ggplot() +
  geom_line(aes(seq_len(d), cv_mse)) +
  geom_point(aes(seq_len(d), cv_mse))

## based on CV, I would choose the polynomial of degree 6. 

which.min(cv_mse)

## The chosen degrees do not match.

#  4. Fit a step function for `age` predicting `wage` with $4$ cut points. You can use the function `cut` to change your quantitative variable into a categorical one. Let `cut` automatically choose the cut locations based on your data.
fit_step_4 <- lm(wage ~ cut(age, breaks = 5), data = Wage)
summary(fit_step_4)

age_grid <- data.frame(age = seq(min(Wage$age), max(Wage$age), length.out = 1000))

ggplot() +
  geom_point(aes(age, wage), data = Wage, alpha = .2) +
  geom_line(aes(age_grid$age, predict(fit_poly[[3]], age_grid)), colour = "red") +  
  geom_line(aes(age_grid$age, predict(fit_poly[[6]], age_grid)), colour = "blue") +
  geom_line(aes(age_grid$age, predict(fit_step_4, age_grid)), colour = "orange")  


## Regression Splines
# 1. Fit `wage` on `age` using a cubic regression spline with knots at ages $25, 40, 60$.
fit_cubic_spline_1 <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)

# 2. Fit `wage` on `age` using a cubic regression spline with 6 degrees of freedom and knots chosen uniformly on the quantiles of the data (this is how `bs` does it by default).
fit_cubic_spline_6 <- lm(wage ~ bs(age, df = 6), data = Wage)

# 3. Fit `wage` on `age` using a natural cubic regression spline with 6 degrees of freedom and knots chosen uniformly on the quantiles of the data.
fit_nat_cubic_spline_6 <- lm(wage ~ ns(age, df = 6), data = Wage)

# 4. Create a scatter plot of `wage` vs `age` with all three of your fitted splines overlayed as well as your chosen polynomial model (either by anova or CV). Comment on the shapes. [**Hint:** `predict` over a grid of `age` values might be helpful.]
age_grid %>%
  mutate(poly_anova = predict(fit_poly[[3]], age_grid),
         poly_cv = predict(fit_poly[[6]], age_grid),
         step = predict(fit_step_4, age_grid),
         cs_fixed = predict(fit_cubic_spline_1, age_grid),
         cs_uni = predict(fit_cubic_spline_6, age_grid),
         ns_uni = predict(fit_nat_cubic_spline_6, age_grid)) %>%
  gather(model, f_hat, -age) %>%
  ggplot() +
  geom_point(aes(age, wage), data = Wage, alpha = .2) +
  geom_line(aes(age, f_hat, colour = model, group = model))

## The shapes of the splines and chosen polynomials are very similar, but the cubic polynomial is a little less
## flexible. In comparing the natural spline to the regression splines, there is obviously less flexibility
## at the boundaries, which might be good given that there is less data in the boundaries (especially at high ages).
## The piecewise constant function does capture the overall shape, but misses any trends within the bins.

## GAMs
#1. Fit a GAM using natural spline functions of `year` and `age`, treating `education` as a quantitative predictor. You can do this using either `lm` (least squares) or `gam` in the `gam` package (fit using back propagation).
fit_gam <- lm(wage ~ ns(year, df = 4) + ns(age, df = 5) + education, data = Wage)
summary(fit_gam)

## with the gam library
library(gam)
fit_gam2 <- gam(wage ~ ns(year, df = 4) + ns(age, df = 5) + education, data = Wage)
summary(fit_gam2)

