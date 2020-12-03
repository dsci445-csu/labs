library(tidyverse) ## data manipulation
library(knitr) ## tables

## reproducible
set.seed(445)

## Data Preparation
# Run the following code to create two datasets.

n1 <- 20
n2 <- 200
p <- 2

## training data sets
x_small <- matrix(rnorm(n1 * p), ncol = p)
x_large <- matrix(rnorm(n2 * p), ncol = p)
y_small <- c(rep(-1, n1/2), rep(1, n1/2))
y_large <- c(rep(1, n2/4*3), rep(2, n2/4))

## shift data farther apart
x_small[y_small == 1,] <- x_small[y_small == 1,] + 1
x_large[1:100,] <- x_large[1:100,] + 2
x_large[101:150,] <- x_large[101:150,] - 2

## put data into dataframes
df_small <- data.frame(x_small, y = as.factor(y_small))
df_large <- data.frame(x_large, y = as.factor(y_large))

# 1. Make two scatterplots to inspect the small and large training data sets. Describe what you see.
ggplot() +
  geom_point(aes(X1, X2, colour = y), data = df_small)

## linear boundary could work reasonably well with this data.

ggplot() +
  geom_point(aes(X1, X2, colour = y), data = df_large)

## linear boundary will work very poorly. Either polynomial or radial basis will 
## be necessary to separate the classes.

## Support Vector Classifier
library(e1071) ## svm library
# 1. Use the `svm` function to fit a support vector classifier on the small data with $C = 10$ (use `scale = FALSE` to disallow rescaling of your data.
svm_linear_fit <- svm(y ~ ., data = df_small, cost = 10, scale = FALSE, kernel = "linear")

# 2. Inspect your model using `summary()`. How many support vectors were used to fit your classifier?
summary(svm_linear_fit)

## 9 support vectors

# 3. Predict (`predict()`) a grid of $\boldsymbol X$ values between the range of $X_1$ and $X_2$. Plot these predictions using `geom_tile()` to visualize the decision boundary and add a scatteplot of training data on top, colored by training label. Describe what you see.
grid <- expand.grid(X1 = seq(min(df_small$X1), max(df_small$X1), length.out = 500),
                    X2 = seq(min(df_small$X2), max(df_small$X2), length.out = 500))

pred_svm_linear_fit <- predict(svm_linear_fit, grid, type = "class")

ggplot() +
  geom_tile(aes(X1, X2, fill = pred_svm_linear_fit), data = grid, alpha = 0.5) +
  geom_point(aes(X1, X2, colour = y), data = df_small)

## The linear decision boundary looks decent, we only misclassify 3 points in the 
## training data set.

# 4. Alter your plot from 2 to change the shape of the support vectors.
sv_idx <- seq_len(nrow(df_small)) %in% svm_linear_fit$index

ggplot() +
  geom_tile(aes(X1, X2, fill = pred_svm_linear_fit), data = grid, alpha = 0.5) +
  geom_point(aes(X1, X2, colour = y, shape = sv_idx), data = df_small)


# 5. Use the `tune()` function to perform CV on the cost parameter. Which value of $C$ would you choose?
svm_linear_cv <- tune(svm, y ~ ., data = df_small, scale = FALSE, kernel = "linear", 
                      ranges = list(cost = seq(0.1, 10, by = 0.1)))

svm_linear_cv$performances %>%
  gather(metric, value, -cost) %>%
  ggplot() +
  geom_line(aes(cost, value, colour = metric))

## C based on error:
best_C <- as.numeric(svm_linear_cv$best.parameters)

# 6. Repeat 3. and 4. using your chosen $C$ value. Describe what you see.
svm_linear_fit_cv <- svm_linear_cv$best.model
summary(svm_linear_fit_cv)

pred_svm_linear_fit_cv <- predict(svm_linear_fit_cv, grid, type = "class")
sv_idx <- seq_len(nrow(df_small)) %in% svm_linear_fit_cv$index

ggplot() +
  geom_tile(aes(X1, X2, fill = pred_svm_linear_fit_cv), data = grid, alpha = 0.5) +
  geom_point(aes(X1, X2, colour = y, shape = sv_idx), data = df_small)
                                                                                              
## Support Vector Machines
# 1. Split the large data frame into 50% training and 50% test.
train <- sample(seq_len(n2), round(n2/2))
test <- -train

# 2. Fit a linear SVM, radial SVM with $\gamma = 1$, and polynomial SVM with $d = 3$ using `tune()` to choose the appropriate cost for each model.
svm_linear_cv <- tune(svm, y ~ ., data = df_large[train,], scale = FALSE, kernel = "linear", ranges = list(cost = seq(0.1, 10, by = 0.1)))
svm_polynomial_cv <- tune(svm, y ~ ., data = df_large[train,], scale = FALSE, degree = 3, kernel = "polynomial", ranges = list(cost = seq(0.1, 10, by = 0.1)))
svm_radial_cv <- tune(svm, y ~ ., data = df_large[train,], scale = FALSE, gamma = 1, kernel = "radial", ranges = list(cost = seq(0.1, 10, by = 0.1)))

svm_linear <- svm_linear_cv$best.model
svm_poly <- svm_polynomial_cv$best.model
svm_radial <- svm_radial_cv$best.model

# 3. Predict (`predict()`) a grid of $\boldsymbol X$ values between the range of $X_1$ and $X_2$. Plot these predictions using `geom_tile()` to visualize the decision boundary and add a scatteplot of training data on top, colored by training label. Describe what you see.
pred_grid <- expand.grid(X1 = seq(min(df_large$X1), max(df_large$X1), length.out = 500),
                         X2 = seq(min(df_large$X2), max(df_large$X2), length.out = 500)) %>%
  mutate(linear = predict(svm_linear, ., type = "class"),
         polynomial = predict(svm_poly, ., type = "class"),
         radial = predict(svm_radial, ., type = "class"))

pred_grid %>%
  gather(model, prediction, -X1, -X2) %>%
  ggplot() +
  geom_tile(aes(X1, X2, fill = prediction), alpha = 0.5) +
  geom_point(aes(X1, X2, colour = y), data = df_large[train,]) +
  facet_grid(.~model)

## radial is the only one producing a decent decision boundary

# 4. Predict your test data with your three models. Which model would you choose?
table(pred = predict(svm_linear, df_large[test,], type = "class"), true =  df_large[test,]$y)
table(pred = predict(svm_poly, df_large[test,], type = "class"), true =  df_large[test,]$y)
table(pred = predict(svm_radial, df_large[test,], type = "class"), true =  df_large[test,]$y)

# I would choose the radial basis function because it has the best test error rate

