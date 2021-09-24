## Stock Market Data ----
# 1. Load and explore (through numerical and graphical summaries) the `Smarket` data (this is in `ISLR` package). 

library(tidyverse)
library(GGally)
library(ISLR)
library(class)
library(MASS)
set.seed(445)

head(Smarket)

ggpairs(Smarket)

## There are no clear relationships with Direction and the other predictors except Today, which is used to
## create the direction variable, so is not viable for us to use in a model. There does look to be a relationship
## between Year and Volume, indicating that there hahve been more trades in recent years. It also looks
## like more recent years have a higher prevelance of Up days.

## Logistic Regression ----
# 1. Fit a logistic regression model to predict `Direction` using `Lag1` through `Lag5` and `Volume`. Describe your results.
m0 <- glm(Direction ~ Volume + Lag1 + Lag2 + Lag3 + Lag4 + Lag5, family = "binomial", data = Smarket)
summary(m0)

## none of the predictors appear to have a significant relationship with Direction. The closest one would be
## Lag1, but this is still not significant at the .05 level.

# 2. Use the `predict` and the `table` function to create a confusion matrix for the training data.
(conf_train <- table(pred = predict(m0, type = "response") > 0.5, true = Smarket$Direction))

# 3. What is the overall error rate of the model?
(conf_train[1, 2] + conf_train[2, 1])/sum(conf_train)

## this is just barely better than flipping a coin.

# 4. Create two data sets, `train` and `test` that correspond to the observations from 2001 to 2004 (`train`) and 2005 (`test`).
train <- Smarket %>% filter(Year <= 2004)
test <- Smarket %>% filter(Year == 2005)

# 5. Repeat 1-3, but obtain the test confusion matrix and error rate.
m1 <- glm(Direction ~ Volume + Lag1 + Lag2 + Lag3 + Lag4 + Lag5, family = "binomial", data = train)
summary(m1)

## similar results, but now Lag1 is even less significant.

(conf_test <- table(pred = predict(m1, newdata = test, type = "response") > 0.5, true = test$Direction))

# 3. What is the overall error rate of the model?
(conf_test[1, 2] + conf_test[2, 1])/sum(conf_test)

## Now our error rate is worse than coin flipping.

# 6. Repeat 5, but with a model of `Direction` on `Lag1` and `Lag2` only.
m_lr <- glm(Direction ~ Lag1 + Lag2, family = "binomial", data = train)
summary(m_lr)

## similar results.

(conf_lr <- table(pred = predict(m_lr, newdata = test, type = "response") > 0.5, true = test$Direction))

# 3. What is the overall error rate of the model?
(conf_lr[1, 2] + conf_lr[2, 1])/sum(conf_lr)

## Slightly better predictions with the less flexible model, but not great.

## LDA ----
# 1. Fit a linear discriminant analysis model (see the `MASS` library) to the `train` data set you created in the previous section with `Direction` as the response and `Lag1` and `Lag2` as the predictors.
m_lda <- lda(Direction ~ Lag1 + Lag2, data = train)
m_lda

# 2. What are the values for $\hat{\pi}_1$ and $\hat{\pi}_2$?
m_lda$prior

# 3.  Use the `predict` and the `table` function to create a confusion matrix for the `test` data.
(conf_lda <- table(pred = predict(m_lda, newdata = test)$class, true = test$Direction))

# 4. What is the test error rate?
(conf_lda[1, 2] + conf_lda[2, 1])/sum(conf_lda)

## exactly the same as the logistic regression model, which makes sense because they both have a
## linear decision boundary.

## QDA ----
# 1. Fit a quadratic discriminant analysis model to the `train` data set you created in the previous section with `Direction` as the response and `Lag1` and `Lag2` as the predictors.
m_qda <- qda(Direction ~ Lag1 + Lag2, data = train)
m_qda

# 2.  Use the `predict` and the `table` function to create a confusion matrix for the `test` data.
(conf_qda <- table(pred = predict(m_qda, newdata = test)$class, true = test$Direction))

# 3. What is the test error rate?
(conf_qda[1, 2] + conf_qda[2, 1])/sum(conf_qda)

## this is a little better with the more flexible boundary

## KNN ----
# 1. Fit a KNN model with $K = 1$ (see the `class` library) to the `train` data set you created in the previous section with `Direction` as the response and `Lag1` and `Lag2` as the predictors.
m_knn_1 <- knn(train[, c("Lag1", "Lag2")], test[, c("Lag1", "Lag2")], train$Direction, k = 1)

# 2. Use `table` function to create a confusion matrix for the `test` data.
(conf_knn_1 <- table(pred = m_knn_1, true = test$Direction))

# 3. What is the test error rate?
(conf_knn_1[1, 2] + conf_knn_1[2, 1])/sum(conf_knn_1)

## The same as flipping a coin

# 4. Repeat 1.-3. with $K = 3$ and $K = 5$.
m_knn_3 <- knn(train[, c("Lag1", "Lag2")], test[, c("Lag1", "Lag2")], train$Direction, k = 3)
(conf_knn_3 <- table(pred = m_knn_3, true = test$Direction))
(conf_knn_3[1, 2] + conf_knn_3[2, 1])/sum(conf_knn_3)

m_knn_5 <- knn(train[, c("Lag1", "Lag2")], test[, c("Lag1", "Lag2")], train$Direction, k = 5)
(conf_knn_5 <- table(pred = m_knn_5, true = test$Direction))
(conf_knn_5[1, 2] + conf_knn_5[2, 1])/sum(conf_knn_5)


#Of all the models you fit today, which would you pick to predict values of `Direction` and why?

## QDA appears to give the best test MSE, indicating a good mix of flexibility.




