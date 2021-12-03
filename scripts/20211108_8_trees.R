library(ISLR) ## data package
library(tidyverse) ## data manipulation
library(knitr) ## tables

## reproducible
set.seed(445)

## data
str(Carseats)

## Data Preparation
# 1. Make a copy of the `Carseats` data frame called `df`.
df <- Carseats

# 2. Create a variable called `high_sales` in `df` that takes the value "high" if `Sales` > 8 and "low" otherwise.
# 3. Convert your `high_sales` column to be a factor.
# 4. Remove the `Sales` column from `df`.
df <- df %>%
  mutate(high_sales = factor(ifelse(Sales > 8, "high", "low"))) %>%
  select(-Sales)

## Decision Trees
library(tree) ## tree package
# 1. Using the `tree` function, fit a large classification tree to predict `high_sales` using every variable in `df`. [**Hint:** The syntax is very similar to `lm`]
tree_fit <- tree(high_sales ~ ., df)

# 2. Inspect your tree using `summary`. How many terminal nodes do you have? Whatt is the training error rate?
summary(tree_fit)

## 27 terminal nodes
## misclassification error rate = .09

# 3. Use the `plot` function to visualize your tree. What is the most important indicator of high sales?
# **Hint:** Adding the following line after you plot the tree will add labels.
# text(tree.fit, pretty = 0)
plot(tree_fit)
text(tree_fit, pretty = 0)

# ShelveLoc and Price  are the most important variables

# 4. Split your observations into a training and a test set with $200$ records each. Estimate the test error rate of your tree. [**Hint:** using `type = "class"` in your `predict` function will get you the actual class predictions.]
n <- nrow(df)
train <- sample(seq_len(n), 200)
test <- -train
trn_df <- df[train,]
tree_fit <- tree(high_sales ~ ., trn_df)

confusion <- table(pred = predict(tree_fit, df[test,], type = "class"), true = df[test,]$high_sales)

## test error rate
(confusion[1, 2] + confusion[2, 1])/sum(confusion)

# 5. Produce a confusion matrix for your test set.
confusion

# 6. Use the `cv.tree` function to perform cross-validation to determine the optimal level of tree complexity. Using `FUN = prune.misclass` indicates that we want to use the classification error rate (instead of deviance) to guide the CV and pruning process. Which $\alpha$ (corresponds to `k` in the output) should we choose?
tree_cv <- cv.tree(tree_fit, FUN = prune.misclass)

# 7. Use the function `prune.misclass` to prune your tree to the chosen complexity.
prune_fit <- prune.misclass(tree_fit, k = tree_cv$k[which.min(tree_cv$dev)])

# 8. Repeat 4-5 using your pruned tree. Which performs better?
confusion_prune <- table(pred = predict(prune_fit, df[test,], type = "class"), true = df[test,]$high_sales)

## test error rate
(confusion_prune[1, 2] + confusion_prune[2, 1])/sum(confusion_prune)

# The pruned tree is very slightly worse.

## Bagging & Random Forests
library(randomForest) # random forests & bagging
# 1. Perform bagging on your training `df` to predict `high_sales`. Specify `importance = TRUE` to also obtain information on the importance of each predictor.
bag_fit <- randomForest(high_sales ~ ., data = trn_df, mtry = ncol(trn_df) - 1, importance = TRUE)

# 2. Make a plot of the importance values for each predictor. What is the predictor with the highes importance?
data.frame(bag_fit$importance )%>%
  mutate(variable = rownames(bag_fit$importance)) %>%
  mutate(variable = factor(variable, levels = variable[order(MeanDecreaseGini)])) %>% ## trick to plot variable by descending Gini
  ggplot() +
  geom_point(aes(MeanDecreaseGini, variable))

## Price, ShelveLoc, and CompPrice are the most important variables

# 3. Estimate the test error rate using your bagged tree model.
confusion_bag <- table(pred = predict(bag_fit, df[test,], type = "class"), true = df[test,]$high_sales)

## test error rate
(confusion_bag[1, 2] + confusion_bag[2, 1])/sum(confusion_bag)

confusion_bag

# 4. Repeat 1-3 using a random forest with $m = \sqrt{p}$.
rf_fit <- randomForest(high_sales ~ ., data = trn_df, mtry = sqrt(ncol(trn_df) - 1), importance = TRUE)

data.frame(rf_fit$importance)%>%
  mutate(variable = rownames(rf_fit$importance)) %>%
  mutate(variable = factor(variable, levels = variable[order(MeanDecreaseGini)])) %>% ## trick to plot variable by descending Gini
  ggplot() +
  geom_point(aes(MeanDecreaseGini, variable))

## Price, ShelveLoc, and CompPrice are still the most important variables
confusion_rf <- table(pred = predict(rf_fit, df[test,], type = "class"), true = df[test,]$high_sales)

## test error rate
(confusion_rf[1, 2] + confusion_rf[2, 1])/sum(confusion_rf)

confusion_rf

## misclassification by true label
c(confusion_rf[2, 1], confusion_rf[1, 2])/colSums(confusion_rf)

## rf does slightly better at predicting the low category at the expense of the high sales category.

# 5. Compare the OOB confusion matrix to your test confusion matrix. [**Hint:** The `confusion` element of the model output is OOB.]
rf_fit$confusion

#This is very similar to the misclassification rate by label in the test data set

## Boosting
library(gbm) ## boosting package
# 1. Fit a boosted tree ensemble to your training `df` predicting `high_sales` with $B = 5,000$ trees, shrinkage parameter of $\lambda = 0.1$, and an interaction depth of $d = 2$. We sure to include `distribution = "bernoulli"` to indicate a classification problem.
## need y to be binary instead of a factor
df <- df %>%
  mutate(high_sales = 2 - as.numeric(high_sales))
boost_fit <- gbm(high_sales ~ ., df[train,], n.trees = 5000, shrinkage = 0.1, interaction.depth = 2, distribution = "bernoulli")

# 2. Estimate the test error rate using your boosted tree model and compare to all previously fit models.
confusion_boost <- table(pred = predict(boost_fit, df[test,], type = "response") > 0.5, true = df[test,]$high_sales)

## test error rate
(confusion_boost[1, 2] + confusion_boost[2, 1])/sum(confusion_boost)

confusion_boost

## misclassification by true label
c(confusion_boost[2, 1], confusion_boost[1, 2])/colSums(confusion_boost)

## the boosted model greatly improved the misclassification rate of high sales data points 
## without reducing accuracy of the low sales points by too much giving an overall better error rate.




