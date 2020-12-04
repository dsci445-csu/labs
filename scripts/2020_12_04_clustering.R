library(tidyverse) ## data manipulation
library(knitr) ## tables

## reproducible
set.seed(445)

## Data Preparation
# Run the following code to create the dataset.
n <- 50
p <- 2
x <- matrix(rnorm(n * p), ncol = p)

## shift the center of one group
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4

# 1. Make a scatterplot to inspect the data. Describe what you see.
x %>%
  data.frame() %>%
  ggplot() +
  geom_point(aes(X1, X2))

## $K$-means Clustering
# 1. Perform $K$-means clustering with $K = 2$.
km2 <- kmeans(x, 2, nstart = 20)

# 2. Create a scatterplot of your data, colored by the resulting clustering. Describe what you see.
x %>%
  data.frame() %>%
  mutate(cluster = as.character(km2$cluster)) %>%
  ggplot() +
  geom_point(aes(X1, X2, colour = cluster))

# 3. Repeat 1-2 with $K = 3$. 
km3 <- kmeans(x, 3, nstart = 20)

x %>%
  data.frame() %>%
  mutate(cluster = as.character(km3$cluster)) %>%
  ggplot() +
  geom_point(aes(X1, X2, colour = cluster))

# 4. The total within sum of squares is available in the `kmeans` object under the name `tot.withinss`. Compare your two clusterings from 1. and 3. Which should you choose?
km2$tot.withinss/km2$betweenss
km3$tot.withinss/km3$betweenss
  
## Hierarchical Clustering
# 1. Use the `dist` function to create a dissimilarity matrix corresponding to euclidean distance for the data you have simulated.
d <- dist(x)

# 2. Create and plot the dendrograms for complete, single, and average linkage using the `hclust` function.
hc.complete <- hclust(d, method = "complete")
hc.single <- hclust(d, method = "single")
hc.average <- hclust(d, method = "average")

plot(hc.complete)
plot(hc.single)
plot(hc.average)

# 3. Cut each dendrogram to result in 2 clusters using the `cutree` function.
hc.complete2 <- cutree(hc.complete, k = 2)
hc.single2 <- cutree(hc.single, k = 2)
hc.average2 <- cutree(hc.average, k = 2)

# 4. Create 3 scatterplots of your data, colored by the resulting clusterings from 3. Describe what you see.
x %>%
  data.frame() %>%
  mutate(cluster = as.character(hc.complete2)) %>%
  ggplot() +
  geom_point(aes(X1, X2, colour = cluster)) +
  ggtitle("Complete Linkage")

x %>%
  data.frame() %>%
  mutate(cluster = as.character(hc.single2)) %>%
  ggplot() +
  geom_point(aes(X1, X2, colour = cluster)) +
  ggtitle("Single Linkage")

x %>%
  data.frame() %>%
  mutate(cluster = as.character(hc.average2)) %>%
  ggplot() +
  geom_point(aes(X1, X2, colour = cluster)) +
  ggtitle("Average Linkage")

# 5. Repeat 1-4. after scaling your data using `scale`. Are there any changes?
x_scale <- scale(x)

# distance matrix
d_scale <- dist(x_scale)

# dendrograms
hc.complete_scale <- hclust(d_scale, method = "complete")
hc.single_scale <- hclust(d_scale, method = "single")
hc.average_scale <- hclust(d_scale, method = "average")

plot(hc.complete_scale)
plot(hc.single_scale)
plot(hc.average_scale)

# cuts
hc.complete_scale2 <- cutree(hc.complete, k = 2)
hc.single_scale2 <- cutree(hc.single, k = 2)
hc.average_scale2 <- cutree(hc.average, k = 2)

# plots
x_scale %>%
  data.frame() %>%
  mutate(cluster = as.character(hc.complete_scale2)) %>%
  ggplot() +
  geom_point(aes(X1, X2, colour = cluster)) +
  ggtitle("Complete Linkage (Scaled)")

x_scale %>%
  data.frame() %>%
  mutate(cluster = as.character(hc.single_scale2)) %>%
  ggplot() +
  geom_point(aes(X1, X2, colour = cluster)) +
  ggtitle("Single Linkage (Scaled)")

x_scale %>%
  data.frame() %>%
  mutate(cluster = as.character(hc.average_scale2)) %>%
  ggplot() +
  geom_point(aes(X1, X2, colour = cluster)) +
  ggtitle("Average Linkage (Scaled)")



