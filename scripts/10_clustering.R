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

## $K$-means Clustering
# 1. Perform $K$-means clustering with $K = 2$.
# 2. Create a scatterplot of your data, colored by the resulting clustering. Describe what you see.
# 3. Repeat 1-2 with $K = 3$. 
# 4. The total within sum of squares is available in the `kmeans` object under the name `tot.withinss`. Compare your two clusterings from 1. and 3. Which should you choose?
  
  
## Hierarchical Clustering
# 1. Use the `dist` function to create a dissimilarity matrix corresponding to euclidean distance for the data you have simulated.
# 2. Create and plot the dendrograms for complete, single, and average linkage using the `hclust` function.
# 3. Cut each dendrogram to result in 2 clusters using the `cutree` function.
# 4. Create 4 scatterplots of your data, colored by the resulting clusterings from 3. Describe what you see.
# 5. Repeat 1-4. after scaling your data using `scale`. Are there any changes?


