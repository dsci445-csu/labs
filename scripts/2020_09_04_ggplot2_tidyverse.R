## ggplot2 --------------------------------------------------------------
# 1. Make a scatterplot of `cty` vs. `hwy` mpg using the `mpg` dataset.
# 2. Describe the relationship that you see.
# 3. Map color and shape to type of drive the car is (see `?mpg` for details on the variables.). Do you see any patterns?
# 4. Alter your plot fro part 3. to make all the points be larger.
# 5. Make a histogram of `hwy`, faceted by `drv`.
# 6. Make a scatterplot that incorporates color, shape, size, and facets.
# 7. BONUS - Color your histograms from 1. by `cyl`. Did this do what you thought it would? (Look at `fill` and `group` as options instead).

## 1
ggplot(mpg) +
  geom_point(aes(cty, hwy))

## 2
# linear, position, strong, 2 potential outliers

## 3
ggplot(mpg) +
  geom_point(aes(cty, hwy, colour = drv, shape = drv))

## 4
ggplot(mpg) +
  geom_point(aes(cty, hwy, colour = drv, shape = drv), size = 4)

## 5
ggplot(mpg) +
  geom_histogram(aes(hwy)) +
  facet_wrap(.~drv)

## 6
ggplot(mpg) +
  geom_point(aes(cty, hwy, colour = drv, shape = drv, size = cyl)) +
  facet_grid(year ~ class)

## 7
ggplot(mpg) +
  geom_histogram(aes(hwy, fill = as.character(cyl), group = as.character(cyl)), 
                 bins = 30, 
                 position = "dodge") +
  facet_wrap(.~drv)

## readr ---------------------------------------------------------------
# 1. Read the NFL salaries dataset from https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv into `R`. 
# 2. What is the highest NFL salary in this dataset? Who is the highest paid player?
# 3. Make a histogram and describe the distribution of NFL salaries.

library(readr)

## 1
nfl <- read_tsv("https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv")

## 2
nfl[which.max(nfl$Salary), ]
nfl[nfl$Salary == mean(nfl$Salary), ] # alternative

## 3
ggplot(nfl) +
  geom_histogram(aes(Salary))

# right skewed with mode at league minimum (presumably)



## dplyr --------------------------------------------------------------
# Using the NFL salaries from https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv that you loaded into `R` in the previous your turn, perform the following.
# 1. What is the team with the highest paid roster?
# 2. What are the top 5 paid players?
# 3. What is the highest paid position on average? the lowest? the most variable?

library(dplyr)

## 1
nfl %>%
  group_by(Team) %>%
  summarise(salary = sum(Salary)) %>%
  arrange(desc(salary)) %>%
  head(1)

## 2
nfl %>%
  arrange(desc(Salary)) %>%
  head(5)

## 3
nfl %>%
  group_by(Position) %>%
  summarise(avg_sal = mean(Salary), sd_sal = sd(Salary)) -> sal_stats

sal_stats %>%
  arrange(desc(avg_sal)) %>%
  head(1)

sal_stats %>%
  arrange(avg_sal) %>%
  head(2)

sal_stats %>%
  arrange(desc(sd_sal)) %>%
  head(1)



## tidyr --------------------------------------------------------------
# 1. Is the NFL salaries from https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv that you loaded into `R` in a previous your turn tidy? Why or why not?
# 2. There is a data set in `tidyr` called `world_bank_pop` that contains information about population from the World Bank (https://data.worldbank.org/). Why is this data not tidy? You may want to read more about the data to answer (`?world_bank_pop`).
# 3. Use functions in `tidyr` to turn this into a tidy form.

library(tidyr)

## 1

# Yes, this looks tide because each variable has its own column and each observation has its own row.

## 2

# sorry, this is in the dev version.
head(world_bank_pop)

# This is not tidy because it has a new column for each year (observation) 
# and uses the year information as a column name
# Also it has total and growth in the same column, which means not each variable is in a column

## 3
world_bank_pop %>%
  gather(year, value, `2000`:`2017`) %>%
  spread(indicator, value)


