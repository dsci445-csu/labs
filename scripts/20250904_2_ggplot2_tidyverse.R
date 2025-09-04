library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

## ggplot2 --------------------------------------------------------------
# 1. Make a scatterplot of `cty` vs. `hwy` mpg using the `mpg` dataset.

ggplot(mpg) +
  geom_point(aes(cty, hwy))

# 2. Describe the relationship that you see.

## Strong, positive, linear relationship. No obvious outliers.

# 3. Map color and shape to type of drive the car is (see `?mpg` for details on the variables.). Do you see any patterns?

ggplot(mpg) +
  geom_point(aes(cty, hwy, colour = drv, shape = drv))

## Front wheel drive cars appear to have higher city and highway mpg than rear or 4wd.

# 4. Alter your plot fro part 3. to make all the points be larger.

ggplot(mpg) +
  geom_point(aes(cty, hwy, colour = drv, shape = drv), size = 2)

## notice the size goes outside the aes because it is not mapping visual elements to data

# 5. Make a histogram of `hwy`, faceted by `drv`.

ggplot(mpg) +
  geom_histogram(aes(hwy), bins = 15) +
  facet_wrap(~drv)

# 6. Make a scatterplot that incorporates color, shape, size, and facets.

ggplot(mpg) +
  geom_point(aes(displ, hwy, color = drv, shape = drv, size = cyl)) +
  facet_wrap(~year)

# 7. BONUS - Color your histograms from 1. by `cyl`. Did this do what you thought it would? (Look at `fill` and `group` as options instead).

ggplot(mpg) +
  geom_histogram(aes(hwy, fill = factor(cyl)), bins = 15, position = "dodge") +
  facet_wrap(~drv)


## readr ---------------------------------------------------------------
# 1. Read the NFL salaries dataset from https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv into `R`. 

nfl <- read_tsv("https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv")

# 2. What is the highest NFL salary in this dataset? Who is the highest paid player?

nfl |>
  filter(Salary == max(Salary))

# 3. Make a histogram and describe the distribution of NFL salaries.

nfl |>
  ggplot() +
  geom_histogram(aes(Salary))

## right skewed distribution with a large single mode a little bit above zero.

## dplyr --------------------------------------------------------------
# Using the NFL salaries from https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv that you loaded into `R` in the previous your turn, perform the following.
# 1. What is the team with the highest paid roster?

nfl |>
  group_by(Team) |>
  summarise(roster_salary = sum(Salary)) |>
  arrange(desc(roster_salary)) |>
  head(1)

# 2. What are the top 5 paid players?

nfl |>
  arrange(desc(Salary)) |>
  head(5)

# 3. What is the highest paid position on average? the lowest? the most variable?


nfl |>
  group_by(Position) |>
  summarise(mean_salary = mean(Salary), 
            sd_salary = sd(Salary)) -> position_salary_stats

## highest avg
position_salary_stats |>
  arrange(desc(mean_salary)) |>
  head(1)

## lowest avg
position_salary_stats |>
  arrange(mean_salary) |>
  head(1)

position_salary_stats |>
  arrange(mean_salary) |>
  filter(!is.na(Position)) |>
  head(1)

## most variable
position_salary_stats |>
  arrange(desc(sd_salary)) |>
  head(1)

## tidyr --------------------------------------------------------------
# 1. Is the NFL salaries from https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv that you loaded into `R` in a previous your turn tidy? Why or why not?

## Yes, each variable has a column and each observation has a row.

# 2. There is a data set in `tidyr` called `world_bank_pop` that contains information about population from the World Bank (https://data.worldbank.org/). Why is this data not tidy? You may want to read more about the data to answer (`?world_bank_pop`).

## The year variable is encoded as the column names, meaning it does not have it's own column => not tidy.
## also, the indicator variables are not each in their own column, but stacked on top


# 3. Use functions in `tidyr` to turn this into a tidy form.

world_bank_pop |>
  pivot_longer(cols = c(-country, -indicator), names_to = "year", values_to = "value") |>
  pivot_wider(names_from = "indicator", values_from = value)

## now each observation has its own row and each variable has its own column.


