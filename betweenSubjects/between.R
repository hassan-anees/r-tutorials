#PART 1 between:

library(tidyverse)

beverage_data <- read_csv("beverage_data.csv")
print(beverage_data)

beverage_data_tidy <- gather(beverage_data,
                             key = "beverage",
                             value = "rating",
                             coffee:tea,
                             factor_key = TRUE)

glimpse(beverage_data_tidy)

coffee_ratings <- beverage_data_tidy %>%
  filter(beverage == "coffee") %>%
  select(rating) %>%
  pull()
tea_ratings <- beverage_data_tidy %>%
  filter(beverage == "tea") %>%
  select(rating) %>%
  pull()

library(psych)
psych::describe(coffee_ratings)
psych::describe(tea_ratings)

library(car)
leveneTest(beverage_data_tidy$rating,
           group = beverage_data_tidy$beverage,
           center = "median")

between.t.test <- t.test(coffee_ratings,tea_ratings,
       var.equal = TRUE,
       alternative = "two.sided")
print(between.t.test)
between.one.sided.p <- 0.02367/2
print(between.one.sided.p)

library(effsize)
between.cohen.d <- cohen.d(formula = rating ~ beverage, data = beverage_data_tidy,
        pooled = TRUE,
        na.rm = TRUE,
        hedges.correction = TRUE,
        noncentral = TRUE)
print(between.cohen.d)

