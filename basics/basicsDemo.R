#Author: Hassan Anees

#loading libraries 
library(tidyverse) #has a bunch of useful packages
library(gapminder) #just a dataset from online


# Overview of gapminder ---------------------------------------------------
#allows us to load specific data sets
data("gapminder")

#this gives us our variables of interest
glimpse(gapminder)

#variables of interest and their statistics
summary(gapminder)

view(gapminder)

# Pipe and filtering ------------------------------------------------------
#lets us get the country china from the data set that is 
#above year 2000 and extract the life expectancy of it

gapminder

gapminder %>% 
  filter(country == "China", year >= 2000) %>% 
  select(lifeExp)

#select helps you pick which variables you want to see
#filter further narrows what you want by filtering from those options
#groupby lets you group a set of entries
#summarise is used to reduce multiple values to a single one
gapminder %>% 
  select(country, lifeExp, gdpPercap) %>% 
  filter(lifeExp >= 50) %>% 
  group_by(country) %>% 
  summarise(newGdp = mean(gdpPercap))


#creating a new dataset
newSet <- gapminder %>% 
  select(country, continent, lifeExp, gdpPercap) %>% 
  filter(continent == "Europe") %>% 
  group_by(country) %>% 
  summarise(newGdp = mean(gdpPercap))

view(newSet)

mean(newSet$newGdp)



