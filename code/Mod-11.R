


rm(list=ls(all=TRUE))

# install.packages("tidymodels")
# install.packages("corrr")
library(tidyverse)
library(broom)
library(modelr)
library(corrr)

gdp <- read_csv("data/gdp-2017.csv")
meat <- read_csv("data/meat-production-2017.csv")
pop <- read_csv("data/total-population-2017.csv")

meats <- meat %>% 
  group_by(country)%>%
  summarize(meat_produced = sum(meat_produced, na.rm = T))

all <- full_join(meats, gdp)%>%
  full_join(pop)%>%
  drop_na()

ggplot()+
  geom_point(data = all, aes(gdp, meat_produced), alpha = .5)

ggplot()+
  geom_point(data = all, aes(total_population, meat_produced), alpha = .5)


# User-defined function for normalizing
normalize <- function(x) {
  n <- (x - min(x)) / (max(x) - min(x))
  return(n)
}

# Normalize the meat_produced, gdp, and total_population columns
all %>%
  mutate(meat_produced = normalize(meat_produced),
         gdp = normalize(gdp),
         total_population = normalize(total_population)) -> all_norm

# Check that each column in the new dataframe ranges from 0-1
summary(all_norm)

# Re-plot the meat_produced vs. gdp figure
ggplot(data = all_norm) +
  geom_point(mapping = aes(x = gdp, y = meat_produced), alpha = 0.5) +
  theme_bw()

ggplot(data = all_norm) +
  geom_point(mapping = aes(x = gdp, y = meat_produced), alpha = 0.5) +
  lims(x = c(0,0.05),
       y = c(0, 0.15)) +
  theme_bw()

ggplot(data = all_norm) +
  geom_point(mapping = aes(x = total_population, y = meat_produced), alpha = 0.5) +
  theme_bw()

all_norm %>%
  dplyr::select(-country) %>%
  correlate() %>%
  fashion()

all_norm %>%
  dplyr::select(-country) %>%
  correlate() %>%
  rplot()
