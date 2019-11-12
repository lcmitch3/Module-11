#Lucas Mitchell
#11/12/2019

# clear workspace
rm(list=ls(all=TRUE))

# install packages and libraies needed
# install.packages("tidymodels")
# install.packages("corrr")
# install.packages("rnaturalearth")
# install.packages("sf")
# install.packages("rnaturalearthdata")

library(tidyverse)
library(broom)
library(modelr)
library(corrr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# read in data
gdp <- read_csv("data/gdp-2017.csv")
meat <- read_csv("data/meat-production-2017.csv")
pop <- read_csv("data/total-population-2017.csv")

# organize and join all data into one table with meat_produced, gdp, and total_population for each country
meats <- meat %>% 
  group_by(country)%>%
  summarize(meat_produced = sum(meat_produced, na.rm = T))

all <- full_join(meats, gdp)%>%
  full_join(pop)%>%
  drop_na()

# create plots to visualize data
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

# Re-plot the meat_produced vs. gdp figure with adjust x an y scales
ggplot(data = all_norm) +
  geom_point(mapping = aes(x = gdp, y = meat_produced), alpha = 0.5) +
  lims(x = c(0,0.05),
       y = c(0, 0.15)) +
  theme_bw()

ggplot(data = all_norm) +
  geom_point(mapping = aes(x = total_population, y = meat_produced), alpha = 0.5) +
  theme_bw()

#normalize the data
all_norm %>%
  dplyr::select(-country) %>%
  correlate() %>%
  fashion()

all_norm %>%
  dplyr::select(-country) %>%
  correlate() %>%
  rplot()

# meat as a function of gdp
meat_gdp <- lm(meat_produced ~ gdp, data = all_norm)
summary(meat_gdp)

# meat produced as a function of total polulation
meat_pop <- lm(meat_produced ~ total_population, data = all_norm)
summary(meat_pop)

#meat produced as a function of gdp and total population independently
meat_all_ind <- lm(meat_produced ~ gdp + total_population, data = all_norm)
summary(meat_all_ind)

#meat produced as a funtion of the interaction between gdp and total population
meat_all_int <- lm(meat_produced ~ gdp * total_population, data = all_norm)
summary(meat_all_int)

# log of meat produced as a funvtion of the log of gdp
log_gdp <- lm(log(meat_produced + 0.0001) ~ log(gdp + 0.0001), data = all_norm)
summary(log_gdp)

# log of meat produced as a funvtion of the log of total population
log_pop <- lm(log(meat_produced + 0.0001) ~ log(total_population + 0.0001), data = all_norm)
summary(log_pop)

# add the new fitted values to the data through augement
all_norm <- augment(data = all_norm, meat_gdp)

# plot modeled meat produced vs. observed meat produced
all_norm%>% 
  ggplot()+
  geom_point(mapping = aes(.fitted, meat_produced), alpha = .5)+
  geom_abline(intercept = 0, slope = 1, colour = "red")+
  labs(x = "Meat produced - modeled",
       y = "Meat produced - observed")+
  lims(x = c(0,0.15),
       y = c(0, 0.15))

# plot the modeled meat produced vs. Residuals
all_norm%>% 
  ggplot()+
  geom_ref_line(h = 0)+
  geom_point(mapping = aes(meat_produced, .resid), alpha = .5)+
  labs(x = "Meat produced",
       y = "Residuals")+
  lims(y = c(-0.2,0.2),
       x = c(0, 0.15))



world <- ne_countries(scale = "medium", returnclass = "sf")
# Merge the world spatial tibble with our assignment data, d. We will merge by country, with the key column in world being admin
all %>% left_join(world, by = c("country" = "admin")) -> d_world
# Check on your own to make sure the data were joined properly

# Create a map where fill varies by meat produced
d_world %>%
  ggplot() +
  geom_sf(aes(fill = meat_produced), color = "black", size = 0.2) +
  scale_fill_viridis_c() +
  theme_minimal()

all$country[all$country == "China, mainland"] <- "China"
all$country[all$country == "China, Hong Kong SAR"] <- "China"
all$country[all$country == "China, Macao SAR"] <- "China"
all$country[all$country == "Russian Federation"] <- "Russia"
all$country[all$country == "Bolivia (Plurinational State of)"] <- "Bolivia"

all %>% left_join(world, by = c("country" = "admin")) -> d_world

d_world %>%
  ggplot() +
  geom_sf(aes(fill = meat_produced), color = "black", size = 0.2) +
  scale_fill_viridis_c() +
  labs(fill = NULL,
       title = "Meat produced in 2017 (tons)",
       caption = "Data: UN FAO") +
  theme_minimal()
