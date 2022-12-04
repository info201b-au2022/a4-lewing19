library(tidyverse)
library(usmap)
library(dplyr)
library(ggplot2)
library(mapproj)

incarceration_df <- read.csv("~/Documents/info201/assignments/a4-lewing19/source/incarceration_trends.csv")
  setwd("~/Documents/info201/assignments/a4-lewing19/docs")
  source("../source/a4-helpers.R")


## Section 2  ---- 
#----------------------------------------------------------------------------#


# What are the states that have the maximum value of Black and White population in jail?
max_black_jail_pop_location <- 
  incarceration_df%>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>% 
  pull(state)

max_white_jail_pop_location <- 
  incarceration_df%>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>% 
  pull(state)

# What is the maximum value of Black population in jail?
max_black_jail_pop_value <- 
  incarceration_df%>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>% 
  pull(black_jail_pop)

# What is the maximum value of White population in jail?

max_white_jail_pop_value <-
  incarceration_df%>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>% 
  pull(white_jail_pop)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

get_year_jail_pop <- function() {
 view_incarceration_df <- incarceration_df %>% 
  select(year, total_jail_pop) %>% 
  drop_na(total_jail_pop)
return(view_incarceration_df)
}

plot_jail_pop_for_us <- function() {
  jail_chart <- ggplot(get_year_jail_pop()) +
  geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    scale_y_continuous(labels = scales:: comma) %>% 
   labs(x= "Year", y = "Total Number of People in Jail", title = "Growth of the United States Prison Population", caption = "The growth of the U.S. prison population from 1970 to 2018")
  return(jail_chart)
}

plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

get_jail_pop_by_states <- function(states) {
  state_jail_population <- incarceration_df %>%
    group_by(state, year) %>% 
    select(state, year, total_jail_pop) %>% 
    filter(state %in% states) %>% 
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 
  return(state_jail_population)
}

states <- c("WA", "CA", "FL", "NY")

# Line Plot
plot_jail_pop_for_states <- function(states) {
  state_jail_line_chart <- ggplot(get_jail_pop_by_states(states)) +
  geom_line(mapping = aes(x = year, y = total_jail_pop, color = state)) +
  labs(title = "Growth of the United States Prison Population by States", x = "Year", y = "Total Number of People in Jail", caption = "The growth of the U.S. prision population from 1970 to 2018 by states")
  return(state_jail_line_chart)
}

plot_jail_pop_for_states(c("WA", "CA", "FL", "NY"))
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#

black_white_jail_compare <- function() {
  california_jail_compare <- incarceration_df %>% 
  group_by(year) %>% 
  select(state, year, black_jail_pop, white_jail_pop) %>%
  filter(year > 1970) %>% 
  summarise(black_jail_pop = sum(black_jail_pop, na.rm = TRUE), white_jail_pop = sum(white_jail_pop, na.rm = TRUE)) %>% 
  return(california_jail_compare)
}

plot_black_white_jail_compare <- function() {
  california_jail_compare_chart <- ggplot(data = black_white_jail_compare()) +
    geom_line(mapping = aes(x = year, y = black_jail_pop, color = 'Black Population')) +
    geom_line(mapping = aes(x = year, y = white_jail_pop, color = 'White Population')) +
    scale_y_continuous(labels = scales:: comma) +
    labs(title = "Black Population versus White Population in Jail in California", x = "Year", y = "White Jail Population versus Black Jail Population", caption = "The comparison of Black and White Jail Population in California")
  return(california_jail_compare_chart)
}

print(plot_black_white_jail_compare())
## Section 6  ---- 
#----------------------------------------------------------------------------#
library(usmap)
mapping <- function() {
mapping_data <- incarceration_df %>% 
  select(state, white_jail_pop, black_jail_pop) %>% 
  group_by(state) %>% 
  summarize(black_jail_pop = sum(black_jail_pop, na.rm = TRUE)) %>% 
  return(mapping_data)
}

actual_map <- function() {
actual_map <- plot_usmap(data = mapping(), values = "black_jail_pop", color = "black") +
  scale_fill_continuous(low = "yellow", high = "red", name = "Number of Black People Incarcerated", label = scales::comma
  ) + theme(legend.position = "right")
  theme_set(theme_minimal()) +
  labs(title = "Map of Number of Black People Incarcerated in the US", caption = "Visualization showing the states black jail population")
  return(actual_map)
}

print(actual_map())


