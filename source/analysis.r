installed.packages("dplyr")
installed.packages("tidyverse")
installed.packages("ggplot2")
install.packages("maps")
install.packages("leaflet")
install.packages("mapproj")
install.packages("mapdata")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("maps")
library("leaflet")
library("mapproj")
library("mapdata")

incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(incarceration_trends)

incarceration_trends_jail_jurisdiction <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")
View(incarceration_trends_jail_jurisdiction)

#Trends over time code

Black_pop <- incarceration_trends[, "black_jail_pop"]
na.omit(Black_pop)




Black_pop_WA <- incarceration_trends %>% 
  select(state, black_jail_pop, year) %>% 
  filter(state == "WA") %>% 
  filter(1990 <= year & year <= 2000) %>% 
na.omit(Black_pop_WA)
Black_pop_WA

total_black_pop <- sum(incarceration_trends$black_jail_pop, na.rm = TRUE)
total_black_pop

#Chart of trends over time
chart_over_time <- ggplot(data = incarceration_trends) +
  geom_point(mapping = aes(x = year, y = Black_pop, color = "WA"))
chart_over_time


#Chart comparing two variables 

white_incarceration <- incarceration_trends[, "white_jail_pop"]
na.omit(white_incarceration)

latinx_incarceration <- incarceration_trends[, "latinx_jail_pop"]
na.omit(latinx_incarceration)


latinx_white_chart <- ggplot(data = incarceration_trends) +
  geom_line(mapping = aes(x = year, y = latinx_incarceration, color = "latinx_incarceration")) + 
  geom_line(mapping = aes(x = year, y = white_incarceration, color = "white_incarceration")) +
  labs(
    x = "year",
    y = "Number Incarcerated"
  )
latinx_white_chart

#total female incarceration
total_female_pop <- sum(incarceration_trends$female_jail_pop, na.rm = TRUE)
total_female_pop

#total latinx incarceration 
total_latinx_pop <- sum(incarceration_trends$latinx_jail_pop, na.rm = TRUE)
total_latinx_pop


  
#map that shows measure of interest 

Male_incarceration <- incarceration_trends[, "male_jail_pop"]
na.omit(Male_incarceration)

Male_states <- incarceration_trends %>% 
  select(state, male_jail_pop, year) %>% 
  na.omit(Male_states)
Male_states

highest_year_Male <- incarceration_trends %>% 
  group_by(year) %>% 
  summarize(male_pop = sum(male_jail_pop, na.rm = TRUE)) %>%
  filter(male_pop == max(male_pop)) %>% 
  pull(year)
highest_year_Male


incarcerated_2003 <- incarceration_trends %>% 
  filter(year == 2003) %>% 
  select(male_jail_pop)
incarcerated_2003




map_male_pop <- incarceration_trends %>%
  filter(year == 2003) %>% 
  mutate(state = tolower(state)) 


highest_state_Male <- incarceration_trends %>% 
  group_by(state) %>% 
  summarize(male_pop = sum(male_jail_pop, na.rm = TRUE)) %>%
  filter(male_pop == max(male_pop)) %>% 
  pull(state)
highest_state_Male




blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),       
    axis.text = element_blank(),        
    axis.ticks = element_blank(),     
    axis.title = element_blank(),       
    plot.background = element_blank(),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank()      
  )

state_shape <- map_data("state") %>% 
  rename(state = area) %>% 
  left_join(map_male_pop, by="state") 

max_male <- max(incarceration_trends$male_jail_pop, na.rm = TRUE)

median_male <- median(incarceration_trends$male_jail_pop, na.rm = TRUE)

min_male <- min(incarceration_trends$male_jail_pop, na.rm = TRUE)


male_map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = median_male),
    color = "white",
    size = .2       
  ) +
  coord_map() + 
  scale_fill_continuous(low = "blue", high = "Red") +
  labs(fill = "male population") +
  blank_theme

male_map










  


