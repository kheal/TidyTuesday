#Load packages
library(tidyverse)
library(tidytuesdayR)
library(cowplot)
library(viridis)
theme_set(theme_cowplot())

#Load up TT data
tuesdata <- tidytuesdayR::tt_load('2020-09-22')
peak_dat <- tuesdata[[1]] #info about each of the peaks
climber_dat <- tuesdata[[2]] #info about each of the climbers who have attempted the climbs
expedition_dat <- tuesdata[[3]] #info about each of the expeditions 

#How has the gender makeup of attempts and successful attempts of the most popular mountains changed over time?

#First pick out some choice peaks
top_peaks <- climber_dat %>%
  filter(success == TRUE) %>%
  group_by(peak_id) %>%
  count() %>% arrange(desc(n)) %>% head(9)

#Start with climber dat, filter out only successes at the *top peaks*
climber_dat_top <- climber_dat %>%
  filter(peak_id %in% top_peaks$peak_id)%>%
  filter(success == TRUE) %>%
  group_by(peak_id, year) %>%
  summarise(climbers_male = sum(sex == "M"),
            climbers_female = sum(sex == "F"),
            climber_total = sum(sex == "F" | sex == "M")) %>%
  mutate(percent_female = climbers_female/climber_total*100) %>%
  mutate(peak_id = factor(peak_id, levels = top_peaks$peak_id))

#https://www.britannica.com/topic/Troubled-2014-Everest-Climbing-Season-The-1994985
#Interesting article about the 2014 Everest season
ggplot(data = climber_dat_top, aes(x = year, 
                                   y = climber_total, size = climber_total, 
                                   color = percent_female))+
  geom_point()+
  scale_color_viridis() +
  facet_wrap(facets = vars(peak_id), scales = "free")
