#Load packages
library(tidyverse)
library(tidytuesdayR)
library(cowplot)
library(geojsonio)
library(rgdal)
library(mapproj)
library(broom)
library(rgeos)
library(viridis)
theme_set(theme_cowplot())
library(gganimate)
library(transformr)

#Load up TT data
kids_dat <- tt_load("2020-09-15")[[1]]

#Explore - where does most of the money go per kid?
#Answer - "PK12ed" "highered" "othercashserv" "pubhealth"  "unemp"
kids_dat_summary1 <- kids_dat %>%
  group_by(variable) %>%
  summarise(total = sum(inf_adj_perchild)) %>%
  arrange(desc(total))

# Load Map Data  using a geojson file of the hex grid and geojson_read() function 
# Instructions from: https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html
spdf <- geojson_read("20200914_EducationCosts/us_states_hexgrid.geojson",  what = "sp")

# Bit of reformating
spdf@data <- spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

#plot(spdf)

spdf_fortified <- broom::tidy(spdf, region = "google_name") #Makes a df of the hexs

#Determine centers for labels by using the gCentroid() function
centers <- gCentroid(spdf, byid=TRUE)%>%
  as.data.frame() %>%
  mutate(id=spdf@data$iso3166_2)

#filter for pubhealth, with one year first
kids_pubhealth_dat <- kids_dat %>%
  filter(variable == "highered") 

#Join map data with kids data
kids_pubhealth_dat_fortified <- spdf_fortified %>%
  left_join(kids_pubhealth_dat, by=c("id"="state")) 

p <-  ggplot(data = kids_pubhealth_dat_fortified) +
  geom_polygon(aes(fill = inf_adj_perchild, x = long, y = lat, group = group)) +
  geom_text(data = centers, aes(x = x, y = y, label = id), color = "grey70") +
  scale_fill_viridis(name = "$ per child \n X1000", option="magma")+
  #scale_fill_brewer(palette = "RdBu")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 8)) +
  labs(title ="Government spending per child on higher education in {closest_state}")+
  coord_map() +
  transition_states(year, 
                    transition_length = 0.1,
                    state_length = 6) 

anim_save("20200914_EducationCosts/20200914_GovSpendingOnKids.gif", p)

