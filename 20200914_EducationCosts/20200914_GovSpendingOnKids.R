#Load packages
library(tidyverse)
library(tidytuesdayR)
library(cowplot)
library(geojsonio)
theme_set(theme_cowplot())

#Load up TT data
kids_dat <- tt_load("2020-09-15")[[1]]

#Explore - where does most of the money go per kid?
#Answer - "PK12ed" "highered" "othercashserv" "pubhealth"  "unemp"
kids_dat_summary1 <- kids_dat %>%
  group_by(variable) %>%
  summarise(total = sum(inf_adj_perchild)) %>%
  arrange(desc(total))

#Lets make an annomated (over time) of hexmaps for one of the variables 
##First lets just make 1 year hexmap
# Load Map Data  using a geojson file of the hex grid and geojson_read() function 
# Instructions from: https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html
spdf <- geojson_read("20200914_EducationCosts/us_states_hexgrid.geojson",  what = "sp")

# Bit of reformating
spdf@data <- spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

plot(spdf)

spdf_fortified <- broom::tidy(spdf, region = "google_name") #Makes a df of the hexs

#Determine centers for labels by using the gCentroid() function
centers <- gCentroid(spdf, byid=TRUE)%>%
  as.data.frame() %>%
  mutate(id=spdf@data$iso3166_2)

#filter for pubhealth, with one year first
kids_pubhealth_dat <- kids_dat %>%
  filter(variable == "PK12ed") %>%
  filter(year == "1997")

#Join map data with kids data
kids_pubhealth_dat_fortified <- spdf_fortified %>%
  left_join(kids_pubhealth_dat, by=c("id"="state")) 

p <-  ggplot(data = kids_pubhealth_dat_fortified) +
  geom_polygon(aes(fill = inf_adj_perchild, x = long, y = lat, group = group)) +
  geom_text(data = centers, aes(x = x, y = y, label = id), color = "grey") +
  #scale_fill_viridis()+
  #scale_fill_brewer(palette = "RdBu")+
  #theme_void() +
  coord_map()
p

cowplot::ggdraw(p) + 
  theme(plot.background = element_rect(fill="#95b0cc", color = NA))+
  cowplot::draw_figure_label(label = "1997", position = "top.right", size = 20)

