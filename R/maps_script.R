### This script works to loop through a shapefile and create a map of each polygon
### It is looping through states, but we can replace with EDs
### We'll need to add the leaflet package so that there is a street map layer
### otherwise it should work with any shapefile

setwd("~/Box Sync/Data/Dismissed/Data")


#### probably don't need all these packages
require(sf)
require(dplyr)
require(tidyverse)
require(stringr)
require(tibble)
require(mapview)
library(RColorBrewer)
require(tmap)
require(tmaptools)
library(grid)

#### this is how you define a color pallete - we're not using it here, but leaving it just in case
#purples <-  c('#d6d4e8', '#b6b3d6', '#9c7dc4', '#7d52b7', '#4c2382')
#diverge <-  c('#a6611a', '#dfc27d', '#ffffe1', '#80cdc1', '#018571')

### importing the state and point shapefiles
state_border <- st_read("~/Box Sync/Data/Library/SHP/States/States_LowRes_WGS84.shp") %>% 
  arrange(STATEFP) 

dismissed_points <- st_read("~/Box Sync/Data/Dismissed/Data/Final/DataForWebsite/small/website_points.shp") %>% 
  mutate(Postal = as.character(Postal),
         uid = as.character(uid)) 

#### creating the data frame to loop through
states <- as.data.frame(dismissed_points) %>%
  select(-geometry) %>% 
  select(Postal) %>%
  distinct()

setwd("~/Box Sync/Data/Dismissed/Visuals/Infographics/one_pager_maps")

### this is the list of the states to loop through below
states_list <- as.list(pull(states, Postal))

for (state in states_list){
  
  temp_border <- state_border %>% 
    filter(STUSPS == state)
  
  temp_points <- dismissed_points %>% 
    filter(Postal == state)
  
  temp_2_map <- tm_shape(temp_border) +
    tm_borders(lwd=1.5, col = "#008ca3", alpha = .5) +
    tm_shape(temp_points) + 
    tm_symbols(col = "#c90000", size = .25) + 
    tm_layout(frame = FALSE) ## remove black border frame

  filename_state <- paste0(state, "_map.png")
  
  #ggsave(g2, filename = filename_state,  bg = "transparent", units = "in", width = 5, height = 8)
  tmap_save(temp_2_map, filename = filename_state, units = "in", dpi=200, width = 4.46)
}

#tmap_save(state_map, "al_map.png", units = "in", dpi=200, width = 4.46)


#### That's it!
