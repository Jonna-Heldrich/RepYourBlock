### This script works to loop through a shapefile and create a map of each polygon
### It is looping through states, but we can replace with EDs
### We'll need to add the leaflet package so that there is a street map layer
### otherwise it should work with any shapefile

setwd("/Users/Hodges/Desktop/ryb")


#### probably don't need all these packages
require(sf)
require(dplyr)
require(mapview)
require(tmap)
require(tmaptools)
require(leaflet)
require(spData)
require(leaflet)
require(OpenStreetMap)
require(mapview)

### import list of all eds in brooklyn
ad_ed_list <- read.csv("~/Desktop/ryb/RepYourBlock/data/ad_ed_list.csv")

### importing the state and point shapefiles
ed_shp <- st_read("raw_data/Election Districts/eds_nyc_20191215.shp") 

bk_ed_shp <- ed_shp %>% 
  right_join(ad_ed_list, by = c("elect_dist" = "ad_ed"))



ad56_27 <- bk_ed_shp %>% 
  mutate(elect_dist = as.character(elect_dist)) %>% 
  filter(elect_dist == "56027" | elect_dist == "51082" |
           elect_dist == "45003")

#### tester with ad56_26
temp_2_map <- tm_basemap("CartoDB.Voyager") +
  tm_shape(ad56_27) +
  tm_borders(lwd=1.5, col = "#008ca3", alpha = .5) +
  tm_layout(frame = FALSE) ## remove black border frame

lf <- tmap_leaflet(temp_2_map)
mapshot(lf, file = "test.png")

#### creating the data frame to loop through
# states <- as.data.frame(dismissed_points) %>%
#   select(-geometry) %>% 
#   select(Postal) %>%
#   distinct()

setwd("~/Box Sync/Data/Dismissed/Visuals/Infographics/one_pager_maps")

### this is the list of the states to loop through below
# states_list <- as.list(dplyr::pull(ad_ed_list, ad_ed))
ad56_27_list <- as.data.frame(ad56_27) %>% 
  select(-geometry) %>% 
  select(elect_dist)

ed_list <- as.list("56027", "51082", "45003") 

for (ed in ed_list){
  
  # temp_border <- state_border %>% 
  #   filter(STUSPS == state)
  temp_border <- ad56_27 %>%
    filter(elect_dist == ed)
  # 
  # temp_points <- dismissed_points %>% 
  #   filter(Postal == state)
  
  temp_2_map <- tm_basemap("CartoDB.Voyager") +
    tm_shape(temp_border) +
    tm_borders(lwd=1.5, col = "#008ca3", alpha = .5) +
    tm_layout(frame = FALSE) ## remove black border frame

  filename_ed <- paste0(ed, "_map.png")
  
  lf <- tmap_leaflet(temp_2_map)
  mapshot(lf, file = filename_ed)
  
  #ggsave(g2, filename = filename_state,  bg = "transparent", units = "in", width = 5, height = 8)
  # tmap_save(temp_2_map, filename = filename_state, units = "in", dpi=200, width = 4.46)
}

#tmap_save(state_map, "al_map.png", units = "in", dpi=200, width = 4.46)


#### That's it!
