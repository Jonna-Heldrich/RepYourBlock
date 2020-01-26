### This script works to loop through a shapefile and create a map of each polygon

setwd("/Users/Hodges/Desktop/ryb")


#### probably don't need all these packages, check later
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
  filter(elect_dist == "56027") 

#############################
#### tester with ad56_27 ###
#############################
temp_2_map <- tm_basemap("CartoDB.Voyager") +
  tm_shape(ad56_27) +
  tm_borders(lwd=3, col = "red", alpha = 1) +
  tm_layout(main.title = "AD 56, ED 27",
    frame = FALSE) ## remove black border frame

lf <- tmap_leaflet(temp_2_map) %>% 
  addControl("test", position = "topright") ### adds title

### writes out an image of the leaflet map above
mapshot(lf, file = "test6.png")

#############################
######## END TESTING #######
#############################

#### ad_ed_list is the dataframe to loop through - use this first  if you want to create new maps for a few districts
aded_list <- ad_ed_list %>%
  filter(ad_ed == "56027" | ad_ed == "51082" |
           ad_ed == "45003")

ad_ed <- as.list(pull(aded_list, ad_ed)) ### turns ad ed list into list to loop thorugh

#### define 
# setwd("/Users/Hodges/Desktop/ryb")

for (ed in ad_ed){
  shape <- bk_ed_shp %>% 
    filter(elect_dist == ed)  ### filters the appropriate election district
  
  ed_title <- paste0("AD ", substr(ed, 0, 2), " ED ", substr(ed, 3, 5))  ### creates title
  
  temp_2_map <- tm_basemap("CartoDB.Voyager") +
    tm_shape(shape) +
    tm_borders(lwd=3, col = "red", alpha = 1) +
    tm_layout(frame = FALSE) ## remove black border frame

  lf <- tmap_leaflet(temp_2_map) %>%
    addControl(ed_title, position = "topright") ### adds title

  ### writes out an image of the leaflet map above
  filename_aded <- paste0("/Users/Hodges/Desktop/ryb/maps/", ed, "_map.png")
  mapshot(lf, file = filename_aded)
}


#### That's it!
