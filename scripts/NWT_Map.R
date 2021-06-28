# NWT Historical Veg Map

library(dplyr)
library(sf)
library(ggplot2)
library(mapview)
library(tmap)
library(spData)


veg_class_map <- st_read("data/raw/veg_zip/veg/veg.shp")

ggplot()+
  geom_sf(data = veg_class_map, aes(fill = TYPE))+
  theme(legend.position = "none")

mapview(veg_class_map, zcol="TYPE", legend = F)

# Add pts
#MRS <- 40.032024, -105.535650
#Tundra_Lab <-  40.054124, -105.588884

veg_class_map %>% 
  mutate(area_m2 = as.numeric(st_area(.))) %>% 
  summarize(total_area_m2 = sum(area_m2))