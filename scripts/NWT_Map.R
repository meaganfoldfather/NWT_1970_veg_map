# NWT Historical Veg Map

library(dplyr)
library(sf)
library(ggplot2)
library(mapview)
library(tmap)
library(spData)


veg_class_map <- st_read("data/raw/veg_zip/veg/veg.shp")

#min area of a polygon
min(veg_class_map$AREA) #~7

# number of polygons
length(veg_class_map$AREA) # 1233 

# number of TYPES
length(unique(veg_class_map$TYPE)) # 25

# Do we need to combine polygons? Combine veg type?
ggplot()+
  geom_sf(data = veg_class_map, aes(fill = TYPE))+
  theme(legend.position = "none")

mapview(veg_class_map, zcol="TYPE", legend = T)

# Add pts
#MRS <- 40.032024, -105.535650
#Tundra_Lab <-  40.054124, -105.588884

# area of veg classes
veg_class_map %>% 
  mutate(area_m2 = as.numeric(st_area(.))) %>% 
  summarize(total_area_m2 = sum(area_m2))

# try sampling points with GRTS
seed <- sample(x = 100000000, size = 1)
set.seed(seed)
library(spsurvey)
att = read.dbf("data/raw/veg_zip/veg/veg.shp")
head(att)


?grts
equal_design <- list(
   strata2 = list(panel=c(set1=6), seltype="UnEqual", over=0),
   strata1 = list(panel=c(set1=3), seltype="Equal", over=0),
   strata3 = list(panel=c(set1=3), seltype="Equal", over=0))

test <- grts(design = equal_design, src.frame = "shapefile",in.shape = "data/raw/veg_zip/veg/veg.shp", att.frame = att, type.frame = "area", DesignID = "sample", shapefile = T, out.shape = "Sampling_Pts")
