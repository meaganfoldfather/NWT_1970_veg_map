# NWT Historical Veg Map

library(dplyr)
library(sf)
library(ggplot2)
library(mapview)
library(tmap)
library(spData)
library(terra)

# bring in 1970s veg map
veg_class_map <- st_read("data/raw/veg_zip/veg/veg.shp")

#min/max area of a polygon
min(veg_class_map$AREA) #7
max(veg_class_map$AREA) #~1084750

# number of polygons
length(veg_class_map$AREA) # 1233 

# number of TYPES; # Do we need to combine polygons? Combine veg type?
length(unique(veg_class_map$TYPE)) # 25

#plot map
ggplot()+
  geom_sf(data = veg_class_map, aes(fill = TYPE))+
  theme(legend.position = "none")

mapview(veg_class_map, zcol="TYPE", legend = T)

# Add landmark pts for reference?
#MRS <- 40.032024, -105.535650
#Tundra_Lab <-  40.054124, -105.588884

# area of all polygons
veg_class_map %>% 
  group_by(TYPE) %>% 
  mutate(area_m2 = as.numeric(st_area(.))) %>% 
  summarize(total_area_m2 = sum(area_m2))


# Sample points for resurvey - GRTS approach
library(spsurvey)
library(rgeos)

# get rid of lake and NA polygons
subsetted_map <- 
veg_class_map %>% 
  filter(!is.na(TYPE)) %>% 
  filter(TYPE != "Lake")
mapview(subsetted_map, zcol="TYPE", legend = T)

# need to dissolve all polygons into a single polygon?
dissolved <- st_union(subsetted_map)
dissolved
mapview(dissolved)

set.seed(9877789)
Equaldsgn <- list(None=list(panel=c(PanelOne=1000), seltype="Equal"))

Equalsites <- grts(design=Equaldsgn,
                   DesignID="EQUAL",
                   type.frame="area",
                   src.frame="sf.object",
                   sf.object=subsetted_map,
                   maxlev = 5,
                   shapefile=FALSE)
Equalsites
design_df <- SpatialPoints(Equalsites)
design_df

design_coords <- as.data.frame(coordinates(design_df))
design_coords

#plot map
ggplot()+
  geom_sf(data = veg_class_map, aes(fill = TYPE))+
  theme_classic()+
  theme(legend.position = "none")+
  geom_point(data = design_coords[1:50,], aes(X, Y))

#write.csv(design_coords, "survey_points_32613.csv")
design_coords

# bring in site coords created before and turn into lat-long
lat_long <- read.csv("survey_points_32613.csv")
colnames(lat_long) <- c("id", "X", "Y")

lat_long <- st_as_sf(lat_long, coords = c("X", "Y"), crs = 32613, remove = F)
mapview(lat_long, zcol = "id")

lat_long <- st_transform(lat_long, crs = 4326)
mapview(lat_long, zcol = "id")
lat_long
plot(density(st_distance(lat_long$geometry)))

st_write(lat_long, dsn = "survey_points_latlong.csv")


# 
# # set seed
# seed <- sample(x = 100000000, size = 1)
# set.seed(seed)
# 
# att = read.dbf("data/raw/veg_zip/veg/veg.shp")
# head(att)
# 
# # try to sample 
# ?grts
# equal_design <- list()
# 
# test <- grts(design = equal_design, src.frame = "shapefile",in.shape = "data/raw/veg_zip/veg/veg.shp", att.frame = att, type.frame = "area", DesignID = "sample", shapefile = T, out.shape = "Sampling_Pts")
