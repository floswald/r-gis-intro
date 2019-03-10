### Introduction to GIS with R: Spatial data with the sp and sf packages ###

# This script goes along with the blog post of the same name,
# which can be found at https://www.jessesadler.com/post/gis-with-r-intro/
# See the Rmarkdown document for the contents of the blog post.

### Load packages and data
library(tidyverse)
library(sp)
library(sf)
library(rnaturalearth)

# Load the data
letters <- read_csv("data/correspondence-data-1585.csv")
locations <- read_csv("data/locations.csv")

########################
## Preparing the data ##
########################

# Letters per source
sources <- letters %>% 
  group_by(source) %>% 
  count() %>% 
  rename(place = source) %>% 
  add_column(type = "source") %>% 
  ungroup()

# Letters per destination
destinations <- letters %>% 
  group_by(destination) %>% 
  count() %>% 
  rename(place = destination) %>% 
  add_column(type = "destination") %>% 
  ungroup()

# Bind the rows of the two data frames
# and change type column to factor
letters_data <- rbind(sources, destinations) %>% 
  mutate(type = as_factor(type))

# Join letters_data to locations
geo_data <- left_join(letters_data, locations, by = "place")

##################################
## Spatial data with sp package ##
##################################

# Create data frame of only longitude and latitude values
coords <- select(geo_data, lon, lat)

# Create SpatialPoints object with coords and CRS
points_sp <- SpatialPoints(coords = coords,
                           proj4string = CRS("+proj=longlat +datum=WGS84"))

# Create SpatialPointsDataFrame object
points_spdf <- SpatialPointsDataFrame(coords = coords,
                                      data = letters_data,  
                                      proj4string = CRS("+proj=longlat +datum=WGS84"))

# Example of subsetting `points_spdf` to return locations with "n" greater than 10
points_spdf[points_spdf@data$n > 10, ]

# Get coastal and country world maps as Spatial objects
coast_sp <- ne_coastline(scale = "medium")
countries_sp <- ne_countries(scale = "medium")

####################################
## Mapping with sp and base plots ##
####################################

### Set up plot ###

# Create a new color palette to distinguish source and destination
palette(alpha(c("darkorchid", "darkorange"), 0.7))
# Set margins for bottom, left, top, and right of plot
par(mar = c(1, 1, 3, 1))

### Plot points ###
plot(points_spdf,
     pch = 20,
     col = points_spdf$type,
     cex = sqrt(points_spdf$n)/2 + 0.25)

# Add a box around the plot
box()
# Add a title
title(main = "Correspondence of Daniel van der Meulen, 1585")

### Plot map with coastlines (lines) data ###
# Pointsize vector for legend
pointsize <- c(1, 50, 100)
par(mar = c(1, 1, 3, 1))

# Plot points
plot(points_spdf,
     pch = 20,
     col = points_spdf$type,
     cex = sqrt(points_spdf$n)/2 + 0.25)
# Plot coastlines background map
plot(coast_sp,
     col = "black",
     add = TRUE)
# Add a box around the plot
box()

# Legend for colors
legend("topright", legend = levels(points_spdf$type),
       pt.cex = 2,
       col = 1:2,
       pch = 15)

# legend for size of points
legend("right", legend = pointsize,
       pt.cex = (sqrt(pointsize)/2 + 0.25),
       col = "black",
       pch = 20,
       title = "Letters")

# Title for the map
title(main = "Correspondence of Daniel van der Meulen, 1585")

# Make bounding box for countries_sp match
# bounding box of points_spdf
countries_sp@bbox <- bbox(points_spdf)

### Plot map with countries (polygons) data ###
par(mar = c(1, 1, 3, 1))

# Plot countries map and color with grays
plot(countries_sp,
     col = gray(0.8),
     border = gray(0.7))
# Plot points
plot(points_spdf,
     pch = 20,
     col = points_spdf$type, 
     cex = sqrt(points_spdf$n)/2 + 0.25,
     add = TRUE)
# Add a box around the plot
box()

# Legend for colors
legend("topright",
       legend = levels(points_spdf$type),
       pt.cex = 2,
       col = 1:2,
       pch = 15)
# legend for size of points
legend("right",
       legend = pointsize,
       pt.cex = (sqrt(pointsize)/2 + 0.25),
       col = "black",
       pch = 20,
       title = "Letters")

# Title for the map
title(main = "Correspondence of Daniel van der Meulen, 1585")

##################################
## Spatial data with sf package ##
##################################

# Create sf object with geo_data data frame and CRS
points_sf <- st_as_sf(geo_data, coords = c("lon", "lat"), crs = 4326)

# Get coastal and country world maps as sf objects
coast_sf <- ne_coastline(scale = "medium", returnclass = "sf")
countries_sf <- ne_countries(scale = "medium", returnclass = "sf")
france_sf <- ne_countries(geounit = "France",type="map_units", returnclass = "sf")

# Subset of locations with "n" greater than 10
filter(points_sf, n > 10)

### Subset of countries object to get South American countries ###
# South American countries with new CRS
countries_sf %>% 
  filter(continent == "South America") %>% 
  select(name) %>% 
  st_transform(crs = "+proj=moll +datum=WGS84")

### Make map of South America ###
# Return to default palette
palette("default")

# Map of South American countries
countries_sf %>% 
  filter(continent == "South America") %>% 
  select(name) %>% 
  st_transform(crs = "+proj=moll +datum=WGS84") %>% 
  plot(key.pos = NULL, graticule = TRUE, main = "South America")

countries_sf %>% 
  filter(continent == "Europe") %>% 
  filter(subregion != "Eastern Europe") %>%
  select(name) %>% 
  st_transform(crs = "+proj=moll +datum=WGS84") %>% 
  plot(key.pos = NULL, graticule = TRUE, main = "Europa")

# Map of France
france_sf %>% 
  select(name) %>% 
  plot(key.pos = NULL, graticule = TRUE, main = "France")


#################################
## Mapping with sf and ggplot2 ##
#################################

f_data <- data.frame(
  city = c("Paris","Lyon","Marseille","Bordeaux"),
  lon = c(2.346941, 4.754196,5.380583	,-0.586191),
  lat = c(48.858884,	45.697190,43.280427,	44.863506	)
)

f_points_sf <- st_as_sf(f_data, coords = c("lon","lat"), crs=4326)
# distance to paris
f_points_sf$dist = st_distance(f_points_sf, subset(f_points_sf,city=="Paris"))
f_points_sf$dist = units::set_units(f_points_sf$dist,km)


ggplot() + 
  geom_sf(data = france_sf) + 
  geom_sf(data = f_points_sf, aes(color = as.factor(round(dist,2)))) + geom_sf_label(data = f_points_sf, aes(label = city),nudge_x = 0.5,nudge_y = 0.5)

### Basic ggplot2 plot with geom_sf ###
ggplot() + 
  geom_sf(data = coast_sf) + 
  geom_sf(data = points_sf,
          aes(color = type, size = n),
          alpha = 0.7,
          show.legend = "point") + coord_sf(xlim = c(-1, 14), ylim = c(44, 55))

# load a shapefile and plot
sh = st_read(file.path("data","CONTOURS-IRIS","1_DONNEES_LIVRAISON_2018-06-00105","CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2017","CONTOURS-IRIS.shp"),stringsAsFactors=FALSE)
sh_lalo = st_transform(sh,crs = 4326)
# paris
p = sh_lalo %>%
  mutate(INSEE_N = as.integer(INSEE_COM)-75100) %>%
  filter((INSEE_N < 21) & (INSEE_N > 0))

# using base plot
#####

# plot paris IRIS inside communes
p %>%
  select("INSEE_COM") %>%
  plot(key.width=lcm(3.5),axes=TRUE)

# aggregating geometry by a feature
# plot number of iris within a commune
p %>%
  group_by(INSEE_COM) %>%
  summarise(n = n()) %>%
  select(n) %>%
  plot(main="Number of Iris within Commune")

# with ggplot and labelling areas
# strangely, this casts automatically to lat/lon CRS
p_arr = p %>%
  group_by(INSEE_N) %>%
  summarise(n = n(),id=INSEE_N[1])
p_arr = p_arr %>%
  mutate(num_IRIS = cut(n,5))

ggplot(p_arr) +
  geom_sf(aes(fill=num_IRIS)) + 
  geom_sf_label(aes(label=id)) + 
  ggtitle("Paris arrondissements with num of IRIS") 

# same and plot centroid
# does not work 
# https://stackoverflow.com/questions/54723899/st-centroid-does-not-lie-within-polygon-on-lambert-93-shapefile
p_arr = p %>%
  group_by(INSEE_COM) %>%
  summarise(n = n()) %>%
  select(n)

# works with pure geometry
plot(st_geometry(p_arr))
plot(st_geometry(st_centroid(p_arr)), pch = 3, col = 'red', add = TRUE)


# works with ggplot?
ggplot(p_arr) + geom_sf(aes(fill=NULL)) + geom_sf(data=st_geometry(st_centroid(p_arr)))





# generate a fake population weights dataset
p_arr$weights = sample(1:20,size=20,replace=FALSE)

wtc <- function(g,w){
  if (!(is(g,"sf")) | !(w %in% colnames(g))){
    stop(paste("requires an sf object with at a column",w))
  }
  centers = st_coordinates(st_centroid(st_geometry(g)))
  # crsx = st_crs(g)
  out = st_point(c(weighted.mean(centers[,"X"],g[[w]]), weighted.mean(centers[,"Y"],g[[w]])))
  return(out)
}

plot(st_geometry(p_arr))
plot(wtc(p_arr,"weights"),add=TRUE,col="red")


# works with ggplot?
center = wtc(p_arr,"weights")

center_sf = st_as_sf(data.frame(lon=center[1],lat=center[2],data = 1),coords = c("lon","lat"), crs=st_crs(p_arr))

ggplot() + geom_sf(data=p_arr) + geom_sf(data=center_sf)
ggplot() + geom_sf(data=st_geometry(st_union(p_arr))) + geom_sf(data=center_sf)



nc = st_read(system.file("shape/nc.shp", package="sf"))
nc$weights = c(rep(1,5),rep(0,95))
plot(st_geometry(nc))
plot(st_centroid(st_union(nc)),col="red",pch=3,add=TRUE)
plot(wtc(nc,"weights"),col="blue",pch=3,add=TRUE)

p_wgs = st_transform(p,4326)
p_arr_wgs = p_wgs %>%
  group_by(INSEE_COM) %>%
  summarise(n = n()) %>%
  select(n)

plot(p_arr_wgs,axes=TRUE)
plot(st_centroid(p_arr_wgs), pch = 3, col = 'red', add = TRUE)


# SO solution
p_arr <- sh %>%
  mutate(INSEE_N = as.integer(INSEE_COM)-75100) %>%
  filter((INSEE_N < 21) & (INSEE_N > 0)) %>%
  group_by(INSEE_COM) %>%
  summarise()
plot(p_arr)
plot(st_geometry(st_centroid(p_arr)),add=TRUE)

library(mapview)
mapview( list( p_arr, st_centroid( p_arr ) ) )


### Plot map with coastlines (lines) data ###
# Load ggrepel package
library(ggrepel)

ggplot() + 
  geom_sf(data = coast_sf) + 
  geom_sf(data = points_sf, 
          aes(color = type, size = n), 
          alpha = 0.7, 
          show.legend = "point") +
  coord_sf(xlim = c(-1, 14), ylim = c(44, 55),
           datum = NA) + # removes graticules
  geom_text_repel(data = locations, 
                  aes(x = lon, y = lat, label = place)) +
  labs(title = "Correspondence of Daniel van der Meulen, 1585",
       size = "Letters",
       color = "Type",
       x = NULL,
       y = NULL) +
  guides(color = guide_legend(override.aes = list(size = 6))) +
  theme_minimal()

### Plot map with countries (polygons) data ###
ggplot() + 
  geom_sf(data = countries_sf,
          fill = gray(0.8), color = gray(0.7)) + 
  geom_sf(data = points_sf, 
          aes(color = type, size = n), 
          alpha = 0.7, 
          show.legend = "point") +
  coord_sf(xlim = c(-1, 14), ylim = c(44, 55),
           datum = NA) + # removes graticules
  geom_text_repel(data = locations, 
                  aes(x = lon, y = lat, label = place)) +
  labs(title = "Correspondence of Daniel van der Meulen, 1585",
       size = "Letters",
       color = "Type",
       x = NULL,
       y = NULL) +
  guides(color = guide_legend(override.aes = list(size = 6))) +
  theme_bw()