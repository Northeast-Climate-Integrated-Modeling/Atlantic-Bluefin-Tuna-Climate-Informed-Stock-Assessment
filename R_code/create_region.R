# Code adapted from Cecilia O'Leary and Cole Monnahan 
# Create a user-defined extrapolation grid for VAST 

rm(list=ls())

# Load libraries
library(sp)
library(sf)
library(rgdal)
library(here)

# Load shapefile of desired extrapolation region
shp <- readOGR(here("Data/GIS/NWAtlantic.shp"))
head(shp@data)
plot(shp)

# Transform to unprojected lat-lon
sps <- spTransform(shp, 
                   CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"))
plot(sps)

# Find UTM
lon <- sum(bbox(sps)[1,])/2
utmzone <- floor((lon + 180)/6)+1
crs_LL <- CRS('+proj=longlat +ellps=WGS84 +no_defs')
sps@proj4string <- crs_LL

# Convert the final in polygon to UTM
crs_UTM <- CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 
                      +units=m +no_defs "))
region_polygon <- spTransform(sps, crs_UTM)
region_polygon_sf <- st_as_sf(region_polygon)
ggplot(ecodata::coast) +
  geom_sf(data=region_polygon_sf, fill="lightblue") +
  geom_sf() +
  coord_sf(xlim=c(-76, -65), ylim=c(35, 46))

# Construct the extrapolation grid for VAST using sf package
# Size of grid **in meters** (since working in UTM). Controls
# the resolution of the grid.
cell_size <- 1000 * 10
# Set to 10Km cells

# This step is slow at high resolutions
region_grid <- st_make_grid(region_polygon_sf, cellsize = cell_size,
                            what = "centers")

# Convert region_grid to Spatial Points to SpatialPointsDataFrame
region_grid <- as(region_grid, "Spatial")
region_grid_sp <- as(region_grid, "SpatialPointsDataFrame")

# combine shapefile data (region_polygon) with Spatial Points
# (region_grid_spatial) & place in SpatialPointsDataFrame data
# (this provides you with your strata identifier (here called
# Id) in your data frame))
region_grid_sp@data <- over(region_grid, region_polygon)
#region_grid_sp <- subset(region_grid_sp, is.na(region_grid_sp@data$OBJECTID)==FALSE)

# Convert back to lon/lat coordinates as that is what VAST uses
region_grid_LL <- as.data.frame(spTransform(region_grid_sp, crs_LL))
region_df <- with(region_grid_LL,
                  data.frame(Lon=coords.x1,
                             Lat=coords.x2, FID,
                             Area_km2=( (cell_size/1000)^2),
                             row=1:nrow(region_grid_LL)))

# Filter out the grid that does not overlap (outside extent)
region <- subset(region_df, is.na(FID)==FALSE)
region_sf <- st_as_sf(region, coords=c('Lon', 'Lat'))
st_crs(region_sf) <- "EPSG: 4326"
ggplot(ecodata::coast) +
  geom_sf(data=region_sf, aes(col=FID)) +
  geom_sf() +
  coord_sf(xlim=c(-76, -65), ylim=c(35, 46))

# Save it to be read in and passed to VAST later.
saveRDS(region, file = here("Data/VAST_regions/tuna_region.rds"))
