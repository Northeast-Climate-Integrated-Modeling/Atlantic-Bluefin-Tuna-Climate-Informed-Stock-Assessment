# Joins existing field station depth data and NOAA bathymetry depth data, tests
# relationship between the two, visualizes.

rm(list=ls())

# Load libraries
library(tidyverse)
library(here)
library(sf)
library(raster)
library(marmap)
library(rgdal)
# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))
# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', size=1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Load station data from OISST-SST merge
stations <- read.csv(here('Data/Clean/AllYears_Canada_UpdatedSST.csv'))
# Remember this is just station data, missing catch data from large category
head(stations)

# Load GEBCO 15 arc second bathy grid
bathy_rast <- raster(here('Data/Bathy/gebco_2022_n50.0_s40.0_w-70.0_e-50.0.asc'))
# Reproject to unprojected wgs84 lat-lon
bathy_rast <- raster::projectRaster(bathy_rast, 
                                    crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Clip raster to NWAtlantic shapefie
nwat <- readOGR(here('Data/GIS/Combined_Bluefin2.shp'))
nwat <- spTransform(nwat, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
bathy_rast <- raster::mask(bathy_rast, nwat)
plot(bathy_rast)

# Clip out coast
# coast <- ecodata::coast
# coast <- st_transform(coast, crs="EPSG:4326")
# coast <- as(coast, 'Spatial')
# coast_rast <- raster::mask(bathy_rast, coast)
# plot(coast_rast)

for(i in 1:length(bathy_rast@data@values)){
  if(!is.na(bathy_rast@data@values[i]) & !is.na(coast_rast@data@values[i])){
    bathy_rast@data@values[i] <- NA
  }
}
bathy_rast@data@values[bathy_rast@data@values > 0] <- NA
plot(bathy_rast)
summary(bathy_rast@data@values)

# Convert stations data to spatialpointsdataframe
stations.sp <- SpatialPointsDataFrame(coords=as.matrix(cbind(stations$lon, stations$lat)),
                                      data=stations,
                                      proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
plot(bathy_rast)
points(stations.sp, pch='.', cex=0.8)

# Extract bathy at station points
stations.sp$bathy <- extract(bathy_rast, stations.sp)
stations.sp$bathy <- round(stations.sp$bathy, 1)

# Reconvert to data frame
stations <- as.data.frame(stations.sp)
stations$coords.x1 <- NULL; stations$coords.x2 <- NULL
  
# Comparisons
# Set sequence of years to plot
yearstoplot <- seq(1993, 2021)

# Plot comparisons, check for adhesion to 1:1 line
for(i in 1:length(yearstoplot)){
  # Set year
  plotyr <- yearstoplot[i]
  
  # Plot OISST vs Field SST for that year
  print(ggplot2::ggplot(stations[stations$year == plotyr,], 
                        aes(x=depth, y=bathy)) +
          geom_point(alpha=0.8, pch=16)+
          geom_abline(intercept = 0, slope = 1) +
          labs(x = "Field depth (m)",
               y = "NOAA bathymetry (m)") +
          theme_bw() +
          ggtitle(paste("Depth differences:", plotyr, sep = " ")))
}

# Plot OISST vs Field SST for all years
ggplot2::ggplot(stations, aes(x=depth, y=bathy, col=year)) +
  geom_point(alpha=0.8, pch=16)+
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Field depth (m)",
       y = "NOAA bathymetry (m)") +
  theme_bw() 
# Oh boy
# Above 1:1 line== shallower NOAA bathymetry than field data
# Below 1:1 line== shallower field data than NOAA bathymetry

# Calculate difference
stations$depthdif <- stations$depth - stations$bathy
stations$depthdif[is.na(stations$depth) | is.na(stations$bathy)] <- NA
hist(stations$depthdif, breaks=seq(-8880, 2790, 10),
     xlim=c(-1000, 1000))
# Huge prevalence of NOAA bathymetry underestimating field depth

# Spatial plof of differences, all years
stations.sf <- st_as_sf(stations, coords=c('lon', 'lat'), crs="EPSG:4326")
coast <- ecodata::coast
coast <- st_transform(coast, crs="EPSG:4326")
ggplot2::ggplot() +
  geom_sf(data = coast) +
  geom_sf(data = stations.sf, 
          aes(colour=depthdif)) +
  coord_sf(xlim = c(-77, -65), 
           ylim = c(35, 45)) + 
  scale_color_gradient2(low = "blue",
                        mid = "yellow",
                        high = "red",
                        midpoint = 0,
                        limits=c(-8900, 2200),
                        na.value = "gray") +
  theme_bw() +
  #facet_wrap(~month) +
  ggtitle(paste("Depth difference survey-NOAA bathy: All years"))

# Spatial plot of differences, year by year
# Set function to map SST difference
yrmap <- function(mapyr){
  coast <- ecodata::coast
  coast <- st_transform(coast, crs="EPSG:4326")
  ggplot2::ggplot() +
    geom_sf(data = coast) +
    geom_sf(data = stations.sf[stations.sf$year == mapyr,], 
            aes(colour=depthdif)) +
    coord_sf(xlim = c(-77, -65), 
             ylim = c(35, 45)) + 
    scale_color_gradient2(low = "blue",
                          mid = "yellow",
                          high = "red",
                          #midpoint = 0,
                          limits=c(-8900, 2200),
                          na.value = "gray") +
    theme_bw() +
    #facet_wrap(~month) +
    ggtitle(paste("Depth difference survey-NOAA bathy:", mapyr, sep = " "))
}

# Plot SST difference in space for every year
# Blue: NOAA shallower than field
# Red: field shallower than NOAA
for(mapyr in 1993:2021){
  print(yrmap(mapyr)) 
}

# Executive decision: if field depth is within 215m deg of NOAA bathy, use field depth value
# If field depth is more than 215m different from NOAA bathy, use NOAA bathy
nrow(stations[!is.na(stations$depthdif) & abs(stations$depthdif) <= 173,]) / 
  nrow(stations[!is.na(stations$depthdif),]) * 100
# 85% of our observations with both values are within 173m

# Call new columns
stations$temp <- NA
stations$depthsource <- NA

# Set temp to OISST when abs(sstdiff) >2
# Retain source information
for(i in 1:nrow(stations)){
  if(!is.na(stations$depthdif[i]) & abs(stations$depthdif[i]) <=173){
    stations$temp[i] <- stations$depth[i]
    stations$depthsource[i] <- 'Field'
  }
  if(is.na(stations$depthdif[i]) | abs(stations$depthdif[i]) >173){
    stations$temp[i] <- stations$bathy[i]
    stations$depthsource[i] <- 'NOAA'
  }
  
}
summary(stations$temp)
# Still have some NA values-- can we fix this
badvals <- stations[is.na(stations$temp),]
badvals.sf <- st_as_sf(badvals, coords=c('lon', 'lat'))
st_crs(badvals.sf) <- 'EPSG:4326'

ggplot() +
  geom_sf(data=coast, fill='gray') +
  geom_sf(data=badvals.sf, col='black') +
  coord_sf(xlim=c(-73, -69),
           ylim=c(41, 43))
# No. Chances are these are values that are actually on land. 
# They will be removed in a later step.

# Check how often we are substituting NOAA data for field
barplot(t(prop.table(table(stations$year, stations$depthsource), margin=1)), legend=T)
abline(h=0.7, col='red')
# All years are more than 70% populated by field depth data

# Remove unnecessary columns
stations <- dplyr::select(stations,
                                   -depth, -bathy, -depthdif)
colnames(stations) <- c(colnames(stations[1:11]), 'depth', 'depthsource')
head(stations)

# Remove NA output
stations <- stations[!is.na(stations$bathy),]

# Save output
write.csv(stations, row.names = F, 
          here('Data/Clean/AllYears_Canada_UpdatedDepth.csv'))
