# Turn prey model output into spatial field 
# Pull values at tuna observations for later use as spatial covariate

rm(list=ls())

# Load libraries and functions
library(tidyverse)
library(here)
library(sf)
library(TMB)
library(VAST)
library(raster)
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

# Load tuna observation data
tuna <- read.csv(here('Data/Clean/BFT_US_catch_VASTdata.csv'))
# Dupe new df to later bind to
tuna.new <- tuna[1,]
tuna.new[1,] <- rep(NA, ncol(tuna.new))
tuna.new$prey <- NA

# Load prey VAST data
herring <- load(here('VAST_runs/herring3/herring_3.Rdata'))

# Pull vector of years
years <- fit$year_labels

# Remake map list locally for recreating plots
mdl <- make_map_info(Region = settings$Region,
                     spatial_list = fit$spatial_list,
                     Extrapolation_List = fit$extrapolation_list)

## Get the model estimate of density for each year;
# link it spatially to a lat/lon extrapolation point.
D_gt <- fit$Report$D_gct[,1,]
# Strip units (units preclude some operations)
D_gt <- strip_units(D_gt)
# Rename dimensions
dimnames(D_gt) <- list(cell=1:nrow(D_gt), year=years)
# Coerce to tibble and reshape data
D_gt <- D_gt %>% as.data.frame() %>%
  tibble::rownames_to_column(var = "cell") %>%
  pivot_longer(-cell, names_to = "Year", values_to='D')
# Bind data to spatial points
D <- merge(D_gt, mdl$PlotDF, by.x='cell', by.y='x2i')
summary(D)

# Adjust dataframe and add log abundance
D$logD <- log(D$D)
D$Year <- as.numeric(D$Year)
D$cell <- as.numeric(D$cell)
D$Include <- NULL
str(D)

# Set CRS 
projargs <- fit$extrapolation_list$projargs
CRS_orig = sp::CRS("+proj=longlat")
CRS_proj = sp::CRS(projargs)

# Set min-max of Zlim for plotting
min.D <- floor(round(min(D$D, na.rm=T),1))
max.D <- ceiling(round(max(D$D, na.rm=T),1))

# Call coast shapefile
coast <- ecodata::coast
coast <- st_transform(coast, crs='EPSG:4326')

# Loop through each year of interest
for(i in 1:length(year.labs)
    #10 # for testing
){
  # Call year
  Year <- year.labs[i]
  # Substitute data to onlyl include year
  Year.sub <- D[D$Year == Year,]
  # Substitute tuna data to only include yar
  tuna.sub <- tuna[tuna$year == Year,]
  tuna.sub <- st_as_sf(tuna.sub, coords=c('lon', 'lat'))
  st_crs(tuna.sub) <- "EPSG:4326"
  # Convert herring data to spatialpointsdataframe
  herring.sp <- SpatialPointsDataFrame(coords=as.matrix(cbind(Year.sub$Lon, Year.sub$Lat)),
                                        data=Year.sub,
                                        proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  
  
  
  
  # Lat-lon of all points
  loc_g <- cbind(Year.sub$Lon, Year.sub$Lat)
  # Number of cells
  n_cells <- dim(loc_g)[1]
  # Coerce to spatial points dataframe
  Points_orig = sp::SpatialPointsDataFrame(coords = loc_g, 
                                           data = data.frame(y = Year.sub$D), 
                                           proj4string = CRS_orig)
  # Transform to unprojected lon-lat
  Points_LongLat = sp::spTransform(Points_orig, sp::CRS("+proj=longlat"))
  # Re-transform to projection (NAD83 UTM19N)
  Points_proj = sp::spTransform(Points_orig, CRS_proj)
  # Set cell size
  cell.size = mean(diff(Points_proj@bbox[1, ]), 
                   diff(Points_proj@bbox[2,]))/floor(sqrt(n_cells))
  # Coerce to sf object
  Points_sf = sf::st_as_sf(Points_proj)
  # Make full grid of area
  abgrid = sf::st_make_grid(Points_sf, cellsize = cell.size)
  abgrid_i = sf::st_intersects(Points_sf, abgrid)
  abgrid_i = do.call(rbind, abgrid_i)
  abgrid_i <- abgrid_i[,1]
  abgrid = sf::st_sf(abgrid, prey = tapply(Points_sf$y, 
                                        INDEX = factor(as.numeric(abgrid_i),
                                                       levels = 1:length(abgrid)), 
                                        FUN = mean, na.rm = TRUE))
  abgrid <- st_transform(abgrid, crs="EPSG:4326")
  
  # p <- ggplot() +
  #   geom_sf(data=abgrid, col=NA, aes(fill=y)) +
  #   scale_fill_viridis_c(option="magma",
  #                        na.value=NA,
  #                        direction = -1) +
  #   geom_sf(data=coast) +
  #   coord_sf(xlim=c(-78, -65),
  #            ylim=c(35, 46)) + 
  #   ggtitle(paste0('BFT aggregate prey distribution, June-October ', Year))
  # 
  # plot(p)

  tuna.sub<- st_intersection(abgrid, tuna.sub)
  tuna.sub <- sfheaders::sf_to_df(tuna.sub, fill=T)
  head(tuna.sub); colnames(tuna.sub)
  tuna.sub <- dplyr::select(tuna.sub, -sfg_id, -point_id)
  colnames(tuna.sub) <- c(colnames(tuna.sub)[1:13], 'lon', 'lat')
  tuna.new <- rbind(tuna.new, tuna.sub)
}

tuna.new <- tuna.new[-1,]
row.names(tuna.new) <- NULL
head(tuna.new)
summary(tuna.new$prey)
# 16 NA values: those must be eliminated prior to VAST modeling.

write.csv(tuna.new, row.names = F, 
          here('Data/Clean/BFT_US_catch_VASTdata.csv'))
