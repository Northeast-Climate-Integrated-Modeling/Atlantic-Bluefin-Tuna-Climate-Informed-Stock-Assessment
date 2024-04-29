rm(list=ls())

library(tidyverse)
library(here)
library(sf)

# Load vast grid
load(here('VAST_runs/tuna13_usonly/gom_only/tuna13_usonly_gomonly_0222_no04.RData'))
# Remove intermediates
rm(list=setdiff(ls(), c('extrap_info_aja',
                        'vast_extrap_grid', 
                        'fit')))

# Load shapefiles
gom <- st_read(here('Data/GIS/NAFO_5Y.shp'), quiet=T)
gom <- st_make_valid(gom)
gom <- st_transform(gom, crs="EPSG:4326")

coast <- st_transform(ecodata::coast, crs="EPSG:4326")

# Convert data to SF
survs.sf <- st_as_sf(fit$data_frame, coords=c('Lon_i', 'Lat_i'),
                     crs="EPSG:4326")

# Remake map list locally for recreating plots
mdl <- make_map_info(Region = fit$extrapolation_list$Area_km2_x,
                     spatial_list = fit$spatial_list,
                     Extrapolation_List = fit$extrapolation_list)
gridcells <- as.data.frame(mdl$PlotDF)
gridcells$ID <- seq(1:nrow(gridcells))

# Set CRS 
projargs <- fit$extrapolation_list$projargs
CRS_orig = sp::CRS("+proj=longlat")
CRS_proj = sp::CRS(projargs)

# Set center of cells
loc_g <- cbind(gridcells$Lon, 
               gridcells$Lat)

n_cells <- dim(loc_g)[1]

Points_orig = sp::SpatialPointsDataFrame(coords = loc_g, 
                                         data = gridcells, 
                                         proj4string = CRS_orig)

Points_LongLat = sp::spTransform(Points_orig, sp::CRS("+proj=longlat"))

Points_proj = sp::spTransform(Points_orig, CRS_proj)

Points_sf = sf::st_as_sf(Points_proj)

survs.sf$x2i = fit$spatial_list$knot_i

ggplot() + 
  geom_sf(data=Points_sf) +
  geom_sf(data=survs.sf, aes(col=as.factor(x2i)))

test <- survs.sf %>% 
  group_by(x2i) %>% 
  summarise(Effort = mean(strip_units(a_i)))
test <- st_transform(test, st_crs(survs.sf))
Points_sf <- st_transform(Points_sf, st_crs(survs.sf))

test <- sfheaders::sf_to_df(test, fill=T)
test <- test %>% 
  dplyr::select(-sfg_id, -multipoint_id, -point_id, -x, -y) %>% 
  unique()
Points_sf <- sfheaders::sf_to_df(Points_sf, fill=T)
Points_sf <- Points_sf %>% 
  dplyr::select(-Include, -ID, -sfg_id, -point_id, -x, -y)

test2 <- left_join(Points_sf, test)
test2 <- st_as_sf(test2, coords=c('Lon', 'Lat'), crs="EPSG:4326")

test2 <- st_transform(test2, crs="EPSG:32619")
coast <- st_transform(coast, crs="EPSG:32619")
gom <- st_transform(gom, crs="EPSG:32619")

ggplot() +
  geom_sf(data=test2, aes(col=Effort), cex=1.5) +
  geom_sf(data=coast, col=NA) +
  geom_sf(data=gom, fill=NA) +
  scale_color_viridis_c(option='viridis',
                        na.value=NA) +
  coord_sf(xlim=c(-71, -67),
           ylim=c(41.5, 45.5),
           crs="EPSG:4326") +
  labs(col='Mean effort\n(hrs)', fill='Mean effort\n(hrs)') +
  ggtitle('Mean effort per knot (k=100),\n2002-2022 *') +
  theme(legend.position = 'right')
