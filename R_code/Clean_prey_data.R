rm(list=ls())

# Load libraries and functions
library(tidyverse)
library(here)
library(sf)
library(units)
library(TMB)
library(VAST)
# Add unitless back as possible unit (removed in units package update Mar 2023)
#install_unit(symbol='unitless', def='unitless', name='unitless')
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

# Load prey data
prey <- read.csv(here('Data/Prey_Data/bft_prey.csv'))
head(prey)
prey$X <- NULL

# Filter prey data
prey <- prey %>% 
  drop_na(Lon) %>% 
  drop_na(Lat) %>% 
  filter(MONTH %in% c(6,7,8,9,10)) %>% 
  filter(YEAR > 1992 & YEAR < 2022)

# Plot to see locations
prey.sf <- st_as_sf(prey, coords=c('Lon', 'Lat'))
st_crs(prey.sf) <- 'EPSG:4326'
coast <- ecodata::coast
coast <- st_transform(coast, st_crs(prey.sf))

ggplot() +
  geom_sf(data=coast, fill='gray')+
  geom_sf(data=prey.sf, aes(col=YEAR)) +
  coord_sf(xlim=c(-79, -66),
           ylim=c(35, 45))

# Solid coverage.

# Convert lbs to kgs
prey$wt_kgs <- prey$wt_lbs * 0.45359237

# Remove columns
prey <- dplyr::select(prey, -wt_lbs, -OBGEARCAT, -CATDISP)
prey.sf <- st_as_sf(prey, coords=c('Lon', 'Lat'))
st_crs(prey.sf) <- 'EPSG:4326'

# Cut to tuna polygon
nwat <- st_read(here('Data/GIS/NWAtlantic.shp'))
nwat <- st_transform(nwat, st_crs(prey.sf))
prey.sf <- st_intersection(prey.sf, nwat)

# Remove locations on land
prey.sf$row <- seq(1:nrow(prey.sf))
onland <- st_intersection(prey.sf, coast)

prey.sf <- prey.sf[prey.sf$row %notin% onland$row,]
prey.sf$row <- NULL

# Plot
ggplot() +
  geom_sf(data=coast, fill='gray')+
  geom_sf(data=prey.sf, aes(col=YEAR)) +
  coord_sf(xlim=c(-79, -66),
           ylim=c(35, 45))

# Great. save.
prey <- sfheaders::sf_to_df(prey.sf, fill=T)
prey <- dplyr::select(prey, -FID, -sfg_id, -point_id)
colnames(prey) <- c(colnames(prey)[1:4], 'lon', 'lat')
colnames(prey) <- tolower(colnames(prey))

write.csv(prey, row.names = F, here('Data/Prey_Data/Clean_prey_data.csv'))
