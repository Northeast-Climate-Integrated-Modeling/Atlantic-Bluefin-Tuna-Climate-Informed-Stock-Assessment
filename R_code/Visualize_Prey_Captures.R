#### Workspace setup ####
rm(list=ls())

# Load libraries
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(tidyverse, quietly=T, verbose=F))
suppressPackageStartupMessages(library(sf, quietly=T, verbose=F))
suppressPackageStartupMessages(library(sp, quietly=T, verbose=F))

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', linewidth=1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

#### Add sample information and covars ####
# Load data
surveys <- read.csv(here('Data/Prey_Data/Prey_dist_to_shore.csv'))
fishcat <- data.frame(
  comname=unique(surveys$comname),
  fish = c('mackerel', 'herring', 'combo',
           'herring', 'mackerel', 'mackerel',
           'menhaden', 'mackerel', 'mackerel',
           'mackerel', 'mackerel')
)
gearcats <- data.frame(
     obgearcat = unique(surveys$obgearcat),
     gear = c('gillnet', 'otter trawl', 'pair trawl',
              'scallop dredge', 'purse seine', 'longline and other line', 'twin trawl', 'pots and traps', 'unknown')
)
surveys <- left_join(surveys, fishcat, by=c('comname'))
surveys <- left_join(surveys, gearcats, by=c('obgearcat'))

# Remove unusual gears
surveys <- surveys[surveys$gear %in% c('gillnet',
                                       'otter trawl', 
                                       'pair trawl'),]

# Remove points not in model domain (on land)
region_shape <- st_read(here('Data/GIS/NAFO_Continental_Shelf_10kmBuff.shp'), 
                        quiet=T)
region_shape <- region_shape[region_shape$Region == 'US',]
region_shape <- as(region_shape, 'Spatial')
region_shape <- st_as_sf(spTransform(region_shape, CRS("+init=epsg:4326")))
region_shape <- st_make_valid(region_shape)
region_shape <- dplyr::select(region_shape, Region, Label, geometry)

surveys_sf <- st_as_sf(surveys, coords=c('lon', 'lat'))
st_crs(surveys_sf) <- 'EPSG:4326'

surveys_sf <- st_intersection(surveys_sf, region_shape)

# Add closed areas
closed.areas <- st_read(here("Data/GIS/Gillnet_Trawl_Closed_Areas.shp"))

# Plot
ggplot() +
  geom_sf(data=region_shape, fill=NA, col='gray20') +
  geom_sf(data=closed.areas, fill='gray90', col=NA) +
  geom_sf(data=surveys_sf, aes(col=fish), cex=0.2) +
  facet_wrap(vars(gear)) +
  labs(x=' ', y=' ', color='Targeted species')
