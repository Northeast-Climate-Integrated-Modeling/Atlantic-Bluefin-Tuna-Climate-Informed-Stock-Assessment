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

# Load herring data
herring <- read.csv(here('VAST_LPS/herring.csv'))
head(herring)
herring$X <- NULL

# Filter herring data
herring <- herring %>% 
  drop_na(Lon) %>% 
  drop_na(Lat) %>% 
  filter(MONTH %in% c(6,7,8,9,10)) %>% 
  filter(YEAR > 1992 & YEAR < 2022)

# Plot to see locations
herring.sf <- st_as_sf(herring, coords=c('Lon', 'Lat'))
st_crs(herring.sf) <- 'EPSG:4326'
coast <- ecodata::coast
coast <- st_transform(coast, st_crs(herring.sf))

ggplot() +
  geom_sf(data=coast, fill='gray')+
  geom_sf(data=herring.sf, aes(col=YEAR)) +
  coord_sf(xlim=c(-79, -66),
           ylim=c(35, 45))

# Ok, pretty concentrated to GOM
# Would be great to get Menhaden too, expect more coverage to the South.

# Attempt to run VAST model to estimate herring abundance across tuna polygon?
herring$row <- seq(1:nrow(herring))
herring.sf$row <- seq(1:nrow(herring.sf))
nwat <- st_read(here('Data/GIS/NWAtlantic.shp'))
nwat <- st_transform(nwat, st_crs(herring.sf))
herring.in <- st_intersection(herring.sf, nwat)

herring <- herring[herring$row %in% herring.in$row,]
herring.sf <- herring.sf[herring.sf$row %in% herring.in$row,]

herring$wt_kgs <- herring$wt_lbs * 0.45359237
survs <- dplyr::select(herring, YEAR, MONTH, Lat, Lon, wt_kgs)

survs$wt_kgs <- as_units(survs$wt_kgs, 'kg')
survs$Effort <- as_units(rep(1, nrow(survs)), 'unitless')

## Run VAST
library(VAST)

setwd(here('VAST_runs/herring1'))
# Load grid
grid_NWA_BTS <- readRDS(here("Data/VAST_regions/tuna_region.rds"))
grid_sf <- st_as_sf(grid_NWA_BTS, coords=c('Lon', 'Lat'))
st_crs(grid_sf) <- 'EPSG:4326'

# Oops. Remove gridpoints overlapping with land
coast <- ecodata::coast
coast <- st_transform(coast, crs=st_crs(grid_sf))
overland <- st_intersection(grid_sf, coast)

grid_sf <- grid_sf[grid_sf$row  %notin% overland$row,]
grid_NWA_BTS <- grid_NWA_BTS[grid_NWA_BTS$row %notin% overland$row,]

ggplot(coast)+
  geom_sf(fill='gray') +
  geom_sf(data=grid_sf) +
  geom_sf(data=herring.sf, col='red', cex=0.5) +
  coord_sf(xlim=c(-79, -65),
           ylim=c(35, 45))

# Make settings
settings <- make_settings(
  n_x = 200,
  purpose = "index2",
  Region = "User",
  fine_scale = FALSE,
  bias.correct = FALSE,
  knot_method = "grid"#,
  #max_cells = 5000
)

#settings$ObsModel[1] <- 4
#settings$ObsModel[2] <- 1

#settings$FieldConfig['Epsilon', 'Component_2'] <- 0

fit = fit_model( 
  # Set wd
  #working_dir = here('VAST_runs/tuna1'),
  
  # Call settings
  settings = settings, 
  
  # Call survey data info
  Lat_i = survs[,'Lat'], 
  Lon_i = survs[,'Lon'], 
  t_i = survs[,'YEAR'],
  #b_i = survs[,'Response_variable'],
  #a_i = survs[,'Effort'],
  b_i = survs[,'wt_kgs'],
  a_i = survs[,'Effort'],
  #c_iz = survs[,'Category'],
  
  # Call covariate info
  #X1_formula = ~ sst + depth,
  #X2_formula = ~ sst + depth, 
  #covariate_data = scaled.covars,
  
  # Call spatial 
  input_grid=grid_NWA_BTS,
  
  # Set naming conventions
  #category_names = cat.labs,
  #year_labels = year.labs,
  
  # Tell model to run
  run_model = TRUE,
  build_model = TRUE)


#beep(8)

#### Plot results ####
rm(coast, dat_sf, grid_sf, overland)
save.image('herring_1.RData')

plot( fit )
beep(8)