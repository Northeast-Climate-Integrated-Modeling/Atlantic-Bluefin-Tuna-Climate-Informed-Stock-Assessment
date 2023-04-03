rm(list=ls())

# Load libraries and functions
library(here)
library(tidyverse)
library(sf)
library(TMB)
library(VAST)
library(units)
library(beepr)
# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))
# Add unitless back as possible unit (removed in units package update Mar 2023)
install_unit(symbol='unitless', def='unitless', name='unitless')
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

# Load data
dat <- read.csv(here('Data/Prey_Data/Clean_prey_data.csv'))
dat_sf <- st_as_sf(dat, coords=c('lon', 'lat'))
st_crs(dat_sf) <- 'EPSG:4326'

#### Run VAST ####
setwd(here('VAST_runs/herring3'))
#cat.labs <- c('Small', 'Large')
year.labs <- seq(1993, 2021)

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

survs <- dplyr::select(dat,
                       wt_kgs, year, lon, lat)
colnames(survs) <- c('Response_variable',
                     'Year', 'Lon', 'Lat')
survs$Response_variable <- as_units(as.numeric(survs$Response_variable), 'unitless')
survs$effort.unitless <- as_units(1, 'unitless')
#survs$effort.unitless <- 1
str(survs)

# Load grid
grid_NWA_BTS <- readRDS(here("Data/VAST_regions/herring_region.rds"))
grid_sf <- st_as_sf(grid_NWA_BTS, coords=c('Lon', 'Lat'))
st_crs(grid_sf) <- 'EPSG:4326'

# Oops. Remove gridpoints overlapping with land
coast <- ecodata::coast
coast <- st_transform(coast, crs=st_crs(grid_sf))

ggplot(coast)+
  geom_sf(fill='gray') +
  geom_sf(data=grid_sf) +
  geom_sf(data=dat_sf, col='red', cex=0.5) +
  coord_sf(xlim=c(-79, -65),
           ylim=c(35, 45))

settings$ObsModel[2] <- 4
settings$Options["treat_nonencounter_as_zero"] <- FALSE
#settings$FieldConfig["Omega", "Component_1"] <- 0
#settings$FieldConfig["Epsilon", "Component_1"] <- 0

#settings$FieldConfig['Epsilon', 'Component_2'] <- 0

fit = fit_model( 
  # Set wd
  #working_dir = here('VAST_runs/tuna1'),
  
  # Up newton steps
  #newtonsteps = 1,
  
  # Call settings
  settings = settings, 
  
  # Call survey data info
  Lat_i = survs[,'Lat'], 
  Lon_i = survs[,'Lon'], 
  t_i = survs[,'Year'],
  b_i = survs[,'Response_variable'],
  #a_i = survs[,'Effort'],
  #b_i = survs[,'CPUE'],
  a_i = survs[,'effort.unitless'],
  #c_iz = survs[,'Category'],
  
  # Call covariate info
  #X1_formula = ~ sst + depth,
  #X2_formula = ~ sst + depth, 
  #covariate_data = scaled.covars,
  
  # Call spatial 
  input_grid=grid_NWA_BTS,
  
  # Set naming conventions
  #category_names = cat.labs,
  year_labels = year.labs,
  
  # Tell model to run
  run_model = TRUE,
  build_model = TRUE)


#beep(8)

#### Plot results ####
rm(coast, dat_sf, grid_sf)
save.image('herring_3.RData')

plot( fit )
beep(8)