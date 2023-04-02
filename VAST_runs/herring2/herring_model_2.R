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
herring <- read.csv(here('Data/Prey_Data/Clean_prey_data.csv'))
head(herring)
summary(herring)

# Create new time series of month-year
#herring$yrmo <- paste0(herring$year,
#                       str_pad(herring$month, 2, "left", "0"))


herring.sf <- st_as_sf(herring, coords=c('lon', 'lat'))
st_crs(herring.sf) <- 'EPSG:4326'

# Run VAST
setwd(here('VAST_runs/herring2'))
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

# Alter herring data for inclusion
survs <- dplyr::select(herring,
                       lon, lat, year, month, wt_kgs)
survs$Effort <- as_units(rep(1, nrow(survs)), 'unitless')
#survs$time <- as.numeric(as.factor(as.numeric(survs$yrmo)))

# year.labs <- seq(1993, 2021)
# year.labs <- as.data.frame(rep(year.labs, 5))
# year.labs$month <- NA
# year.labs$month[1:29] <- 'June'
# year.labs$month[30:58] <- 'July'
# year.labs$month[59:87] <- 'August'
# year.labs$month[88:116] <- 'September'
# year.labs$month[117:145] <- 'October'
# table(year.labs$`rep(year.labs, 5)`, year.labs$month)
# year.labs$year.labs <- paste0(year.labs$`rep(year.labs, 5)`, " ", year.labs$month)
# year.labs <- as.vector(year.labs$year.labs)

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

# Change settings to reflect 100% encounter rate
settings$ObsModel[2] <- 4

# Turn off omega values
#settings$FieldConfig['Omega',] <- c(0,0)
#settings$FieldConfig['Epsilon',] <- c(0,0)

fit = fit_model( 
  # Set wd
  #working_dir = here('VAST_runs/tuna1'),
  
  # Call settings
  settings = settings, 
  
  # Call survey data info
  Lat_i = survs[,'lon'], 
  Lon_i = survs[,'lat'], 
  t_i = survs[,'year'],
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
save.image('herring_2.RData')

plot( fit )
beep(8)
