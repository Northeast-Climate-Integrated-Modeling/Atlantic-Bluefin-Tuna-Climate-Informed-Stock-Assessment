#### BFT VAST ####
#### US LARGE FISH ONLY ####

#### Workspace setup ####
# Clear workspace
rm(list=ls())

# Set wd
setwd("~/VAST_BFT")

# Load libraries
suppressPackageStartupMessages(library(TMB))
suppressPackageStartupMessages(library(units, quietly=T, verbose=F))
suppressPackageStartupMessages(library(VAST))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(tidyverse, quietly=T, verbose=F))
suppressPackageStartupMessages(library(sf, quietly=T, verbose=F))
suppressPackageStartupMessages(library(sp, quietly=T, verbose=F))
suppressPackageStartupMessages(library(raster, quietly=T, verbose=F))
suppressPackageStartupMessages(library(splines, quietly=T, verbose=F))
suppressPackageStartupMessages(library(INLAspacetime, quietly=T, verbose=F))

# Add unitless back as possible unit (removed in units package update Mar 2023)
install_unit(symbol='unitless', def='unitless', name='unitless')

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

# Turn off warnings temporarily
options(warn=-1)

#### Add sample information and covars ####
# Load data
surveys <- read.csv(here("Data/Clean/BFT_BothCountries_VAST3.csv"))
surveys <- surveys[surveys$location =='us',]

# Remove points not in model domain (on land)
region_shape <- st_read(here('Data/GIS/NWAtlantic.shp'), quiet=T)
region_shape <- as(region_shape, 'Spatial')
region_shape <- st_as_sf(spTransform(region_shape, CRS("+init=epsg:4326")))
region_shape <- st_make_valid(region_shape)
region_shape <- dplyr::select(region_shape, FID, geometry)
region_shape$FID <- 'US'

surveys_sf <- st_as_sf(surveys, coords=c('lon', 'lat'))
st_crs(surveys_sf) <- 'EPSG:4326'

surveys_sf <- st_intersection(surveys_sf, region_shape)

# Test for missing values
surveys <- surveys[!is.na(surveys$amo) &
                     !is.na(surveys$nao) &
                     !is.na(surveys$BATHY.DEPTH) &
                     !is.na(surveys$sst),]

#### Finalize sampling data inputs ####
# Save sampling data
survs <- surveys

# Set variables
survs$RESPONSE <- as_units(survs$catch, 'counts')
survs$swept <- as_units(survs$fhours, 'unitless')
# survs$vessel <- as.numeric(as.factor(paste0(survs$location, " ",
#                                            survs$Gear))) - 1

survs <- dplyr::select(survs, lon, lat, year, RESPONSE, 
		       # vessel,
                       swept)
names(survs) <- c('Lon', 'Lat', 'Year', 'Response_variable', 
		  # vessel,
                  'swept')

# Save covariates
covars <- dplyr::select(surveys,
                        lon, lat, year,
                        BATHY.DEPTH, nao, amo, sst)

names(covars) <- c('Lon', 'Lat', 'Year', names(covars)[4:ncol(covars)])

# Rescale covariates to have mean 0 and SD 1 (author rec)
scaled.covars <- covars[,4:ncol(covars)] %>% 
  mutate(across(where(is.numeric), scale))
scaled.covars <- cbind(covars[,1:3], scaled.covars)
scaled.covars <- data.frame(
  Lon         = as.numeric(scaled.covars$Lon),
  Lat         = as.numeric(scaled.covars$Lat),
  Year        = as.numeric(scaled.covars$Year),
  BATHY.DEPTH = as.numeric(scaled.covars$BATHY.DEPTH),
  nao         = as.numeric(scaled.covars$nao),
  amo         = as.numeric(scaled.covars$amo),
  sst         = as.numeric(scaled.covars$sst)
)

#### Output success ####
print('Data inputs made successfully')

#### Make region ####
source(here('R_code/utilities/vast_functions.R'))
# First, we need our region shapefile
region_shape<- st_read(here::here("Data/GIS/NWAtlantic.shp"), quiet=T)
region_shape <- as(region_shape, 'Spatial')
region_shape <- st_as_sf(spTransform(region_shape, CRS("+init=epsg:4326")))
region_shape <- st_make_valid(region_shape)
region_shape <- dplyr::select(region_shape, FID, geometry)
region_shape$FID <- 'US'
colnames(region_shape) <- c("Region", 'geometry')

# Second, get our index area shapefile
# We could just use this same shapefile in the "index_shapes" argument, but to 
# show off the new functions we wrote, we will also want to have a sf multipolygon
# shapefiles with areas defined within this general region
index_areas<- c("US")

# Load spatial information
stocks <- st_read(here('Data/GIS/NWAtlantic.shp'), quiet=T)
stocks <- as(stocks, 'Spatial')
stocks <- st_as_sf(spTransform(stocks, CRS("+init=epsg:4326")))
stocks$Region <- 'US'
stocks <- dplyr::select(stocks, -FID)
stocks <- st_make_valid(stocks)

for(i in 1:length(index_areas)){
  areause <- index_areas[i]
  index_area_temp<- stocks[stocks$Region == paste0(areause),]
  index_area_temp <- as(index_area_temp, 'Spatial')
  index_area_temp <- st_as_sf(spTransform(index_area_temp, CRS("+init=epsg:4326")))
  index_area_temp <- st_make_valid(index_area_temp)
  index_area_temp <- dplyr::select(index_area_temp, Region, geometry)
  colnames(index_area_temp) <- c("Region", 'geometry')
  
  if(i == 1){
    index_area_shapes<- index_area_temp
  } else {
    index_area_shapes<- bind_rows(index_area_shapes, index_area_temp)
  }
}

index_area_shapes <- bind_rows(index_area_shapes, region_shape)

index_area_shapes <- st_make_valid(index_area_shapes)

# Finally, run `vast_make_extrap_grid` function after specifying the strata we want to use
strata_use<- data.frame(STRATA = c("US"))
vast_extrap_grid<- vast_make_extrap_grid(region_shapefile = region_shape, 
                                         index_shapes = index_area_shapes, 
                                         cell_size = 1000)
vast_extrap_grid <- dplyr::select(vast_extrap_grid, Lon, Lat, Area_km2, STRATA)
row.names(vast_extrap_grid) <- NULL

# Remove intermediates
rm(list=setdiff(ls(), c("scaled.covars", "settings", 'strata_use', 'survs',
                        'vast_extrap_grid')))

working_dir <- here::here("VAST_runs/tuna10_usonly")
# Create working directory if it doesn't exist
if(!dir.exists(working_dir)) {
  dir.create(working_dir)
}

setwd(working_dir)

source(here('R_code/utilities/vast_function_edits.R'))
use_edited_funs<- TRUE
if (use_edited_funs) {
  source(here::here("R_code/utilities/vast_function_edits.R"))
  assignInNamespace("match_strata_fn", 
                    match_strata_fn, 
                    "FishStatsUtils")
  assignInNamespace("Prepare_User_Extrapolation_Data_Fn", 
                    Prepare_User_Extrapolation_Data_Fn, 
                    "FishStatsUtils")
}

extrap_info_aja <- make_extrapolation_info(Region="User",
                                           strata.limits=strata_use,
                                           input_grid = vast_extrap_grid,
                                           max_cells = 3000
)

# Remove intermediates
rm(list=setdiff(ls(), c("scaled.covars", "settings", 'strata_use', 'survs',
                        'vast_extrap_grid', 'extrap_info',
                        'working_dir', 'extrap_info_aja')))

#### Output success ####
print('Region mesh successfully made')

#### Run model ####
# Set labels
year.labs <- seq(2002, 2022)

# Make settings
settings <- make_settings(
  n_x = 300,
  purpose = "index2",
  Region = "User",
  fine_scale = TRUE,
  bias.correct = FALSE,
  knot_method = "grid",
  max_cells = 3000,
  strata.limits = strata_use
)

# Change `ObsModel` settings
# Value 1: Delta-lognormal using bias-corrected-mean and log-coeff of var
# Value 2: Poisson-link delta-model using log-link for numbers density and 
#          log-link for biomass per number
settings$ObsModel[1] <- 4
settings$ObsModel[2] <- 1

# Change `RhoConfig` settings
# Value 1: The spatio-temporal variation structure among time intervals
# is a random effect following a first-order autoregressive process,
# thus estimating variance as fixed effects and a separate first-order
# AR parameter for each category
# Value 2: Each time step as a fixed effect (cannot have temporal structure
# when model does not converge with spatio-temporal effects in second linear predictor)
settings$RhoConfig[3] <- 4
settings$RhoConfig[4] <- 0

# Turn off spatio-temporal effects in second linear predictor
settings$FieldConfig['Epsilon','Component_2'] <- 0

# Turn off anisotropy
settings$use_anisotropy <- FALSE

# Density covariate model formula
hab_formula<- ~ 
  bs(BATHY.DEPTH, degree = 3, intercept = FALSE) +
  bs(sst, degree = 3, intercept = FALSE) +
  bs(nao, degree = 3, intercept = FALSE) +
  bs(amo, degree = 3, intercept = FALSE)

# Turn warnings back on
options(warn=0)

# Model run
fit = fit_model( 
  
  # Call settings
  settings = settings, 
  
  # Turn on joint precision (need for range edges)
  getJointPrecision = TRUE,
  
  # Call survey data info
  Lat_i = survs[,'Lat'], 
  Lon_i = survs[,'Lon'], 
  t_i = survs[,'Year'],
  b_i = survs[,'Response_variable'],
  a_i = survs[,'swept'],
  # v_i = survs[,'vessel'],
  
  # Call covariate info
  X1_formula = hab_formula,
  X2_formula = hab_formula,
  covariate_data = scaled.covars,
  
  # Call spatial 
  input_grid=vast_extrap_grid,
  "extrapolation_list" = extrap_info_aja,
  
  # Set naming conventions
  year_labels = year.labs,
  
  # Tell model to build but not run
  build_model = TRUE,
  run_model = TRUE
  
)

#### Save results ####

save.image('tuna10_usonly.RData')