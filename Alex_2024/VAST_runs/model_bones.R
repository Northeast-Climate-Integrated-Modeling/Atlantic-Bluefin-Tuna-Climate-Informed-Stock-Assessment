#### BFT VAST ####
#### US ONLY LARGE FISH ONLY ####
#### Gulf of Maine ONLY ####
#### With 2022, no samples east of 69W ####
#### Random walk temporal correlation on LP1 ####
#### LP2 turned off ####
#### All covariates, not scaled ####
#### Natural splines, DF5 ####
#### Fine scale on, bias correct off ####

#### Workspace setup ####
# Clear workspace
rm(list=ls())

# Set wd (HPC USE ONLY)
#setwd("~/VAST_BFT")

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

# Turn warnings off temporarily
options(warn=-1)

#### Add sample information and covars ####
# Load data
lines <- read.csv(here('Data/Clean/LPS_LargeTarget_Clean_2024_2.csv'))
lines <- dplyr::select(lines, id, lines)

surveys <- read.csv(here("Data/Clean/BFT_BothCountries_Large_Prey_VAST_5.csv"))
surveys <- surveys %>% 
  filter(location == 'us') %>% 
  rename(prey = Abundance)

surveys <- left_join(surveys, lines, by=c('id'))
surveys <- surveys %>% 
  drop_na(lines)

# Remove points not in model domain (on land)
region_shape <- st_read(here('Data/GIS/NAFO_5Y.shp'),
                        quiet=T)
region_shape <- as(region_shape, 'Spatial')
region_shape <- st_as_sf(spTransform(region_shape, CRS("+init=epsg:4326")))
region_shape <- st_make_valid(region_shape)
region_shape <- dplyr::select(region_shape, geometry)
region_shape$Region <- 'US'
colnames(region_shape) <- c('geometry', 'Region')
st_geometry(region_shape) <- 'geometry'
region_shape <- dplyr::select(region_shape, Region, geometry)

surveys_sf <- st_as_sf(surveys, coords=c('lon', 'lat'))
st_crs(surveys_sf) <- 'EPSG:4326'

surveys_sf <- st_intersection(surveys_sf, region_shape)
surveys <- sfheaders::sf_to_df(surveys_sf, fill=T)
surveys <- surveys %>% 
  dplyr::select(-Region, -sfg_id, -point_id) %>% 
  rename(sst = oisst,
         lon=x, 
         lat=y)

# Test for missing values
surveys <- surveys[!is.na(surveys$amo) &
                     #!is.na(surveys$nao) &
                     !is.na(surveys$BATHY.DEPTH) &
                     !is.na(surveys$sst) &
                     !is.na(surveys$prey),]
# Lose 104 fishing trips mostly from June-July 2020 (no herring observer data)

#### Finalize sampling data inputs ####
# Save sampling data
# Remove samples east of -69 deg lon
survs <- surveys %>% 
  filter(lon < -69)

# Set variables
survs$RESPONSE <- as_units(survs$catch, 'count')
survs$swept <- as_units(survs$fhours, 'unitless')

survs <- dplyr::select(survs, lon, lat, year, RESPONSE, 
                       lines, 
                       swept)
names(survs) <- c('Lon', 'Lat', 'Year', 'Response_variable', 
                  'lines',
                  'swept')

# Save covariates
covars <- dplyr::select(surveys,
                        lon, lat, year,
                        BATHY.DEPTH, nao, amo, sst, prey)

names(covars) <- c('Lon', 'Lat', 'Year', names(covars)[4:ncol(covars)])

# Rescale covariates to have mean 0 and SD 1 (author rec)
scaled.covars <- covars[,4:ncol(covars)] %>% 
  mutate(across(where(is.numeric), scale))
scaled.covars <- cbind(covars[,1:3], scaled.covars)
scaled.attrs <- scaled.covars
scaled.covars <- data.frame(
  Lon         = as.numeric(scaled.covars$Lon),
  Lat         = as.numeric(scaled.covars$Lat),
  Year        = as.numeric(scaled.covars$Year),
  BATHY.DEPTH = as.numeric(scaled.covars$BATHY.DEPTH),
  nao         = as.numeric(scaled.covars$nao),
  amo         = as.numeric(scaled.covars$amo),
  sst         = as.numeric(scaled.covars$sst),
  prey        = as.numeric(scaled.covars$prey)
)

#### Output success ####
message('Data inputs made successfully')

#### Make region ####
source(here('R_code/utilities/vast_functions.R'))
# Second, get our index area shapefile
# We could just use this same shapefile in the "index_shapes" argument, but to 
# show off the new functions we wrote, we will also want to have a sf multipolygon
# shapefiles with areas defined within this general region

# Load spatial information
index_area_shapes <- region_shape

st_geometry(index_area_shapes) <- 'geometry'
st_geometry(region_shape) <- 'geometry'
index_area_shapes <- st_make_valid(index_area_shapes)

# Finally, run `vast_make_extrap_grid` function after specifying the strata we want to use
strata_use<- data.frame(STRATA = c("US"))
vast_extrap_grid<- vast_make_extrap_grid(region_shapefile = region_shape, 
                                         index_shapes = index_area_shapes, 
                                         cell_size = 1000)
vast_extrap_grid <- dplyr::select(vast_extrap_grid, Lon, Lat, Area_km2, STRATA)
row.names(vast_extrap_grid) <- NULL

# Remove intermediates
rm(list=setdiff(ls(), c("scaled.covars", "settings", 'strata_use', 
                        'survs', 'scaled.attrs',
                        'vast_extrap_grid')))

working_dir <- here::here("VAST_runs")
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
                                           max_cells = 1000
)

# Remove intermediates
rm(list=setdiff(ls(), c("scaled.covars", "settings", 'strata_use', 
                        'survs', 'scaled.attrs',
                        'vast_extrap_grid', 'extrap_info',
                        'working_dir', 'extrap_info_aja')))

#### Output success ####
message('Region mesh successfully made')

#### Run model ####

# Make settings
settings <- make_settings(
  n_x = 100,
  purpose = "index2",
  Region = "User",
  fine_scale = TRUE,
  bias.correct = FALSE,
  knot_method = "grid",
  max_cells = 1000,
  strata.limits = strata_use
)

# Change `ObsModel` settings
# Value 1: Delta-lognormal using bias-corrected-mean and log-coeff of var
# Value 2: Poisson-link delta-model using log-link for numbers density and 
#          log-link for biomass per number
settings$ObsModel[1] <- 4
settings$ObsModel[2] <- 1

# Change `RhoConfig` settings
# Value 1: Each year's intercept for the presence/absence linear predictor is
# a fiexed effect
# Value 2: Same for second linear predictor, abundance.
# Value 3: The spatio-temporal variation structure among time intervals
# is a random effect following a random walk,
# thus estimating variance as fixed effects
# Value 4: Hyperparameters have identical values to the first linear predictor's
# spatio-temporal variation.
settings$RhoConfig[1] <- 0
settings$RhoConfig[2] <- 0
settings$RhoConfig[3] <- 2
settings$RhoConfig[4] <- 0

# Turn off spatial and spatio-temporal effects in second linear predictor
# Model fails with this turned on because basically either 0 or 1 fish are caught in each sample.
# Out of 9365 fishing attempts, only 131 caught 2 or more fish.
# Do not turn off pure temporal effects.
settings$FieldConfig['Omega', 'Component_2'] <- 0
settings$FieldConfig['Epsilon','Component_2'] <- 0

# Turn off anisotropy
settings$use_anisotropy <- FALSE

# Density covariate model formula
hab_formula<- ~ 
  ns(BATHY.DEPTH,  intercept = FALSE, df=5) +
  ns(sst,  intercept = FALSE, df=5) +
  ns(amo,  intercept = FALSE, df=5) +
  ns(prey,  intercept = FALSE, df=5)

# Catchability data
catchability_data <- dplyr::select(survs,
                                  Lon, Lat, Year, lines)

# Turn warnings back on
options(warn=0)

# Model run
fit = fit_model( 
  
  # Call settings
  settings = settings, 
  
  # Turn on joint precision (need for range edges)
  getJointPrecision = TRUE,
  #Use_REML = TRUE,
  
  # Call survey data info
  Lat_i = survs[,'Lat'], 
  Lon_i = survs[,'Lon'], 
  t_i = survs[,'Year'],
  b_i = survs[,'Response_variable'],
  a_i = survs[,'swept'],
  #v_i = survs[,'vessel'],
  
  # Call covariate info
  X1_formula = hab_formula,
  #X2_formula = hab_formula,
  covariate_data = covars,
  
  # Call catchability info
  Q1_formula = ~lines,
  Q2_formula = ~lines,
  catchability_data = catchability_data,
  
  # Call spatial 
  input_grid=vast_extrap_grid,
  "extrapolation_list" = extrap_info_aja,
  
  # Set naming conventions
  #year_labels = year.labs,
  
  # Tell model to build and run
  build_model = TRUE,
  run_model = TRUE
  
)

#### Save results ####

save.image('allcovs_naturalsplines_x1_covsnotscaled_finescaleon_knots5.RData')
