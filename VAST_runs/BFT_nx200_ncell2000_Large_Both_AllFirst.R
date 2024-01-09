#### BFT VAST ####
#### BOTH COUNTRIES LARGE FISH ONLY ####

#### Workspace setup ####
# Clear workspace
rm(list=ls())

# Set wd
setwd("~/VAST_BFT")

# Load libraries
# IMPORTANT NOTE: VAST must be running >=V14, will not work with V13.
library(TMB)
library(units)
library(VAST)
library(here)
library(tidyverse)
#library(beepr)
library(sf)
#library(rgdal)
library(sp)
library(raster)
#library(ggcorrplot)
library(splines)  # Used to include basis-splines
library(INLAspacetime) # used for mesh-building
#library(INLA)

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

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

#### Add sample information and covars ####
# Load data
surveys <- read.csv(here("Data/Clean/BFT_BothCountries_VAST3.csv"))

# Remove points not in model domain (on land)
region_shape <- st_read(here('Data/GIS/Combined_Bluefin2.shp'))
region_shape <- as(region_shape, 'Spatial')
region_shape <- st_as_sf(spTransform(region_shape, CRS("+init=epsg:4326")))
#region_shape <- st_transform(region_shape, "EPSG:4326")
region_shape <- st_make_valid(region_shape)
region_shape <- dplyr::select(region_shape, FID_1, geometry)
region_shape$FID_1 <- 'ALL'

surveys_sf <- st_as_sf(surveys, coords=c('lon', 'lat'))
st_crs(surveys_sf) <- 'EPSG:4326'

surveys_sf <- st_intersection(surveys_sf, region_shape)

# Test for missing values
summary(surveys)
surveys <- surveys[!is.na(surveys$amo) &
                     !is.na(surveys$nao) &
                     !is.na(surveys$BATHY.DEPTH) &
                     !is.na(surveys$sst),]

#### Finalize sampling data inputs ####
# Save sampling data
survs <- surveys

# Remove 2022 data
#survs <- surveys[surveys$year < 2022,]

survs$RESPONSE <- as_units(survs$catch, 'counts')
survs$swept <- as_units(survs$fhours, 'unitless')
survs$class <- paste0(survs$location, " ", survs$Gear)
survs$vessel <- as.numeric(as.factor(paste0(survs$location, " ",
                                            survs$Gear))) - 1
# Can Harpoon: 0
# Can RodReel: 1
# Can Tendedline: 2
# US RodReel: 3

# survs$AGEGROUP <- as.numeric(factor(survs$Size_class, levels=c(
#   'Small', 
#   'Large'
#   )
#   )) - 1
# # Small: 0
# # Large: 1

survs <- dplyr::select(survs, lon, lat, year, RESPONSE, 
                       #AGEGROUP, 
                       vessel, 
                       swept)
names(survs) <- c('Lon', 'Lat', 'Year', 'Response_variable', 
                  #'Age', 
                  'vessel', 
                  'swept')
str(survs)
summary(survs)

# Save covariates
covars <- dplyr::select(surveys,
                        lon, lat, year,
                        BATHY.DEPTH, nao, amo, sst)

names(covars) <- c('Lon', 'Lat', 'Year', names(covars)[4:ncol(covars)])
table(covars$Year)
#covars <- covars[covars$Size_class == 'Large',]


# Test correlation
# Create correlation matrix
# df_cormat <- dplyr::select(covars,                         
#                           BATHY.DEPTH, nao, amo, sst)
# model.matrix(~0+., data=df_cormat) %>%
#   cor(use="all.obs", method="spearman") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3)

# Rescale covariates to have mean 0 and SD 1 (author rec)
scaled.covars <- covars[,4:ncol(covars)] %>% 
  mutate(across(where(is.numeric), scale))
scaled.covars <- cbind(covars[,1:3], scaled.covars)
summary(scaled.covars)
scaled.covars <- data.frame(
  Lon         = as.numeric(scaled.covars$Lon),
  Lat         = as.numeric(scaled.covars$Lat),
  Year        = as.numeric(scaled.covars$Year),
  BATHY.DEPTH = as.numeric(scaled.covars$BATHY.DEPTH),
  nao         = as.numeric(scaled.covars$nao),
  amo         = as.numeric(scaled.covars$amo),
  sst         = as.numeric(scaled.covars$sst)
)
str(scaled.covars)
summary(scaled.covars)

#### Make region ####
source(here('R_code/utilities/vast_functions.R'))
# First, we need our region shapefile
region_shape<- st_read(here::here("Data/GIS/Combined_Bluefin2.shp"))
region_shape <- as(region_shape, 'Spatial')
region_shape <- st_as_sf(spTransform(region_shape, CRS("+init=epsg:4326")))
#region_shape <- st_transform(region_shape, "EPSG:4326")
region_shape <- st_make_valid(region_shape)
region_shape <- dplyr::select(region_shape, FID_1, geometry)
region_shape$FID_1 <- 'ALL'
colnames(region_shape) <- c("Region", 'geometry')

# Second, get our index area shapefile
# We could just use this same shapefile in the "index_shapes" argument, but to 
# show off the new functions we wrote, we will also want to have a sf multipolygon
# shapefiles with areas defined within this general region
index_areas<- c("US", "Canada")

# Load spatial information
coast <- ecodata::coast
#coast <- st_transform(coast, "EPSG:4326")
coast <- as(coast, 'Spatial')
proj4string(coast) <- CRS("+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
coast <- st_as_sf(spTransform(coast, CRS("+init=epsg:4326")))
us <- st_read(here('Data/GIS/NWAtlantic.shp'), quiet=T)
us <- as(us, 'Spatial')
us <- st_as_sf(spTransform(us, CRS("+init=epsg:4326")))
#us <- st_transform(us, st_crs(coast))
us$Region <- 'US'
us <- dplyr::select(us, -FID)
can <- st_read(here('Data/GIS/CanadaEEZ.shp'), quiet=T)
can <- st_transform(can, st_crs(coast))
can$Region <- 'Canada'
can <- dplyr::select(can, -OBJECTID, -Id, -Shape_Leng, -Shape_Area)
stocks <- rbind(us, can)

stocks <- as(stocks, 'Spatial')
stocks <- st_as_sf(spTransform(stocks, CRS("+init=epsg:4326")))
#stocks <- st_transform(stocks, st_crs(coast))
stocks <- st_make_valid(stocks)

for(i in 1:length(index_areas)){
  areause <- index_areas[i]
  index_area_temp<- stocks[stocks$Region == paste0(areause),]
  index_area_temp <- as(index_area_temp, 'Spatial')
  index_area_temp <- st_as_sf(spTransform(index_area_temp, CRS("+init=epsg:4326")))
  #index_area_temp <- st_transform(index_area_temp, "EPSG:4326")
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
strata_use<- data.frame(STRATA = c("ALL", "US", "Canada"))
vast_extrap_grid<- vast_make_extrap_grid(region_shapefile = region_shape, 
                                         index_shapes = index_area_shapes, 
                                         #strata.limits = strata_use, 
                                         cell_size = 1000)
vast_extrap_grid <- dplyr::select(vast_extrap_grid, Lon, Lat, Area_km2, STRATA)
row.names(vast_extrap_grid) <- NULL

# Remove intermediates
rm(list=setdiff(ls(), c("scaled.covars", "settings", 'strata_use', 'survs',
                        'vast_extrap_grid')))
gc()

working_dir <- here::here("VAST_runs/tuna10_both")
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
                                           max_cells = 2000
)
head(extrap_info_aja$a_el)
head(extrap_info_aja$Data_Extrap)

# Remove intermediates
rm(list=setdiff(ls(), c("scaled.covars", "settings", 'strata_use', 'survs',
                        'vast_extrap_grid', 'extrap_info',
                        'working_dir', 'extrap_info_aja')))
gc()

# saveRDS(extrap_info_aja,
#         here('VAST_runs/add_climate_aja7/extrap_info_aja7.RDS'))
# saveRDS(vast_extrap_grid,
#         here('VAST_runs/add_climate_aja7/vast_extrap_grid_aja7.RDS'))


#### Run model ####
# Only larges for this run
# survs <- survs[survs$Age == 1,]
#scaled.covars <- unique(scaled.covars)

# Set year, category, strata labels
#cat.labs <- c('Small', 'Large')
year.labs <- seq(2002, 2022)
strat.labs <- c('ALL', 'US', 'Canada')

# Make settings
settings <- make_settings(
  n_x = 400,
  purpose = "index2",
  Region = "User",
  fine_scale = TRUE,
  bias.correct = FALSE,
  knot_method = "grid",
  max_cells = 2000,
  strata.limits = strata_use
)

getwd()
# Change `ObsModel` settings
# Value 1: Delta-lognormal using bias-corrected-mean and log-coeff of var
# Value 2: Poisson-link delta-model" using log-link for numbers density and 
#          log-link for biomass per number
settings$ObsModel[1] <- 4
settings$ObsModel[2] <- 1

# Change `RhoConfig` settings
# Value 1: The spatio-temporal variation structure among time intervals
# is a random effect following a first-order autoregressive process,
# thus estimating variance as fixed effects and a separate first-order
# AR parameter for each category
# Value 2: Copy values from first predictor to second predictor
settings$RhoConfig[3] <- 5
settings$RhoConfig[4] <- 6

# Correlated overdispersion among categories for each level of v_1
settings$OverdispersionConfig

# Model does not like spatial variation in second linear predictor
settings$FieldConfig['Omega','Component_2'] <- 0

# Density covariate model formula
hab_formula<- ~ 
  bs(BATHY.DEPTH, degree = 3, intercept = FALSE) +
  bs(sst, degree = 3, intercept = FALSE) +
  bs(nao, degree = 3, intercept = FALSE) +
  bs(amo, degree = 3, intercept = FALSE)

fit = fit_model( 
  
  # Call settings
  settings = settings, 
  
  # Turn on joint precision (need for range edges)
  #getJointPrecision = TRUE,
  
  # Call survey data info
  Lat_i = survs[,'Lat'], 
  Lon_i = survs[,'Lon'], 
  t_i = survs[,'Year'],
  b_i = survs[,'Response_variable'],
  a_i = survs[,'swept'],
  v_i = survs[,'vessel'],
  #c_iz = survs[,'Age'],
  
  # Call covariate info
  X1_formula = hab_formula,
  X2_formula = hab_formula,
  covariate_data = scaled.covars,
  
  # Call spatial 
  input_grid=vast_extrap_grid,
  "extrapolation_list" = extrap_info_aja,
  
  # Set naming conventions
  #category_names = cat.labs,
  year_labels = year.labs,
  #strata_labels = strat.labs,
  
  # Tell model to build but not run
  build_model = TRUE,
  run_model = FALSE
  
)

#### Plot results ####

save.image('tuna10_both_allfirst.RData')

plot( fit )