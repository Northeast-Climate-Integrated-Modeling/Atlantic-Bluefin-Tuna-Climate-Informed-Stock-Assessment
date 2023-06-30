rm(list=ls())

# Load libraries and functions
library(here)
library(tidyverse)
library(sf)
library(TMB)
library(VAST)
library(units)
library(beepr)
library(ggcorrplot)
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
dat <- read.csv(here('Data/Clean/BFT_BothCountries_VAST.csv'))

# View
head(dat)
summary(dat)

# Remove unknowns
dat <- dat[dat$Size_class != 'Unknown',]

# Remove observations without environmental data
dat <- dat[!is.na(dat$slp),]

# Remove American 2021 (need Canadian 2021)
dat <- dat[dat$year < 2021,]

# Check distribution
table(dat$year, dat$Size_class)

# Convert to sf for plotting
dat_sf <- st_as_sf(dat, coords=c('lon', 'lat'))
st_crs(dat_sf) <- 'EPSG:4326'

# Plot
bluefin.zone <- st_read(here('Data/GIS/Combined_Bluefin2.shp'))
bluefin.zone <- st_transform(bluefin.zone, st_crs(dat_sf))
coast <- ecodata::coast
coast <- st_transform(coast, st_crs(dat_sf))
ggplot() +
  geom_sf(data=coast, fill='gray') +
  geom_sf(data=bluefin.zone, fill=NA, col='red') +
  geom_sf(data=dat_sf,
          aes(col=location))+
  coord_sf(xlim=c(-78, -55),
           ylim=c(35, 48))


#### Run VAST ####
setwd(here('VAST_runs/tuna8'))
# Set working directory
working_dir <- here::here('VAST_runs/tuna8')
# Create working directory if it doesn't exist
if(!dir.exists(working_dir)) {
  dir.create(working_dir)
}
setwd(working_dir)

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

# Set temporal correlations for spatio-temporal variation
# Year intercepts will be fixed effects but correlation between years will 
# have a random effect forllowing a first-order AR process
settings$RhoConfig[3] <- 4
settings$RhoConfig[4] <- 4

# Pull survey values
survs <- dplyr::select(dat,
                       Size_class, catch, fhours, year, lon, lat, location)
colnames(survs) <- c('Category', 'Response_variable', 'Effort',
                     'Year', 'Lon', 'Lat', 'fleet')
survs$Category <- factor(survs$Category,
                         levels=c('small', 'large'))
survs$Category <- as.numeric(survs$Category) - 1
#survs$Response_variable <- as_units(as.numeric(survs$Response_variable), 'counts')
#survs$Effort <- as_units(survs$Effort, 'km^2')
survs$CPUE <- as_units(survs$Response_variable / survs$Effort, 'unitless')
survs$effort.unitless <- as_units(1, 'unitless')
survs$fleet <- factor(survs$fleet,
                      levels=c('us', 'can'))
survs$fleet <- as.numeric(survs$fleet)-1

str(survs)

# Pull covariate values
covars <- dplyr::select(dat,
                        year, lon, lat, sst, depth, slp, nao, amo)
colnames(covars) <- c('Year', 'Lon', 'Lat', 'sst', 'depth', 'slp', 'nao', 'amo')

# Test correlation
df_cormat <- dplyr::select(covars, sst, depth, slp, nao, amo)
model.matrix(~0+., data=df_cormat) %>%
  cor(use="all.obs", method="spearman") %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3)

# Scale covariate values
scaled.covars <- covars[,4:ncol(covars)] %>% 
  mutate(across(where(is.numeric), scale))
scaled.covars <- cbind(covars[,1:3], scaled.covars)
summary(scaled.covars)
scaled.covars <- data.frame(
  Lon         = as.numeric(scaled.covars$Lon),
  Lat         = as.numeric(scaled.covars$Lat),
  Year        = as.numeric(scaled.covars$Year),
  sst         = as.numeric(scaled.covars$sst),
  depth       = as.numeric(scaled.covars$depth),
  slp         = as.numeric(scaled.covars$slp),
  nao         = as.numeric(scaled.covars$nao),
  amo         = as.numeric(scaled.covars$amo)
)
str(scaled.covars)

# Set obsmodel to reflect CPUE response variable measure
settings$ObsModel[1] <- 4
settings$ObsModel[2] <- 1

#### Make region ####
source(here('R_code/utilities/vast_functions.R'))
# First, we need our region shapefile
region_shape<- st_read(here::here("Data/GIS/Combined_Bluefin2.shp"))
region_shape <- st_transform(region_shape, "EPSG:4326")
region_shape <- st_make_valid(region_shape)
colnames(region_shape) <- c('OBJECTID', 'geometry')
region_shape <- dplyr::select(region_shape, OBJECTID, geometry)
region_shape$OBJECTID <- 'ALL'
colnames(region_shape) <- c("Region", 'geometry')

# Second, get our index area shapefile
# We could just use this same shapefile in the "index_shapes" argument, but to 
# show off the new functions we wrote, we will also want to have a sf multipolygon
# shapefiles with areas defined within this general region
index_areas<- c("CanadaEEZ", "NWAtlantic")

for(i in seq_along(index_areas)){
  index_area_temp<- st_read(paste0(here::here("", "Data/GIS"), '/', 
                                   index_areas[i], ".shp"))
  index_area_temp <- st_transform(index_area_temp, "EPSG:4326")
  index_area_temp <- st_make_valid(index_area_temp)
  if(index_areas[i]=='CanadaEEZ'){
    index_area_temp <- dplyr::select(index_area_temp,
                                     geometry)
    index_area_temp$STOCK <- 'Canada'
  }
  if(index_areas[i]=='NWAtlantic'){
    index_area_temp <- dplyr::select(index_area_temp,
                                     geometry)
    index_area_temp$STOCK <- 'US'
  }
  index_area_temp <- dplyr::select(index_area_temp, STOCK, geometry)
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
strata_use<- data.frame(STRATA = c("Canada", "US", "ALL"))
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

cat.labs <- c('Small', 'Large')
year.labs <- seq(1993, 2020)

fit = fit_model( 
  # Set wd
  #working_dir = here('VAST_runs/tuna1'),
  
  # Call settings
  settings = settings, 
  
  # Call survey data info
  Lat_i = survs[,'Lat'], 
  Lon_i = survs[,'Lon'], 
  t_i = survs[,'Year'],
  #b_i = survs[,'Response_variable'],
  #a_i = survs[,'Effort'],
  b_i = survs[,'CPUE'],
  a_i = survs[,'effort.unitless'],
  c_iz = survs[,'Category'],
  
  # Call covariate info
  X1_formula = ~ sst + depth + slp + nao + amo,
  X2_formula = ~ sst + depth + slp + nao + amo, 
  covariate_data = scaled.covars,
  
  # Call spatial 
  input_grid=vast_extrap_grid,
  "extrapolation_list" = extrap_info_aja,
  
  # Set naming conventions
  category_names = cat.labs,
  year_labels = year.labs,
  
  # Tell model to run
  run_model = TRUE,
  build_model = TRUE)


#beep(8)

#### Plot results ####
rm(coast, dat_sf, grid_sf, overland)
save.image('tuna_8.RData')

plot( fit )
beep(8)