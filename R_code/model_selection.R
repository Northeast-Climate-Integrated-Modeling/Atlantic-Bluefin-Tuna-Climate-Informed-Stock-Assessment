# VAST attempt 2 univariate model selection as a script
# modified from 
# https://github.com/James-Thorson-NOAA/VAST/wiki/Index-standardization
###############################################################################
# Load necessities
rm(list=ls())
gc()

# Load packages
library(here)
library(dplyr)
library(VAST)
library(tidyverse)
library(units)
library(sf)
# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))
# Add unitless back as possible unit (removed in units package update Mar 2023)
install_unit(symbol='unitless', def='unitless', name='unitless')
# Read in data
# Load data
dat <- read.csv(here('Data/Clean/BFT_US_catch_VASTdata.csv'))
dat$depth <- dat$depth * -1
dat$date <- as.POSIXct(paste0(dat$year, '-', dat$month, '-', dat$day),
                       format='%Y-%m-%d')

# Filter observations with no prey data
dat <- dat %>% 
  drop_na(prey)

# Add AMO and NAO
nao <- read.csv(here('Data/Climate_Indices/daily_nao.csv'))
nao$date <- as.POSIXct(paste0(nao$year, '-',
                              nao$month, '-',
                              nao$day),
                       format="%Y-%m-%d")
nao$nao.scale <- scale(nao$aao_index_cdas)
ggplot() +
  geom_point(data=nao[nao$year >=1993,], aes(x=date, y=nao.scale)) +
  geom_smooth(data=nao[nao$year >=1993,], aes(x=date, y=nao.scale))

nao <- nao %>% 
  filter(year >= 1993 & year <=2021) %>% 
  filter(month %in% seq(6, 10, 1))
colnames(nao) <- c('year', 'month', 'day', 'nao', 'date', 'nao.scale')

nao <- dplyr::select(nao, date, nao)
head(nao)
ggplot() +
  geom_point(data=nao, aes(x=date, y=scale(nao))) +
  geom_smooth(data=nao, aes(x=date, y=scale(nao)))

dat <- merge(dat, nao, by=c('date'))

amo <- read.csv(here('Data/Climate_Indices/monthly_amo.csv'))
amo <- amo %>% 
  filter(Year >= 1993 & Year <=2021) %>% 
  filter(Month %in% c('Jun', 'Jul', 'Aug', 'Sep', 'Oct'))
amo$monthno <- match(amo$Month,month.abb)
amo$yrmo <- paste0(amo$Year, '-', amo$monthno)
amo <- dplyr::select(amo, yrmo, Value)
colnames(amo) <- c('yrmo', 'amo')
dat$yrmo <- paste0(dat$year, '-', dat$month)
dat <- merge(dat, amo, by=c('yrmo'))

# Convert to sf for plotting
dat_sf <- st_as_sf(dat, coords=c('lon', 'lat'))
st_crs(dat_sf) <- 'EPSG:4326'

# Pull survey values
survs <- dplyr::select(dat,
                       Size_class, catch, fhours, year, lon, lat)
colnames(survs) <- c('Category', 'Response_variable', 'Effort',
                     'Year', 'Lon', 'Lat')
survs$Category <- factor(survs$Category,
                         levels=c('small', 'large'))
survs$Category <- as.numeric(survs$Category) - 1
#survs$Response_variable <- as_units(as.numeric(survs$Response_variable), 'counts')
#survs$Effort <- as_units(survs$Effort, 'km^2')
survs$CPUE <- as_units(survs$Response_variable / survs$Effort, 'unitless')
survs$effort.unitless <- as_units(1, 'unitless')
str(survs)

# Pull covariate values
covars <- dplyr::select(dat,
                        year, lon, lat, depth, sst, prey, amo, nao)
colnames(covars) <- c('Year', 'Lon', 'Lat', 'depth', 'sst', 'prey', 'amo', 'nao')

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
user_region <- grid_NWA_BTS

###############################################################################
# Describe model selection efforts

# Model selection 1 (spatial, spatio-temporal effects, no covariates) options 

# Field configs
# _alleffectson             FieldConfig default (all IID)
# _noaniso                  FieldConfig default (all IID) and 
#                                use_anistropy = FALSE
# _noomeps2                 FieldConfig 0 for Omega2, Epsilon2
# _noomeps2_noaniso         FieldConfig 0 for Omega2, Epsilon2 and 
#                                use_anistropy = FALSE
# _noomeps2_noeps1          FieldConfig 0 for Omega2, Epsilon2, Epsilon1
# _noomeps2_noeps1_noaniso  FieldConfig 0 for Omega2, Epsilon2, Epsilon1 and 
#                                use_anistropy = FALSE
# _noomeps12                FieldConfig both Omega, Epsilon 0
# _noomeps12_noaniso        FieldConfig both Omega, Epsilon 0 and 
#                                use_anistropy = FALSE

# default configs
FieldConfig = matrix( "IID", ncol=2, nrow=3, 
                      dimnames=list(c("Omega","Epsilon","Beta"),
                                    c("Component_1","Component_2")))

# Rho configs
# not testing alternative RhoConfigs here just noted for completeness
# 0 off (fixed effects)
# 1 independent
# 2 random walk
# 3 constant among years (fixed effect)
# 4 AR1

RhoConfig <- c(
  "Beta1" = 0,      # temporal structure on years (intercepts) 
  "Beta2" = 0, 
  "Epsilon1" = 0,   # temporal structure on spatio-temporal variation
  "Epsilon2" = 0
) 

# Anisotropy
use_anisotropy <- TRUE


# Model selection 2 (covariates) options, FieldConfig default (all IID)
# _base         No covariates
# _sst, _depth, _prey, _nao, _amo 
# _sstdepth, _sstprey, _sstnao, _sstamo
# _depthprey, _depthnao, _depthamo
# _preynao, _preyamo
# _naoamo
# _sstdepthprey _sstdepthnao, _sstdepthamo, _sstpreynao, _sstpreyamo, _sstnaoamo
# _depthpreynao, _depthpreyamo, _depthnaoamo
# _preynaoamo
# _sstdepthpreynao, _sstdepthpreyamo, _sstpreynaoamo
# _depthpreynaoamo
# _all          all listed covariates
# _eta1         vessel overdispersion in 1st predictor
# _eta2         vessel overdispersion in 1st and 2nd predictors

OverdispersionConfig	<- c("eta1"=0, "eta2"=0)
# eta0 = no vessel effects
# eta1 = vessel effects on prey encounter rate
# eta2 = vessel effects on prey weight

# list of data, settings, and directory for output for each option
mod.config <- c("alleffectson", "noaniso", 
                "noomeps2", "noomeps2_noaniso", 
                "noomeps2_noeps1", "noomeps2_noeps1_noaniso",
                "noomeps12", "noomeps12_noaniso")

# Define possible field configurations
FieldConfig1 <- matrix( "IID", ncol=2, nrow=3, 
                        dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")))
FieldConfig2 <- matrix( c("IID","IID","IID",0,0,"IID"), ncol=2, nrow=3, 
                        dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")))
FieldConfig3 <- matrix( c("IID",0,"IID",0,0,"IID"), ncol=2, nrow=3, 
                        dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")))
FieldConfig4 <- matrix( c(0,0,"IID",0,0,"IID"), ncol=2, nrow=3, 
                        dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")))

# Pull field configs into list
mod.FieldConfig <- list(FieldConfig1, FieldConfig1,
                        FieldConfig2, FieldConfig2,
                        FieldConfig3, FieldConfig3,
                        FieldConfig4, FieldConfig4)

# Name list items
names(mod.FieldConfig) <- mod.config

# List possible anisotropy options
mod.use_anistropy <- list(TRUE, FALSE, 
                          TRUE, FALSE,
                          TRUE, FALSE,
                          TRUE, FALSE)
# Name list items
names(mod.use_anistropy) <- mod.config

###############################################################################
# Run  model selection 1

# Subset for model selection 1
sel1 <- survs

# Loop through options
for(i in 1:length(mod.config)) {
  # Define name of model run
  name <- mod.config[i]
  # Set working directory
  working_dir <- here::here(sprintf("mod_selection/%s", name))
  # Create working directory if it doesn't exist
  if(!dir.exists(working_dir)) {
    dir.create(working_dir)
  }
  # Call model options to be used
  FieldConfig <- mod.FieldConfig[[i]]
  use_anisotropy <- mod.use_anistropy[[i]]
  # Make settings
  settings <- make_settings(n_x = 200,
                            Region = "User",
                            Version = "VAST_v14_0_1",
                            purpose = "index2",
                            bias.correct = FALSE,
                            use_anisotropy = use_anisotropy,
                            FieldConfig = FieldConfig,
                            RhoConfig = RhoConfig, # always default
                            OverdispersionConfig = OverdispersionConfig #default
  )
  
  # Set obsmodel to reflect CPUE response variable measure
  settings$ObsModel[1] <- 4
  settings$ObsModel[2] <- 1
  # Fit model
  fit <- fit_model(
    # Call REML
    Use_REML = TRUE,
    # Call settings
    settings = settings,
    # Call survey data info
    Lat_i = sel1[,'Lat'],
    Lon_i = sel1[,'Lon'],
    t_i = sel1[,'Year'],
    b_i = sel1[,'CPUE'],
    a_i = sel1[,'effort.unitless'],
    # Call spatial
    input_grid = user_region,
    # Set directory
    working_dir = paste0(working_dir, "/"),
    # Tell model to run
    run_model = TRUE)
} # end config loop

###############################################################################
# Compare model selection 1 results
# Set directory 
outdir <- here("mod_selection")
# Call file names
moddirs <- list.dirs(outdir) 
# Remove name of upper level file
moddirs <- moddirs[-1]
# keep folder name
modnames <- list.dirs(outdir, full.names = FALSE)

# function to apply extracting info
getmodinfo <- function(d.name){
  # read settings
  modpath <- stringr::str_split(d.name, "/", simplify = TRUE)
  modname <- modpath[length(modpath)]
  
  settings <- read.table(file.path(d.name, "settings.txt"), comment.char = "",
                         fill = TRUE, header = FALSE)
  
  n_x <- as.numeric(as.character(settings[(which(settings[,1]=="$n_x")+1),2]))
  grid_size_km <- as.numeric(as.character(settings[(
    which(settings[,1]=="$grid_size_km")+1),2]))
  max_cells <- as.numeric(as.character(settings[(
    which(settings[,1]=="$max_cells")+1),2]))
  use_anisotropy <- as.character(settings[(
    which(settings[,1]=="$use_anisotropy")+1),2])
  fine_scale <- as.character(settings[(
    which(settings[,1]=="$fine_scale")+1),2])
  bias.correct <- as.character(settings[(
    which(settings[,1]=="$bias.correct")+1),2])
  
  #FieldConfig
  if(settings[(which(settings[,1]=="$FieldConfig")+1),1]=="Component_1"){
    omega1 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+2),2])
    omega2 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+3),1])
    epsilon1 <- as.character(settings[(
      which(settings[,1]=="$FieldConfig")+4),2])
    epsilon2 <- as.character(settings[(
      which(settings[,1]=="$FieldConfig")+5),1])
    beta1 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+6),2])
    beta2 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+7),1])
  }
  
  if(settings[(which(settings[,1]=="$FieldConfig")+1),1]=="Omega1"){
    omega1 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+3),1])
    omega2 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+4),1])
    epsilon1 <- as.character(settings[(
      which(settings[,1]=="$FieldConfig")+3),2])
    epsilon2 <- as.character(settings[(
      which(settings[,1]=="$FieldConfig")+4),2])
    beta1 <- "IID"
    beta2 <- "IID"
  }
  
  #RhoConfig
  rho_beta1 <- as.numeric(as.character(settings[(
    which(settings[,1]=="$RhoConfig")+3),1]))
  rho_beta2 <- as.numeric(as.character(settings[(
    which(settings[,1]=="$RhoConfig")+3),2]))
  rho_epsilon1 <- as.numeric(as.character(settings[(
    which(settings[,1]=="$RhoConfig")+4),1]))
  rho_epsilon2 <- as.numeric(as.character(settings[(
    which(settings[,1]=="$RhoConfig")+4),2]))
  
  # read parameter estimates, object is called parameter_Estimates
  load(file.path(d.name, "parameter_estimates.RData"))
  
  AIC <- parameter_estimates$AIC[1]  
  converged <- parameter_estimates$Convergence_check[1]
  fixedcoeff <- unname(parameter_estimates$number_of_coefficients[2])
  randomcoeff <- unname(parameter_estimates$number_of_coefficients[3])
  
  # return model atributes as a dataframe
  out <- data.frame(modname = modname,
                    n_x = n_x,
                    grid_size_km = grid_size_km,
                    max_cells = max_cells,
                    use_anisotropy = use_anisotropy,
                    fine_scale =  fine_scale,
                    bias.correct = bias.correct,
                    omega1 = omega1,
                    omega2 = omega2,
                    epsilon1 = epsilon1,
                    epsilon2 = epsilon2,
                    beta1 = beta1,
                    beta2 = beta2,
                    rho_epsilon1 = rho_epsilon1,
                    rho_epsilon2 = rho_epsilon2,
                    rho_beta1 = rho_beta1,
                    rho_beta2 = rho_beta2,
                    AIC = AIC,
                    converged = converged,
                    fixedcoeff = fixedcoeff,
                    randomcoeff = randomcoeff
  )
  return(out)
}

# Pull models
modselect <- purrr::map_dfr(moddirs, getmodinfo)

# Build table to compare models
modselect.200 <- modselect %>%
  filter(n_x == 200) %>%
  mutate(converged2 = case_when(str_detect(converged, 
                                           "no evidence") ~ "likely",
                                str_detect(converged, 
                                           "is likely not") ~ "unlikely",
                                TRUE ~ as.character(NA))) %>%
  mutate(deltaAIC = AIC-min(AIC)) %>%
  select(modname, deltaAIC, fixedcoeff,
         randomcoeff, use_anisotropy, 
         omega1, omega2, epsilon1, epsilon2, 
         beta1, beta2, AIC, converged2) %>%
  arrange(AIC)

# Print table
DT::datatable(modselect.200, rownames = FALSE, 
              options= list(pageLength = 25, scrollX = TRUE))

# Evidence suggests best model includes all effects
# Model is marginally better without anisotropy, but effect is less than 1 unit AIC.
# Will retain anisotropy

###############################################################################
# Model selection 2 setup: covariates
# Define covariate combinations

mod.covar <- c("base", 
               "sst", "depth", "prey", "nao", "amo", 
               "sstdepth", "sstprey", "sstnao", "sstamo",
               "depthprey", 'depthnao', 'depthamo',
               'preynao', 'preyamo',
               'naoamo',
               'sstdepthprey', 'sstdepthnao', 'sstdepthamo', 'sstpreynao', 'sstpreyamo', 'sstnaoamo',
               'depthpreynao', 'depthpreyamo', 'depthnaoamo',
               'preynaoamo',
               'sstdepthpreynao', 'sstdepthpreyamo', 'sstpreynaoamo',
               'depthpreynaoamo', 
               'all')
# 31 total models


OverdispersionConfig	<- c("eta1"=0, "eta2"=0)
# eta1 = vessel effects on prey encounter rate
# eta2 = vessel effects on prey weight
# We have no effects for vessel so this doesn't matter
#OverdispersionConfig1 <- c("eta1"=1, "eta2"=0)
#OverdispersionConfig2 <- c("eta1"=1, "eta2"=1)

mod.eta <- list(OverdispersionConfig, OverdispersionConfig, OverdispersionConfig,
                OverdispersionConfig, OverdispersionConfig, OverdispersionConfig,
                OverdispersionConfig, OverdispersionConfig, OverdispersionConfig,
                OverdispersionConfig, OverdispersionConfig, OverdispersionConfig,
                OverdispersionConfig, OverdispersionConfig, OverdispersionConfig,
                OverdispersionConfig, OverdispersionConfig, OverdispersionConfig,
                OverdispersionConfig, OverdispersionConfig, OverdispersionConfig,
                OverdispersionConfig, OverdispersionConfig, OverdispersionConfig,
                OverdispersionConfig, OverdispersionConfig, OverdispersionConfig,
                OverdispersionConfig, OverdispersionConfig, OverdispersionConfig,
                OverdispersionConfig)

names(mod.eta) <- mod.covar

#########################################################
# Run model selection 2
# Subset to 10 years of data for time
sel2 <- survs
# Scale covariate values
scaled.covars <- covars[,4:ncol(covars)] %>% 
  mutate(across(where(is.numeric), scale))
scaled.covars <- cbind(covars[,1:3], scaled.covars)
summary(scaled.covars)
scaled.covars <- data.frame(
  Lon         = as.numeric(scaled.covars$Lon),
  Lat         = as.numeric(scaled.covars$Lat),
  Year        = as.numeric(scaled.covars$Year),
  depth       = as.numeric(scaled.covars$depth),
  sst         = as.numeric(scaled.covars$sst),
  prey        = as.numeric(scaled.covars$prey),
  amo         = as.numeric(scaled.covars$amo),
  nao         = as.numeric(scaled.covars$nao)
)
str(scaled.covars)

dat <- scaled.covars

# Define density covariate formulas
Q_ikbase        <-   NULL
Q_iksst         <- ~ sst
Q_ikdepth       <- ~ depth
Q_ikprey        <- ~ prey
Q_iknao         <- ~ nao
Q_ikamo         <- ~ amo

Q_iksstdepth    <- ~ sst + depth
Q_iksstprey     <- ~ sst + prey
Q_iksstnao      <- ~ sst + nao
Q_iksstamo      <- ~ sst + amo
Q_ikdepthprey   <- ~ depth + prey
Q_ikdepthnao    <- ~ depth + nao
Q_ikdepthamo    <- ~ depth + amo
Q_ikpreynao     <- ~ prey + nao
Q_ikpreyamo     <- ~ prey + amo
Q_iknaoamo      <- ~ nao + amo

Q_iksstdepthprey <- ~ sst + depth + prey
Q_iksstdepthnao  <- ~ sst + depth + nao
Q_iksstdepthamo  <- ~ sst + depth + amo
Q_iksstpreynao   <- ~ sst + prey + nao
Q_iksstpreyamo   <- ~ sst + prey + amo
Q_iksstnaoamo    <- ~ sst + nao + amo
Q_ikdepthpreynao <- ~ depth + prey + nao
Q_ikdepthpreyamo <- ~ depth + prey + amo
Q_ikdepthnaoamo  <- ~ depth + nao + amo
Q_ikpreynaoamo   <- ~ prey + nao + amo

Q_iksstdepthpreynao <- ~ sst + depth + prey + nao
Q_iksstdepthpreyamo <- ~ sst + depth + prey + amo
Q_iksstpreynaoamo   <- ~ sst + prey + nao + amo
Q_ikdepthpreynaoamo <- ~ depth + prey + nao + amo

Q_ikall <- ~ sst + depth + prey + nao + amo


# Pull formulas into list
mod.Qik <- list(Q_ikbase,
                Q_iksst, Q_ikdepth, Q_ikprey, Q_iknao, Q_ikamo,
                Q_iksstdepth, Q_iksstprey, Q_iksstnao, Q_iksstamo,
                Q_ikdepthprey, Q_ikdepthnao, Q_ikdepthamo,
                Q_ikpreynao, Q_ikpreyamo,
                Q_iknaoamo,
                Q_iksstdepthprey, Q_iksstdepthnao, Q_iksstdepthamo, Q_iksstpreynao, Q_iksstpreyamo, Q_iksstnaoamo,
                Q_ikdepthpreynao, Q_ikdepthpreyamo, Q_ikdepthnaoamo,
                Q_ikpreynaoamo,
                Q_iksstdepthpreynao, Q_iksstdepthpreyamo, Q_iksstpreynaoamo,
                Q_ikdepthpreynaoamo,
                Q_ikall)

# Name formula list items
names(mod.Qik) <- mod.covar

# Loop through density covariate options
for(i in 15:length(mod.covar)) {
  # Define name of model
  name <- paste0(mod.covar[i])
  # Name working directory
  working_dir <- here::here(sprintf("covar_selection/%s/", name))
  # Make folder if it doesn't exist
  if(!dir.exists(working_dir)) {
    dir.create(working_dir)
  }
  # Set model options
  # winners of model selection 1
  use_anisotropy <- TRUE
  FieldConfig <- FieldConfig1
  OverdispersionConfig <- mod.eta[[i]]
  Q_ik <- mod.Qik[[i]]
  # Make settings
  settings <- make_settings( n_x = 200,
                             Region = "User",
                             Version = "VAST_v14_0_1",
                             purpose = "index2",
                             bias.correct = FALSE,
                             use_anisotropy = use_anisotropy,
                             FieldConfig = FieldConfig,
                             RhoConfig = RhoConfig, # always default
                             OverdispersionConfig = OverdispersionConfig
  )
  # Set obsmodel to reflect CPUE response variable measure
  settings$ObsModel[1] <- 4
  settings$ObsModel[2] <- 1
  
  # Fit model
  fit = fit_model(
    # Call settings
    settings = settings,
    # Call survey data info
    Lat_i = sel2[,'Lat'],
    Lon_i = sel2[,'Lon'],
    t_i = sel2[,'Year'],
    b_i = sel2[,'CPUE'],
    a_i = sel2[,'effort.unitless'],
    # Call covariate info
    X1_formula = mod.Qik[[i]],
    covariate_data = scaled.covars,
    # Call spatial
    input_grid=user_region,
    # Set working dir
    working_dir = paste0(working_dir, "/"),
    # Tell model to run
    run_model = TRUE)
} # end covar loop

# Loop through base options (no covars)
for(i in c(1)) {
  # Define name of model
  name <- paste0(mod.covar[i])
  # Name working directory
  working_dir <- here::here(sprintf("covar_selection/%s/", name))
  # Make folder if it doesn't exist
  if(!dir.exists(working_dir)) {
    dir.create(working_dir)
  }
  # Set model options
  # winners of model selection 1
  use_anisotropy <- TRUE
  FieldConfig <- FieldConfig1
  OverdispersionConfig <- mod.eta[[i]]
  Q_ik <- mod.Qik[[i]]
  # Make settings
  settings <- make_settings( n_x = 200,
                             Region = "User",
                             Version = "VAST_v14_0_1",
                             purpose = "index2",
                             bias.correct = FALSE,
                             use_anisotropy = use_anisotropy,
                             FieldConfig = FieldConfig,
                             RhoConfig = RhoConfig, # always default
                             OverdispersionConfig = OverdispersionConfig
  )
  # Set obsmodel to reflect CPUE response variable measure
  settings$ObsModel[1] <- 4
  settings$ObsModel[2] <- 1
  
  # Fit model
  fit = fit_model(
    # Call settings
    settings = settings,
    # Call survey data info
    Lat_i = sel2[,'Lat'],
    Lon_i = sel2[,'Lon'],
    t_i = sel2[,'Year'],
    b_i = sel2[,'CPUE'],
    a_i = sel2[,'effort.unitless'],
    # Call covariate info
    #X1_formula = mod.Qik[[i]],
    #covariate_data = scaled.covars,
    # Call spatial
    input_grid=user_region,
    # Set working dir
    working_dir = paste0(working_dir, "/"),
    # Tell model to run
    run_model = TRUE)

} # end covar loop
beep(8)
###############################################################################
# Model selection for covariates
# Set folder 
outdir <- here("covar_selection")
# List folders in outer folder
moddirs <- list.dirs(outdir) 
# Remove top level folder
moddirs <- moddirs[-c(1,3)]
# keep folder name
modnames <- c('all', 'alleta11' ,'base', 'bathrug', 'bathrugsst', 'bathsst', 
              'bathy', 
              'cobble', 'eta10', 'eta11', 'gravel', 'mud', 'rugos', 'rugsst',
              'sand', 'seds', 'sedsbath', 'sedsbathrug', 'sedsbathsst', 
              'sedsbathssteta11',
              'sedsrug', 'sedsrugsst', 'sedssst', 'sst')

# function to apply extracting info
getmodinfo <- function(d.name){
  # read settings
  modpath <- stringr::str_split(d.name, "/", simplify = TRUE)
  modname <- modpath[length(modpath)]
  
  settings <- read.table(file.path(d.name, "settings.txt"), comment.char = "",
                         fill = TRUE, header = FALSE)
  
  n_x <- as.numeric(as.character(settings[(which(settings[,1]=="$n_x")+1),2]))
  grid_size_km <- as.numeric(as.character(settings[(
    which(settings[,1]=="$grid_size_km")+1),2]))
  max_cells <- as.numeric(as.character(settings[(
    which(settings[,1]=="$max_cells")+1),2]))
  use_anisotropy <- as.character(settings[(
    which(settings[,1]=="$use_anisotropy")+1),2])
  fine_scale <- as.character(settings[(
    which(settings[,1]=="$fine_scale")+1),2])
  bias.correct <- as.character(settings[(
    which(settings[,1]=="$bias.correct")+1),2])
  
  #FieldConfig
  if(settings[(which(settings[,1]=="$FieldConfig")+1),1]=="Component_1"){
    omega1 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+2),2])
    omega2 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+3),1])
    epsilon1 <- as.character(settings[(
      which(settings[,1]=="$FieldConfig")+4),2])
    epsilon2 <- as.character(settings[(
      which(settings[,1]=="$FieldConfig")+5),1])
    beta1 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+6),2])
    beta2 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+7),1])
  }
  
  if(settings[(which(settings[,1]=="$FieldConfig")+1),1]=="Omega1"){
    omega1 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+3),1])
    omega2 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+4),1])
    epsilon1 <- as.character(settings[(
      which(settings[,1]=="$FieldConfig")+3),2])
    epsilon2 <- as.character(settings[(
      which(settings[,1]=="$FieldConfig")+4),2])
    beta1 <- "IID"
    beta2 <- "IID"
  }
  
  
  #RhoConfig
  rho_beta1 <- as.numeric(as.character(settings[(
    which(settings[,1]=="$RhoConfig")+3),1]))
  rho_beta2 <- as.numeric(as.character(settings[(
    which(settings[,1]=="$RhoConfig")+3),2]))
  rho_epsilon1 <- as.numeric(as.character(settings[(
    which(settings[,1]=="$RhoConfig")+4),1]))
  rho_epsilon2 <- as.numeric(as.character(settings[(
    which(settings[,1]=="$RhoConfig")+4),2]))
  
  # read parameter estimates, object is called parameter_Estimates
  load(file.path(d.name, "parameter_estimates.RData"))
  
  AIC <- parameter_estimates$AIC[1]  
  converged <- parameter_estimates$Convergence_check[1]
  fixedcoeff <- unname(parameter_estimates$number_of_coefficients[2])
  randomcoeff <- unname(parameter_estimates$number_of_coefficients[3])
  
  
  # return model atributes as a dataframe
  out <- data.frame(modname = modname,
                    n_x = n_x,
                    grid_size_km = grid_size_km,
                    max_cells = max_cells,
                    use_anisotropy = use_anisotropy,
                    fine_scale =  fine_scale,
                    bias.correct = bias.correct,
                    omega1 = omega1,
                    omega2 = omega2,
                    epsilon1 = epsilon1,
                    epsilon2 = epsilon2,
                    beta1 = beta1,
                    beta2 = beta2,
                    rho_epsilon1 = rho_epsilon1,
                    rho_epsilon2 = rho_epsilon2,
                    rho_beta1 = rho_beta1,
                    rho_beta2 = rho_beta2,
                    AIC = AIC,
                    converged = converged,
                    fixedcoeff = fixedcoeff,
                    randomcoeff = randomcoeff
  )
  return(out)
}

# combine into one table for comparison
modselect <- purrr::map_dfr(moddirs, getmodinfo)

# Build table to compare models
modselect.cov <- modselect %>%
  filter(n_x == 200) %>%
  #filter(str_detect(modname, "base|eta|len|_no$")) %>%
  # mutate(converged2 = case_when(str_detect(converged, 
  #                                          "no evidence") ~ "likely",
  #                               str_detect(converged, 
  #                                          "is likely not") ~ "unlikely",
  #                               TRUE ~ as.character(NA))) %>%
  mutate(deltaAIC = AIC-min(AIC)) %>%
  select(modname, deltaAIC, fixedcoeff,
         randomcoeff, use_anisotropy, 
         omega1, omega2, epsilon1, epsilon2, 
         beta1, beta2, AIC) %>%
  arrange(AIC)

# Print table
DT::datatable(modselect.cov, rownames = FALSE, 
              options= list(pageLength = 25, scrollX = TRUE))
