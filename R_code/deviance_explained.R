rm(list=ls())

library(here)
library(sf)
library(VAST)
library(tidyverse)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Root
root <- here('VAST_runs/tuna13_usonly/natural_splines/SingleCovs')
ending <- '_naturalsplines_x1_covsnotscaled_finescaleoff_knots5.RData'

load(paste0(root, '/All/allcovs', ending))

load(paste0(root, '/None/nocovs', ending))

load(paste0(root, '/Bathy/onlybathy', ending))

load(paste0(root, '/AMO/onlyamo', ending))

load(paste0(root, '/Prey/onlyprey', ending))

load(paste0(root, '/SST/onlysst', ending))

rm(list=setdiff(ls(), c('allcovs.devexp', 'nocovs.devexp', 
                        'bathy.devexp', 'prey.devexp', 
                        'sst.devexp',
                        'amo.devexp')))

devcomp <- data.frame(
  model=c('all covariates', 'no covariates'#,
          #'bathy', 'amo', 'prey', 'sst'
          ),
  deviance = c(
          allcovs$Report$deviance,
          nocovs$Report$deviance#,
          #bathy$Report$deviance,
          #amo$Report$deviance,
          #prey$Report$deviance,
          #sst$Report$deviance
          )
)

devcomp <- devcomp[with(devcomp, order(deviance, decreasing = T)),]

devcomp$pct.dev.exp <- NA
for(i in 1:nrow(devcomp)){
  devcomp$pct.dev.exp[i] <- 
    (1 - devcomp$deviance/fit0$Report$deviance)
}
devcomp
