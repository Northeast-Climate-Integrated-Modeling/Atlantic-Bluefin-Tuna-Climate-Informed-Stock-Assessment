
rm(list=ls())

library(here)
library(sf)
library(VAST)
library(tidyverse)

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

# Root
root <- here('VAST_runs/tuna13_usonly/natural_splines')
ending <- '_naturalsplines_x1_covsnotscaled_finescaleoff_knots5.RData'

load(paste0(root, '/No AMO DF5 Not Scaled Coarse/noamo', ending))
amo <- fit

load(paste0(root, '/No Bathy DF5 Not Scaled Coarse/nobathy', ending))
bathy <- fit

load(paste0(root, '/No Prey DF5 Not Scaled Coarse/noprey', ending))
prey <- fit

load(paste0(root, '/No SST DF5 Not Scaled Coarse/nosst', ending))
sst <- fit

load(paste0(root, '/No Covs Coarse/nocovs', ending))
nocovs <- fit

load(paste0(root, '/All Covs DF5 Not Scaled Coarse/allcovs', ending))
allcovs <- fit


rm(list=setdiff(ls(), c('amo', 'bathy', 'prey', 'sst',
                        'nocovs', 'allcovs')))

aiccomp <- data.frame(
  model=c('amo', 'bathy', 'prey', 'sst',
          'nocovs', 'allcovs'),
  aic = c(amo$parameter_estimates$AIC,
          bathy$parameter_estimates$AIC,
          prey$parameter_estimates$AIC,
          sst$parameter_estimates$AIC,
          nocovs$parameter_estimates$AIC,
          allcovs$parameter_estimates$AIC)
)

aiccomp <- aiccomp[with(aiccomp, order(aic)),]

aiccomp$delta.aic <- NA
for(i in 2:nrow(aiccomp)){
  aiccomp$delta.aic[i] <- 
    aiccomp$aic[i] - aiccomp$aic[(i-1)]
}
aiccomp$delta.aic[1] <- 0
aiccomp

devcomp <- data.frame(
  model=c('amo', 'bathy', 'prey', 'sst',
          'nocovs', 'allcovs'),
  deviance = c(
          amo$Report$deviance,
          bathy$Report$deviance,
          prey$Report$deviance,
          sst$Report$deviance,
          nocovs$Report$deviance,
          allcovs$Report$deviance)
)

devcomp <- devcomp[with(devcomp, order(deviance, decreasing = T)),]

devcomp$pct.dev.exp <- NA
for(i in 1:nrow(devcomp)){
  devcomp$pct.dev.exp[i] <- 
    (1 - devcomp$deviance[i]/nocovs$Report$deviance)
}
devcomp
