
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
root <- here('VAST_runs/medium/AIC/')
ending <- '_medcod_wholearea_natsplin_fsOFF.RData'

load(paste0(root, '/amo/noamo', ending))
amo <- fit

load(paste0(root, '/bathy/nobathy', ending))
bathy <- fit

load(paste0(root, '/bottomtemp/nobottomtemp', ending))
bottomtemp <- fit

load(paste0(root, '/cobble/nocobble', ending))
cobble <- fit

load(paste0(root, '/gravel/nogravel', ending))
gravel <- fit

load(paste0(root, '/mud/nomud', ending))
mud <- fit

load(paste0(root, '/nao/nonao', ending))
nao <- fit

load(paste0(root, '/rugos/norugos', ending))
rugos <- fit

load(paste0(root, '/sand/nosand', ending))
sand <- fit

load(paste0(root, '/none/nonecovs', ending))
nocovs <- fit

load(paste0(root, '/all/allcovs', ending))
allcovs <- fit


rm(list=setdiff(ls(), c('amo', 'bathy', 'bottomtemp',
                        'cobble', 'gravel', 'mud',
                        'nao', 'rugos', 'sand',
                        'nocovs', 'allcovs')))

aiccomp <- data.frame(
  model=c('amo', 'bathy', 'bottomtemp',
          'cobble', 'gravel', 'mud',
          'nao', 'rugos', 'sand',
          'nocovs', 'allcovs'),
  aic = c(amo$parameter_estimates$AIC,
          bathy$parameter_estimates$AIC,
          bottomtemp$parameter_estimates$AIC,
          cobble$parameter_estimates$AIC,
          gravel$parameter_estimates$AIC,
          mud$parameter_estimates$AIC,
          nao$parameter_estimates$AIC,
          rugos$parameter_estimates$AIC,
          sand$parameter_estimates$AIC,
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
  model=c('amo', 'bathy', 'bottomtemp',
          'cobble', 'gravel', 'mud',
          'nao', 'rugos', 'sand',
          'nocovs', 'allcovs', 'selectcovs'),
  deviance = c(amo$Report$deviance,
          bathy$Report$deviance,
          bottomtemp$Report$deviance,
          cobble$Report$deviance,
          gravel$Report$deviance,
          mud$Report$deviance,
          nao$Report$deviance,
          rugos$Report$deviance,
          sand$Report$deviance,
          nocovs$Report$deviance,
          allcovs$Report$deviance,
          selectcovs$Report$deviance)
)

devcomp <- devcomp[with(devcomp, order(deviance, decreasing = T)),]

devcomp$pct.dev.exp <- NA
for(i in 1:nrow(devcomp)){
  devcomp$pct.dev.exp[i] <- 
    (1 - devcomp$deviance[i]/nocovs$Report$deviance)
}
devcomp
