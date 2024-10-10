rm(list=ls())

library(VAST)
library(here)
library(tidyverse)

install_unit(symbol='unitless', def='unitless', name='unitless')

source(here('R_code/utilities/extract_fit_cog.R'))
source(here('R_code/utilities/extract_fit_range_edges.R'))
source(here('R_code/utilities/extract_fit_eff.R'))
source(here('R_code/utilities/plot_maps_kl.R'))
source(here('R_code/utilities/plot_results_kl.R'))

# Root
root <- here('VAST_runs/tuna13_usonly/natural_splines/')
ending <- '/Betas0_NS0_R4/betas0_ns0_r4'

load(paste0(root, ending, '.RData'))
rm(list=setdiff(ls(), c('fit', 'extract_fit_cog', 'extract_fit_eff',
                        'extract_fit_range_edges',
                        'plot_results_kl', 
                        'plot_maps_kl')))

setwd(here("VAST_runs/tuna13_usonly/natural_splines/Betas0_NS0_R4/"))

fit_new <- reload_model(x = fit)

extract_fit_cog(fit=fit_new)
extract_fit_range_edges(fit=fit_new,
                       quantvals = c(0.05, 0.5, 0.95))
extract_fit_eff(fit=fit_new)

plot_results_kl(fit_new, plot_set = c(1, 2, 3, 6, 16),
                check_residuals = TRUE)

