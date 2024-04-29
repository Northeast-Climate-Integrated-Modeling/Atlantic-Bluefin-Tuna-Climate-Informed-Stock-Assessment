rm(list=ls())

library(VAST)
library(here)

install_unit(symbol='unitless', def='unitless', name='unitless')

source(here('R_code/utilities/extract_fit_cog.R'))
source(here('R_code/utilities/extract_fit_range_edges.R'))
source(here('R_code/utilities/extract_fit_eff.R'))

# Root
root <- here('VAST_runs/tuna13/')
ending <- '/allcovs_naturalsplines_x1_covsnotscaled_'

load(paste0(root, ending, 'finescaleon_knots5_spateffoff.RData'))

setwd(here("VAST_runs/tuna13/natural_splines/CA"))

extract_fit_cog(fit=fit)
extract_fit_range_edges(fit=fit,
                       quantvals = c(0.05, 0.5, 0.95))
extract_fit_eff(fit=fit)

plot_results(fit, plot_set = c(1, 2, 3, 6, 7, 8, 9,
                               11, 12, 13, 14, 15, 16, 17, 18, 
                               19, 20, 21))

