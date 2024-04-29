rm(list=ls())

# Load libraries
library(VAST)
library(effects)
library(tidyverse)
library(here)
library(splines)

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

# load data
load(here("VAST_runs/tuna13_usonly/natural_splines/All Covs DF5 Not Scaled Fine/allcovs_naturalsplines_x1_covsnotscaled_finescaleon_knots5.Rdata"))

# Set WD for plotting
out_dir = here("VAST_runs/tuna13/natural_splines/All")

# Load functions
source(here("R_code/utilities/vast_functions.R"))
# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Call parameter names
params <- colnames(scaled.covars)
params <- params[params %notin% c('Lon', 'Lat', 'Year')]

# Call category names
catnames <- fit$category_names
ncat = length(catnames)

  vast_covariate_effects<- get_vast_covariate_effects(vast_fit = fit, 
                                                 params_plot = c(params), 
                                                 params_plot_levels = 100, 
                                                 effects_pad_values = c(), 
                                                 nice_category_names = 'Giant BFT NS DF5 Not Scaled Fine',
                                                 out_dir = out_dir,
                                                 category_to_use = 1,
                                                 ncat = ncat,
                                                 strata_to_use = 'US')
  
  # Remove intermediates
  rm(list=setdiff(ls(), c('vast_covariate_effects', 'fit')))
  
  names_stay <- c("fit", "se", "lower", "upper", "Lin_pred")

  betternames=data.frame(
    Covariate = c('BATHY.DEPTH', 'amo', 'sst'),
    Better = c('Depth (m)', 'AMO', 'SST')
  )
  
  vast_cov_eff_l <- vast_covariate_effects %>%
    drop_na(Value)
  
  vast_cov_eff_l <- merge(vast_cov_eff_l, betternames)
  
  vast_cov_eff_l <- vast_cov_eff_l %>% 
    dplyr::select(-Covariate) %>% 
    rename(Covariate = Better)
  
  ylim_dat <- vast_cov_eff_l %>%
    group_by(., Lin_pred, Covariate) %>%
    summarize(., Min = min(lower, na.rm = TRUE), Max = max(upper, na.rm = TRUE))
  
  names_keep <- unique(vast_cov_eff_l$Covariate)
  
  colnames(fit$covariate_data)[4:6] <- c('Depth (m)',
                                         'AMO', 'SST')
  
  samp_dat <- fit$covariate_data %>% dplyr::select({
    {
      names_keep
    }
  }) %>% gather(., "Covariate", "Value")
  
  samp_quants <- samp_dat %>% 
    group_by(Covariate) %>% 
    mutate(
           q0.33=quantile(Value, 0.33),
           q0.66=quantile(Value, 0.66)) %>% 
    dplyr::select(-Value) %>% 
    unique() %>% 
    as.data.frame() %>% 
    pivot_longer(cols=c('q0.33', 'q0.66'))
  
  
  plot_out <- ggplot() +
    geom_ribbon(data = vast_cov_eff_l[vast_cov_eff_l$Lin_pred == 'X2',], 
                aes(x = Value, ymin = lower, ymax = upper), 
                fill = "#bdbdbd", alpha=0.7) +
    geom_line(data = vast_cov_eff_l[vast_cov_eff_l$Lin_pred == 'X2',], aes(x = Value, y = fit)) +
    geom_vline(data=samp_quants, aes(xintercept=value),
               lty=2, col='darkgray', alpha=0.8) +
    xlab("Covariate value") +
    ylab("Linear predictor 2 fitted value") +
    facet_wrap(vars(Covariate), scales = "free_x", nrow=1) +
    theme(strip.text.x = element_text(size=12),
          strip.background = element_rect(fill='transparent',
                                          linewidth = 0))
  

  
  
  plot_out2 <- plot_out +
    geom_rug(data = samp_dat, aes(x = Value))

  plot_out2

  ggsave(here('VAST_runs/tuna13/natural_splines/All/New_Effects_X2.png'),
         plot_out2,
         height=3, width = 6)  
  