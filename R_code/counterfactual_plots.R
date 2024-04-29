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

#### Center of Gravity ####
# All covariates, GMRF, Omega 1 and 2 off
gmrf.cog <- read.csv(here('VAST_runs/tuna13_usonly/counterfactual/Fine Scale All Covs X1/COG.csv'))
gmrf.cog$Model <- 'GMRF with Covariates'
load(here('VAST_runs/tuna13_usonly/counterfactual/allcovs_finescale_omega1off.RData'))
gmrf <- fit

# All covariates, Coarse, Omega 1 and 2 off
crs.cog <- read.csv(here('VAST_runs/tuna13_usonly/counterfactual/All Covs X1 No Omega 1/COG.csv'))
crs.cog$Model <- 'Covariates Only'
load(here('VAST_runs/tuna13_usonly/counterfactual/allcovs_coarsescale_omega1off.RData'))
crs <- fit

# No covariates, GMRF, Omega 1 and 2 off
no.cog <- read.csv(here('VAST_runs/tuna13_usonly/counterfactual/Fine Scale No Covs/COG.csv'))
no.cog$Model <- 'GMRF without covariates'
load(here('VAST_runs/tuna13_usonly/counterfactual/nocovs_finescale_omega1off.RData'))
no <- fit

# Final model, all covariates, Coarse, Omega 1 on, no covariates on x2
final.cog <- read.csv(here("VAST_runs/tuna13_usonly/counterfactual/All Covariates X1/COG.csv"))
final.cog$Model <- 'Final'
load(here('VAST_runs/tuna13_usonly/counterfactual/allcovs_x1.RData'))
allx1 <- fit

load(here('VAST_runs/tuna13_usonly/counterfactual/allcovs_x1x2.RData'))
allx1x2 <- fit

load(here('VAST_runs/tuna13_usonly/counterfactual/allcovs_finescale_omega1off_naturalsplines.RData'))
natsplin <- fit

rm(list=setdiff(ls(), c('allx1', 'allx1x2', 'crs', 'gmrf', 'no')))


# Combine
allmods <- rbind(gmrf.cog, crs.cog, no.cog, final.cog)
allmods <- allmods %>% 
  mutate(easting = easting / 1000,
         northing = northing / 1000,
         e.sd = e.sd / 1000,
         n.sd = n.sd / 1000) %>% 
  dplyr::select(-units)

# Plot
ggplot(data=allmods) +
  geom_line(aes(x=Year, y=northing, col=Model),
            lwd=1) +
  geom_point(aes(x=Year, y=northing, col=Model),
             cex=2) +
  geom_ribbon(aes(x=Year,
                  ymin=northing - n.sd,
                  ymax=northing + n.sd,
                  fill=Model),
              alpha=0.3) +
  labs(y='Northing (km)', x='Year') +
  ggtitle('Northing')

ggplot(data=allmods) +
  geom_line(aes(x=Year, y=easting, col=Model),
            lwd=1) +
  geom_point(aes(x=Year, y=easting, col=Model),
             cex=2) +
  geom_ribbon(aes(x=Year,
                  ymin=easting - e.sd,
                  ymax=easting + e.sd,
                  fill=Model),
              alpha=0.3) +
  labs(y='Easting (km)', x='Year') +
  ggtitle('Easting')

#### Indices ####
# All covariates, GMRF, Omega 1 and 2 off
gmrf.ind <- read.csv(here('VAST_runs/tuna13_usonly/counterfactual/Fine Scale All Covs X1/Index.csv'))
gmrf.ind$Model <- 'GMRF with Covariates'

# All covariates, Coarse, Omega 1 and 2 off
crs.ind <- read.csv(here('VAST_runs/tuna13_usonly/counterfactual/All Covs X1 No Omega 1/Index.csv'))
crs.ind$Model <- 'Covariates Only'

# No covariates, GMRF, Omega 1 and 2 off
no.ind <- read.csv(here('VAST_runs/tuna13_usonly/counterfactual/Fine Scale No Covs/Index.csv'))
no.ind$Model <- 'GMRF without covariates'

# Final model, all covariates, Coarse, Omega 1 on, no covariates on x2
final.ind <- read.csv(here("VAST_runs/tuna13_usonly/counterfactual/All Covariates X1/Index.csv"))
final.ind$Model <- 'Final'

# Combine
allinds <- rbind(gmrf.ind, crs.ind, no.ind, final.ind)

# Fix
allinds <- allinds %>% 
  dplyr::select(-Category, -Stratum, -Units) %>% 
  rename(st.err = Std..Error.for.Estimate,
         st.err.ln = Std..Error.for.ln.Estimate.)

# Plot
ggplot(data=allinds) +
  geom_line(aes(x=Time, y=Estimate, col=Model)) +
  geom_point(aes(x=Time, y=Estimate, col=Model),
             cex=2) +
  geom_ribbon(aes(x=Time,
                    ymin=Estimate - st.err,
                    ymax=Estimate + st.err,
                    fill=Model),
                alpha = 0.3) +
  ggtitle('Relative Abundance')

#### Range Edges ####
# All covariates, GMRF, Omega 1 and 2 off
gmrf.re <- read.csv(here('VAST_runs/tuna13_usonly/counterfactual/Fine Scale All Covs X1/RangeEdges.csv'))
gmrf.re$Model <- 'GMRF with Covariates'

# All covariates, Coarse, Omega 1 and 2 off
crs.re <- read.csv(here('VAST_runs/tuna13_usonly/counterfactual/All Covs X1 No Omega 1/RangeEdges.csv'))
crs.re$Model <- 'Covariates Only'

# No covariates, GMRF, Omega 1 and 2 off
no.re <- read.csv(here('VAST_runs/tuna13_usonly/counterfactual/Fine Scale No Covs/RangeEdges.csv'))
no.re$Model <- 'GMRF without covariates'

# Final model, all covariates, Coarse, Omega 1 on, no covariates on x2
final.re <- read.csv(here("VAST_runs/tuna13_usonly/counterfactual/All Covariates X1/RangeEdges.csv"))
final.re$Model <- 'Final'

# Combine
alledge <- rbind(gmrf.re, crs.re, no.re, final.re)
alledge$Quantile <- as.factor(alledge$Quantile)

# Plot
ggplot(data=alledge) +
  geom_line(aes(x=Year, y=Northing.Est, col=Model, pch=Quantile),
            lwd=1) +
  geom_point(aes(x=Year, y=Northing.Est, col=Model, pch=Quantile),
             cex=3) +
  geom_ribbon(aes(x=Year,
                  ymin=Northing.Est - Northing.SD,
                  ymax=Northing.Est + Northing.SD,
                  fill=Model, pch=Quantile),
              alpha=0.3) +
  labs(y='Northing (km)', x='Year') +
  ggtitle('Northing')

ggplot(data=alledge) +
  geom_line(aes(x=Year, y=Easting.Est, col=Model, pch=Quantile),
            lwd=1) +
  geom_point(aes(x=Year, y=Easting.Est, col=Model, pch=Quantile),
             cex=3) +
  geom_ribbon(aes(x=Year,
                    ymin=Easting.Est - Easting.SD,
                    ymax=Easting.Est + Easting.SD,
                    fill=Model, pch=Quantile),
                alpha=0.3) +
  labs(y='Easting (km)', x='Year') +
  ggtitle('Easting')
