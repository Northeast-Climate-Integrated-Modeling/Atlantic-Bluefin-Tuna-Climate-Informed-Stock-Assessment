rm(list=ls())
library(tidyverse)
library(here)
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

# Load model fits
load(here('VAST_runs/tuna13_usonly/counterfactual/allcovs_finescale_omega1off.RData'))
gmrf <- fit
rm(list=setdiff(ls(), c("gmrf")))

load(here("VAST_runs/tuna13_usonly/counterfactual/allcovs_x1.RData"))
final <- fit
rm(list=setdiff(ls(), c('gmrf', 'final')))

# Load indices
ind.gmrf <- read.csv(here('VAST_runs/tuna13_usonly/counterfactual/Fine Scale All Covs X1/Index.csv'))
ind.gmrf$Model <- 'GMRF with Covs'
ind.final <- read.csv(here('VAST_runs/tuna13_usonly/counterfactual/All Covariates X1/Index.csv'))
ind.final$Model <- 'Final Model'

ind <- rbind(ind.gmrf, ind.final)

head(ind)

ind$upper <- ind$Estimate + ind$Std..Error.for.Estimate
ind$lower <- ind$Estimate - ind$Std..Error.for.Estimate

for(i in 1:nrow(ind)){
  if(ind$lower[i]< 0){
    ind$lower[i] <- 0
  }
}

#ind$Stratum[ind$Stratum == 'Stratum_1'] <- 'Canada'
#ind$Stratum[ind$Stratum == 'Stratum_2'] <- 'US'
#ind$Stratum[ind$Stratum == 'Stratum_3'] <- 'Both'

head(ind)

library(ggplot2)

ind.p <- ggplot() +
  geom_line(data=ind, 
            aes(x=Time, y=Estimate, col=Model)) +
  geom_point(data=ind,
             aes(x=Time, y=Estimate, col=Model),
             cex=1.5) +
  geom_ribbon(data=ind, alpha=0.2,
              aes(x=Time, ymin=lower, ymax=upper,
                  fill=Model))+
  #facet_wrap(vars(Category),
  #           scales = "free_y") +
  labs(x='Year', 
       y='Estimate') +
  theme(strip.text.x = element_text(size=12))

ggsave(ind.p,
       filename=paste0(here('VAST_runs/tuna13_usonly/counterfactual'), "/index_usonly.png"),
       width = 8, height = 4, units='in')

rm(ind.final, ind.gmrf, ind.p)

# Load center of gravity
cog.gmrf <- read.csv(here('VAST_runs/tuna13_usonly/counterfactual/Fine Scale All Covs X1/COG.csv'))
cog.gmrf$Model <- 'GMRF with Covs'
cog.final <- read.csv(here('VAST_runs/tuna13_usonly/counterfactual/All Covariates X1/COG.csv'))
cog.final$Model <- 'Final Model'

cog <- rbind(cog.gmrf, cog.final)

cog <- cog %>% 
  mutate(easting = easting / 1000,
         northing = northing / 1000,
         e.sd = e.sd / 1000,
         n.sd = n.sd / 1000) %>% 
  dplyr::select(-units)

northing <- ggplot(data=cog) +
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
  ggtitle('Center of Gravity - Northing')

easting <- ggplot(data=cog) +
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
  ggtitle('Center of Gravity - Easting')

ggsave(northing,
       filename=paste0(here('VAST_runs/tuna13_usonly/counterfactual'), "/cog_northing.png"),
       width = 8, height = 4, units='in')

ggsave(easting,
       filename=paste0(here('VAST_runs/tuna13_usonly/counterfactual'), "/cog_easting.png"),
       width = 8, height = 4, units='in')

rm(northing, easting, cog.final, cog.gmrf)

# Load range edges
re.gmrf <- read.csv(here('VAST_runs/tuna13_usonly/counterfactual/Fine Scale All Covs X1/RangeEdges.csv'))
re.gmrf$Model <- 'GMRF with Covs'
re.final <- read.csv(here('VAST_runs/tuna13_usonly/counterfactual/All Covariates X1/RangeEdges.csv'))
re.final$Model <- 'Final Model'

re <- rbind(re.gmrf, re.final)
re$Quantile <- as.factor(re$Quantile)

northing <- ggplot(data=re) +
  geom_line(aes(x=Year, y=Northing.Est, col=Model, pch=Quantile),
            lwd=1) +
  geom_point(aes(x=Year, y=Northing.Est, col=Model, pch=Quantile),
             cex=2.5) +
  geom_ribbon(aes(x=Year,
                  ymin=Northing.Est - Northing.SD,
                  ymax=Northing.Est + Northing.SD,
                  fill=Model, pch=Quantile),
              alpha=0.3) +
  labs(y='Northing (km)', x='Year') +
  ggtitle('Range Edges - Northing')

easting <- ggplot(data=re) +
  geom_line(aes(x=Year, y=Easting.Est, col=Model, pch=Quantile),
            lwd=1) +
  geom_point(aes(x=Year, y=Easting.Est, col=Model, pch=Quantile),
             cex=2.5) +
  geom_ribbon(aes(x=Year,
                  ymin=Easting.Est - Easting.SD,
                  ymax=Easting.Est + Easting.SD,
                  fill=Model, pch=Quantile),
              alpha=0.3) +
  labs(y='Easting (km)', x='Year') +
  ggtitle('Range Edges - Easting')

ggsave(northing,
       filename=paste0(here('VAST_runs/tuna13_usonly/counterfactual'), "/edges_northing.png"),
       width = 8, height = 4, units='in')

ggsave(easting,
       filename=paste0(here('VAST_runs/tuna13_usonly/counterfactual'), "/edges_easting_edges.png"),
       width = 8, height = 4, units='in')

rm(easting, northing, re.final, re.gmrf)


eao <- rbind(eao.gmrf, eao.final)

arroc <- ggplot(data=eao) +
  geom_line(aes(x=Year, y=area.occ, col=Model)) +
  geom_point(aes(x=Year, y=area.occ, col=Model),
             cex=1.5) +
  geom_ribbon(alpha=0.2,
              aes(x=Year, ymin=area.occ - sd.err, 
                  ymax=area.occ + sd.err,
                  fill=Model))+
  labs(x='Year', 
       y='Area Occupied (sq km)') +
  ggtitle('Effective Area Occupied')

ggsave(arroc,
       filename=paste0(here('VAST_runs/tuna13_usonly/counterfactual'), "/area_occupied.png"),
       width = 8, height = 4, units='in')
