rm(list=ls())

library(VAST)
library(here)
library(tidyverse)

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', linewidth=1, fill=NA),
                legend.position = "inside",
                legend.background = element_rect(fill='transparent', colour = 'transparent'),
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

cog.u <- read.csv(here('VAST_runs/tuna13/natural_splines/US/COG.csv'))
cog.u$Model <- 'US'
cog.a <- read.csv(here('VAST_runs/tuna13/natural_splines/All/COG.csv'))
cog.a$Model <- 'All'
cog.c<- read.csv(here('VAST_runs/tuna13/natural_splines/CA/COG.csv'))
cog.c$Model <- 'Canada'
cog <- rbind(cog.a, cog.u, cog.c)

re.u <- read.csv(here('VAST_runs/tuna13/natural_splines/US/RangeEdges.csv'))
re.u$Model <- 'US'
re.a <- read.csv(here('VAST_runs/tuna13/natural_splines/All/RangeEdges.csv'))
re.a$Model <- 'All'
re.c<- read.csv(here('VAST_runs/tuna13/natural_splines/CA/RangeEdges.csv'))
re.c$Model <- 'Canada'
re <- rbind(re.a, re.u, re.c)

ind.u <- read.csv(here('VAST_runs/tuna13/natural_splines/US/Index.csv'))
ind.u$Model <- 'US'
ind.a <- read.csv(here('VAST_runs/tuna13/natural_splines/All/Index.csv'))
ind.a$Model <- 'All'
ind.c<- read.csv(here('VAST_runs/tuna13/natural_splines/CA/Index.csv'))
ind.c$Model <- 'Canada'
ind <- rbind(ind.a, ind.u, ind.c)

arrocc.u <- read.csv(here('VAST_runs/tuna13/natural_splines/US/AreaOcc.csv'))
arrocc.u$Model <- 'US'
arrocc.a <- read.csv(here('VAST_runs/tuna13/natural_splines/All/AreaOcc.csv'))
arrocc.a$Model <- 'All'
arrocc.c<- read.csv(here('VAST_runs/tuna13/natural_splines/CA/AreaOcc.csv'))
arrocc.c$Model <- 'Canada'
arrocc <- rbind(arrocc.a, arrocc.u, arrocc.c)

cog <- cog %>% 
  mutate(easting = easting / 1000,
         northing = northing / 1000,
         e.sd = e.sd / 1000,
         n.sd = n.sd / 1000) %>% 
  dplyr::select(-units)

arrocc <- arrocc %>% 
  mutate(area.occ = area.occ / 1000,
         std.err = std.err / 1000) %>% 
  dplyr::select(-units)

lm(northing ~ Year, data=cog[cog$Model == 'US',])
lm(easting ~ Year, data=cog[cog$Model == 'US',])

lm(northing ~ Year, data=cog[cog$Model == 'All',])
lm(easting ~ Year, data=cog[cog$Model == 'All',])

lm(northing ~ Year, data=cog[cog$Model == 'Canada',])
lm(easting ~ Year, data=cog[cog$Model == 'Canada',])

cog.northing <- ggplot() +
  geom_point(data=cog,
             aes(x=Year, y=northing, col=Model),
             cex=1) +
  geom_line(data=cog,
            aes(x=Year, y=northing, col=Model),
            lwd=1) +
  geom_ribbon(data=cog,
              aes(x=Year, ymin = northing - n.sd,
                  ymax=northing + n.sd,
                  fill=Model),
              alpha=0.2) +
  labs(x='Year', y='Northing (km)') +
  ggtitle('Northing, Center of Gravity')+
  theme(legend.position='bottom')

cog.easting <- ggplot() +
  geom_point(data=cog,
            aes(x=Year, y=easting, col=Model),
            cex=1) +
  geom_line(data=cog,
            aes(x=Year, y=easting, col=Model),
            lwd=1) +
  geom_ribbon(data=cog,
              aes(x=Year, ymin = easting - e.sd,
                  ymax=easting + e.sd,
                  fill=Model),
              alpha=0.2) +
  labs(x='Year', y='Easting (km)') +
  ggtitle('Easting, Center of Gravity') +
  theme(legend.position='bottom')

re$Quantile <- as.factor(re$Quantile)

lm(Northing.Est ~ Year, data=re[re$Quantile == '0.05' & re$Model == 'All',])
lm(Northing.Est ~ Year, data=re[re$Quantile == '0.5'& re$Model == 'All',])
lm(Northing.Est ~ Year, data=re[re$Quantile == '0.95' & re$Model == 'All',])
lm(Easting.Est ~ Year, data=re[re$Quantile == '0.05' & re$Model == 'All',])
lm(Easting.Est ~ Year, data=re[re$Quantile == '0.5'& re$Model == 'All',])
lm(Easting.Est ~ Year, data=re[re$Quantile == '0.95' & re$Model == 'All',])

re.northing <- ggplot() +
  geom_line(data=re[re$Year <=2022,],
             aes(x=Year, y=Northing.Est, 
                 group=interaction(Model,Quantile))) +
  geom_point(data=re[re$Year <=2022,],
            aes(x=Year, y=Northing.Est, 
                pch=Quantile, col=Model)) +
  geom_ribbon(data=re[re$Year <=2022,],
              aes(x=Year, ymin=Northing.Est - Northing.SD,
                  ymax=Northing.Est + Northing.SD,
                  group=interaction(Model, Quantile),
                  fill=Model),
              alpha=0.2) +
  labs(x='Year', y='Northing (km)') +
  ggtitle('Northing')+
  theme(legend.position = 'bottom')

re.easting <- ggplot() +
  geom_line(data=re[re$Year <=2022,],
            aes(x=Year, y=Easting.Est, 
                group=interaction(Model,Quantile))) +
  geom_point(data=re[re$Year <=2022,],
             aes(x=Year, y=Easting.Est, 
                 pch=Quantile, col=Model)) +
  geom_ribbon(data=re[re$Year <=2022,],
              aes(x=Year, ymin=Easting.Est - Easting.SD,
                  ymax=Easting.Est + Easting.SD,
                  group=interaction(Model, Quantile),
                  fill=Model),
              alpha=0.2) +
  labs(x='Year', y='Easting (km)') +
  ggtitle('Easting')+
  theme(legend.position = 'bottom')

rel.abund <- ggplot() +
  geom_line(data=ind[ind$Model=='All',],
            aes(x=Time, y=Estimate, col=Stratum)) +
  geom_ribbon(data=ind[ind$Model == 'All',],
              aes(x=Time, ymin=Estimate-`Std..Error.for.Estimate`,
                  ymax=Estimate + `Std..Error.for.Estimate`,
                  fill=Stratum),
              alpha=0.2) +
  ggtitle('Relative Abundance') +
  theme(legend.position.inside = c(0.1, 0.8))

lm(area.occ ~ Year, data=arrocc[arrocc$Model == 'US',])
lm(area.occ ~ Year, data=arrocc[arrocc$Model == 'All',])
lm(area.occ ~ Year, data=arrocc[arrocc$Model == 'Canada',])

eff.arr.oc <- ggplot() +
  geom_line(data=arrocc[arrocc$Year<=2022,],
            aes(x=Year, y=area.occ, col=Model)) +
  geom_ribbon(data=arrocc[arrocc$Year<=2022,],
              aes(x=Year, 
                  ymin=area.occ-std.err,
                  ymax=area.occ+std.err,
                  fill=Model),
              alpha=0.2) +
  ggtitle('Effective Area Occupied') +
  ylab('Area Occupied (km)') +
  theme(legend.position = 'bottom')

# Remove intermediates
rm(list=setdiff(ls(), c('cog.easting', 'cog.northing',
                        're.easting', 're.northing', 
                        'rel.abund', 'eff.arr.oc')))
setwd(here('VAST_runs/tuna13/natural_splines'))
# Save plots
ggsave('cog_easting.png', cog.easting, width=6, height=3)
ggsave('cog_northing.png', cog.northing, width=6, height=3)

ggsave('re_easting.png', re.easting, width=6, height=4)
ggsave('re_northing.png', re.northing, width=6, height=4)

ggsave('rel_abund.png', rel.abund, width=6, height=3)

ggsave('area_occupied.png', eff.arr.oc, width=6, height=3)
