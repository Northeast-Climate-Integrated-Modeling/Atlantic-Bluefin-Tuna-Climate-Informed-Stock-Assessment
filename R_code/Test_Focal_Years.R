rm(list=ls())

library(VAST)
library(here)
library(sf)
library(tidyverse)
library(gridExtra)
library(egg)

#install_unit(symbol='unitless', def='unitless', name='unitless')

load(here('VAST_runs/tuna13_usonly/gom_only/tuna13_usonly_gomonly_0222_no04.RData'))
# Remove intermediates
rm(list=setdiff(ls(), c("surveys")))

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

# Why does the model not fit 2022 well?
gom <- st_read(here('Data/GIS/NAFO_5Y.shp'), quiet=T)
coast <- st_transform(ecodata::coast, crs="EPSG:4326")
survs.sf <- st_as_sf(surveys, coords=c('lon', 'lat'), crs="EPSG:4326")
gom <- st_transform(gom, crs="EPSG:4326")

# All survey information, annual means
annual.means <- surveys %>% 
  dplyr::select(-X, -id, -location, -Gear, -nao) %>% 
  mutate(Date = as.Date(paste0(year, '-', month, '-', day))) %>% 
  group_by(year) %>% 
  summarise(Date = mean(Date, na.rm=T),
            fhours = round(mean(fhours, na.rm=T), 2), 
            catch = round(mean(catch,  na.rm=T), 3),
            amo = round(mean(amo, na.rm=T), 2),
            sst = round(mean(sst, na.rm=T), 2),
            depth = round(mean(BATHY.DEPTH, na.rm=T), 2),
            prey = round(mean(prey, na.rm=T), 2),
            lines = round(mean(lines, na.rm=T), 2),
            lon = mean(lon, na.rm=T),
            lat = mean(lat, na.rm=T)) %>% 
  as.data.frame()
  
annual.means.no <- surveys %>% 
  filter(catch != 0) %>% 
  dplyr::select(-X, -id, -location, -Gear, -nao) %>% 
  mutate(Date = as.Date(paste0(year, '-', month, '-', day))) %>% 
  group_by(year) %>% 
  summarise(Date = mean(Date, na.rm=T),
            fhours = round(mean(fhours, na.rm=T), 2), 
            catch = round(mean(catch,  na.rm=T), 3),
            amo = round(mean(amo, na.rm=T), 2),
            sst = round(mean(sst, na.rm=T), 2),
            depth = round(mean(BATHY.DEPTH, na.rm=T), 2),
            prey = round(mean(prey, na.rm=T), 2),
            lines = round(mean(lines, na.rm=T), 2),
            lon = mean(lon, na.rm=T),
            lat = mean(lat, na.rm=T)) %>% 
  as.data.frame()

# Plots - COG, all observations
annual.sf <- st_as_sf(annual.means, coords=c('lon', 'lat'), crs="EPSG:4326")

cog.all <- ggplot() +
  geom_sf(data=coast, col=NA) +
  geom_sf(data=gom, fill=NA) +
  geom_sf(data=annual.sf,
          cex=1, alpha=0.4) +
  geom_sf(data=annual.sf[annual.sf$year %in% c(2004, 2022),],
          aes(col=as.factor(year)),
          cex=1, alpha=1) +
  coord_sf(xlim=c(-71, -70),
           ylim=c(42.2, 43)) +
  labs(col='Focal years', x=' ', y=' ') +
  annotate('text', x=-70.7, y=42.6, label='Gloucester',
           col='gray40') +
  ggtitle("Focal years fishing effort COG")

# Plots - COG, positive catches
annualno.sf <- st_as_sf(annual.means.no, coords=c('lon', 'lat'), crs="EPSG:4326")

cog.pos <- ggplot() +
  geom_sf(data=coast, col=NA) +
  geom_sf(data=gom, fill=NA) +
  geom_sf(data=annualno.sf,
          cex=1, alpha=0.4) +
  geom_sf(data=annualno.sf[annualno.sf$year %in% c(2004, 2022),],
          aes(col=as.factor(year)),
          cex=1, alpha=1) +
  coord_sf(xlim=c(-71, -70),
           ylim=c(42.2, 43)) +
  labs(col='Focal years', x=' ', y= ' ') +
  annotate('text', x=-70.7, y=42.6, label='Gloucester',
           col='gray40') +
  ggtitle("Focal years fishing effort COG - Positive catch only")

# Plots - 2004 sampling
dummyvar <- survs.sf %>% 
  dplyr::select(-X, -id, -location, -month, -day, -fhours, -Gear, -catch,
                -nao, -amo, -sst, -prey, -lines) %>% 
  filter(year != 2004) %>% 
  dplyr::select(-year) %>% 
  unique()

samps.04 <- ggplot() +
  geom_sf(data=coast, col=NA) +
  geom_sf(data=gom, fill=NA) +
  geom_sf(data=dummyvar,
          cex=0.2, alpha=0.2) +
  geom_sf(data=survs.sf[survs.sf$year %in% c(2004) &
               survs.sf$catch == 0,],
          cex=1, alpha=0.6) +
  geom_sf(data=survs.sf[survs.sf$year %in% c(2004) &
                          survs.sf$catch != 0,],
          aes(col=as.factor(catch)),
          #col='red',
          cex=1, alpha=0.9) +
  coord_sf(xlim=c(-71, -67),
           ylim=c(41.5, 45)) +
  labs(col='Fish caught', x=' ', y= ' ') +
  ggtitle("2004 Fishing Locations")

# Plots - 2022 sampling
dummyvar <- survs.sf %>% 
  dplyr::select(-X, -id, -location, -month, -day, -fhours, -Gear, -catch,
                -nao, -amo, -sst, -prey, -lines) %>% 
  filter(year != 2022) %>% 
  dplyr::select(-year) %>% 
  unique()
samps.22 <- ggplot() +
  geom_sf(data=coast, col=NA) +
  geom_sf(data=gom, fill=NA) +
  geom_sf(data=dummyvar,
          cex=0.2, alpha=0.2) +
  geom_sf(data=survs.sf[survs.sf$year %in% c(2022) &
                          survs.sf$catch == 0,],
          cex=1, alpha=0.6) +
  geom_sf(data=survs.sf[survs.sf$year %in% c(2022) &
                          survs.sf$catch != 0,],
          aes(col=as.factor(catch)),
          cex=1, alpha=0.9) +
  coord_sf(xlim=c(-71, -67),
           ylim=c(41.5, 45)) +
  labs(col='Fish caught', x=' ', y= ' ') +
  ggtitle("2022 Fishing Locations")

# Environment and effort over time
# Date 
surveys$Date <- as.Date(paste0(2024, '-', surveys$month, '-', surveys$day))
surveys$jday <- yday(as.Date(paste0(surveys$year, '-', surveys$month, '-', surveys$day)))
dat <- ggplot() +
  geom_jitter(data=surveys,
             aes(x=year, y=Date),
             width=0.2, alpha=0.4) +
  geom_smooth(method='lm',data=surveys,
              aes(x=year, y=Date)) +
  labs(x='Year', y='Fishing Date')
summary(lm(jday ~ year, data=surveys))
# Average shift 0.16 days later each year
# interpret with caution: Jun-Jul 2020 artificially removed, missing prey data

# Fhours 
fhrs <- ggplot() +
  geom_jitter(data=surveys,
             aes(x=year, y=fhours), 
             width =0.2, alpha=0.4) +
  geom_smooth(method='lm',data=surveys,
              aes(x=year, y=fhours)) +
  labs(x="Year", y='Fishing Hours')
summary(lm(fhours ~ year, data=surveys))
# Not significant

# Catch 
cat <- ggplot() +
  geom_jitter(data=surveys,
              aes(x=year, y=catch), 
              width =0.2, alpha=0.4) +
  geom_smooth(method='lm',data=surveys,
              aes(x=year, y=catch)) +
  labs(x='Year', y='Fish caught')
summary(lm(catch ~ year, data=surveys))
# Average catch 0.02 more fish per trip each year

# SST 
sst <- ggplot() +
  geom_jitter(data=surveys,
              aes(x=year, y=sst), 
              width =0.2, alpha=0.4) +
  geom_smooth(method='lm',data=surveys,
              aes(x=year, y=sst)) +
  labs(x='Year', y='SST (C)')
summary(lm(sst ~ year, data=surveys))
# Average shift 0.08 deg hotter each year

# Depth 
dep <- ggplot() +
  geom_jitter(data=surveys,
              aes(x=year, y=BATHY.DEPTH), 
              width =0.2, alpha=0.4) +
  geom_smooth(method='lm',data=surveys,
              aes(x=year, y=BATHY.DEPTH)) +
  labs(x='Year', y='Depth (m)')
summary(lm(BATHY.DEPTH ~ year, data=surveys))
# Average shift 0.47 m shallower each year

# Prey 
pry <- ggplot() +
  geom_jitter(data=surveys,
              aes(x=year, y=prey), 
              width =0.2, alpha=0.4) +
  geom_smooth(method='lm',data=surveys,
              aes(x=year, y=prey)) +
  labs(x='Year', y='log(Prey density)')
summary(lm(prey ~ year, data=surveys))
# Average shift 0.02 log(prey/sq km) less per year

# Lines 
lin <- ggplot() +
  geom_jitter(data=surveys,
              aes(x=year, y=lines), 
              width =0.2, alpha=0.4) +
  geom_smooth(method='lm',data=surveys,
              aes(x=year, y=lines)) +
  labs(x='Year', y='Lines per trip')
summary(lm(lines ~ year, data=surveys))
# Average 0.04 fewer lines per trip per year

# Remove intermediates
rm(annual.means, annual.means.no, annual.sf,
   annualno.sf, catchability_data, coast, covars, dummyvar,
   extrap_info_aja, fit, gom, scaled.covars, settings, strata_use,
   surveys, survs, survs.sf, vast_extrap_grid, hab_formula,
   working_dir)

# Space plots
samps <- grid.arrange(
  grobs=list(samps.04, samps.22),
  layout_matrix=rbind(c(1,2),
                      c(1,2))
)
rm(samps.04, samps.22)

# Cog plots
cog <- grid.arrange(
  grobs=list(cog.all, cog.pos),
  layout_matrix=rbind(c(1,2),
                      c(1,2))
)
rm(cog.all, cog.pos)

# Effort metrics
effort <- egg::ggarrange(dat + 
                 theme(axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.title.x = element_blank(),
                       plot.margin = margin(t = 1, b = 1, l=1) ), 
          fhrs + 
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title.x = element_blank(),
                  plot.margin = margin(t = 1, b = 1, l=1) ), 
          lin + theme(plot.margin = margin(l=1)) ,
          nrow = 3)
rm(dat, fhrs, lin)

# Environmental  metrics
env <- egg::ggarrange(dep + 
                 theme(axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.title.x = element_blank(),
                       plot.margin = margin(t = 1, b = 1, l=1) ), 
               pry + 
                 theme(axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.title.x = element_blank(),
                       plot.margin = margin(t = 1, b = 1, l=1) ), 
               sst + theme(plot.margin = margin(l=1)) ,
               nrow = 3)
rm(dep, pry, sst)

# Catch
cat
