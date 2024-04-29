# Temporal and spatial effort exploration

#### Workspace setup ####
# Clear workspace
rm(list=ls())

# Load libraries
suppressPackageStartupMessages(library(VAST))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(tidyverse, quietly=T, verbose=F))
suppressPackageStartupMessages(library(sf, quietly=T, verbose=F))
suppressPackageStartupMessages(library(sp, quietly=T, verbose=F))

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

#### Add sample information and covars ####
# Load data
lines <- read.csv(here('Data/Clean/LPS_LargeTarget_Clean_2024_2.csv'))
lines <- dplyr::select(lines, id, lines)

surveys <- read.csv(here("Data/Clean/BFT_BothCountries_Large_Prey_VAST_5.csv"))
surveys <- surveys %>% 
  filter(location == 'us') %>% 
  rename(prey = Abundance)

surveys <- left_join(surveys, lines, by=c('id'))
surveys <- surveys %>% 
  drop_na(lines)

# Remove points not in model domain (on land)
region_shape <- st_read(here('Data/GIS/NAFO_5Y.shp'),
                        quiet=T)
region_shape <- as(region_shape, 'Spatial')
region_shape <- st_as_sf(spTransform(region_shape, CRS("+init=epsg:4326")))
region_shape <- st_make_valid(region_shape)
region_shape <- dplyr::select(region_shape, geometry)
region_shape$Region <- 'US'
colnames(region_shape) <- c('geometry', 'Region')
st_geometry(region_shape) <- 'geometry'
region_shape <- dplyr::select(region_shape, Region, geometry)

surveys_sf <- st_as_sf(surveys, coords=c('lon', 'lat'))
st_crs(surveys_sf) <- 'EPSG:4326'

surveys_sf <- st_intersection(surveys_sf, region_shape)
surveys <- sfheaders::sf_to_df(surveys_sf, fill=T)
surveys <- surveys %>% 
  dplyr::select(-Region, -sfg_id, -point_id) %>% 
  rename(sst = oisst,
         lon=x, 
         lat=y)

# Test for missing values
surveys <- surveys[!is.na(surveys$amo) &
                     #!is.na(surveys$nao) &
                     !is.na(surveys$BATHY.DEPTH) &
                     !is.na(surveys$sst) &
                     !is.na(surveys$prey),]
# Lose 104 fishing trips mostly from June-July 2020 (no herring observer data)

#### Finalize sampling data inputs ####
# Save sampling data
# Remove samples east of -69 deg lon
survs <- surveys %>% 
  filter(lon < -69)

survs.sf <- st_as_sf(survs, coords=c('lon', 'lat'), crs="EPSG:4326")

# Remove intermediates
rm(list=setdiff(ls(), c("survs", 'survs.sf')))

# Temporal effort
survs$date <- as.Date(paste0(survs$year, '-', survs$month, '-', survs$day))
md <- survs %>% 
  group_by(year) %>% 
  summarise(min.date = min(date),
            max.date = max(date),
            mean.date = mean(date),
            median.date = median(date),
            sd.date = sd(date))

md$min.date <- as.Date(paste0('2002-', substr(md$min.date, start=6, stop=10)))
md$max.date <- as.Date(paste0('2002-', substr(md$max.date, start=6, stop=10)))
md$mean.date <- as.Date(paste0('2002-', substr(md$mean.date, start=6, stop=10)))
md$median.date <- as.Date(paste0('2002-', substr(md$median.date, start=6, stop=10)))

md$sd.min <- md$mean.date - md$sd.date
md$sd.max <- md$mean.date + md$sd.date

temef <- ggplot(data=md) +
  geom_line(aes(x=year, y=mean.date)) +
  geom_point(aes(x=year, y=mean.date)) +
  geom_errorbar(aes(x=year, ymin=sd.min, ymax=sd.max),
                width=0.2) +
  geom_ribbon(aes(x=year, ymin=min.date, ymax=max.date),
              alpha=0.2) + 
  labs(x='Year', y='Date within year') +
  ggtitle('Mean, SD, and bounds of temporal fishing effort in GOM')

rm(md)

# Spatial effort 
coast <- st_transform(ecodata::coast, crs="EPSG:4326")
sb <- st_transform(st_read(here('Data/GIS/Stellwagen_NMS.shp'), quiet=T),
                   st_crs(coast))

ms <- survs %>% 
  summarise(mean.lat = mean(lat),
            mean.lon = mean(lon),
            sd.lat = sd(lat),
            sd.lon = sd(lon)) %>% 
  st_as_sf(coords=c('mean.lon', 'mean.lat'), crs="EPSG:4326")

oveff <- ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=sb, col='darkgray', lwd=0.3, fill=NA) +
  geom_sf(data=survs.sf, pch='x', col='darkgray') +
  geom_sf(data=ms) +
  coord_sf(xlim=c(-71, -69),
           ylim=c(41.5, 44)) +
  theme(axis.text.x = element_text(angle=35, size=8),
        axis.text.y = element_text(size=8)) +
  ggtitle('Spatial effort COG, entire TS')
rm(ms)

# Spatiotemporal effort - year
ms <- survs %>% 
  group_by(year) %>% 
  summarise(mean.lat = mean(lat),
            mean.lon = mean(lon),
            sd.lat = sd(lat),
            sd.lon = sd(lon))

ms <- st_as_sf(ms, coords=c('mean.lon', 'mean.lat'), crs="EPSG:4326")

ml <- ms %>%
  #filter(year != 2022) %>% 
  dplyr::summarize(do_union=FALSE) %>%  # do_union=FALSE doesn't work as well
  st_cast("MULTILINESTRING") 

fl <- ms %>% 
  filter(year %in% c(2002, 2022))

yeff <- ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=sb, col='darkgray', lwd=0.3, fill=NA) +
  geom_sf(data=survs.sf, pch='x', col='darkgray') +
  geom_sf(data=ml, lwd=0.3) +
  geom_sf(data=fl, aes(col=as.factor(year))) +
  coord_sf(xlim=c(-71, -69),
           ylim=c(41.5, 44)) +
  labs(col='Year') +
  theme(axis.text.x = element_text(angle=35, size=8),
        axis.text.y = element_text(size=8),
        legend.position='inside',
        legend.position.inside = c(0.55, 0.05),
        legend.direction = 'horizontal',
        legend.text = element_text(size=8),
        legend.title=element_text(size=8),
        legend.background = element_rect(linewidth = 0, fill='transparent')) +
  ggtitle('Spatial effort shift per year') 
rm(fl, ml, ms)

# Spatiotemporal effort - month
ms <- survs %>% 
  #filter(year != 2022) %>% 
  group_by(month) %>% 
  summarise(mean.lat = mean(lat),
            mean.lon = mean(lon),
            sd.lat = sd(lat),
            sd.lon = sd(lon))

ms <- st_as_sf(ms, coords=c('mean.lon', 'mean.lat'), crs="EPSG:4326")

ml <- ms %>%
  dplyr::summarize(do_union=FALSE) %>%  # do_union=FALSE doesn't work as well
  st_cast("MULTILINESTRING") 

meff <- ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=sb, col='darkgray', lwd=0.3, fill=NA) +
  geom_sf(data=survs.sf, pch='x', col='darkgray') +
  geom_sf(data=ml, lwd=0.3) +
  geom_sf(data=ms, aes(col=as.factor(month))) +
  coord_sf(xlim=c(-71, -69),
           ylim=c(41.5, 44)) +
  labs(col='Month') +
  theme(axis.text.x = element_text(angle=35, size=8),
        axis.text.y = element_text(size=8),
        legend.position='inside',
        legend.position.inside = c(0.85, 0.25),
        legend.direction = 'vertical',
        legend.text = element_text(size=8),
        legend.title=element_text(size=8),
        legend.background = element_rect(linewidth = 0, fill='transparent')) +
  ggtitle('Spatial effort shift per month, disregard year')
rm(ms, ml)

# Spatiotemporal effort - year, month
ms <- survs %>% 
  group_by(year, month) %>% 
  summarise(mean.lat = mean(lat),
            mean.lon = mean(lon),
            sd.lat = sd(lat),
            sd.lon = sd(lon))

ms <- st_as_sf(ms, coords=c('mean.lon', 'mean.lat'), crs="EPSG:4326")

ml <- ms %>%
  group_by(month) %>%
  dplyr::summarize(do_union=FALSE) %>%  # do_union=FALSE doesn't work as well
  st_cast("MULTILINESTRING") 

fl <- ms %>% 
  filter(year %in% c(2002, 2022))

ymeff <- ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=sb, col='darkgray', lwd=0.3, fill=NA) +
  geom_sf(data=survs.sf, pch='x', col='darkgray') +
  geom_sf(data=ml, lwd=0.3) +
  geom_sf(data=fl, aes(col=as.factor(year))) +
  coord_sf(xlim=c(-71, -69),
           ylim=c(41.5, 44)) +
  facet_wrap(vars(month)) +
  labs(col='Year') +
  theme(axis.text.x = element_text(angle=20, size=10),
        axis.text.y = element_text(size=10),
        strip.text.x=element_text(margin=margin(0.1,0,0.1,0, "cm"))) +
  ggtitle('Spatial effort shift per month-year')
rm(fl, ml, ms)

ggsave(here('US_effort_illustrations/temporal_effort.png'),
       temef)

ggsave(here('US_effort_illustrations/spatial_effort.png'),
       oveff)

ggsave(here('US_effort_illustrations/spatial_yearly_effort.png'),
       yeff)

ggsave(here('US_effort_illustrations/spatial_monthly_effort.png'),
       meff)

ggsave(here('US_effort_illustrations/spatial_ym_effort.png'),
       ymeff, height=11, width =8.5, units='in')
