# Final combine of station, catch, sst, and depth data

rm(list=ls())

# Load libraries
library(tidyverse)
library(sf)
library(here)

# Load data
dat.1993 <- read.csv(here('Data/Clean/LPS_Pre_2002_Clean.csv'))
dat.2021 <- read.csv(here('Data/Clean/LPS_Post_2002_Clean.csv'))
dat <- rbind(dat.1993, dat.2021)
dat <- dplyr::select(dat, -depth, -sst)

sst <- read.csv(here('Data/Clean/AllYears_UpdatedSST.csv'))
sst <- dplyr::select(sst, id, sst, sstsource)
dep <- read.csv(here('Data/Clean/AllYears_UpdatedDepth.csv'))
dep <- dplyr::select(dep, id, depth, depthsource)

rm(dat.1993, dat.2021)

# Check data
head(dat)
head(dep)
head(sst)

length(unique(dat$id))
length(unique(sst$id))
length(unique(dep$id))

# Merge data
dat <- left_join(dat, sst, by=c('id'))
dat <- left_join(dat, dep, by=c('id'))

# Check result
head(dat)
summary(dat$depth)
summary(dat$sst)

# Drop stations without depth information
dat <- dat %>% 
  drop_na(depth)

# Plot
coast <- ecodata::coast
coast <- st_transform(coast, crs='EPSG:4326')
dat.sf <- st_as_sf(dat, coords=c('lon', 'lat'))
st_crs(dat.sf) <- 'EPSG:4326'

ggplot()+
  geom_sf(data=coast, fill='gray') +
  geom_sf(data=dat.sf, aes(col=depth)) +
  coord_sf(xlim=c(-78, -65),
           ylim=c(35, 46))


summary(dat)

# Save
write.csv(dat, row.names = F, here('Data/Clean/BFT_US_catch_VASTdata.csv'))
