# Final combine of station, catch, sst, and depth data

rm(list=ls())

# Load libraries
library(tidyverse)
library(sf)
library(here)

# Load data
dat <- read.csv(here('Data/Clean/LPS_LargeTarget_Clean_2024.csv'))
dat <- dplyr::select(dat, -depth, -sst)

sst <- read.csv(here('Data/Clean/AllYears_UpdatedSST.csv'))
sst <- dplyr::select(sst, id, sst, sstsource)
dep <- read.csv(here('Data/Clean/AllYears_UpdatedDepth.csv'))
dep <- dplyr::select(dep, id, depth, depthsource)
slp <- read.csv(here('Data/Clean/AllYears_IncludeSLP.csv'))
slp <- dplyr::select(slp, id, slp)

#rm(dat.1993, dat.2021)

# Check data
head(dat)
head(dep)
head(sst)
head(slp)

length(unique(dat$id))
length(unique(sst$id))
length(unique(dep$id))
length(unique(slp$id))

# Merge data
dat <- left_join(dat, sst, by=c('id'))
dat <- left_join(dat, dep, by=c('id'))
dat <- left_join(dat, slp, by=c('id'))

# Check result
head(dat)
summary(dat$depth)
summary(dat$sst)
summary(dat$slp)

# Merge with climate indices
# Add NAO
nao <- read.csv(here('Data/Climate_Indices/daily_nao.csv'))
nao$date <- as.POSIXct(paste0(nao$year, '-',
                              nao$month, '-',
                              nao$day),
                       format="%Y-%m-%d")

nao <- nao %>% 
  filter(year >= 1993 & year <=2021) %>% 
  filter(month %in% seq(6, 10, 1))
colnames(nao) <- c('year', 'month', 'day', 'nao', 'date')

nao <- dplyr::select(nao, date, nao)
head(nao)

dat$date <- as.POSIXct(paste0(dat$year, '-',
                              str_pad(dat$month, 2, 'left', '0'), '-',
                              str_pad(dat$day, 2, 'left', '0')),
                       format='%Y-%m-%d')

dat <- merge(dat, nao, by=c('date'))
head(dat)

# Add AMO
amo <- read.csv(here('Data/Climate_Indices/monthly_amo.csv'))
amo <- amo %>% 
  filter(Year >= 1993 & Year <=2021) %>% 
  filter(Month %in% c('Jun', 'Jul', 'Aug', 'Sep', 'Oct'))
amo$monthno <- match(amo$Month,month.abb)
amo$yrmo <- paste0(amo$Year, '-', amo$monthno)
amo <- dplyr::select(amo, yrmo, Value)
colnames(amo) <- c('yrmo', 'amo')
dat$yrmo <- paste0(dat$year, '-', dat$month)
dat <- merge(dat, amo, by=c('yrmo'))
head(dat)

# Remove extraneous
dat <- dplyr::select(dat, -yrmo, -date)

# Add prey in another file
# join_prey_vastoutput.R

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
