# Merge American and Canadian data

rm(list=ls())

# Load libraries and functions
library(here)
library(tidyverse)
library(sf)
library(TMB)
library(VAST)
library(units)
library(beepr)
library(ggcorrplot)
# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))
# Add unitless back as possible unit (removed in units package update Mar 2023)
install_unit(symbol='unitless', def='unitless', name='unitless')
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

# Load US data
us <- read.csv(here('Data/Clean/BFT_US_catch_VASTdata.csv'))

# Load Canada data
can.env <- read.csv(here('Data/Clean/AllYears_Canada_IncludeSLP.csv'))
can.env <- dplyr::select(can.env,
                         id, sst, sstsource, bathy, slp)
can.cat <- read.csv(here('Data/Clean/Canada_landings.csv'))
colnames(can.cat)[2] <- 'id'
can.cat$sst <- NULL
# Merge Canda
can <- left_join(can.cat, can.env, by=c('id'))
# Remove 1992
can <- subset(can, year > 1992)

# Merge with climate indices
# Add NAO
nao <- read.csv(here('Data/Climate_Indices/daily_nao.csv'))
nao$date <- as.POSIXct(paste0(nao$year, '-',
                              nao$month, '-',
                              nao$day),
                       format="%Y-%m-%d")

nao <- nao %>% 
  filter(year >= 1993 & year <=2020) %>% 
  filter(month %in% seq(6, 10, 1))
colnames(nao) <- c('year', 'month', 'day', 'nao', 'date')

nao <- dplyr::select(nao, date, nao)
head(nao)

can$date <- as.POSIXct(paste0(can$year, '-',
                              str_pad(can$month, 2, 'left', '0'), '-',
                              str_pad(can$day, 2, 'left', '0')),
                       format='%Y-%m-%d')

can <- merge(can, nao, by=c('date'))
head(can)

# Add AMO
amo <- read.csv(here('Data/Climate_Indices/monthly_amo.csv'))
amo <- amo %>% 
  filter(Year >= 1993 & Year <=2021) %>% 
  filter(Month %in% c('Jun', 'Jul', 'Aug', 'Sep', 'Oct'))
amo$monthno <- match(amo$Month,month.abb)
amo$yrmo <- paste0(amo$Year, '-', amo$monthno)
amo <- dplyr::select(amo, yrmo, Value)
colnames(amo) <- c('yrmo', 'amo')
can$yrmo <- paste0(can$year, '-', can$month)
can <- merge(can, amo, by=c('yrmo'))
head(can)

# Remove extraneous
can <- dplyr::select(can, -yrmo, -date)

# Add columns
can$tourn <- 1
can$depthsource <- 'NOAA'

# Merge
us$prey <- NULL
colnames(can)[13] <- 'depth'
can$data <- NULL

us$location <- 'us'
can$location <- 'can'

both <- rbind(can, us)

# Remove those sites out of subject area
nwat <- st_read(here('Data/GIS/Combined_Bluefin2.shp'))
nwat <- st_transform(nwat, 'EPSG:4326')

both.sf <- st_as_sf(both, coords=c('lon', 'lat'))
st_crs(both.sf) <- 'EPSG:4326'

good <- st_intersection(both.sf, nwat)

# This is it. Save.
finaldf <- sfheaders::sf_to_df(good, fill=T)
finaldf <- dplyr::select(finaldf,
                         -FID_1, -sfg_id, -point_id)
colnames(finaldf)[17:18] <- c('lon', 'lat')

write.csv(finaldf, 
          here('Data/Clean/BFT_BothCountries_VAST.csv'),
          row.names = F)
