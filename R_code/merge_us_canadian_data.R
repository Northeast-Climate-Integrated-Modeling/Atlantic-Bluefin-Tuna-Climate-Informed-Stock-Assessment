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
#install_unit(symbol='unitless', def='unitless', name='unitless')
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
us <- read.csv(here('Data/Clean/BFT_US_catch_VASTdata2.csv'))

# Load Canada data
can <- read.csv(here('Data/Canada_Clean/compiled_clean_canada.csv'))
can$date <- as.Date(paste(can$Year, can$Jday, sep="-"),"%Y-%j")
can$date <- as.POSIXct(can$date)

# Merge with climate indices
# Add NAO
nao <- read.csv(here('Data/Climate_Indices/daily_nao.csv'))
nao$date <- as.POSIXct(paste0(nao$year, '-',
                              nao$month, '-',
                              nao$day),
                       format="%Y-%m-%d")

nao <- nao %>% 
  filter(year >= 1996 & year <=2022) %>% 
  filter(month %in% seq(6, 12, 1))
colnames(nao) <- c('year', 'month', 'day', 'nao', 'date')

nao <- dplyr::select(nao, date, nao)
head(nao)

# can$date <- as.POSIXct(paste0(can$year, '-',
#                               str_pad(can$month, 2, 'left', '0'), '-',
#                               str_pad(can$day, 2, 'left', '0')),
#                        format='%Y-%m-%d')

can <- merge(can, nao, by=c('date'))
head(can)
us$date <- paste0(us$year, '-', str_pad(us$month, 2, 'left', '0'), '-',
                  str_pad(us$day, 2, 'left', '0'))
us$date <- as.POSIXct(us$date, format="%Y-%m-%d")
us <- merge(us, nao, by=c('date'))


# Add AMO
amo <- read.csv(here('Data/Climate_Indices/monthly_amo.csv'))
amo <- amo %>% 
  filter(Year >= 1996 & Year <=2022) %>% 
  filter(Month %in% c('Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
amo$monthno <- match(amo$Month,month.abb)
amo$yrmo <- paste0(amo$Year, '-', amo$monthno)
amo <- dplyr::select(amo, yrmo, Value)
colnames(amo) <- c('yrmo', 'amo')
can$yrmo <- paste0(can$Year, '-', lubridate::month(can$date))
can <- merge(can, amo, by=c('yrmo'))
head(can)
us$yrmo <- paste0(us$year, '-', lubridate::month(us$date))
us <- merge(us, amo, by=c('yrmo'))


# Remove extraneous
can <- dplyr::select(can, -yrmo, -date)
us <- dplyr::select(us, -yrmo, -date)

# Add columns
can$fhours <- can$Effort * 24

can <- can %>% 
  dplyr::select(Weight_Class, Trip_ID, Vessel_ID, spacetime,
                Year, Month, Jday, fhours, longitude, latitude,
                COUNT_for_size, SST, nao, amo, Gear)
can <- can %>% 
  rename(Size_class = Weight_Class) %>% 
  mutate(id = paste0(Trip_ID, "_",Vessel_ID, "_", spacetime)) %>% 
  rename(lon = longitude) %>% 
  rename(lat = latitude)

can <- can %>% 
  dplyr::select(-Trip_ID, -Vessel_ID, -spacetime, -Jday) %>% 
  rename(year = Year) %>% 
  rename(month = Month) %>% 
  rename(catch = COUNT_for_size) %>% 
  rename(sst = SST)

us <- us %>% 
  dplyr::select(-day, -depth, -tourn)

us$Gear <- 'RR'

# Merge
#us$prey <- NULL
#colnames(can)[13] <- 'depth'
#can$data <- NULL

us$location <- 'us'
can$location <- 'can'

both <- rbind(can, us)

# Add depth
# Pull NOAA bathymetry data
library(raster)
Bathy <- raster(here('Data/Bathy/gebco_2023.tif'))

# Convert data to raster
Bathy_Raster <- Bathy

# Join with characteristics of bathy raster
survs_sf <- st_as_sf(both, coords=c('lon', 'lat'))
st_crs(survs_sf) <- "EPSG:4326"
survs.spdf <- as(survs_sf, "Spatial")
survs.spdf$value <- raster::extract(Bathy_Raster,
                                    survs.spdf)
survs.df <- as.data.frame(survs.spdf)
survs.df <- dplyr::select(survs.df, id, value)
colnames(survs.df) <- c('id', 'BATHY.DEPTH')
survs_sf <- merge(survs_sf, survs.df, by="id")
survs_sf <- survs_sf[with(survs_sf,
                          order(id)),]
rownames(survs_sf) <- NULL
head(survs_sf)
survs_sf <- unique(survs_sf)
both <- sfheaders::sf_to_df(survs_sf, fill=TRUE)
both <- both %>% 
  dplyr::select(-sfg_id, -point_id) %>% 
  rename(lon=x) %>% 
  rename(lat=y)

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
finaldf <- finaldf %>% 
  rename(lon=x) %>% 
  rename(lat=y)

finaldf$Size_class[finaldf$Size_class == 'large'] <- 'Large'
finaldf$Size_class[finaldf$Size_class == 'small'] <- 'Small'
finaldf <- finaldf[finaldf$Size_class != 'Medium',]

finaldf <- finaldf[finaldf$BATHY.DEPTH < 0,]

finaldf$BATHY.DEPTH <- finaldf$BATHY.DEPTH * -1

finaldf <- finaldf[finaldf$year >= 1996,]

finaldf <- finaldf %>% 
  mutate_at(c('Size_class', 'location', 'Gear'), as.factor)

finaldf <- finaldf[finaldf$catch <=12,]

crap <- as.data.frame(table(finaldf$id))
crap <- crap[crap$Freq ==1,]

finaldf <- finaldf[finaldf$id %notin% crap$Var1,]

write.csv(finaldf, 
          here('Data/Clean/BFT_BothCountries_VAST2.csv'),
          row.names = F)
