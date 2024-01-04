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
us <- read.csv(here('Data/Clean/LPS_LargeTarget_Clean_2024.csv'))

# Load Canada data
can <- read.csv(here('Data/Canada_Clean/compiled_clean_canada.csv'))
can$date <- as.Date(paste(can$Year, can$Jday, sep="-"),"%Y-%j")
can$date <- as.POSIXct(can$date)
can <- can[can$Year >=2002,]

# Merge with climate indices
# Add NAO
nao <- read.csv(here('Data/Climate_Indices/daily_nao.csv'))
nao$date <- as.POSIXct(paste0(nao$year, '-',
                              nao$month, '-',
                              nao$day),
                       format="%Y-%m-%d")

nao <- nao %>% 
  filter(year >= 2002 & year <=2022) %>% 
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
  filter(Year >= 2002 & Year <=2022) %>% 
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

# Day
can$day <- lubridate::day(can$date)

# Remove extraneous
can <- dplyr::select(can, -yrmo, -date)
us <- dplyr::select(us, -yrmo, -date)

# Add columns
can$fhours <- can$Effort * 24

can <- can %>% 
  dplyr::select(Weight_Class, Trip_ID, Vessel_ID, spacetime,
                Year, Month, day, Jday, fhours, longitude, latitude,
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
  dplyr::select(-depth)

us$Gear <- 'RR'

# Merge
#us$prey <- NULL
#colnames(can)[13] <- 'depth'
#can$data <- NULL

us$location <- 'us'
can$location <- 'can'

# Remove smaller sizes from Canada
can <- can[can$Size_class == 'Large',]

# Edit for merging
can <- dplyr::select(can, -Size_class)
us <- dplyr::select(us, -prim_op, -lines, -party, -bt_art, -bt_live, -bt_dead,
                    -fm_troll, -fm_chunk, -fm_chum, -state, -primary, 
                    -secondary)
us <- us %>% 
  rename(catch = catch_n)

both <- rbind(can, us)

both <- dplyr::select(both,
                      id, location, year, month, day, lon, lat,
                      fhours, Gear, catch, sst, nao, amo)
both <- both[with(both, order(location, year, month, id)),]
rownames(both) <- NULL

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

# Plot
coast <- st_transform(ecodata::coast, st_crs(both.sf))

ggplot() +
  geom_sf(data=coast, fill="gray") +
  geom_sf(data=nwat, col='blue', fill=NA) +
  geom_sf(data=both.sf, cex=0.4) +
  coord_sf(xlim=c(st_bbox(nwat)[1], st_bbox(nwat)[3]),
           ylim=c(st_bbox(nwat)[2], st_bbox(nwat)[4]))

good <- st_intersection(both.sf, nwat)

# This is it. Save.
finaldf <- sfheaders::sf_to_df(good, fill=T)
finaldf <- dplyr::select(finaldf,
                         -FID_1, -sfg_id, -point_id)
finaldf <- finaldf %>% 
  rename(lon=x) %>% 
  rename(lat=y)

finaldf <- finaldf[finaldf$BATHY.DEPTH < 0,]

finaldf$BATHY.DEPTH <- finaldf$BATHY.DEPTH * -1

finaldf <- finaldf[finaldf$year >= 2002,]

finaldf <- finaldf %>% 
  mutate_at(c('location', 'Gear'), as.factor)

finaldf <- finaldf[finaldf$catch <=12,]

summary(finaldf)

write.csv(finaldf, 
          here('Data/Clean/BFT_BothCountries_VAST3.csv'),
          row.names = F)

rm(list=setdiff(ls(), "finaldf"))

### Plot check ###
# Load spatial information
coast <- ecodata::coast
coast <- st_transform(coast, "EPSG:4326")
us <- st_read(here('Data/GIS/NWAtlantic.shp'), quiet=T)
us <- st_transform(us, st_crs(coast))
us$Region <- 'US'
us <- dplyr::select(us, -FID)
can <- st_read(here('Data/GIS/CanadaEEZ.shp'), quiet=T)
can <- st_transform(can, st_crs(coast))
can$Region <- 'Canada'
can <- dplyr::select(can, -OBJECTID, -Id, -Shape_Leng, -Shape_Area)
stocks <- rbind(us, can)
stocks <- st_transform(stocks, st_crs(coast))
stocks <- st_make_valid(stocks)

# COnvert to sf
surv.sf <- st_as_sf(finaldf, coords=c('lon', 'lat'))
st_crs(surv.sf) <- st_crs(coast)

# Plot
yp.l.1 <- ggplot() +
  geom_sf(data=stocks, 
          aes(fill=Region), alpha=0.1) +
  geom_sf(data=coast) +
  geom_sf(data=surv.sf[surv.sf$catch !=0,],
          aes(cex=catch),
          cex=0.75, alpha=0.7) +
  # geom_sf(data=surv.sf[surv.sf$catch ==0,],
  #         pch='x', col='black',
  #         cex=0.75) +
  scale_color_viridis_c(option='viridis') +
  coord_sf(xlim=c(-78, -55),
           ylim=c(35, 47)) +
    facet_wrap(vars(year))

ggsave(plot=yp.l.1,
       filename=here('Plot_Output/LargeSize_Dist_0222.png'),
       device='png',
       height=7.5, width=10, units = 'in')
