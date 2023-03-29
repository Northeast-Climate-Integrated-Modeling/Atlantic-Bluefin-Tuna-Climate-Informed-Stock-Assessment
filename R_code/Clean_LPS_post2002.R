rm(list=ls())

# Load libraries and functions
library(here)
library(tidyverse)
library(sf)
# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))
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

# Create dataframe of state codes
statecodes <- data.frame(
  stcode = c(9,10,23,24,25,33,34,36,44,51),
  state = c('CT', 'DE', 'ME', 'MD', 'MA', 'NH', 'NJ', 'NY', 'RI', 'VA')
)

# Create dataframe of species size codes
speccode <- data.frame(
  prim = c(4673, 4677, 4678, 4676, 4679, 4671, 4670, 4672),
  size = c('young school', 'school', 'large school', 'small med',
           'large med', 'giant', 'unknown', 'school or large school')
)

# Load data pulled directly from NMFS
dat <- read.csv(here('Data/LPS/ALL/LPS_trip_level_0221.csv'))
# Reduce dataframe to necessesities
dat <- dplyr::select(dat,
                     id, year, month, day,
                     stcode, prim_op,
                     prim1, prim2, fhours,
                     latddmm, londdmm, depth, sst,
                     young_school_bft, school_bft, large_school_bft,
                     small_med_bft, large_med_bft, giant_bft, unk_bft)
# Add total catch
dat$tot_bft <- dat$young_school_bft + dat$school_bft + dat$large_school_bft +
  dat$small_med_bft + dat$large_med_bft + dat$giant_bft + dat$unk_bft

# FILTER -- 
# June through october, private chartered or headboat, fished bt 1 and 24 hours
target_bft <- subset(dat,
                     month %in% c(6:10) &
                     prim_op %in% c(1,2,3) &
                     fhours > 0.99 &
                     fhours < 24.01)
# Trips must intentionally target bluefin of some any class
target_bft=subset(target_bft,
                  prim1 %in% c(4673, 4677, 4678, 4676, 4679, 4671, 4670, 4672)|
                  prim2 %in% c(4673, 4677, 4678, 4676, 4679, 4671, 4670, 4672))

# Alter state from code to abbrev
target_bft <- merge(target_bft, statecodes, by=c('stcode'))
target_bft$stcode <- NULL

# Alter species codes
colnames(speccode) <- c('prim1', 'primary')
target_bft <- merge(target_bft, speccode, by=c('prim1'), all=T)
colnames(speccode) <- c('prim2', 'secondary')
target_bft <- merge(target_bft, speccode, by=c('prim2'), all=T)
target_bft <- dplyr::select(target_bft, -prim1, -prim2)

# Remove observations without spatial data
target_bft <- subset(target_bft, londdmm < 9000)
target_bft <- subset(target_bft, latddmm < 9000)

# Calculate lat-lon
# This is reported in DDMM, so must be converted
# First, remove instances where MM > 60 for both lon and lat: reported wrong
target_bft$latmm <- substr(target_bft$latddmm,start=3,stop=4)
target_bft$lonmm <- substr(target_bft$londdmm,start=3,stop=4)
badvals <- target_bft[target_bft$latmm > 60 |
                        target_bft$lonmm > 60,]
target_bft <- subset(target_bft,
                     (substr(target_bft$londdmm, start=3, stop=4) < 61))
target_bft <- subset(target_bft,
                     (substr(target_bft$latddmm, start=3, stop=4) < 61))
                     
# Calculate decimal degrees from degree-minutes
target_bft$lon <- (as.numeric(substr(target_bft$londdmm, start=1, stop=2)) +
  (as.numeric(substr(target_bft$londdmm, start=3, stop = 4)) / 60)) * (-1)
target_bft$lat <- (as.numeric(substr(target_bft$latddmm, start=1, stop=2)) +
  (as.numeric(substr(target_bft$latddmm, start=3, stop = 4)) / 60))

# Check accuracy of lat-lon
bft_sf <- st_as_sf(target_bft, coords=c('lon', 'lat'), crs='EPSG:4326')
coast <- st_transform(ecodata::coast, crs=st_crs(bft_sf))
# Plot just to see
nwat <- st_read(here('Data/GIS/NWAtlanticshp.shp'))
nwat <- st_transform(nwat, crs=st_crs(coast))

# Remove points on land
ggplot(coast)+
  geom_sf(fill='gray') +
  geom_sf(data=nwat, fill='lightblue') +
  geom_sf(data=bft_sf, cex=0.5) +
  coord_sf(xlim=c(-79, -65),
           ylim=c(34, 47)) 

inwat <- st_intersection(bft_sf, nwat)

bft_sf <- bft_sf[bft_sf$id %in% inwat$id,]
target_bft <- target_bft[target_bft$id %in% inwat$id,]

# Calculate SST in celsius
target_bft$sst[target_bft$sst > 90] <- NA
target_bft$sst <- weathermetrics::fahrenheit.to.celsius(target_bft$sst, 
                                                        round = 2)
# Fix depth
target_bft$depth <- target_bft$depth * -1
target_bft$depth[target_bft$depth < -9000] <- NA

# Order 
target_bft <- target_bft[with(target_bft, order(year, month, day, id)),]
rownames(target_bft) <- NULL
head(target_bft)

# Remove unnecessary columns
target_bft <- dplyr::select(target_bft, id, year, fhours, depth, sst,
                            young_school_bft, school_bft, large_school_bft,
                            large_med_bft, giant_bft, lon, lat)


# Create one row for each kind of catch
dat.list <- split(target_bft, f=target_bft$id)
catchtype2 <- c('small', 'large')
tempdf <- data.frame(
  Size_class = catchtype2,
  id=rep(NA, length(catchtype2)),
  year=rep(NA, length(catchtype2)),
  #month=rep(NA, length(catchtype)),
  #day=rep(NA, length(catchtype)),
  fhours=rep(NA, length(catchtype2)),
  depth=rep(NA, length(catchtype2)),
  sst=rep(NA, length(catchtype2)),
  lon=rep(NA, length(catchtype2)),
  lat=rep(NA, length(catchtype2)),
  
  catch=rep(NA, length(catchtype2))
  
)

for(i in 1:length(dat.list)){
  temp <- dat.list[[i]]
  
  tempdf.in <- tempdf
  
  tempdf.in$id <- temp$id
  tempdf.in$year <- temp$year
  #tempdf.in$month <- temp$month
  #tempdf.in$day <- temp$day
  tempdf.in$fhours <- temp$fhours
  tempdf.in$depth <- temp$depth
  tempdf.in$sst <- temp$sst
  tempdf.in$lon <- temp$lon
  tempdf.in$lat <- temp$lat
  
  tempdf.in$catch[1] <- temp$young_school_bft +temp$school_bft+temp$large_school_bft
  tempdf.in$catch[2] <- temp$large_med_bft + temp$giant_bft
  
  
  dat.list[[i]] <- tempdf.in
  
  rm(temp, tempdf.in)
}
dat2 <- do.call(rbind, dat.list)
dat2 <- dat2[with(dat2, order(year, id)),]
rownames(dat2) <- NULL
head(dat2)

write.csv(dat2, here('Data/Clean/LPS_Post_2002_Clean.csv'), row.names = F)
