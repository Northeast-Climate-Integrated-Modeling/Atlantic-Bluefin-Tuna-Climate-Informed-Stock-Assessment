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
dat <- read.csv(here('Data/Clean/LPS_LargeTarget_TripLevel.csv'))
# Remove duplicates
dat <- unique(dat)
# Reduce dataframe to necessesities
dat <- dplyr::select(dat,
                     -tracker, -control, -docno, -intcode,
                     -cluster, -sitetype, -site_no, -measur, -given.spec)

# FILTER -- 
# June through October, 
# private chartered or headboat, 
# fished bt 1 and 24 hours
# Targeted large-med or giants
target_bft <- subset(dat,
                     month %in% c(6:10) &
                     prim_op %in% c(1,2,3) &
                     fhours > 0.99 &
                     fhours < 24.01)
# Trips must intentionally target bluefin of some any class
target_bft=subset(target_bft,
                  prim1 %in% c(4679, 4671)|
                  prim2 %in% c(4679, 4671))

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
ggplot(coast) +
  geom_sf(fill='darkgray') +
  geom_sf(data=bft_sf, cex=0.5) +
  coord_sf(xlim=c(st_bbox(bft_sf)[1], st_bbox(bft_sf)[3]),
           ylim=c(st_bbox(bft_sf)[2], st_bbox(bft_sf)[4]))

# Some of these have accidentally flipped lat-lon
flipped1 <- target_bft[round(as.numeric(substr(target_bft$latddmm,
                                              start=1, stop=2)))
                      > 45,]
flipped <- flipped1[flipped1$lon > -45,]
flipped$klon <- flipped$lat * -1
flipped$klat <- flipped$lon * -1
flipped <- flipped %>% 
  dplyr::select(-lon, -lat) %>% 
  rename(lat = klat) %>% 
  rename(lon = klon)
target_bft <- target_bft[target_bft$id %notin% flipped1$id,]
target_bft <- rbind(target_bft, flipped)

# Plot again
bft_sf <- st_as_sf(target_bft, coords=c('lon', 'lat'), crs='EPSG:4326')
ggplot(coast) +
  geom_sf(fill='darkgray') +
  geom_sf(data=bft_sf, cex=0.5) +
  coord_sf(xlim=c(st_bbox(bft_sf)[1], st_bbox(bft_sf)[3]),
           ylim=c(st_bbox(bft_sf)[2], st_bbox(bft_sf)[4]))

# There's a few observations that are way out there in Canadian waters??
# Pull out observations not in area of interest
full_data <- target_bft
target_bft <- subset(target_bft, target_bft$lat > 35 & target_bft$lat < 48)
target_bft <- subset(target_bft, target_bft$lon >(-79) & target_bft$lon < (-65))
bft_sf <- bft_sf[bft_sf$id %in% target_bft$id,]
# Plot just to see
ggplot(coast) +
  geom_sf(fill='darkgray') +
  geom_sf(data=bft_sf, cex=0.5) +
  coord_sf(xlim=c(st_bbox(bft_sf)[1], st_bbox(bft_sf)[3]),
           ylim=c(st_bbox(bft_sf)[2], st_bbox(bft_sf)[4]))

# Still some clearly incorrect values.
# Pull out observations on land
onland <- st_intersection(bft_sf, coast)
onland.df <- target_bft[target_bft$id %in% onland$id,]
# Put these to the side to check them out
maybekeep <- target_bft[target_bft$id %in% onland.df$id,]
maybekeep <- st_as_sf(maybekeep, coords=c('lon', 'lat'), crs='EPSG:4326')
# Remove
target_bft <- target_bft[target_bft$id %notin% onland$id,]
bft_sf <- bft_sf[bft_sf$id %notin% onland$id,]
# Plot just to see
ggplot(coast) +
  geom_sf(fill='darkgray') +
  geom_sf(data=bft_sf, cex=0.5) +
  geom_sf(data=maybekeep, col='red', cex=0.5) +
  coord_sf(xlim=c(-77, -67),
           ylim=c(36, 46))
# Plot just to see
ggplot(coast) +
  geom_sf(fill='darkgray') +
  geom_sf(data=maybekeep, cex=0.5,
          aes(col=state)) +
  coord_sf(xlim=c(st_bbox(maybekeep)[1], st_bbox(maybekeep)[3]),
           ylim=c(st_bbox(maybekeep)[2], st_bbox(maybekeep)[4]))
# Right. I cannot keep these.

# Shift back to df
target_bft <- sfheaders::sf_to_df(bft_sf, fill=T)
target_bft <- target_bft %>% 
  dplyr::select(-sfg_id, -point_id, -latmm, 
                -lonmm, -latddmm, -londdmm) %>% 
  rename(lon=x) %>% 
  rename(lat=y) %>% 
  dplyr::select(-catch, -hook, -gear)

# Order
target_bft <- target_bft[with(target_bft, order(year, month, day, state)),]
rownames(target_bft) <- NULL

# Calculate SST in Celsius
target_bft$sst[target_bft$sst > 90] <- NA
target_bft$sst <- weathermetrics::fahrenheit.to.celsius(target_bft$sst, 
                                                        round = 2)
# Fix depth
target_bft$depth <- target_bft$depth * -1
target_bft$depth[target_bft$depth < -9000] <- NA

# View
target_bft$lines[target_bft$lines > 90] <- NA
target_bft$party[target_bft$party > 90] <- NA

target_bft$bt_live[target_bft$bt_live > 1] <- NA
target_bft$bt_dead[target_bft$bt_dead > 1] <- NA
target_bft$bt_art[target_bft$bt_art > 1] <- NA
target_bft$fm_troll[target_bft$fm_troll > 1] <- NA
target_bft$fm_chunk[target_bft$fm_chunk > 1] <- NA
target_bft$fm_chum[target_bft$fm_chum > 1] <- NA

target_bft$bt_live[target_bft$bt_live == 0] <- 'No'
target_bft$bt_dead[target_bft$bt_dead == 0] <- 'No'
target_bft$bt_art[target_bft$bt_art == 0] <- 'No'
target_bft$fm_troll[target_bft$fm_troll == 0] <- 'No'
target_bft$fm_chunk[target_bft$fm_chunk == 0] <- 'No'
target_bft$fm_chum[target_bft$fm_chum == 0] <- 'No'

target_bft$bt_live[target_bft$bt_live == 1] <- 'Yes'
target_bft$bt_dead[target_bft$bt_dead == 1] <- 'Yes'
target_bft$bt_art[target_bft$bt_art == 1] <- 'Yes'
target_bft$fm_troll[target_bft$fm_troll == 1] <- 'Yes'
target_bft$fm_chunk[target_bft$fm_chunk == 1] <- 'Yes'
target_bft$fm_chum[target_bft$fm_chum == 1] <- 'Yes'

target_bft$prim_op[target_bft$prim_op == 1] <- 'Private'
target_bft$prim_op[target_bft$prim_op == 2] <- 'Charter'
target_bft$prim_op[target_bft$prim_op == 3] <- 'Party'

head(target_bft)
target_bft <- target_bft %>% 
  mutate_at(c('prim_op', 'bt_live', 'bt_art', 'bt_dead',
              'fm_troll', 'fm_chunk', 'fm_chum',
              'state', 'primary', 'secondary'), as.factor)
summary(target_bft)



## AS OF JANUARY 2024, STOP HERE. YOU DO NOT HAVE TO COMPLETE CLEANING 
## 1993-2001.
write.csv(target_bft,
          here('Data/Clean/LPS_LargeTarget_Clean_2024.csv'),
          row.names = F)

# Are there obsevations in the prepared data that don't match my data
# This would obviously exclude data prior to 2002 (I don't have access to it)
nwa.not.in.bft <- nwa[nwa$id  %notin% target_bft$id,]
# There are 111 observations in the GitHub data that are not in the filtered data.
# Is it primary operator?
nrow(dat[dat$id %in% nwa.not.in.bft$id & dat$prim_op %notin% c(1,2),])
# Yeah, for 28 observations. Still missing another 83.
nrow(onland[onland$id %in% nwa.not.in.bft$id,])
# Ok, there's another 74. I excluded them for being outside the area of interest
# or on land.
# I think the last 9 were excluded for having lat or lon MM values > 60.
nrow(badvals[badvals$id %in% nwa.not.in.bft$id,])
# That's all but one. Not worth tracking that last one down.

# Are there observations in my data that don't match the prepared data
# This would obviously exclude data post 2020 (not available to Alex at the time)
bft.not.in.nwa <- target_bft[target_bft$id %notin% nwa$id & 
                               target_bft$year < 2021,]
table(bft.not.in.nwa$year)
nrow(bft.not.in.nwa)
# That's so much? What is happening here. Why do I have 9594 more observations.
# Can't really make out what is happening here.

# Are we sure that lat-lon is right
# Pull out observations that are represented in both dataframes (use id to match)
pull.nwa <- nwa[nwa$id %in% target_bft$id,]
pull.bft <- target_bft[target_bft$id %in% nwa$id,]
pull.nwa <- dplyr::select(pull.nwa, id, year, month, day, STATE, lat, lon)
pull.nwa$data <- 'github'
colnames(pull.nwa) <- c('id', 'year', 'month', 'day', 'state', 'lat', 'lon','data')
pull.bft <- merge(pull.bft, statecodes, by=c('state'))
pull.bft$data <- 'created'
pull.bft <- dplyr::select(pull.bft, colnames(pull.nwa))

# Combine
both <- rbind(pull.nwa,pull.bft)
both <- unique(both)
both <- both[with(both, order(id)),]

# View lat-lon
head(both)

# It really looks like lat-lon are wrong in the GitHub dataframe.
# Let's loop back in the latmm and lonmm stuff we grabbed earlier
bft.both <- target_bft[target_bft$id %in% both$id,]
bft.both <- dplyr::select(bft.both, id, latmm, lonmm)
both <- merge(both, bft.both, by=c('id'))
head(both)

# I see what happened. 
# Lat-Lon in the GitHub dataframe was calculated as DD.MM instead of DD + (MM/60).
# This doesn't actually work and causes the weird gridding we see in plots of
# that original dataframe.
# When decimal degrees are correctly calculated as DD + (MM/60), we fix the 
# gridding.

# This means I need the original 1993-2002 data to correct this mistake and 
# include all pertinent data-- some may have been excluded when it didn't 
# need to be.

# Can I temporarily fix the 1993-2002 data
nwa.1993 <- read.csv(here('VAST_LPS/lpsVAST.csv'))
# Remove columns that are just old row names
nwa.1993 <- dplyr::select(nwa.1993, -X, -X.1)
# Remove duplicates
nwa.1993 <- unique(nwa.1993)
# Focus on data pre 2002
nwa.1993 <- subset(nwa.1993, year < 2002)

# Check ID
nwa.1993 <- dplyr::select(nwa.1993,
                          id, year, month, day, triptype, 
                          fhours, lat, lon, depth, temp,
                          young_school_bft, school_bft, large_school_bft,
                          small_med_bft, large_med_bft, giant_bft,
                          STATE)
nwa.1993$latmm <- substr(nwa.1993$lat,start=4,stop=5)
nwa.1993$lonmm <- substr(nwa.1993$lon,start=5,stop=6)
nwa.1993 <- subset(nwa.1993, latmm < 61)
nwa.1993 <- subset(nwa.1993, lonmm < 61)

nwa.1993$lon <- as.numeric(substr(nwa.1993$lon,start=1,stop=2)) +
  (as.numeric(nwa.1993$lonmm) / 60)
nwa.1993$lat <- as.numeric(substr(nwa.1993$lat,start=1,stop=2)) +
  (as.numeric(nwa.1993$latmm) / 60)

head(nwa.1993)
