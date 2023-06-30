# Clean and prepare 1995-2003 SWNS data

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

# Load data
load(here('Data/DisaggregatedSWNSdata.RDATA'))
#rm(LogSlip9503_detail)

#### Start with Hdata ####
head(Hdata)
Hdata <- unique(Hdata)

# Strip
hdata2 <- dplyr::select(Hdata,
                        TRIP_ID, YEAR, VR_NUMBER, TRIP_NUMBER, MONTH, DAY, SEADAYS,
                        LON, LAT, TOTAL_NUM_BFT_CAUGHT_TRIP3,
                         
                        RND_KGS_251, RND_KGS_252, RND_KGS_253,
                        RND_KGS_255, RND_KGS_256,
                        GEAR, NAFO_UNIT, LANDED_DATE)

colnames(hdata2) <- tolower(colnames(hdata2))
colnames(hdata2) <- c('trip', 'year', 'vr_number', 'trip_number',
                      'month', 'day', 'seadays',
                      'lon', 'lat', 'catch', 'rnd_251',
                      'rnd_252', 'rnd_253', 'rnd_255', 'rnd_256',
                      'gear', 'unit', 'landed_date')

### Data checks
## Remove stuff I cannot work with or need to filter out
hdata2 <- unique(hdata2)
hdata2 <- subset(hdata2, year >=1992)
hdata2 <- hdata2[hdata2$month %in% seq(6, 10),]
hdata2$seadays <- as.vector(hdata2$seadays)
hdata2 <- hdata2[hdata2$seadays >= (1/24) &
                   hdata2$seadays <=1,]
hdata2 <- hdata2[!is.na(hdata2$lon),]
hdata2 <- hdata2[!is.na(hdata2$lat),]

hdata2 <- hdata2[with(hdata2,
                      order(year, month, day, vr_number, trip, catch)),]

## Trip
# I think this is an aggregate of all BFT caught 
# This is a problem for size class info but we can work with it for now
# Should have one row per trip
nrow(hdata2) == length(unique(hdata2$trip))
dups <- hdata2$trip[duplicated(hdata2$trip)]
dups <- hdata2[hdata2$trip %in% dups,]
# There is a discrepancy in NAFO unit. This does not matter. I will drop the second duplicate.
keep <- dups[1,]
hdata2 <- hdata2[hdata2$trip != "307316",]
hdata2 <- rbind(hdata2, keep)

## Effort
hdata2$fhours <- hdata2$seadays * 24
hdata2$seadays <- NULL

## Spatial location
summary(hdata2$lon)
hdata2 <- hdata2[hdata2$lon > 10,]
hdata2$lon <- hdata2$lon * -1
summary(hdata2$lat)
# Plot
hdata2.sf <- st_as_sf(hdata2, coords=c('lon', 'lat'))
st_crs(hdata2.sf) <- 'EPSG:4326'
coast <- ecodata::coast
coast <- st_transform(coast, st_crs(hdata2.sf))
ggplot() +
  geom_sf(data=coast, fill='gray') +
  geom_sf(data=hdata2.sf,
          aes(col=unit)) +
  #facet_wrap(vars(unit)) +
  coord_sf(xlim=c(st_bbox(hdata2.sf)[1], st_bbox(hdata2.sf)[3]),
           ylim=c(st_bbox(hdata2.sf)[2], st_bbox(hdata2.sf)[4]))
# There's one way out there that has to be deleted for incorrectness
hdata2 <- hdata2[hdata2$lon < -55,]
hdata2.sf <- st_as_sf(hdata2, coords=c('lon', 'lat'))
st_crs(hdata2.sf) <- 'EPSG:4326'
ggplot() +
  geom_sf(data=coast, fill='gray') +
  geom_sf(data=hdata2.sf,
          aes(col=unit)) +
  #facet_wrap(vars(unit)) +
  coord_sf(xlim=c(st_bbox(hdata2.sf)[1], st_bbox(hdata2.sf)[3]),
           ylim=c(st_bbox(hdata2.sf)[2], st_bbox(hdata2.sf)[4]))
# Remove points on land
overland <- st_intersection(hdata2.sf, coast)
hdata3 <- hdata2[hdata2$trip %notin% overland$trip,]
# Plot again
hdata3.sf <- st_as_sf(hdata3, coords=c('lon', 'lat'))
st_crs(hdata3.sf) <- 'EPSG:4326'
ggplot() +
  geom_sf(data=coast, fill='gray') +
  geom_sf(data=hdata3.sf,
          aes(col=unit)) +
  facet_wrap(vars(unit)) +
  coord_sf(xlim=c(st_bbox(hdata3.sf)[1], st_bbox(hdata3.sf)[3]),
           ylim=c(st_bbox(hdata3.sf)[2], st_bbox(hdata3.sf)[4]))
# A few are mislabeled
badtrip1 <- hdata3$trip[hdata3$unit == '4W' & hdata3$lon < -64]
badtrip2 <- hdata3$trip[hdata3$unit == '4X' & hdata3$lon > -63]
badtrips <- c(badtrip1, badtrip2)
hdata3 <- hdata3[hdata3$trip %notin% badtrips,]
rm(badtrip1, badtrip2, hdata2.sf, hdata3.sf, dups, bad, keep, badtrips)

## Catch
summary(hdata3$catch)

## Weird things I can't figure out
summary(hdata3$rnd_251)
summary(hdata3$rnd_252)
summary(hdata3$rnd_253)
summary(hdata3$rnd_255)
summary(hdata3$rnd_256)
# They're not useful. Remove.
hdata3 <- dplyr::select(hdata3,
                        trip, year, month, day, lon, lat, catch,
                        gear, unit, fhours)

## Gear
hdata3$gear <- as.factor(hdata3$gear)
table(hdata3$gear)

## NAFO Unit
hdata3$unit <- as.factor(hdata3$unit)
table(hdata3$unit)

## Finalize
hdata4 <- dplyr::select(hdata3,
                        trip, year, month, day, fhours,
                        lon, lat, catch)
hdata4$Size_class <- 'Unknown'
hdata4 <- unique(hdata4)

#### marfis.bftlog ####
rm(overland)

marfis <- marfis.bftlog
#rm(marfis.bftlog)

head(marfis)

## Strip
marfis <- dplyr::select(marfis,
                        TRIP_ID, VR_NUMBER, TRIP_NUMBER,
                        YEAR, MONTH,
                        DATE_FISHED, SEADAYS,
                        LONGITUDE, LATITUDE,
                        NUM_OF_LINES, FLANK_LENGTH, FLANK_LENGTH_UOM,
                        TOTAL_NUM_BFT_CAUGHT_TRIP,
                        BFT_LANDED_WEIGHT_LBS,
                        WATER_TEMP, WATER_TEMP_UOM,
                        GEAR, COUNT, MOON, WATER_TEMP2,
                        NAFO_UNIT, NAFO_UNIT2
                        )
colnames(marfis) <- tolower(colnames(marfis))
colnames(marfis)[1] <- 'trip'
marfis$day <- lubridate::day(marfis$date_fished)

marfis <- marfis[with(marfis, 
                      order(year, month, trip)),]

# Get rid of stuff I cannot work with
marfis <- marfis[!is.na(marfis$longitude),]
marfis <- marfis[!is.na(marfis$latitude),]
marfis <- subset(marfis, year >=1992)
marfis <- marfis[marfis$month %in% seq(6, 10),]

# Check sea days
marfis$seahours <- marfis$seadays * 24
marfis <- marfis[!is.na(marfis$seahours),]
marfis <- marfis[marfis$seahours >=1 &
                 marfis$seahours <=24,]
marfis$seadays <- NULL
summary(marfis$seahours)

## Trip information
# This tab has bio information on caught fish. Some of it will match info from
# the Hdata dataframe. There will be mulitiple instances of trip if more than one
# fish was caught on that trip.

## Spatial information
# This is written as ddmmss. Needs to be changed to decimal degrees.
marfis$latitudedd <- substr(marfis$latitude, start=1, stop=2)
marfis$latitudedd <- as.numeric(marfis$latitudedd)
marfis$latitudemm <- substr(marfis$latitude, start=3, stop=4)
marfis$latitudemm <- as.numeric(marfis$latitudemm)
marfis$latitudess <- substr(marfis$latitude, start=5, stop=6)
marfis$latitudess <- as.numeric(marfis$latitudess)

marfis$LAT <- marfis$latitudedd +
  (marfis$latitudemm / 60) +
  (marfis$latitudess / 3600)

marfis$longitudedd <- substr(marfis$longitude, start=1, stop=2)
marfis$longitudedd <- as.numeric(marfis$longitudedd)
marfis$longitudemm <- substr(marfis$longitude, start=3, stop=4)
marfis$longitudemm <- as.numeric(marfis$longitudemm)
marfis$longitudess <- substr(marfis$longitude, start=5, stop=6)
marfis$longitudess <- as.numeric(marfis$longitudess)

marfis$LON <- marfis$longitudedd +
  (marfis$longitudemm / 60) +
  (marfis$longitudess / 3600)

marfis$LON <- marfis$LON * -1

# Remove extraneous
marfis <- dplyr::select(marfis,
                        -latitudedd, -latitudemm, -latitudess,
                        -longitudedd, -longitudemm, -longitudess, 
                        -latitude, -longitude)
# Check
summary(marfis$LON)
summary(marfis$LAT)
marfis <- marfis[!is.na(marfis$LON),]
# Plot
marfis.sf <- st_as_sf(marfis, coords=c('LON', 'LAT'))
st_crs(marfis.sf) <- 'EPSG:4326'
ggplot() +
  geom_sf(data=coast, fill='gray') +
  geom_sf(data=marfis.sf,
          aes(col = nafo_unit)) +
  #facet_wrap(vars(unit2)) +
  coord_sf(xlim=c(st_bbox(marfis.sf)[1], st_bbox(marfis.sf)[3]),
           ylim=c(st_bbox(marfis.sf)[2], st_bbox(marfis.sf)[4]))
# A few bad points
marfis <- marfis[marfis$LON < -10,]
marfis <- marfis[marfis$LAT < 50,]
marfis.sf <- st_as_sf(marfis, coords=c('LON', 'LAT'))
st_crs(marfis.sf) <- 'EPSG:4326'
ggplot() +
  geom_sf(data=coast, fill='gray') +
  geom_sf(data=marfis.sf,
          aes(col = nafo_unit)) +
  #facet_wrap(vars(unit2)) +
  coord_sf(xlim=c(st_bbox(marfis.sf)[1], st_bbox(marfis.sf)[3]),
           ylim=c(st_bbox(marfis.sf)[2], st_bbox(marfis.sf)[4]))
# The rest may hae some issues, but i'm not sure of them. will work out later.
# Remove over land
overland <- st_intersection(marfis.sf, coast)
marfis2 <- marfis[marfis$trip %notin% overland$trip,]
marfis.sf <- st_as_sf(marfis2, coords=c('LON', 'LAT'))
st_crs(marfis.sf) <- 'EPSG:4326'
ggplot() +
  geom_sf(data=coast, fill='gray') +
  geom_sf(data=marfis.sf,
          aes(col = nafo_unit2)) +
  #facet_wrap(vars(unit2)) +
  coord_sf(xlim=c(st_bbox(marfis.sf)[1], st_bbox(marfis.sf)[3]),
           ylim=c(st_bbox(marfis.sf)[2], st_bbox(marfis.sf)[4]))

## Trip specifics
summary(marfis2$num_of_lines)
marfis2$gear <- as.factor(marfis2$gear)
summary(marfis2$gear)
marfis2$nafo_unit2 <- as.factor(marfis2$nafo_unit2)
table(marfis2$nafo_unit2)
marfis2$nafo_unit <- NULL

## Fish information
# Assume NA value in catch is actually 0
marfis2$total_num_bft_caught_trip[is.na(marfis2$total_num_bft_caught_trip)] <- 0
summary(marfis2$total_num_bft_caught_trip)
table(marfis2$total_num_bft_caught_trip,
      marfis2$count)
# Oh nevermind. Count is the correct column
marfis2$total_num_bft_caught_trip <- NULL

# Remove extraneous
marfis2 <- dplyr::select(marfis2,
                         -flank_length,
                         -flank_length_uom)

## Environment
marfis2$water_temp <- as.numeric(marfis2$water_temp)
summary(marfis2$water_temp)
# That looks awful.
marfis2$water_temp2 <- as.numeric(marfis2$water_temp2)
summary(marfis2$water_temp2)
# That's way better
marfis2 <- dplyr::select(marfis2,
                         -water_temp, -water_temp_uom)
summary(marfis2$moon)

## Assign size classes
# Create size classes to match US size classes
size.class <- data.frame(
  class = c('young_school', 'school', 'large_school',
            'small_medium', 'large_medium', 'giant'),
  wt_min_lbs = c(0.1, 14, 
                 66, 135, 
                 235, 310),
  wt_max_lbs = c(13.999, 65.999,
                 134.999, 234.999,
                 309.999, 15000),
  final_class=c('small', 'small', 'small', NA,
                'large', 'large')
)
size.class$wt_min_kgs <- size.class$wt_min_lbs * 0.453592
size.class$wt_max_kgs <- size.class$wt_max_lbs * 0.453592
# Assign size classes by weight
summary(marfis2$bft_landed_weight_lbs)
marfis2$weight_kgs <- marfis2$bft_landed_weight_lbs * 0.453592
marfis2$bft_landed_weight_lbs <- NULL

marfis2$size_class[marfis2$weight_kgs > 0 &
                     marfis2$weight_kgs < 6.349834 ] <- 'small'
marfis2$size_class[marfis2$weight_kgs >= 6.349834 &
                     marfis2$weight_kgs< 29.936618] <- 'small'
marfis2$size_class[marfis2$weight_kgs >=29.936618 &
                     marfis2$weight_kgs< 61.234466] <- 'small'
marfis2$size_class[marfis2$weight_kgs >=61.234466 &
                     marfis2$weight_kgs< 106.593666] <- 'remove'
marfis2$size_class[marfis2$weight_kgs >=106.593666 &
                     marfis2$weight_kgs< 140.613066] <- 'large'
marfis2$size_class[marfis2$weight_kgs >=140.613066 &
                     marfis2$weight_kgs< 10000] <- 'large'

table(marfis2$size_class)
marfis2 <- marfis2[marfis2$size_class !='remove',]
marfis2$size_class <- as.factor(marfis2$size_class)
summary(marfis2$size_class)

## Final
marfis3 <- dplyr::select(marfis2,
                        size_class,
                        trip, year,month, day, date_fished, seahours,
                        LON, LAT, count, water_temp2)

marfis4 <- marfis3 %>% count(trip, size_class)
colnames(marfis4) <- c('trip', 'size_class', 'catch')

# Merge back to full dataset
marfis5 <- left_join(marfis3, marfis4, by=c('trip', 'size_class'))
marfis5 <- marfis5[!is.na(marfis5$trip),]
marfis5 <- unique(marfis5)

marfis5 <- dplyr::select(marfis5, 
                         -count)
colnames(marfis5) <- c('Size_class', 'trip', 'year', 'month', 'day',
                       'date_fished', 'fhours', 'lon', 'lat', 'sst', 'catch')

# Don't save yet
# There are times that fish with bio data have already been noted in the Hdata
# Let's remove any size-classed bio data fish from Hdata
marfis5$Size_class <- as.character(marfis5$Size_class)
marfis5$Size_class[is.na(marfis5$Size_class)] <- 'Unknown'

# Average spatial information- sometimes report multiple catch locations when 
# there are multiple fish
marfis6 <- marfis5
marfis6 <- marfis5 %>% 
  group_by(trip, Size_class) %>% 
  summarise(mean.lat = mean(lat),
            mean.lon = mean(lon),
            mean.sst = mean(sst))
marfis6 <- as.data.frame(marfis6)

marfis6 <- left_join(marfis5, marfis6, by=c('trip', 'Size_class'))

# Check if that works
marfis6$londif <- abs(marfis6$lon - marfis6$mean.lon)
marfis6$latdif <- abs(marfis6$lat - marfis6$mean.lat)

bads <- marfis6[marfis6$latdif > 0 | marfis6$londif > 0,]
summary(bads$londif)
summary(bads$latdif)

badlat <- bads[bads$latdif > 0.25,]
badlon <- bads[bads$londif > 0.25,]

badtrips <- c(badlat$trip, badlon$trip)
badtrips <- unique(badtrips)

badtrips <- marfis6[marfis6$trip %in% badtrips,]
badtrips <- badtrips[with(badtrips, order(trip)),]

# Some of these are typos
#1
marfis6$lon[marfis6$trip == '22970'][1] <- 
  marfis6$lon[marfis6$trip == '22970'][1] - 3
#2
marfis6$lat[marfis6$trip == '114216'][2] <- 
  marfis6$lat[marfis6$trip == '114216'][2] + 1
#3
marfis6 <- marfis6[marfis6$trip != '115173',]
#4
marfis6$lat[marfis6$trip == '115577'] <- 44.2
#5
marfis6$lat[marfis6$trip == '116340'][3] <- 
  marfis6$lat[marfis6$trip == '116340'][1]
#6
marfis6$lat[marfis6$trip == '119053'][4] <- 
  marfis6$lat[marfis6$trip == '119053'][1]
#7
marfis6$lat[marfis6$trip == '147037'][2] <- 
  marfis6$lat[marfis6$trip == '147037'][1]
#8
marfis6$lat[marfis6$trip == '180285'][2:3] <- 
  marfis6$lat[marfis6$trip == '180285'][2:3] +1
#9
marfis6 <- marfis6[marfis6$trip != '180728',]
#10
marfis6$remove[marfis6$trip == 257573 & 
                 marfis6$month == 9 &
                 marfis6$day == 22] <- 'yes'
marfis6 <- marfis6[is.na(marfis6$remove),]
marfis6$remove <- NULL
marfis6$catch[marfis6$trip == '257573'] <- 3
#11
marfis6$lat[marfis6$trip == '325401'][1] <- 
  marfis6$lat[marfis6$trip == '325401'][1] -1
#12
marfis6$lat[marfis6$trip == '326822'][2] <- 
  marfis6$lat[marfis6$trip == '326822'][1]
#13
marfis6 <- marfis6[marfis6$trip != '373903',]
#14
marfis6$lat[marfis6$trip == '391175'][1] <- 
  marfis6$lat[marfis6$trip == '391175'][2]
#15
marfis6$lat[marfis6$trip == '392945'][1] <- 
  marfis6$lat[marfis6$trip == '392945'][2]
#16
marfis6$lon[marfis6$trip == '419592'][1] <- 
  marfis6$lon[marfis6$trip == '419592'][1] - 0.6
#17
marfis6$lat[marfis6$trip == '443854'][4] <- 
  marfis6$lat[marfis6$trip == '443854'][4] -2
#18
marfis6$lon[marfis6$trip == '445704'][4] <- 
  marfis6$lon[marfis6$trip == '445704'][4] +2
#19
marfis6$lon[marfis6$trip == '469863'][4] <- 
  marfis6$lon[marfis6$trip == '469863'][4] + 3
#20
marfis6$lat[marfis6$trip == '470852'][2] <- 
  marfis6$lat[marfis6$trip == '470852'][2] -1
#21
marfis6 <- marfis6[marfis6$trip != '472458',]
#22
marfis6 <- marfis6[marfis6$trip != '515297',]
#23
marfis6$lon[marfis6$trip == '515370'][2] <- 
  marfis6$lon[marfis6$trip == '515370'][2] + 3 
#24
marfis6$lon[marfis6$trip == '517082'][1] <- 
  marfis6$lon[marfis6$trip == '517082'][1] -1 
#25
marfis6$lat[marfis6$trip == '534969'][2] <- 
  marfis6$lat[marfis6$trip == '534969'][2] -2 
#26
marfis6$lon[marfis6$trip == '538403'][4] <- 
  marfis6$lon[marfis6$trip == '538403'][4] +2 
#27
marfis6$lon[marfis6$trip == '538673'][1] <- 
  marfis6$lon[marfis6$trip == '538673'][1] -2 

# REdo
marfis6 <- dplyr::select(marfis6, -mean.lat, -mean.lon, -mean.sst)
marfis7 <- marfis6
marfis7 <- marfis7 %>% 
  group_by(trip, Size_class) %>% 
  summarise(mean.lat = mean(lat),
            mean.lon = mean(lon),
            mean.sst = mean(sst))
marfis7 <- as.data.frame(marfis7)

marfis7 <- left_join(marfis6, marfis7, by=c('trip', 'Size_class'))

# Check if that works
marfis7$londif <- abs(marfis7$lon - marfis7$mean.lon)
marfis7$latdif <- abs(marfis7$lat - marfis7$mean.lat)

bads <- marfis7[marfis7$latdif > 0 | marfis7$londif > 0,]
summary(bads$londif)
summary(bads$latdif)

badlat <- bads[bads$latdif > 0.25,]
badlon <- bads[bads$londif > 0.25,]

badtrips <- c(badlat$trip, badlon$trip)
badtrips <- unique(badtrips)

badtrips <- marfis7[marfis7$trip %in% badtrips,]
badtrips <- badtrips[with(badtrips, order(trip)),]
badtrips
# Looks good. Check that it all makes sense on a map.

marfis7 <- dplyr::select(marfis7, 
                          Size_class, trip, year, month, day, fhours, 
                          mean.lon, mean.lat, mean.sst, catch)
colnames(marfis7)[7] <- 'lon'
colnames(marfis7)[8] <- 'lat'
colnames(marfis7)[9] <- 'sst'

marfis7 <- unique(marfis7)

head(marfis7[with(marfis7, order(year,month, day, trip, Size_class, lat)),])

# Check it on a map
marfis.sf <- st_as_sf(marfis7, coords=c('lon', 'lat'))
st_crs(marfis.sf) <- 'EPSG:4326'
ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=marfis.sf) +
  coord_sf(xlim=c(st_bbox(marfis.sf)[1], st_bbox(marfis.sf)[3]),
          ylim=c(st_bbox(marfis.sf)[2], st_bbox(marfis.sf)[4]))
# REmove the one in Hudson Bay area
marfis7 <- marfis7[marfis7$lat < 48,]

exists.in.bio <- hdata4[hdata4$trip %in% marfis7$trip,]
hdata5 <- hdata4[hdata4$trip %notin% exists.in.bio$trip,]
hdata5$Size_class <- 'Unknown'

exists.in.h <- marfis7[marfis7$trip %in% hdata5$trip,]

# We can merge these now
hdata5$sst <- NA
hdata5$data <- 'hdata'
marfis7$data <- 'marfis'

merged.data <- rbind(hdata5, marfis7)
merged.data <- unique(merged.data)

merged.data$trip.fish <- paste0(merged.data$trip, "_", merged.data$Size_class)
shit <- as.data.frame(table(merged.data$trip.fish))
shit <- subset(shit, Freq != 1)

merged.data$Var1 <- merged.data$trip.fish
bads <- merged.data
bads <- bads[bads$Var1 %in% shit$Var1,]
# This looks like a discrepancy in day the fish was reported vs day sailed or something
# This truly doesn't matter much to us. Let's average and round day.

marfis8 <- marfis7 %>% 
  group_by(trip, Size_class) %>% 
  summarise(mean.day = round(mean(day)))

marfis8 <- as.data.frame(marfis8)
marfis9 <- left_join(marfis7, marfis8, by=c('trip', 'Size_class'))
table(marfis9$mean.day, marfis9$month)
# There is no September 31st. Remove.
marfis9 <- marfis9[marfis9$trip %notin% c('57064', '57065'),]
marfis9 <- dplyr::select(marfis9,
                         Size_class, trip, year, month, mean.day, fhours,
                         lon, lat, sst, catch, data)
colnames(marfis9)[5] <- 'day'

# Check that nrow sizeclass-trip == catch 
marfis9$size.trip <- paste0(marfis9$Size_class, '_', marfis9$trip)
marfis9$flag <- 0
marfis.list <- split(marfis9, f=marfis9$size.trip)
for(i in 1:length(marfis.list)){
  if(nrow(marfis.list[[i]]) > marfis.list[[i]]$catch[1]){
    marfis.list[[i]]$flag <- 1
  }
  if(nrow(marfis.list[[i]]) < marfis.list[[i]]$catch[1]){
    marfis.list[[i]]$flag <- 2
  }
}
marfis9 <- do.call(rbind, marfis.list)
rownames(marfis9) <- NULL
bads <- marfis9[marfis9$flag !=0,]
# There are 945 trips where bio information is mising for 1 or more fish
# Without that information, we cannot assign class. Remove these fish.
marfis9$flag <- 0
marfis.list <- split(marfis9, f=marfis9$size.trip)
for(i in 1:length(marfis.list)){
  if(nrow(marfis.list[[i]]) < marfis.list[[i]]$catch[1]){
    marfis.list[[i]]$flag <- 2
    marfis.list[[i]]$catch <- nrow(marfis.list[[i]])
  }
}
marfis9 <- do.call(rbind, marfis.list)
rownames(marfis9) <- NULL
bads <- marfis9[marfis9$flag !=0,]
# I think that should do it.
marfis9 <- dplyr::select(marfis9, -flag, -size.trip)

# Merge
merged.data <- rbind(hdata5, marfis9)

merged.data <- unique(merged.data)
shit <- as.data.frame(table(merged.data$trip.fish))
shit <- subset(shit, Freq != 1)
bads <- merged.data
bads <- bads[bads$Var1 %in% shit$Var1,]
bads
# No bads left

merged.data <- dplyr::select(merged.data,
                             Size_class, trip, year, month, day, fhours,
                             lon, lat, sst, catch, data)
merged.data <- as.data.frame(merged.data)

# Finally, write.
write.csv(merged.data,
          here('Data/Clean/SWNS_Disagg_Clean.csv'),
          row.names = F)
