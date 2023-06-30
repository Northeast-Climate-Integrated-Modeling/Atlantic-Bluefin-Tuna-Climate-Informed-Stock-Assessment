# Clean and prepare 1995-2003 SWNS slip data

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
load(here('Data/comland95_03_detail.RDATA'))
comland <- LogSlip9503_detail
#rm(LogSlip9503_detail)

#### View data ####
head(comland)
str(comland)

# Strip to necessary variables
comland <- dplyr::select(comland,
                         LOG_CODE, TRIP_CODE,
                         
                         FLEET, GEAR, GEAR_CODE,
                         
                         UNIT_AREA, UNIT_AREA2,
                         
                         DATE_SAILED2, DATE_FISHED, DATE_LANDED2,
                         T_SPECIES_SOUGHT, SPECIES_1, LIVE_WEIGHT_KG,
                         
                         T_NBR_ORIG_LL_EFF, EFFORT_MEASURE,
                         
                         T_LATITUDE, T_LONGITUDE,
                         
                         SEADAYS, MOON, 
                         T_SURFACE_TEMP, WATER_TEMP2
                         )

# Rename
colnames(comland) <- c('log', 'trip', 'fleet', 'gear', 'gear_code', 'unit',
                       'unit2', 'date_sailed', 'date_fished', 'date_landed',
                       'species_sought', 'species_landed', 'live_wt_kg',
                       'orig_ll_eff', 'eff_measure', 'lat', 'lon',
                       'seadays', 'moon', 'surface_temp', 'surface_temp2')

#### Data cleaning ####
## Log and trip ID
# This dataset has a row for every fish landed.
# The number of unique log and trip IDs should be equal
length(unique(comland$log)) == length(unique(comland$trip))

## Fleet
# I'm not clear on what fleet means but it should be a category
comland$fleet <- as.factor(comland$fleet)
table(comland$fleet)

## Gear
# HARP == harpoon, RR == rod and reel, TL == tended line
comland$gear <- as.factor(comland$gear)
comland$gear_code <- as.numeric(comland$gear_code)
table(comland$gear, comland$gear_code)

## Spatial management units
# NAFO units
comland$unit <- as.factor(comland$unit)
comland$unit2 <- as.factor(comland$unit2)
table(comland$unit, comland$unit2)
# There is some discrepancy here. Will probably end up scrapping this information
# and re-doing it with available shapefiles.

## Time at sea
# We can calculate this from the provided information and check it against seadays
comland$days_fished <- difftime(comland$date_landed, comland$date_sailed,
                                 units='days')
comland$days_fished <- as.vector(comland$days_fished)
comland$dif_days <- comland$days_fished - comland$seadays
summary(comland$dif_days)
# Some minor discrepancies. We'll stick with seadays and remove others.
comland$date_landed <- NULL
comland$date_sailed <- NULL
comland$dif_days <- NULL
comland$days_fished <- NULL
# Check validity
summary(comland$seadays)
# Negative values are not allowed
comland <- comland[comland$seadays > 0,]
# Create hours
comland$seahours <- comland$seadays * 24

## Species sought and landed
comland$species_landed <- as.factor(comland$species_landed)
comland$species_sought <- as.factor(comland$species_sought)
# Sought should all be the same -- 0251, unspecified size bluefin
table(comland$species_sought)
# Landed should be variable
table(comland$species_landed)
# Unsure what these categories are. Check by weight.
boxplot(comland$live_wt_kg ~ comland$species_landed)
dunn.test::dunn.test(comland$live_wt_kg, g=comland$species_landed,
                     method='bonferroni', kw=TRUE, label=TRUE,
                     list=TRUE)
# They're all different.
# Create size classes to match US size classes
size.class <- data.frame(
  class = c('young_school', 'school', 'large_school',
            'small_medium', 'large_medium', 'giant'),
  wt_min_lbs = c(0.1, 14, 
                 66, 135, 
                 235, 310),
  wt_max_lbs = c(13.999, 65.999,
                 134.999, 234.999,
                 309.999, 15000)
)
size.class$wt_min_kgs <- size.class$wt_min_lbs * 0.453592
size.class$wt_max_kgs <- size.class$wt_max_lbs * 0.453592
# Assign size classes by weight
comland$size_class[comland$live_wt_kg > 0 &
                     comland$live_wt_kg < 6.349834 ] <- 'small'
comland$size_class[comland$live_wt_kg >= 6.349834 &
                     comland$live_wt_kg< 29.936618] <- 'small'
comland$size_class[comland$live_wt_kg >=29.936618 &
                     comland$live_wt_kg< 61.234466] <- 'small'
comland$size_class[comland$live_wt_kg >=61.234466 &
                     comland$live_wt_kg< 106.593666] <- 'remove'
comland$size_class[comland$live_wt_kg >=106.593666 &
                     comland$live_wt_kg< 140.613066] <- 'large'
comland$size_class[comland$live_wt_kg >=140.613066 &
                     comland$live_wt_kg< 10000] <- 'large'
table(comland$size_class)
comland <- comland[comland$size_class != 'remove',]
comland$size_class <- as.factor(comland$size_class)
boxplot(comland$live_wt_kg ~ comland$size_class)
table(comland$size_class, comland$species_landed)

new.size.chart <- comland %>% 
  group_by(size_class) %>% 
  summarise(mean.wt = mean(live_wt_kg),
            min.wt = min(live_wt_kg),
            max.wt = max(live_wt_kg))

size.chart <- comland %>% 
  group_by(species_landed) %>% 
  summarise(mean.wt = mean(live_wt_kg),
            min.wt = min(live_wt_kg),
            max.wt = max(live_wt_kg))
table(comland$size_class, comland$species_landed)
as.data.frame(size.chart); as.data.frame(new.size.chart)
# Something is going on here and I need further information to figure it out.

## Effort
summary(comland$orig_ll_eff)
comland$eff_measure <- as.factor(comland$eff_measure)
table(comland$eff_measure)
# I do not know what J means in this context. H likely means hours?

## Spatial location
summary(comland$lat)
summary(comland$lon)
# Cannot keep instances without spatial information
comland <- comland[comland$lat > 0,]
comland <- comland[!is.na(comland$lat),]
comland <- comland[comland$lon > 0,]
comland <- comland[!is.na(comland$lon),]
# Lon needs to be negative
comland$lon <- comland$lon * -1
# Plot
comland.sf <- st_as_sf(comland, coords=c('lon', 'lat'))
st_crs(comland.sf) <- 'EPSG:4326'
coast <- ecodata::coast
coast <- st_transform(coast, st_crs(comland.sf))
ggplot() +
  geom_sf(data=coast, fill='gray') +
  geom_sf(data=comland.sf,
          aes(col = unit2)) +
  coord_sf(xlim=c(st_bbox(comland.sf)[1], st_bbox(comland.sf)[3]),
           ylim=c(st_bbox(comland.sf)[2], st_bbox(comland.sf)[4]))
# Some instances of flipped lat-lon. Can fix.
bads <- comland[comland$lat > 60,]
table(bads$trip)
# Unfixable typo for trip 10379719950914
comland$lat[comland$trip == "15104019981013"] <- 45.02
comland$lon[comland$trip == "15104019981013"] <- -61.47
comland$lat[comland$trip == "15104019981014"] <- 45.10
comland$lon[comland$trip == "15104019981014"] <- -61.45
comland <- comland[comland$trip != "10379719950914",]
# Replot
comland.sf <- st_as_sf(comland, coords=c('lon', 'lat'))
st_crs(comland.sf) <- 'EPSG:4326'
ggplot() +
  geom_sf(data=coast, fill='gray') +
  geom_sf(data=comland.sf,
          aes(col = unit2)) +
  coord_sf(xlim=c(st_bbox(comland.sf)[1], st_bbox(comland.sf)[3]),
           ylim=c(st_bbox(comland.sf)[2], st_bbox(comland.sf)[4]))
# Remove instances on land
overland <- st_intersection(comland.sf, coast)
comland2 <- comland[comland$trip %notin% overland$trip,]
# Replot
comland.sf <- st_as_sf(comland2, coords=c('lon', 'lat'))
st_crs(comland.sf) <- 'EPSG:4326'
ggplot() +
  geom_sf(data=coast, fill='gray') +
  geom_sf(data=comland.sf,
          aes(col = unit2)) +
  facet_wrap(vars(unit2)) +
  coord_sf(xlim=c(st_bbox(comland.sf)[1], st_bbox(comland.sf)[3]),
           ylim=c(st_bbox(comland.sf)[2], st_bbox(comland.sf)[4]))
# Remove final spatial error
comland2 <- comland2[comland2$trip != '15513319981014',]

rm(bads, coast, comland.sf)

## Environmental covariates
summary(comland2$moon)
summary(comland2$surface_temp)
summary(comland2$surface_temp2)
# It looks like surface_temp is uncleaned and surface_temp2 is clean and in degc
comland2$surface_temp <- NULL
comland2$surf_temp_degc <- comland2$surface_temp2
comland2$surface_temp2 <- NULL

## Temporal constraints
# Add year and month
comland2$year <- lubridate::year(comland2$date_fished)
comland2$month <- lubridate::month(comland2$date_fished)
comland2$day <- lubridate::day(comland2$date_fished)
# American dataset is limited to 1992 - present
comland2 <- subset(comland2, year >=1992)
# American dataset is limited to months Jun-Oct
comland2 <- subset(comland2, month %in% c(seq(6,10)))
table(comland2$year, comland2$month)
# American datast is limited to fishing effort bt 1 and 24 hrs
comland2 <- comland2[comland2$seahours >=1 & comland2$seahours <=24,]

# Final strip
comland2 <- dplyr::select(comland2,
                          size_class, trip, year, month, day, seahours,
                          lon, lat, surf_temp_degc)
colnames(comland2) <- c('Size_class', 'trip', 'year', 'month', 'day',
                        'fhours','lon', 'lat', 'sst')

# Count number of fish of a size class caught each trip
comland3 <- comland2 %>% count(trip, Size_class)
colnames(comland3) <- c('trip', 'Size_class', 'catch')

# Merge back to full dataset
comland4 <- left_join(comland2, comland3, by=c('trip', 'Size_class'))
comland4 <- unique(comland4)

# Average spatial information- sometimes report multiple catch locations when 
# there are multiple fish
comland5 <- comland4
comland5 <- comland4 %>% 
  group_by(trip, Size_class) %>% 
  summarise(mean.lat = mean(lat),
            mean.lon = mean(lon),
            mean.sst = mean(sst, na.rm=TRUE))
comland5 <- as.data.frame(comland5)

comland6 <- left_join(comland4, comland5, by=c('trip', 'Size_class'))
comland6 <- dplyr::select(comland6, 
                          Size_class, trip, year, month, day, fhours, 
                          mean.lon, mean.lat, mean.sst, catch)
colnames(comland6)[7] <- 'lon'
colnames(comland6)[8] <- 'lat'
colnames(comland6)[9] <- 'sst' 

comland6 <- unique(comland6)

head(comland6[with(comland6, order(year,month, day, trip, Size_class, lat)),])


#### Save output ####
write.csv(comland6, 
          here('Data/Clean/SWNS_comland_9503_clean.csv'),
          row.names = F)
