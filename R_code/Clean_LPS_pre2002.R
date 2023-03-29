# Clean and prepare pre-2002 LPS data

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

# Load data
dat <- read.csv(here('Data/LPS/LPS_rawdata_8001_edit.csv'))
head(dat)

# FILTER -- 
# June through october, private chartered or headboat, fished bt 1 and 24 hours
target_bft <- subset(dat,
                     MONTH %in% c(6:10) &
                       YEAR > 92 &
                       STATE %in% statecodes$state &
                       BOATTYPE %in% c(1,2,3) &
                       HOURS > 0.99 &
                       HOURS < 24.01)

# Trips must intentionally target bluefin of some any class
target_bft=subset(target_bft,
                    SPECMKT1  %in% speccode$prim|
                    SPECMKT2  %in% speccode$prim|
                    SPECMKT3  %in% speccode$prim|
                    SPECMKT4  %in% speccode$prim|
                    SPECMKT5  %in% speccode$prim|
                    SPECMKT6  %in% speccode$prim|
                    SPECMKT7  %in% speccode$prim|
                    SPECMKT8  %in% speccode$prim|
                    SPECMKT9  %in% speccode$prim)

# Fix year
for(i in 1:nrow(target_bft)){
  if(target_bft$YEAR[i] < 100){
    target_bft$YEAR[i] <- target_bft$YEAR[i] + 1900
  }
}
table(target_bft$YEAR)

# Drop instances without spatial data
# Filter
target_bft <- target_bft%>%
  drop_na(LATDEG) %>%
  drop_na(LONDEG) %>%
  drop_na(LATMIN) %>% 
  drop_na(LONMIN) %>% 
  filter(LATDEG != 0 & LATDEG !=99) %>% 
  filter(LONDEG != 0 & LONDEG !=99)

# Check values
head(target_bft)
table(target_bft$LONDEG)
table(target_bft$LATDEG)

# Pull out bad values-- could be saved
badvals <- target_bft[target_bft$LONDEG < 61 |
                      target_bft$LATDEG < 35 | 
                      target_bft$LATDEG > 46 |
                      target_bft$LONMIN > 60 |
                      target_bft$LATMIN > 60,]

# It looks like some of these are input errors
# I'm going to fix in excel. It's so much easier.
# Then I'll restart this script with the edited (saved separate) file.
# After altering data, there are 17 observations with incorrect lat-lon.

target_bft <- target_bft %>% 
  filter(LONDEG > 60) %>% 
  filter(LATDEG > 34) %>% 
  filter(LONMIN < 61) %>% 
  filter(LATMIN < 61)

# Check ID-- should be unique
target_bft <- target_bft[with(target_bft, order(ID, YEAR, MONTH, DAY)),]
head(target_bft)
dupids <- target_bft$ID[duplicated(target_bft$ID)]
dups <- target_bft[target_bft$ID  %in% dupids,]
dups <- dups[with(dups, order(ID, YEAR, MONTH, DAY)),]
head(dups)
# It's not unique. Make our own.
target_bft$ID <- paste0(target_bft$SAMPLER, "_", 
                        target_bft$STATE, "_",
                        target_bft$YEAR, 
                        str_pad(target_bft$MONTH, width=2, side="left", "0"),
                        str_pad(target_bft$DAY, width=2, side="left", "0"),
                        "_",
                        str_pad(target_bft$docnum, width=6, side="left", "0"))

dupids <- target_bft$ID[duplicated(target_bft$ID)]
dups <- target_bft[target_bft$ID  %in% dupids,]
# No duplicates left
rm(badvals, dups, dupids)

# Order and view
target_bft <- target_bft[with(target_bft, order(YEAR, MONTH, DAY, ID)),]
rownames(target_bft) <- NULL
head(target_bft)

# Remove unnecessary columns
target_bft <- dplyr::select(target_bft, 
                            -SAMPLER, -docnum, -TOURN, -PHONDOCK, -MARINA,
                            -INLET, -BOAT, -TARGET, -FISHMETH, -GEAR,
                            -ANGLERS, -LINES, -FISHAREA, -LANDCODE, 
                            -SURVTYPE)
head(target_bft)

# Compute catch
target_bft$CATCH1 <- target_bft$KEPT1 + target_bft$REL1
target_bft$CATCH2 <- target_bft$KEPT2 + target_bft$REL2
target_bft$CATCH3 <- target_bft$KEPT3 + target_bft$REL3
target_bft$CATCH4 <- target_bft$KEPT4 + target_bft$REL4
target_bft$CATCH5 <- target_bft$KEPT5 + target_bft$REL5
target_bft$CATCH6 <- target_bft$KEPT6 + target_bft$REL6
target_bft$CATCH7 <- target_bft$KEPT7 + target_bft$REL7
target_bft$CATCH8 <- target_bft$KEPT8 + target_bft$REL8
target_bft$CATCH9 <- target_bft$KEPT9 + target_bft$REL9

# Reduce data
target_bft <- dplyr::select(target_bft,
                            ID, YEAR, MONTH, DAY, STATE, BOATTYPE, HOURS, 
                            LATDEG, LATMIN, LONDEG, LONMIN,
                            DEPTH, TEMP, SPECMKT1, CATCH1, SPECMKT2, CATCH2,
                            SPECMKT3, CATCH3, SPECMKT4, CATCH4, SPECMKT5,
                            CATCH5, SPECMKT6, CATCH6, SPECMKT7, CATCH7,
                            SPECMKT8, CATCH8, SPECMKT9, CATCH9)
head(target_bft)

# Reshape data
bf.split <- split(target_bft, f=target_bft$ID)
tempdat <- data.frame(
  ID=rep(NA, 9),
  YEAR=rep(NA, 9),
  MONTH =rep(NA, 9),
  DAY =rep(NA, 9),
  STATE=rep(NA, 9),
  BOATTYPE =rep(NA, 9),
  HOURS=rep(NA, 9),
  LATDEG=rep(NA, 9),
  LATMIN=rep(NA, 9),
  LONDEG=rep(NA, 9),
  LONMIN=rep(NA, 9),
  DEPTH=rep(NA, 9),
  TEMP=rep(NA, 9),
  SPEC=rep(NA, 9),
  CATCH=rep(NA, 9)
)

for(i in 1:length(bf.split)){
  temp <- bf.split[[i]]
  
  keep <- tempdat
  
  keepspec <- c(temp$SPECMKT1, temp$SPECMKT2, temp$SPECMKT3,
                temp$SPECMKT4, temp$SPECMKT5, temp$SPECMKT6,
                temp$SPECMKT7, temp$SPECMKT8, temp$SPECMKT9)
  
  keepcatch <- c(temp$CATCH1, temp$CATCH2, temp$CATCH3,
                 temp$CATCH4, temp$CATCH5, temp$CATCH6,
                 temp$CATCH7, temp$CATCH8, temp$CATCH9)
  
  keep$ID <-  temp$ID[1]
  keep$YEAR <- temp$YEAR[1]
  keep$MONTH <-  temp$MONTH[1]
  keep$DAY <- temp$DAY[1]
  keep$STATE <-  temp$STATE[1]
  keep$BOATTYPE <- temp$BOATTYPE[1]
  keep$HOURS <-  temp$HOURS[1]
  keep$LATDEG <- temp$LATDEG[1]
  keep$LATMIN <-  temp$LATMIN[1]
  keep$LONDEG <- temp$LONDEG[1]
  keep$LONMIN <-  temp$LONMIN[1]
  keep$DEPTH <- temp$DEPTH[1]
  keep$TEMP <- temp$TEMP[1]
  keep$SPEC <-  keepspec
  keep$CATCH <- keepcatch
  
  bf.split[[i]] <- keep
  
  rm(temp, keep, keepspec, keepcatch)
}

# Rebind to data frame
tbft <- do.call(rbind, bf.split)
rownames(tbft) <- NULL

# Reduce data
tbft <- tbft[!is.na(tbft$SPEC),]
tbft <- subset(tbft, SPEC %in% speccode$prim)

# Reorder and view
tbft <- tbft[with(tbft, order(YEAR, MONTH, DAY, STATE, ID)),]
rownames(tbft) <- NULL
head(tbft)

# Fix lat-lon
tbft$lat <- tbft$LATDEG + (tbft$LATMIN / 60)
tbft$lon <- tbft$LONDEG + (tbft$LONMIN / 60)
tbft$lon <- tbft$lon * -1
tbft <- dplyr::select(tbft,
                      -LATDEG, -LATMIN, -LONDEG, -LONMIN)

# Plot
plotprep <- dplyr::select(tbft, ID, lon, lat)
plotprep <- unique(plotprep)
dat_sf <- st_as_sf(plotprep, coords=c('lon', 'lat'))
st_crs(dat_sf) <- "EPSG:4326"
coast <- ecodata::coast
coast <- st_transform(coast, crs=st_crs(dat_sf))

nwat <- st_read(here('Data/GIS/NWAtlantic.shp'))
nwat <- st_transform(nwat, crs=st_crs(coast))

# Remove points on land
ggplot(coast)+
  geom_sf(fill='gray') +
  geom_sf(data=nwat, fill='lightblue') +
  geom_sf(data=dat_sf, cex=0.5) +
  coord_sf(xlim=c(-79, -65),
           ylim=c(34, 47)) 

innwat.1993 <- st_intersection(dat_sf, nwat)

dat_sf <- dat_sf[dat_sf$ID %in% innwat.1993$ID,]

# Remove points on land or far away
ggplot(coast)+
  geom_sf(data=nwat, fill='lightblue') +
  geom_sf(fill='gray') +
  geom_sf(data=dat_sf, cex=0.5) +
  coord_sf(xlim=c(-79, -65),
           ylim=c(34, 47)) 

tbft <- tbft[tbft$ID %in% dat_sf$ID,]

colnames(tbft) <- tolower(colnames(tbft))

head(tbft)
tbft <- dplyr::select(tbft, id, year, month, day, hours, depth, temp, spec, catch, lat, lon)

colnames(tbft) <- c('id', 'year', 'month', 'day', 'fhours','depth', 'sst', 'prim', 'catch', 'lat', 'lon')

tbft <- merge(tbft, speccode, by=c('prim'))
tbft <- dplyr::select(tbft, id, year, month, day, fhours, depth, sst, lon, lat,
                      size, catch)
head(tbft)

tb.list <- split(tbft, f=tbft$id)
catchtype2 <- c('small', 'large')
tempdf <- data.frame(
  Size_class = catchtype2,
  id=rep(NA, length(catchtype2)),
  year=rep(NA, length(catchtype2)),
  month=rep(NA, length(catchtype2)),
  day=rep(NA, length(catchtype2)),
  #month=rep(NA, length(catchtype)),
  #day=rep(NA, length(catchtype)),
  fhours=rep(NA, length(catchtype2)),
  depth=rep(NA, length(catchtype2)),
  sst=rep(NA, length(catchtype2)),
  lon=rep(NA, length(catchtype2)),
  lat=rep(NA, length(catchtype2)),
  
  catch=rep(NA, length(catchtype2))
  
)

for(i in 1:length(tb.list)){
  temp <- tb.list[[i]]
  
  holddf <- tempdf
  
  holddf$id <- temp$id[1]
  holddf$year <- temp$year[1]
  holddf$month <- temp$month[1]
  holddf$day <- temp$day[1]
  holddf$fhours <- temp$fhours[1]
  holddf$depth <- temp$depth[1]
  holddf$sst <- temp$sst[1]
  holddf$lon <- temp$lon[1]
  holddf$lat <- temp$lat[1]
  
  catchsm <- temp$catch[temp$size %in% c('young school',
                                                 'school', 
                                                 'large school')]
  catchlg <- temp$catch[temp$size %in% c('large med', 'giant')]
  
  if(length(catchsm) > 0){holddf$catch[1] <- sum(catchsm)}
  if(length(catchsm) == 0){holddf$catch[1] <- 0}
  
  if(length(catchlg) > 0){holddf$catch[2] <- sum(catchlg)}
  if(length(catchlg) == 0){holddf$catch[2] <- 0}
  
  tb.list[[i]] <- holddf
  
  rm(temp, holddf, catchsm, catchlg)
  
}
tb <- do.call(rbind, tb.list)
rownames(tb) <- NULL
head(tb)

# Convert depth to m
table(tb$depth)
tb$depth[tb$depth > 8999 | tb$depth == 0] <- NA
tb$depth <- round(tb$depth * -0.3048,1)

# Convert temp to C
table(tb$sst)
tb$sst[tb$sst > 90 | tb$sst < 37] <- NA
tb$sst <- weathermetrics::fahrenheit.to.celsius(tb$sst, 
                                                        round = 2)

head(tb)
write.csv(tb, here('Data/Clean/LPS_Pre_2002_Clean.csv'), row.names = F)
