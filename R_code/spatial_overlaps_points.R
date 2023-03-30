rm(list=ls())

# Load libraries and functions
library(here)
library(tidyverse)
library(sf)
library(TMB)
library(VAST)
library(units)
library(beepr)
# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))
# Add unitless back as possible unit (removed in units package update Mar 2023)
#install_unit(symbol='unitless', def='unitless', name='unitless')
# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', size=1, fill=NA),
                legend.position = "n",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=16, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Load data
dat <- read.csv(here('Data/Clean/BFT_US_catch_VASTdata.csv'))
dat <- subset(dat, Size_class == 'small')
dat_sf <- st_as_sf(dat, coords=c('lon', 'lat'))
st_crs(dat_sf) <- 'EPSG:4326'

coast <- ecodata::coast
coast <- st_transform(coast, st_crs(dat_sf))

dat$lonlat <- paste0(dat$lon,dat$lat)
lattab <- as.data.frame(table(dat$lonlat))
lattab <- lattab[with(lattab, order(-Freq)),]
head(lattab, 10)

lattab <- subset(lattab, Freq > 20)

bad <- dat[dat$lonlat %in% lattab$Var1,]
table(bad$tourn)
bad.sf <- st_as_sf(bad, coords=c('lon', 'lat'))
st_crs(bad.sf) <- 'EPSG:4326'

# Check for overlaps on the same day
dat$realbad <- paste0(dat$lonlat, "-", dat$year, 
                      str_pad(dat$month, 2, "left", "0"), 
                      str_pad(dat$day, 2, "left", "0"))
badtab <- as.data.frame(table(dat$realbad))
badtab <- subset(badtab, Freq > 1)
badtab <- badtab[with(badtab, order(-Freq)),]
head(badtab,20)

uhoh <- dat[dat$realbad %in% badtab$Var1,]
table(uhoh$tourn)

# Pull NOAA bathymetry data
Bathy <- getNOAA.bathy(lon1 = -77, lon2 = -68,
                       lat1 = 36, lat2 = 44, 
                       resolution = 1)
# Ignore any error messages, it still produces what we want.

# Now do a bit of data wrangling so we can plot it:
# Convert data to matrix
Bathy_Final <- as.matrix(Bathy)
class(Bathy_Final) <- "matrix"

# Reshape for plotting
BathyData <- Bathy_Final %>%
  as.data.frame() %>% 
  rownames_to_column(var = "lon") %>%
  gather(lat, value, -1) %>%
  mutate_all(funs(as.numeric))

#BathyData$value[BathyData$value > 0] <- NA
BathyData$value[BathyData$value < -3000] <- NA

# Set depth breaks and colors
dbreaks <- c(seq(0, 20, 2),
             seq(20, 40, 4),
             seq(40, 70, 5),
             seq(70, 100, 10),
             seq(100, 200, 20),
             seq(200, 400, 50),
             seq(400, 1000, 100),
             seq(1000, 2000, 200),
             seq(2000, 2000, 500))
dbreaks <- unique(dbreaks) * -1
dcol <- colorRampPalette(c("#DEF5E5", "#40498E"))
dcol <- dcol(length(dbreaks))

bad.sf <- st_as_sf(uhoh, coords=c('lon', 'lat'))
st_crs(bad.sf) <- 'EPSG:4326'

p <- ggplot() +
  geom_contour_filled(data = BathyData, aes(x = lon, y = lat, z = value),
                      breaks = dbreaks) + 
  geom_sf(data=coast, fill="gray")+
  geom_sf(data=bad.sf, cex=0.5)+
  coord_sf(xlim=c(-77, -68),
           ylim=c(36, 45)) +
  ggtitle('Locations with multiple observations') + 
  xlab('Longitude') +
  ylab('Latitude') +
  scale_fill_manual(values =  dcol)
p

ggsave(p,
       device='png',
       filename='spatial_overlap_points.png')

# Are these tournaments?
table(bad.sf$year, bad.sf$tourn)
# They're mostly not
# Just areas of high fishing activity
ggplot() +
  geom_contour_filled(data = BathyData, aes(x = lon, y = lat, z = value),
                      breaks = dbreaks) + 
  geom_sf(data=coast, fill="gray")+
  geom_sf(data=bad.sf, cex=0.5,
          aes(col=as.factor(tourn)))+
  coord_sf(xlim=c(-77, -68),
           ylim=c(36, 45)) +
  ggtitle('Locations with multiple observations') + 
  xlab('Longitude') +
  ylab('Latitude') +
  scale_fill_manual(values =  dcol)
