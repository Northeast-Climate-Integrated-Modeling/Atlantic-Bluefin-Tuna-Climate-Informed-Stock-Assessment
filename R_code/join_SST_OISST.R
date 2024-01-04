# Adapted from code by Sarah Gaichas
# Joins existing field station SST data and OISST satellite SST data, tests
# relationship between the two, visualizes OISST inputs at stations missing
# empirical data.

# Clear workspace
rm(list=ls())

# Load libraries
library(tidyverse)
library(here)
library(DT)
library(FishStatsUtils)
library(sf)
library(raster)
library(terra)
library(nngeo)
library(data.table)

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


#list of SST dataframes
SSTdfs <- list.files(here("Data/OISST/"), pattern = "*.rds")
# Remove 2021 (no 2021 canadian data)
SSTdfs <- SSTdfs[10:30]

# Create empty tibble to fill
stn_OISST <- tibble()

# Load station data
# Load data
stations <- read.csv(here('Data/Clean/BFT_BothCountries_VAST3.csv'))
# Confirm all have dats
stations[is.na(stations$year),]
stations[is.na(stations$month),]
stations[is.na(stations$day),]

# Remove large category (it's fine, it's a spatial duplicate of small.)
#stations <- subset(stations, Size_class == 'small')

stations$DATE <- paste0(stations$year, '-',
                        str_pad(stations$month, 2, "left", "0"), "-",
                        str_pad(stations$day, 2, "left", '0'))

# Create datestring to merge SST and OISST
stations$DATE <- as.POSIXct(stations$DATE, 
                            format = "%Y-%m-%d")
stations$yrmody <- paste0(stations$year, 
                          str_pad(stations$month, 2, "left", '0'), 
                          str_pad(stations$day, 2, "left", '0'))

stations <- stations[is.na(stations$sst),]
# stations <- dplyr::select(stations,
#                           -year, -month, -day, -sst, -yrmody)

stations <- st_as_sf(stations, coords=c("lon", "lat"),
                     na.fail=T)

#stations <- subset(stations, YEAR > 1981)

# Initialize progress bar
pb <- txtProgressBar(min=0, max=length(SSTdfs), initial=0, char="=", style=3)

# Merge in year-based loops
for(df in SSTdfs){
  # Call the number of the current .rds file in the vector of .rds files
  t <- match(df, SSTdfs)
  
  # Update progress bar
  setTxtProgressBar(pb, t)
  getTxtProgressBar(pb)
  
  # Call annual .rds OISST file
  sstdf <- readRDS(paste0(here("Data/OISST/", df)))
  sstdf$yrmody <- paste0(sstdf$year, sstdf$month, sstdf$day)
  
  # Create string of sampled years (removes cod data prior to 1982)
  stationsyr <- stations %>%
    filter(year == unique(sstdf$year))
  
  # Filter out OISST days not sampled for bluefin, convert to sf object
  sstdf_survdays <- sstdf %>%
    dplyr::mutate(yrmody = as.numeric(paste0(year, month, day)) )%>%
    dplyr::filter(yrmody %in% unique(stationsyr$yrmody)) %>%
    dplyr::mutate(year = as.numeric(year),
                  month = as.numeric(month),
                  day = as.numeric(day),
                  declon = Lon,
                  declat = Lat) %>%
    dplyr::select(-Lon, -Lat) %>%
    sf::st_as_sf(coords=c("declon","declat"), crs=4326, remove=FALSE)  
  
  # Join by nearest neighbor and date
  yrdietOISST <- suppressMessages(do.call('rbind', 
                         lapply(split(stationsyr, 
                                      1:nrow(stationsyr)), 
                  function(x) {
                        st_join(x, 
                                sstdf_survdays[sstdf_survdays$yrmody == unique(x$yrmody),],
                                join = st_nn, k = 1, progress = TRUE)
                               }
  )))
  
  yrdietOISST <- yrdietOISST %>% 
    dplyr::select(-year.x, -month.x, -day.x, -sst.x, -yrmody.x) %>% 
    rename(year=year.y) %>% 
    rename(month=month.y) %>% 
    rename(day=day.y) %>% 
    rename(sst=sst.y) %>% 
    rename(yrmody=yrmody.y)
  
  # Bind one year of the loop to the initialized tibble
  stn_OISST <- rbind(stn_OISST, yrdietOISST)
  
  # Close segment of progress bar every loop
  close(pb)
  
}

stn_OISST <- sfheaders::sf_to_df(stn_OISST, fill=T)

stn_OISST <- stn_OISST %>% 
  dplyr::select(-yrmody, -declat, -declon, -sfg_id,
                -point_id, -DATE) %>% 
  rename(lon=x) %>% 
  rename(lat=y)

stations <- read.csv(here('Data/Clean/BFT_BothCountries_VAST3.csv'))
stations <- stations[!is.na(stations$sst),]
stations <- rbind(stations, stn_OISST)

# Save output
#saveRDS(stn_OISST, here("data/RData_Storage/stn_OISST.rds"))

# Read in station data and station-OISST
stn_OISST_merge <- stn_OISST %>%
  dplyr::select(-month.y,
                -day.y,
                -yrmody.y) %>% 
  dplyr::rename(month = month.x,
                day = day.x,
                yrmody = yrmody.x,
                oisst = sst.y) %>%
  dplyr::select(trip, oisst) %>%
  sf::st_drop_geometry()

# Merge
agg_stn_all_OISST <- left_join(stations, stn_OISST_merge)

# Save output
#saveRDS(agg_stn_all_OISST, here("Data/Clean/agg_stn_all_OISST.rds"))
#agg_stn_all_OISST <- readRDS(here("data/agg_stn_all_OISST.rds"))

# Create dataset of comparisons
comparesst <- agg_stn_all_OISST %>%
  #dplyr::filter(YEAR>1981)%>%
  dplyr::select(sst, oisst, year) %>%
  na.omit()

# Set sequence of years to plot
yearstoplot <- seq(2002, 2022)

# Plot comparisons, check for adhesion to 1:1 line
for(i in 1:length(yearstoplot)){
  # Set year
  plotyr <- yearstoplot[i]
  
  # Plot OISST vs Field SST for that year
  print(ggplot2::ggplot(comparesst[comparesst$year == plotyr,], 
                        aes(x=sst, y=oisst)) +
    geom_point(alpha=0.8, pch=16)+
    geom_abline(intercept = 0, slope = 1) +
    labs(x = "Field surface temperature (deg C)",
         y = "OISST (deg C)") +
    theme_bw() +
      ggtitle(paste("SST difference survey-OISST:", plotyr, sep = " ")))
}

# Plot OISST vs Field SST for all years
ggplot2::ggplot(comparesst, aes(x=sst, y=oisst, col=year)) +
  geom_point(alpha=0.8, pch=16)+
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Field surface temperature (deg C)",
       y = "OISST (deg C)") +
  theme_bw() 
# Looks like we have mostly good field data. 
# Field reporrs typically colder than OISST reports.

# Map annual shifts
mapsst <- agg_stn_all_OISST %>%
  #dplyr::filter(YEAR>1981) %>%
  #dplyr::filter(SEASON == 'FALL' | SEASON=='SPRING') %>% 
  dplyr::mutate(sstdiff = sst-oisst) %>%
  dplyr::select(trip, year, month, sst, oisst, sstdiff) 

# Set CRS
st_crs(mapsst) <- "EPSG:4326"

# Set function to map SST difference
yrmap <- function(mapyr){
  coast <- ecodata::coast
  coast <- st_transform(coast, crs="EPSG:4326")
  ggplot2::ggplot() +
    geom_sf(data = coast) +
    geom_sf(data = mapsst[mapsst$year == mapyr,], 
            aes(colour=sstdiff)) +
    coord_sf(xlim = c(-77, -65), 
             ylim = c(35, 45)) + 
    scale_color_gradient2(low = "blue",
                          mid = "gray",
                          high = "red",
                          midpoint = 0,
                          limits=c(-25, 15),
                          na.value = "gray") +
    theme_bw() +
    #facet_wrap(~month) +
    ggtitle(paste("SST difference survey-OISST:", mapyr, sep = " "))
}

# Plot SST difference in space for every year
for(mapyr in 1992:2020){
  print(yrmap(mapyr)) 
}

# Executive decision: if field SST is within 2 deg of OISST, use field SST value
# If field SST is more than 2 deg different from OISST, use OISST
agg_stn_all_OISST$sstdiff <- agg_stn_all_OISST$sst - agg_stn_all_OISST$oisst
summary(agg_stn_all_OISST$sstdiff)
hist(agg_stn_all_OISST$sstdiff, breaks=seq(-14, 14, 1))
nrow(agg_stn_all_OISST[!is.na(agg_stn_all_OISST$sstdiff) & abs(agg_stn_all_OISST$sstdiff) <=2.0,]) / 
  nrow(agg_stn_all_OISST[!is.na(agg_stn_all_OISST$sstdiff),]) * 100
# 64% of our observations with both values are within 2 deg.

# Call new columns
agg_stn_all_OISST$temp <- NA
agg_stn_all_OISST$sstsource <- NA

# Set temp to OISST when abs(sstdiff) >2
# Retain source information
for(i in 1:nrow(agg_stn_all_OISST)){
  if(!is.na(agg_stn_all_OISST$sstdiff[i]) & abs(agg_stn_all_OISST$sstdiff[i]) <=2){
    agg_stn_all_OISST$temp[i] <- agg_stn_all_OISST$sst[i]
    agg_stn_all_OISST$sstsource[i] <- 'Field'
  }
  if(is.na(agg_stn_all_OISST$sstdiff[i]) | abs(agg_stn_all_OISST$sstdiff[i]) >2){
    agg_stn_all_OISST$temp[i] <- agg_stn_all_OISST$oisst[i]
    agg_stn_all_OISST$sstsource[i] <- 'OISST'
  }
  
}
# Check how often we are substituting field data for OISST
barplot(t(prop.table(table(agg_stn_all_OISST$year, agg_stn_all_OISST$sstsource), margin=1)), legend=T)
# Most years are more than 70% populated by OISST data

# Convert sf to df
agg_stn_all_OISST <- sfheaders::sf_to_df(agg_stn_all_OISST, fill=T)
agg_stn_all_OISST <- dplyr::select(agg_stn_all_OISST,
                                   trip, year, month, day, x, y,
                                   Size_class, catch, fhours, temp, sstsource)
colnames(agg_stn_all_OISST) <- c('id', 'year', 'month', 'day', 'lon', 'lat',
                                 'Size_class', 'catch', 'fhours', 'sst',
                                 'sstsource')

# Save output
write.csv(agg_stn_all_OISST, row.names = F, 
          here('Data/Clean/AllYears_Canada_UpdatedSST.csv'))