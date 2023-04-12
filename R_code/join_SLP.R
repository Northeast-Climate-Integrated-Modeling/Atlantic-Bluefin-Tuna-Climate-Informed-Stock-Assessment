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


#list of SLP dataframes
SLPdfs <- list.files(here("Data/SLP/"), pattern = "*.rds")

# Create empty tibble to fill
stn_SLP <- tibble()

# Load station data
# Load data
dat.1993 <- read.csv(here('Data/Clean/LPS_Pre_2002_Clean.csv'))
dat.2021 <- read.csv(here('Data/Clean/LPS_Post_2002_Clean.csv'))
stations <- rbind(dat.1993, dat.2021)
rm(dat.1993, dat.2021)

# Remove large category (it's fine, it's a spatial duplicate of small.)
stations <- subset(stations, Size_class == 'small')

stations$DATE <- paste0(stations$year, '-',
                        str_pad(stations$month, 2, "left", "0"), "-",
                        str_pad(stations$day, 2, "left", '0'))

# Create datestring to merge SLP
stations$DATE <- as.POSIXct(stations$DATE, 
                            format = "%Y-%m-%d")
stations$yrmody <- paste0(stations$year, 
                          str_pad(stations$month, 2, "left", '0'), 
                          str_pad(stations$day, 2, "left", '0'))
stations <- st_as_sf(stations, coords=c("lon", "lat"),
                     na.fail=T)
#stations <- subset(stations, YEAR > 1981)

# Initialize progress bar
pb <- txtProgressBar(min=0, max=length(SLPdfs), initial=0, char="=", style=3)

# Merge in year-based loops
for(df in SLPdfs){
  # Call the number of the current .rds file in the vector of .rds files
  t <- match(df, SLPdfs)
  
  # Update progress bar
  setTxtProgressBar(pb, t)
  getTxtProgressBar(pb)
  
  # Call annual .rds SLP file
  SLPdf <- readRDS(paste0(here("Data/SLP/", df)))
  SLPdf$yrmody <- paste0(SLPdf$year, SLPdf$month, SLPdf$day)
  SLPdf <- dplyr::select(SLPdf, Lon, Lat, sst, year, month, day, yrmody)
  colnames(SLPdf) <- c('Lon', 'Lat', 'slp', 'year', 'month', 'day', 'yrmody')
  
  # Create string of sampled slp in year
  stationsyr <- stations %>%
    filter(year == unique(SLPdf$year))
  
  # Filter out SLP days not sampled for bluefin, convert to sf object
  SLPdf_survdays <- SLPdf %>%
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
  yrdietSLP <- suppressMessages(do.call('rbind', 
                         lapply(split(stationsyr, 
                                      1:nrow(stationsyr)), 
                  function(x) {
                        st_join(x, 
                                SLPdf_survdays[SLPdf_survdays$yrmody == unique(x$yrmody),],
                                join = st_nn, k = 1, progress = TRUE)
                               }
  )))
  
  # Bind one year of the loop to the initialized tibble
  stn_SLP <- rbind(stn_SLP, yrdietSLP)
  
  # Close segment of progress bar every loop
  close(pb)
  
}

# Save output
#saveRDS(stn_SLP, here("data/RData_Storage/stn_SLP.rds"))

# Read in station data and station-SLP
stn_SLP_merge <- stn_SLP %>%
  dplyr::select(-month.y,
                -day.y,
                -yrmody.y) %>% 
  dplyr::rename(month = month.x,
                day = day.x,
                yrmody = yrmody.x) %>%
  dplyr::select(id, slp) %>%
  sf::st_drop_geometry()

# Merge
agg_stn_all_SLP <- left_join(stations, stn_SLP_merge)

# Save output
#saveRDS(agg_stn_all_SLP, here("Data/Clean/agg_stn_all_SLP.rds"))
#agg_stn_all_SLP <- readRDS(here("data/agg_stn_all_SLP.rds"))

# Convert sf to df
agg_stn_all_SLP <- sfheaders::sf_to_df(agg_stn_all_SLP, fill=T)
agg_stn_all_SLP <- dplyr::select(agg_stn_all_SLP,
                                   id, year, month, day, x, y,
                                   Size_class, catch, fhours, slp)
colnames(agg_stn_all_SLP) <- c('id', 'year', 'month', 'day', 'lon', 'lat',
                                 'Size_class', 'catch', 'fhours', 'slp')

# Save output
write.csv(agg_stn_all_SLP, row.names = F,
         here('Data/Clean/AllYears_IncludeSLP.csv'))
