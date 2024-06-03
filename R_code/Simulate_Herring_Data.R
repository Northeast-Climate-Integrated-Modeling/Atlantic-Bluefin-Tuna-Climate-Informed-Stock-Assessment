# Make fake points in Jun-Jul 2020 and all summer 2023

#### Workspace setup ####
rm(list=ls())

# Set wd
#setwd("~/VAST_BFT")

# Load libraries
suppressPackageStartupMessages(library(TMB))
suppressPackageStartupMessages(library(units, quietly=T, verbose=F))
suppressPackageStartupMessages(library(VAST))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(tidyverse, quietly=T, verbose=F))
suppressPackageStartupMessages(library(sf, quietly=T, verbose=F))
suppressPackageStartupMessages(library(sp, quietly=T, verbose=F))
suppressPackageStartupMessages(library(raster, quietly=T, verbose=F))
suppressPackageStartupMessages(library(splines, quietly=T, verbose=F))
suppressPackageStartupMessages(library(INLAspacetime, quietly=T, verbose=F))

# Add unitless back as possible unit (removed in units package update Mar 2023)
#install_unit(symbol='unitless', def='unitless', name='unitless')

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', linewidth=1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

#### Add sample information and covars ####
# Load data
surveys <- read.csv(here('Data/Prey_Data/species_sep_prey.csv'))

# Remove uncommon gears
surveys <- surveys[surveys$gear %in% c('otter trawl'),]

# Focus on one species (spatial info the same for all 3)
surveys <- surveys[surveys$fish == 'herring',]

# Remove points not in model domain (on land)
region_shape <- st_read(here('Data/GIS/NAFO_Continental_Shelf_10kmBuff.shp'), quiet=T)
region_shape <- region_shape %>% 
  filter(#Label != '6B',
    Region == 'US')
region_shape <- as(region_shape, 'Spatial')
region_shape <- st_as_sf(spTransform(region_shape, CRS("+init=epsg:4326")))
region_shape <- st_make_valid(region_shape)
region_shape <- dplyr::select(region_shape, Region, Label, geometry)

surveys_sf <- st_as_sf(surveys, coords=c('lon', 'lat'))
st_crs(surveys_sf) <- 'EPSG:4326'

surveys_sf <- st_intersection(surveys_sf, region_shape)

# Convert back to dataframe
surveys <- sfheaders::sf_to_df(surveys_sf, fill=TRUE)
surveys <- surveys %>%
  rename(lon = x) %>% 
  rename(lat = y) %>% 
  dplyr::select(-sfg_id, -point_id, -Label)

#### Predict how many samples should happen in each year-month-strata combo ####
# Add 2023 as a possibility
missing <- data.frame(
  Freq = rep(NA, 5),
  Year = rep(2023, 5),
  Nafo.Unit = c('5Y', '5Zu', '5Zw', '6A', '6B')
  
)
# Blank dataframe to append to
blankdat <- data.frame(
  Year = NA,
  Month = NA,
  Pred.Samps = NA,
  group = NA
)
# Split by month 
monthlist <- split(surveys, f=surveys$month)
# Loop through months
for(i in 1:length(monthlist)){
  # Pull number of samps per month-strat-year combo
  monthtab <- as.data.frame(table(monthlist[[i]]$year,
                                     monthlist[[i]]$nafo.unit))
  monthtab <- monthtab %>% 
    mutate(Year = as.numeric(as.character(Var1)),
           Nafo.Unit = Var2) %>% 
    dplyr::select(-Var1, -Var2)
  
  # Add 2023
  monthtab <- rbind(monthtab, missing)
  monthtab <- monthtab[with(monthtab, order(Year, Nafo.Unit)),]  
  # Simple GAM with cubic splines to fill out missing months
  gg.gam <- ggplot(data=monthtab) +
    geom_point(aes(x=Year, y=Freq, col=Nafo.Unit)) +
    geom_smooth(aes(x=Year, y=Freq, col=Nafo.Unit, fill=Nafo.Unit),
                method='gam', fullrange=T, alpha=0.4) +
    facet_wrap(vars(Nafo.Unit))
  # Extract GAM results
  gextract <- ggplot_build(gg.gam)$data[[2]]
  gextract <- gextract %>% 
    dplyr::select(x, y, group) %>% 
    rename(Year = x) %>% 
    rename(Pred.Samps = y) %>% 
    mutate(Month = paste0(names(monthlist)[i]))
  # Append to blank dataframe
  blankdat <- rbind(blankdat, gextract)
  # Remove intermediates, reloop
  rm(monthtab, gg.gam, gextract)
}
# Remove empty first row
blankdat <- blankdat[!is.na(blankdat$Year),]
# Fix strata names
strat <- data.frame(
  group = seq(1, 5, 1),
  Nafo.Unit = c('5Y', '5Zu', '5Zw', '6A', '6B')
)
blankdat <- left_join(blankdat, strat, by=c('group'))
blankdat <- blankdat %>% 
  dplyr::select(-group)

# Extract samples that should have taken place Jun-Jul 2020
pull2020 <- blankdat %>% 
  filter(Month %in% c(6, 7),
         plyr::round_any(Year, 0.1, f=floor) == 2020)
# Extract samples that likely took place 2023
pull2023 <- blankdat %>% 
  filter(Year == 2023)
# Bind predicted samples, clean
predicted.samples <- rbind(pull2020, pull2023)
predicted.samples <- predicted.samples %>% 
  mutate(Year = round(Year),
         Pred.Samps = round(Pred.Samps))
# Can't have negative samples.
predicted.samples$Pred.Samps[predicted.samples$Pred.Samps < 0] <- 0

#### Pull that number of points in each strat-month-year ####
# We don't want these to be random, actually
# Fishermen target specific areas they think balance finding fish and costs
# Let's make a DF of month-strata places (disregard year)
# Then randomly sample n number of points in there.

# Make column for month-strata
predicted.samples$helper <- paste0(predicted.samples$Month, '-',
                                   predicted.samples$Nafo.Unit)
surveys$helper <- paste0(surveys$month, '-', 
                         surveys$nafo.unit)
# Identify rows
surveys$id <- seq(1, nrow(surveys), 1)
# Split predicted samples by month-strata
pred.list <- split(predicted.samples, f=predicted.samples$helper)
# Blank vector to append to 
holder <- NA
yearholder <- NA
# Loop through list
for(i in 1:length(pred.list)){
  # Extract row numbers of survey stations
  temp <- sample(surveys$id[surveys$helper == 
                           paste0(pred.list[[i]]$helper[1])],
                 size=sum(pred.list[[i]]$Pred.Samps))
  yeartemp <- rep(pred.list[[i]]$Year, paste0(pred.list[[i]]$Pred.Samps))
  # Append to blank vector
  holder <- c(holder, temp)
  yearholder <- c(yearholder, yeartemp)
  # Remove intermediates
  rm(temp, yeartemp)
}
# Remove blank item of vector
holder <- holder[!is.na(holder)]
yearholder <- yearholder[!is.na(yearholder)]

# Extract sample rows we identified
extracted.samples <- 
  surveys[holder,]

# Clean extracted samples
extracted.samples <- extracted.samples %>% 
  dplyr::select(month, nafo.unit, lon, lat, helper)
extracted.samples$Year <- yearholder

extracted.samples <- extracted.samples %>% 
  mutate(tripid= NA,
         haulnum=NA,
         fish='herring',
         gear='ottertrawl',
         wt_kgs=NA,
         Region = 'US')

surveys <- surveys %>% 
  dplyr::select(-helper, -id)
extracted.samples$helper <- NULL
extracted.samples <- extracted.samples %>% 
  rename(year = Year)

write.csv(extracted.samples, 
          here('Data/Prey_Data/prey_filltime.csv'),
          row.names = F)

