# Extract and append monthly prey density to VAST input dataset, US only #
rm(list=ls())

# Load libraries
library(VAST)                           # 3.8.0
library(ggplot2)                        # 2.10.0
library(dplyr)
library(tidyr)
library(here)
library(sf)
library(ggpubr)
library(ggpattern)
library(ggnewscale)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Set GGplot auto theme
theme_set(theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', size=1, fill=NA),
                legend.title = element_text(size=10),
                legend.text = element_text(size=10),
                legend.background = element_blank(),
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=16, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Load VAST fit data
load(here("VAST_runs/herring8_monthly/herring_8_monthly.Rdata"))
rm(list=setdiff(ls(), c("fit", "%notin%", "year.labs")))
country.use <- c('US')

# Make time labels
year.labs <- rep(seq(2002, 2022), 5)
year.labs <- year.labs[order(year.labs)]
year.labs <- as.data.frame(year.labs)
year.labs$month <- rep(c('Jun', 'Jul', 'Aug', 'Sep', 'Oct'), 21)
colnames(year.labs) <- c('Year', 'Month')
year.labs$combo <- paste0(year.labs$Month, ' ', year.labs$Year)
year.labs <- year.labs[year.labs$combo %notin%
                         c('Jun 2020', 'Jul 2020'),]
year.labs$Time <- seq(1:nrow(year.labs))

# Remake map list locally for recreating plots
mdl <- make_map_info(Region = fit$extrapolation_list$Area_km2_x,
                     spatial_list = fit$spatial_list,
                     Extrapolation_List = fit$extrapolation_list)

# Load outline of mapped area
area.outline <- st_read(here('Data/GIS/NAFO_Continental_Shelf_10kmBuff.shp'))
area.outline <- st_transform(area.outline, "EPSG:4326")

D <- data.frame(
  cell=NA, Year=NA, D=NA, Lat=NA, Lon=NA, Include=NA, fish=NA
)

cat.labs <- c('herring', 'mackerel', 'menhaden')

for(i in 1:length(cat.labs)){
  ## Get the model estimate of density for each category and year;
  # link it spatially to a lat/lon extrapolation point.
  D_gt <- fit$Report$D_gct[,i,] # drop the category
  dimnames(D_gt) <- list(cell=1:nrow(D_gt), 
                         #category=c('herring', 'mackerel', 'menhaden'),
                         year=year.labs$combo)
  D_gt <- D_gt %>% as.data.frame() %>%
    tibble::rownames_to_column(var = "cell") %>%
    pivot_longer(-cell, names_to = "Year", values_to='D')
  #D_gt <- D_gt %>% 
  #  separate(Year, into=c('Category', 'Year'), sep='.')
  D.fish <- merge(D_gt, mdl$PlotDF, by.x='cell', by.y='x2i')
  
  # Adjust data to log abundance, strip units
  D.fish$D <- strip_units(D.fish$D)
  #D.fish$D <- log(D.fish$D)
  D.fish$fish <- cat.labs[i]
  
  D <- rbind(D, D.fish)
  
  rm(D.fish, D_gt)
}
D <- D[!is.na(D$fish),]

# Set CRS 
projargs <- fit$extrapolation_list$projargs
CRS_orig = sp::CRS("+proj=longlat")
CRS_proj = sp::CRS(projargs)

# Load spatial information
coast <- ecodata::coast
coast <- st_transform(coast, "EPSG:4326")

us <- st_read(here('Data/GIS/NAFO_Continental_Shelf_10kmBuff.shp'))
us <- st_transform(us, st_crs(coast))
us <- us[us$Region == 'US',]
us$STOCK <- 'US'

stocks <- us
stocks <- st_transform(stocks, st_crs(coast))
stocks <- st_make_valid(stocks)

stocks <- stocks[stocks$STOCK %in% paste0(country.use),]

new_bb <- st_bbox(stocks)

Dlims <- data.frame(
  Year=NA,
  Min=NA,
  Max=NA
)

# Outer loop: Years
for(i in 1:nrow(year.labs)){
  Cat.sub <- D[D$Year == paste0(year.labs$combo[i]),]
  Cat.sub <- Cat.sub %>% 
    group_by(cell, Year, Lat, Lon, Include) %>% 
    summarise(D = sum(D))
  
  Inlims <- data.frame(
    Year=NA,
    Min=NA,
    Max=NA
  )
  Inlims$Year <- Cat.sub$Year[1]
  Inlims$Min <- min(Cat.sub$D, na.rm=T)  
  Inlims$Max <- max(Cat.sub$D, na.rm=T)
  
  Dlims <- rbind(Dlims, Inlims)
  
  rm(Cat.sub, Inlims)
}
Dlims <- Dlims[!is.na(Dlims$Year),]
summary(Dlims)
Dlims$Min <- log(Dlims$Min)
Dlims$Max <- log(Dlims$Max)

# Add survey data
surveys <- read.csv(here('Data/Clean/BFT_BothCountries_Large_VAST_5.csv'))
surveys <- surveys %>% 
  filter(location == 'us')
surveys <- st_as_sf(surveys, coords=c('lon', 'lat'), crs="EPSG:4326")

surveys$Month[surveys$month == 6] <- 'Jun'
surveys$Month[surveys$month == 7] <- 'Jul'
surveys$Month[surveys$month == 8] <- 'Aug'
surveys$Month[surveys$month == 9] <- 'Sep'
surveys$Month[surveys$month == 10] <- 'Oct'

surveys$combo <- paste0(surveys$Month, ' ', surveys$year)

surveys <- split(surveys, f=surveys$combo)

for(i in 1:nrow(year.labs)){
  
  if(year.labs$combo[i] %in%
     c('Jun 2004', 'Jun 2005', 'Jun 2006', 'Jun 2007')){
    next()
  }
  
  print(i)
  
  Cat.sub <- D[D$Year == paste0(year.labs$combo[i]),]
  
  Cat.sub <- Cat.sub %>% 
    group_by(cell, Year, Lat, Lon, Include) %>% 
    summarise(D = sum(D)) %>% 
    mutate(D=log(D))
  
  # Set center of cells
  loc_g <- cbind(Cat.sub$Lon, 
                 Cat.sub$Lat)
  
  n_cells <- dim(loc_g)[1]
  
  Points_orig = sp::SpatialPointsDataFrame(coords = loc_g, 
                                           data = Cat.sub, 
                                           proj4string = CRS_orig)
  
  Points_LongLat = sp::spTransform(Points_orig, sp::CRS("+proj=longlat"))
  
  Points_proj = sp::spTransform(Points_orig, CRS_proj)
  
  cell.size = mean(diff(Points_proj@bbox[1, ]), 
                   diff(Points_proj@bbox[2,]))/floor(sqrt(n_cells))
  
  Points_sf = sf::st_as_sf(Points_proj)
  
  grid_fall = sf::st_make_grid(Points_sf, cellsize = cell.size)
  grid_i_fall = sf::st_intersects(Points_sf, grid_fall)
  grid_fall = sf::st_sf(grid_fall, Abundance = tapply(Points_sf$D, 
                                                      INDEX = factor(as.numeric(grid_i_fall),
                                                                     levels = 1:length(grid_fall)), 
                                                      FUN = mean, na.rm = TRUE))
  grid_fall <- st_transform(grid_fall, "EPSG:4326")
  
  use.survs <- surveys[[paste0(year.labs$combo)[i]]]
  
  use.survs <- st_intersection(use.survs, grid_fall)
  
  surveys[[paste0(year.labs$combo)[i]]] <- use.survs
  
}

# Remove Jun Jul 2020 (no herring data)
surveys <- surveys[-which(names(surveys) %in% 
                            c('Jun 2020', 'Jul 2020'))]

survs <- do.call(rbind, surveys)
survs <- sfheaders::sf_to_df(survs, fill=T)

survs <- survs %>% 
  dplyr::select(-sfg_id, -point_id,
                -Month, -combo) %>% 
  rename(lon=x,
         lat=y)

allsurvs <- read.csv(here('Data/Clean/BFT_BothCountries_Large_VAST_5.csv'))
allsurvs <- allsurvs[allsurvs$id %notin% survs$id,]

allsurvs$Abundance <- NA
withprey <- rbind(allsurvs, survs)

write.csv(withprey,
          here('Data/Clean/BFT_BothCountries_Large_Prey_VAST_5.csv'))
