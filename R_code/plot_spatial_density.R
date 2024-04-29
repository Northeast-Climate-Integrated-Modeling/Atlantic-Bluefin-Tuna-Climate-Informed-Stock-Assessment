### A quick demonstration of how to extract map quantities and
### plot them externally. Cole Monnahan | May 2021
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
  D.fish$D <- log(D.fish$D)
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
  Cat.sub <- D[D$Year == paste0(year.labs$combo[i])&
                 D$fish == 'herring',]
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

for(i in 1:nrow(year.labs)){
  Cat.sub <- D[D$Year == paste0(year.labs$combo[i]) &
                 D$fish == 'herring',]
  
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

  # Plot fall
  fall <- ggplot()+
      geom_sf(data=grid_fall, aes(fill=Abundance, col=Abundance)) +
    
      scale_color_viridis_c(limits=c(min(Dlims$Min), max(Dlims$Max)),
                            breaks=c(min(Dlims$Min), max(Dlims$Max)),
                            labels=c('Low',
                                     'High'),
                            
                            na.value = 'transparent',
                            option=('viridis'),
                            direction = 1,
                            alpha = 0.8) +
    
      scale_fill_viridis_c(limits=c(min(Dlims$Min), max(Dlims$Max)),
                           breaks=c(min(Dlims$Min), max(Dlims$Max)),
                           labels=c('Low', 
                                    'High'),
                           
                           na.value = 'transparent',
                           option=('viridis'),
                           direction = 1,
                           alpha = 0.8) +

      geom_sf(data=stocks, fill=NA, col='black', pch=19, cex=0.5)+
    
      geom_sf(data=coast, fill='gray')+
      
      coord_sf(xlim=c(new_bb[1], new_bb[3]),
               ylim=c(new_bb[2], new_bb[4]),
               crs="EPSG:4326")+
      
      theme(legend.position = c(0.85,0.22),
            legend.background = element_rect(fill='white', linewidth = 0)) +
    
      theme(legend.key.size = unit(0.2, 'in')) +
    
      ggtitle(paste0(year.labs$combo)[i])
    
      # Save
      ggsave(fall, 
             filename = 
               paste0(here(),
                      '/VAST_runs/herring8_monthly/Annual_SpatDens/herring/',
                      year.labs$combo[i], "_spatialdensity.png"),
             device="png",
             width = 11, height = 8.5, units='in'
             )
  
}
