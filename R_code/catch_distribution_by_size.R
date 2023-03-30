# Plot distribution of positive catch by size class

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
                legend.position = "right",
                legend.text = element_text(size=12),
                strip.text = element_text(size=12),
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=16, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Load data
dat <- read.csv(here('Data/Clean/BFT_US_catch_VASTdata.csv'))
dat_sf <- st_as_sf(dat, coords=c('lon', 'lat'))
st_crs(dat_sf) <- 'EPSG:4326'

coast <- ecodata::coast
coast <- st_transform(coast, st_crs(dat_sf))

# Remove instances of 0 catch
catches <- dat_sf[dat_sf$catch > 0,]

# Create pooled year sets by timeframe of surveys
catches$yearpool[catches$year %in% seq(1993, 2001)] <- '1993-2001'
catches$yearpool[catches$year %in% seq(2002, 2021)] <- '2002-2021'

# Plot
p <- ggplot() +
  geom_sf(data=coast, fill='gray')+
  geom_sf(data=catches, aes(col=as.factor(Size_class))) +
  coord_sf(xlim=c(-77, -66),
           ylim=c(35, 46)) +
  facet_wrap(vars(yearpool)) +
  scale_color_manual('Size Class',
                     values=c("#F8766D", "#00BFC4"))
ggsave(p,
       device='png',
       filename='catch_distribution_by_size.png')
