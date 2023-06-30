# Merge Canadian data
rm(list=ls())

# Load libraries and functions
library(here)
library(tidyverse)
library(sf)
library(TMB)
library(VAST)
library(units)
library(beepr)
library(ggcorrplot)
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

comland <- read.csv(here('Data/Clean/SWNS_comland_9503_clean.csv'))
comland$data <- 'comland'
marfis <- read.csv(here('Data/Clean/SWNS_Disagg_clean.csv'))

canada <- rbind(comland, marfis)
canada <- unique(canada)
head(canada)
canada <- canada[with(canada, order(year, month, lon, lat, Size_class)),]
rownames(canada) <- NULL
head(canada)
table(canada$year, canada$Size_class)

table(canada$trip, canada$Size_class)

canada.list <- split(canada, f=canada$trip)
for(i in 1:length(canada.list)){
  if("large" %notin% canada.list[[i]]$Size_class){
    large.df <- data.frame(
      Size_class = 'large',
      trip=canada.list[[i]]$trip[1],
      year=canada.list[[i]]$year[1],
      month=canada.list[[i]]$month[1],
      day=canada.list[[i]]$day[1],
      fhours=canada.list[[i]]$fhours[1],
      lon=canada.list[[i]]$lon[1],
      lat=canada.list[[i]]$lat[1],
      sst=canada.list[[i]]$sst[1],
      catch=0,
      data=canada.list[[i]]$data[1]
    )
    canada.list[[i]] <- rbind(canada.list[[i]], large.df)
    rm(large.df)
  }

  if("small" %notin% canada.list[[i]]$Size_class){
    small.df <- data.frame(
      Size_class = 'small',
      trip=canada.list[[i]]$trip[1],
      year=canada.list[[i]]$year[1],
      month=canada.list[[i]]$month[1],
      day=canada.list[[i]]$day[1],
      fhours=canada.list[[i]]$fhours[1],
      lon=canada.list[[i]]$lon[1],
      lat=canada.list[[i]]$lat[1],
      sst=canada.list[[i]]$sst[1],
      catch=0,
      data=canada.list[[i]]$data[1]
    )
    canada.list[[i]] <- rbind(canada.list[[i]], small.df)
    rm(small.df)
  }
  
  if("Unknown" %notin% canada.list[[i]]$Size_class){
    Unknown.df <- data.frame(
      Size_class = 'Unknown',
      trip=canada.list[[i]]$trip[1],
      year=canada.list[[i]]$year[1],
      month=canada.list[[i]]$month[1],
      day=canada.list[[i]]$day[1],
      fhours=canada.list[[i]]$fhours[1],
      lon=canada.list[[i]]$lon[1],
      lat=canada.list[[i]]$lat[1],
      sst=canada.list[[i]]$sst[1],
      catch=0,
      data=canada.list[[i]]$data[1]
    )
    canada.list[[i]] <- rbind(canada.list[[i]], Unknown.df)
    rm(Unknown.df)
  }
}

canada2 <- do.call(rbind, canada.list)
canada2 <- canada2[with(canada2, order(trip, Size_class)),]
rownames(canada2) <- NULL
head(canada2)

table(canada2$year, canada2$Size_class)
table(canada2$trip, canada2$Size_class)

write.csv(canada2,
          here('Data/Clean/Canada_landings.csv'),
          row.names = F)


