# Personalized plotting using ggplot for various VAST default plots

# Prepare workspace
rm(list=ls())

# Load libraries
library(VAST)
library(tidyverse)
library(sf)
library(here)
library(ggpubr)
library(ggnewscale)
library(patchwork)

# Load data
load(here('VAST_runs/herring8_yearly/herring_8_yearly.rdata'))
rm(list=setdiff(ls(), c("fit", "%notin%", "year.labs")))
country.use <- 'US'
setwd(here('VAST_runs/herring8_yearly'))
year.labs <- seq(2002, 2022)
cat.labs <- c('Atlantic herring', 
              'Mackerel spp.', 
              'Atlantic menhaden')

# Load functions
# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Set GGplot auto theme
theme_set(theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                panel.grid.major = element_line(color='lightgray'),
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

# Create objects needed to plot
Sdreport = fit$parameter_estimates$SD
SD = TMB::summary.sdreport(Sdreport)
TmbData = fit$data_list
Report = fit$Report

# Name where data are stored in report
CogName = "mean_Z_ctm"
EffectiveName = "effective_area_ctl"

###############################################################################
####                     Seasonal Center of gravity                        ####
###############################################################################
# Pull data
SD_mean_Z_ctm = array(NA, dim = c(unlist(TmbData[c("n_c", 
                                                   "n_t", 
                                                   "n_m"#, 
                                                   #"n_l"
                                                   )]), 
                                  2), 
                      dimnames = list(NULL, NULL, NULL, #NULL,
                                      c("Estimate", "Std. Error")))
# Pull standard error
SD_mean_Z_ctm[] = SD[which(rownames(SD) == CogName), 
                           c("Estimate", "Std. Error")]
# Name dimensions      
names(dim(SD_mean_Z_ctm)) <- c('Category',
                               'Time', 
                               'Location', 
                               #'Strata',
                               'Est.Err')
# Separate categories      
SD_mean_Z_ctm_herring <- SD_mean_Z_ctm[1,,,]
SD_mean_Z_ctm_mackerel <- SD_mean_Z_ctm[2,,,]
SD_mean_Z_ctm_menhaden <- SD_mean_Z_ctm[3,,,]

# Concatenate sizes to list, name
size.list.cog <- list(SD_mean_Z_ctm_herring,
                      SD_mean_Z_ctm_mackerel,
                      SD_mean_Z_ctm_menhaden)

names(size.list.cog) <- c('herring', 'mackerel', 'menhaden')
rm(SD_mean_Z_ctm,
   SD_mean_Z_ctm_herring, SD_mean_Z_ctm_mackerel, SD_mean_Z_ctm_menhaden)

###############################################################################
####                   Seasonal Effective area occupied                    ####
###############################################################################
# Pull data
SD_effective_area_ctl = 
  SD_log_effective_area_ctl = array(NA, 
                                    dim = c(unlist(
                                      TmbData[c("n_c", 
                                                "n_t", 
                                                "n_l")]),2), 
                                    dimnames = list(NULL, 
                                                    NULL, 
                                                    NULL, 
                                                    c("Estimate", 
                                                      "Std. Error")))
# Pull standard error
SD_effective_area_ctl[] = SD[which(rownames(SD) == 
                                           EffectiveName), 
                                   c("Estimate", "Std. Error")]
# Name dimensions
names(dim(SD_effective_area_ctl)) <- c('Category', 
                                       'Time', 
                                       'Strata', 
                                       'Est.Err')

# Separate categories      
SD_effective_area_ctl_herring <- SD_effective_area_ctl[1,,,]
SD_effective_area_ctl_mackerel <- SD_effective_area_ctl[2,,,]
SD_effective_area_ctl_menhaden <- SD_effective_area_ctl[3,,,]

# Concatenate sizes to list, name
size.list.eao <- list(SD_effective_area_ctl_herring,
                      SD_effective_area_ctl_mackerel,
                      SD_effective_area_ctl_menhaden)

names(size.list.eao) <- c('herring', 'mackerel', 'menhaden')
rm(SD_effective_area_ctl,
   SD_effective_area_ctl_herring, SD_effective_area_ctl_mackerel, 
   SD_effective_area_ctl_menhaden)

rm(SD, Sdreport, TmbData, CogName, EffectiveName, SD_log_effective_area_ctl)

###############################################################################
####                       Plot EAO and COG together                       ####
###############################################################################
# Load spatial information
coast <- ecodata::coast
coast <- st_transform(coast, "EPSG:4326")
us <- st_read(here('Data/GIS/NAFO_Continental_Shelf_10kmBuff.shp'))
us <- st_transform(us, st_crs(coast))
us$STOCK <- 'US'
us <- us[us$Region == 'US',]

stocks <- us
stocks <- st_union(stocks)
stocks <- st_as_sf(stocks)
stocks <- st_transform(stocks, st_crs(coast))
stocks <- st_make_valid(stocks)
new_bb <- st_bbox(stocks)
stocks$STOCK <- 'US'

# Loop through sizes, split into seasons
for(i in 1:length(size.list.cog)){ # Number of sizes
  
    # COG
    cog <- as.data.frame(size.list.cog[[i]][,,])
    colnames(cog) <- c('easting', 'northing', 'e.sd', 'n.sd')
    cog$Year <- year.labs
    cog$Year <- as.numeric(cog$Year)
    
    eao <- as.data.frame(size.list.eao[[i]][,])
    colnames(eao) <- c('area.occ', 'sd.err')
    eao$Year <- year.labs
    eao$Year <- as.numeric(eao$Year)
    
    # Both Spring
    SD_plotting <- merge(cog, eao, by=c("Year"))
    
    # Plot northing
    northing <- ggplot(SD_plotting) +
      geom_line(aes(x=Year, y=northing), col='#F8766D', lwd=1) +
      geom_ribbon(aes(ymin=northing-n.sd,
                     ymax=northing+n.sd,
                     x=Year),
                  fill=alpha('#F8766D', 0.2)) +
      #ylim(c(4600,4825)) +
      ylab("Northing (km)") +
      xlab("")
    
    # Plot easting
    easting <- ggplot(SD_plotting) +
      geom_line(aes(x=Year, y=easting), col='#F8766D', lwd=1) +
      geom_ribbon(aes(ymin=easting-e.sd,
                      ymax=easting+e.sd,
                      x=Year),
                  fill=alpha('#F8766D', 0.2)) +
      #ylim(c(300, 700)) +
      ylab("Easting (km)")+
      xlab("")
    
    # Plot effective area occupied
    arr.occ <- ggplot(SD_plotting) +
      geom_line(aes(x=Year, y=area.occ), col='#F8766D',lwd=1) +
      geom_ribbon(aes(ymin=area.occ-sd.err,
                      ymax=area.occ+sd.err,
                      x=Year),
                  fill='#F8766D',
                  alpha=0.2) +
      #ylim(c(-5000, 75000)) +
      ylab(bquote("Area Occupied km "^2))
    
    # Arrange to plot
    spring.lines <- ggarrange(northing, easting, arr.occ, nrow=3)
    
    # COG
    SD_plotting.cog <- size.list.cog[[i]]
    SD_plotting.cog <- as.data.frame(SD_plotting.cog[,,])
    colnames(SD_plotting.cog) <- c('easting', 'northing', 'e.sd', 'n.sd')
    SD_plotting.cog$Year <- year.labs
    SD_plotting.cog$easting <- SD_plotting.cog$easting * 1000
    SD_plotting.cog$northing <- SD_plotting.cog$northing * 1000
    SD_plotting.cog$Year <- as.numeric(SD_plotting.cog$Year)
    
    # Convert to sf for plotting
    SD_plotting <- st_as_sf(SD_plotting.cog, coords=c("easting", "northing"))
    st_crs(SD_plotting) <- "EPSG:32619"
    
    # Spring
    SD_plotting.spring <- SD_plotting
    points <- st_cast(st_geometry(SD_plotting.spring), "POINT") 
    # Number of total linestrings to be created
    n <- length(points) - 1
    # Build linestrings
    linestrings <- lapply(X = 1:n, FUN = function(x) {
      
      pair <- st_combine(c(points[x], points[x + 1]))
      line <- st_cast(pair, "LINESTRING")
      return(line)
    })
    # Split to individual linestrings, associate year
    t.spring <- st_multilinestring(do.call("rbind", linestrings))
    t.spring <-  nngeo::st_segments(t.spring)
    t.spring <- st_sf(t.spring)
    t.spring$Year <- seq(2003, 2022, 1)
    st_crs(t.spring) <- "EPSG:32619"
    
    SD_plotting.spring <- st_transform(SD_plotting.spring, st_crs(coast))
    t.spring <- st_transform(t.spring, st_crs(coast))
    
    # Plot
    spring.vis <- ggplot() +
      geom_sf(data=coast, fill='gray') +
      geom_sf(data=stocks, fill='transparent', lwd=0.25) +
      guides(col=guide_legend(title="Stock", nrow=2,byrow=TRUE)) +
      new_scale_color() +
      geom_sf(data=SD_plotting.spring, aes(col=Year), pch=19, cex=0.5) +
      scale_color_continuous(
        limits = c(2002,2022), 
        breaks = c(2002, 2009, 2015, 2022),
        labels = c('2002', ' ', ' ',  '2022'),
        guide = guide_colourbar(nbin = 100, draw.ulim = FALSE, draw.llim = FALSE)
      )+
      geom_sf(data=t.spring, aes(col=Year)) +
      coord_sf(xlim=c(st_bbox(stocks[stocks$STOCK == paste0(country.use),])[1], 
                      st_bbox(stocks[stocks$STOCK == paste0(country.use),])[3]),
               ylim=c(st_bbox(stocks[stocks$STOCK == paste0(country.use),])[2], 
                      st_bbox(stocks[stocks$STOCK == paste0(country.use),])[4])) +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle(paste0(cat.labs[i], ' center of gravity'))
    
    spring <- ggarrange(spring.vis, spring.lines, ncol=2) + bgcolor("white")
    
    ggsave(spring,
           filename=paste0(here('VAST_runs/herring8_monthly'),
                           '/', 
                           names(size.list.cog)[i],
                           "_COG.png"),
           width = 10, height = 8, units='in')
}
