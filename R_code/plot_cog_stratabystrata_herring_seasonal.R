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
load(here('VAST_runs/herring_graham/herring1.Rdata'))
rm(list=setdiff(ls(), c("fit", "%notin%", "year.labs")))
strata_order <- fit$settings$strata.limits
strata_order$Stratum <- c('Stratum_1', 'Stratum_2', 'Stratum_3',
                          'Stratum_4', 'Stratum_5')
setwd(here('VAST_runs/herring_graham'))
year.labs <- fit$year_labels


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
                                                   "n_m", 
                                                   "n_l"
                                                   )]), 
                                  2), 
                      dimnames = list(NULL, NULL, NULL, NULL,
                                      c("Estimate", "Std. Error")))
# Pull standard error
SD_mean_Z_ctm[] = SD[which(rownames(SD) == CogName), 
                           c("Estimate", "Std. Error")]
# Name dimensions      
names(dim(SD_mean_Z_ctm)) <- c('Category',
                               'Time', 
                               'Location', 
                               'Strata',
                               'Est.Err')
# Separate categories      
SD_mean_Z_ctm_herring <- SD_mean_Z_ctm[1,,,,]

# Concatenate sizes to list, name
size.list.cog <- list(SD_mean_Z_ctm_herring)

names(size.list.cog) <- c('herring')
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

# Concatenate sizes to list, name
size.list.eao <- list(SD_effective_area_ctl_herring)

names(size.list.eao) <- c('herring')
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
us <- st_read(here('Data/GIS/Prey_Management/Herring_Management_Areas.shp'))
us <- st_transform(us, st_crs(coast))

stocks <- st_union(us)
stocks <- st_as_sf(stocks)
stocks <- st_transform(stocks, st_crs(coast))
stocks <- st_make_valid(us)
new_bb <- st_bbox(stocks)

# Loop through sizes, split into seasons
for(i in 1:length(size.list.cog)){ # Number of sizes
  
    # COG
    cog <- as.data.frame(size.list.cog[[1]][,,i,])
    colnames(cog) <- c('easting', 'northing', 'e.sd', 'n.sd')
    cog$Year <- year.labs
    cog <- cog %>% 
      separate(Year, into=c('Year', 'Season'), sep=" ")
    cog$Year <- as.numeric(cog$Year)
    cog$Season[cog$Season == 'Jan-Jun'] <- 'Winter-Spring Feeding'
    cog$Season[cog$Season == 'Jul-Dec'] <- 'Summer Feeding-Spawning'
    
    eao <- as.data.frame(size.list.eao[[1]][,i,])
    colnames(eao) <- c('area.occ', 'sd.err')
    eao$Year <- year.labs
    eao <- eao %>% 
      separate(Year, into=c('Year', 'Season'), sep=" ")
    eao$Year <- as.numeric(eao$Year)
    eao$Season[eao$Season == 'Jan-Jun'] <- 'Winter-Spring Feeding'
    eao$Season[eao$Season == 'Jul-Dec'] <- 'Summer Feeding-Spawning'
    
    # Both Spring
    SD_plotting <- merge(cog, eao, by=c("Year", "Season"))
    
    # Plot northing
    northing.sfs <- ggplot(SD_plotting[SD_plotting$Season ==
                                         'Winter-Spring Feeding',]) +
      geom_line(aes(x=Year, y=northing), col='#00BFC4', lwd=1) +
      geom_ribbon(aes(ymin=northing-n.sd,
                     ymax=northing+n.sd,
                     x=Year),
                  fill=alpha('#00BFC4', 0.2)) +
      #ylim(c(4600,4825)) +
      ylab("Northing (km)") +
      xlab("")
    
    # Plot easting
    easting.sfs <- ggplot(SD_plotting[SD_plotting$Season ==
                                        'Winter-Spring Feeding',]) +
      geom_line(aes(x=Year, y=easting), col='#00BFC4', lwd=1) +
      geom_ribbon(aes(ymin=easting-e.sd,
                      ymax=easting+e.sd,
                      x=Year),
                  fill=alpha('#00BFC4', 0.2)) +
      #ylim(c(300, 700)) +
      ylab("Easting (km)")+
      xlab("")
    
    # Plot effective area occupied
    arr.occ.sfs <- ggplot(SD_plotting[SD_plotting$Season == 
                                        'Winter-Spring Feeding',]) +
      geom_line(aes(x=Year, y=area.occ), col='#00BFC4',lwd=1) +
      geom_ribbon(aes(ymin=area.occ-sd.err,
                      ymax=area.occ+sd.err,
                      x=Year),
                  fill='#00BFC4',
                  alpha=0.2) +
      #ylim(c(-5000, 75000)) +
      ylab(bquote("Area Occupied km "^2))
    
    # Arrange to plot
    spring.lines <- ggarrange(northing.sfs, easting.sfs, arr.occ.sfs, nrow=3)
    
    # COG
    SD_plotting.cog.sfs <- cog[cog$Season == 'Winter-Spring Feeding',]
    #SD_plotting.cog <- as.data.frame(SD_plotting.cog[,,])
    #colnames(SD_plotting.cog) <- c('easting', 'northing', 'e.sd', 'n.sd')
    #SD_plotting.cog$Year <- year.labs
    SD_plotting.cog.sfs$easting <- SD_plotting.cog.sfs$easting * 1000
    SD_plotting.cog.sfs$northing <- SD_plotting.cog.sfs$northing * 1000
    SD_plotting.cog.sfs$Year <- as.numeric(SD_plotting.cog.sfs$Year)
    
    # Convert to sf for plotting
    SD_plotting.sfs <- st_as_sf(SD_plotting.cog.sfs, coords=c("easting", "northing"))
    st_crs(SD_plotting.sfs) <- "EPSG:32619"
    
    # Spring
    points <- st_cast(st_geometry(SD_plotting.sfs), "POINT") 
    # Number of total linestrings to be created
    n <- length(points) - 1
    # Build linestrings
    linestrings <- lapply(X = 1:n, FUN = function(x) {
      
      pair <- st_combine(c(points[x], points[x + 1]))
      line <- st_cast(pair, "LINESTRING")
      return(line)
    })
    # Split to individual linestrings, associate year
    t.sfs <- st_multilinestring(do.call("rbind", linestrings))
    t.sfs <-  nngeo::st_segments(t.sfs)
    t.sfs <- st_sf(t.sfs)
    t.sfs$Year <- seq(1990, 2022, 1)
    st_crs(t.sfs) <- "EPSG:32619"
    
    SD_plotting.sfs <- st_transform(SD_plotting.sfs, st_crs(coast))
    t.sfs <- st_transform(t.sfs, st_crs(coast))
    
    # Plot
    sfs.vis <- ggplot() +
      geom_sf(data=coast, fill='gray') +
      geom_sf(data=stocks, fill='transparent', lwd=0.25) +
      guides(col=guide_legend(title="Stock", nrow=2,byrow=TRUE)) +
      new_scale_color() +
      geom_sf(data=SD_plotting.sfs, aes(col=Year), pch=19, cex=0.5) +
      scale_color_continuous(
        limits = c(1989,2022), 
        breaks = c(1989, 1997, 2006, 2014, 2022),
        labels = c('1989', ' ', ' ', ' ', '2022'),
        guide = guide_colourbar(nbin = 100, draw.ulim = FALSE, draw.llim = FALSE)
      )+
      geom_sf(data=t.sfs, aes(col=Year)) +
      coord_sf(xlim=c(st_bbox(stocks)[1], 
                      st_bbox(stocks)[3]),
               ylim=c(st_bbox(stocks)[2], 
                      st_bbox(stocks)[4])) +
      xlab("Longitude") + ylab("Latitude")
    
    sfs <- ggarrange(sfs.vis, spring.lines, ncol=2) + bgcolor("white")
    
    ggsave(sfs,
           filename=paste0(here('VAST_runs/herring_graham'),
                          "/WinterSpringFeeding_COG.png"),
           width = 10, height = 8, units='in')
}
