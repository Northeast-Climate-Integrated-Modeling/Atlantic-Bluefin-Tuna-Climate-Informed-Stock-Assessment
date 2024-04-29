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
load(here('VAST_runs/tuna13_usonly/gom_only/tuna13_usonly_gomonly_0222_no04.RData'))
rm(list=setdiff(ls(), c("fit", "%notin%", "year.labs")))
country.use <- 'US'
setwd(here('VAST_runs/tuna13_usonly/gom_only'))

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
####                      Annual Center of gravity                         ####
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
# Keep only US strata, drop category      
SD_mean_Z_ctm <- SD_mean_Z_ctm[1,,,1,]

# Load spatial information
coast <- ecodata::coast
coast <- st_transform(coast, "EPSG:4326")
us <- st_read(here('Data/GIS/NAFO_5Y.shp'))
us <- st_transform(us, st_crs(coast))
#us <- us[us$Region == 'US',]

# COG
cog <- as.data.frame(SD_mean_Z_ctm[,,])
colnames(cog) <- c('easting', 'northing', 'e.sd', 'n.sd')
cog$Year <- fit$year_labels
cog$Year <- as.numeric(cog$Year)

# Convert to meters
cog$easting <- cog$easting * 1000
cog$northing <- cog$northing * 1000
cog$e.sd <- cog$e.sd * 1000
cog$n.sd <- cog$n.sd * 1000

# Find LR linestring (COG +/- SD)
LR <- c(cog$easting + cog$e.sd,
            cog$easting - cog$e.sd)
LR <- as.data.frame(LR)
LR$northing <- rep(cog$northing, 2)
LR$Year <- rep(seq(2002, 2022),2)
LR <- st_as_sf(LR, coords=c('LR', 'northing'), crs="EPSG:32619")
LR <- LR %>% 
  group_by(Year) %>% 
  summarize() %>% 
  st_cast("LINESTRING")

# Find UD linestring (COG +/- SD)
UD <- c(cog$northing + cog$n.sd,
        cog$northing - cog$n.sd)
UD <- as.data.frame(UD)
UD$easting <- rep(cog$easting, 2)
UD$Year <- rep(seq(2002, 2022),2)
UD <- st_as_sf(UD, coords=c('easting', 'UD'), crs="EPSG:32619")
UD <- UD %>% 
  group_by(Year) %>% 
  summarize() %>% 
  st_cast("LINESTRING")

coast <- st_transform(ecodata::coast, st_crs(UD))

ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=LR,#[LR$Year <=2011,], 
          aes(col=(Year))) +
  #scale_color_viridis_d(option='viridis',
  #                      end=0.8) +
  geom_sf(data=UD,#[UD$Year <=2011,], 
          aes(col=(Year))) +
  #scale_color_viridis_d(option='viridis',
  #                      end=0.8) +
  coord_sf(xlim=c(-70, -68),
           ylim=c(42.5, 44.5),
           crs="EPSG:4326")

ggplot() +
  geom_line(data=cog[cog$Year != 2022,],
            aes(x=Year, y=northing)) +
  geom_ribbon(data=cog[cog$Year != 2022,],
              aes(x=Year, ymin=northing - n.sd, ymax=northing + n.sd),
              alpha=0.2) +
  geom_smooth(data=cog[cog$Year != 2022,],
              aes(x=Year, y=northing),
              method='lm')

summary(lm(northing ~ Year, data=cog))
summary(lm(easting ~ Year, data=cog))    

ndifs <- vector(length = nrow(cog))
ndifs[1] <- 0
for(i in 2:nrow(cog)){
  ndifs[i] <- cog$northing[i] - cog$northing[(i-1)]
}
ndifs <- cbind(ndifs, seq(2002, 2022))
ndifs <- as.data.frame(ndifs)
colnames(ndifs) <- c('n.from.prev', 'Year')

ggplot() +
  geom_col(data=ndifs,
           aes(x=Year, y=n.from.prev))
