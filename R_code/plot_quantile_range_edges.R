# Personalized plotting of range edges

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

# Call data needed in standard function
Obj = fit$tmb_list$Obj
Report = Obj$report()
TmbData = Obj$env$data
Sdreport = fit$parameter_estimates$SD
SD = TMB::summary.sdreport(Sdreport)

# Call labels
year_labels = fit$year_labels
D_name = "D_gct"
years_to_plot = 1:TmbData$n_t
strata_names = 1:TmbData$n_l
category_names = 1:TmbData$n_c
m_labels = colnames(TmbData$Z_gm)

# Set wd
working_dir = getwd()
    
# Set sampling parameters
quantiles = c(0.05, 0.5, 0.95)
n_samples = 100
interval_width = 1
    
# Set plotting parameters
width = NULL
height = NULL
calculate_relative_to_average = FALSE
seed = 123456 

# Must provide necessary inputs
D_gctr = sample_variable(Sdreport = Sdreport, 
                         Obj = Obj,
                         variable_name = D_name, 
                         n_samples = n_samples, 
                         seed = seed)
if (any(D_gctr == Inf)) 
    stop("`sample_variable` in `plot_range_edge` is producing D=Inf; please use `n_samples=0` to avoid error")
if (FALSE) {
    D_gctr = sample_variable(Sdreport = Sdreport, Obj = Obj, 
                             variable_name = "L_epsilon2_cf", 
                             n_samples = n_samples, 
                             seed = seed)
}

# Create arrays
E_zctm = array(NA, dim = c(length(quantiles), dim(Report[[D_name]])[2:3], 
                           ncol(TmbData$Z_gm)))
E_zctmr = array(NA, dim = c(length(quantiles), dim(Report[[D_name]])[2:3], 
                            ncol(TmbData$Z_gm), n_samples))
Mean_cmr = array(NA, dim = c(dim(Report[[D_name]])[2], ncol(TmbData$Z_gm), 
                             n_samples))
prop_zctm = array(NA, dim = c(dim(Report[[D_name]])[1:3], 
                              ncol(TmbData$Z_gm)))
prop_zctmr = array(NA, dim = c(dim(Report[[D_name]])[1:3], 
                               ncol(TmbData$Z_gm), n_samples))

# Sample from distribution 
for (rI in 0:n_samples) {
  for (mI in 1:ncol(TmbData$Z_gm)) {
    order_g = order(TmbData$Z_gm[, mI], decreasing = FALSE)
    if (rI == 0) 
      prop_zctm[, , , mI] = apply(Report[[D_name]], 
                                  MARGIN = 2:3, FUN = function(vec) {
                                    cumsum(vec[order_g])/sum(vec)
                                  })
    if (rI >= 0) 
      prop_zctmr[, , , mI, rI] = apply(D_gctr[, , , 
                                              rI, drop = FALSE], MARGIN = 2:3, FUN = function(vec) {
                                                cumsum(vec[order_g])/sum(vec)
                                              })
    for (cI in 1:dim(E_zctm)[2]) {
      if (rI >= 1) {
        if (calculate_relative_to_average == TRUE) {
          Mean_cmr[cI, mI, rI] = weighted.mean(as.vector(TmbData$Z_gm[, 
                                                                      mI] %o% rep(1, dim(Report[[D_name]])[3])), 
                                               w = as.vector(D_gctr[, cI, , rI]))
        }
        else {
          Mean_cmr[cI, mI, rI] = 0
        }
      }
      for (zI in 1:dim(E_zctm)[1]) {
        for (tI in 1:dim(E_zctm)[3]) {
          if (rI == 0) {
            index_tmp = which.min((prop_zctm[, cI, 
                                             tI, mI] - quantiles[zI])^2)
            E_zctm[zI, cI, tI, mI] = TmbData$Z_gm[order_g[index_tmp], 
                                                  mI]
          }
          if (rI >= 1) {
            index_tmp = which.min((prop_zctmr[, cI, 
                                              tI, mI, rI] - quantiles[zI])^2)
            E_zctmr[zI, cI, tI, mI, rI] = TmbData$Z_gm[order_g[index_tmp], 
                                                       mI] - Mean_cmr[cI, mI, rI]
          }
        }
      }
    }
  }
}

# Create array of Quantile-Direction-Year with SD
SE_zctm = apply(E_zctmr, MARGIN = 1:4, FUN = sd)
Edge_zctm = abind::abind(Estimate = E_zctm, `Std. Error` = SE_zctm, 
                         along = 5)
dimnames(Edge_zctm)[[1]] = paste0("quantile_", quantiles)

# Remove extraneous
rm(list=setdiff(ls(), c("fit", "%notin%", "year.labs",
                        "Edge_zctm")))

  # Northing
  mI = 2
  Index_zct = array(Edge_zctm[, , , mI, "Estimate"], dim(Edge_zctm)[1:3])
  sd_Index_zct = array(Edge_zctm[, , , mI, "Std. Error"], 
                       dim(Edge_zctm)[1:3])
  
  est <- aperm(Index_zct, c(2,3,1))
  est <- as.data.frame(est[,,])
  colnames(est) <- c('Quant05', 'Quant50', 'Quant95')
  est <- as.data.frame(pivot_longer(est, cols=c(colnames(est))))
  yl <- rep(seq(2002, 2022),3)
  yl <- yl[order(yl)]
  est$Year <- yl
  colnames(est)[2] <- 'est'
  
  sdest <- aperm(sd_Index_zct, c(2,3,1))
  sdest <- as.data.frame(sdest[,,])
  colnames(sdest) <- c('Quant05', 'Quant50', 'Quant95')
  sdest <- as.data.frame(pivot_longer(sdest, cols=c(colnames(sdest))))
  sdest$Year <- yl
  colnames(sdest)[2] <- 'sd'
  
  northing.re <- merge(est, sdest, by=c('name', 'Year'))
  colnames(northing.re) <- c('Quantile', 'Year', 'Northing.Est', 
                             'Northing.SD')
  
  # Easting 
  mI = 1
  Index_zct = array(Edge_zctm[, , , mI, "Estimate"], dim(Edge_zctm)[1:3])
  sd_Index_zct = array(Edge_zctm[, , , mI, "Std. Error"], 
                       dim(Edge_zctm)[1:3])
  
  est <- aperm(Index_zct, c(2,3,1))
  est <- as.data.frame(est[,,])
  colnames(est) <- c('Quant05', 'Quant50', 'Quant95')
  est <- as.data.frame(pivot_longer(est, cols=c(colnames(est))))
  yl <- rep(seq(2002, 2022),3)
  yl <- yl[order(yl)]
  est$Year <- yl
  colnames(est)[2] <- 'est'
  
  sdest <- aperm(sd_Index_zct, c(2,3,1))
  sdest <- as.data.frame(sdest[,,])
  colnames(sdest) <- c('Quant05', 'Quant50', 'Quant95')
  sdest <- as.data.frame(pivot_longer(sdest, cols=c(colnames(sdest))))
  sdest$Year <- yl
  colnames(sdest)[2] <- 'sd'
  
  easting.re <- merge(est, sdest, by=c('name', 'Year'))
  colnames(easting.re) <- c('Quantile', 'Year', 'Easting.Est', 
                             'Easting.SD')
  
  re <- merge(northing.re, easting.re, by=c('Quantile', 'Year'))
  
  # Plot Northing
  ggplot() + 
    geom_line(data=re, 
              aes(x=Year, y=Northing.Est, col=Quantile)) +
    geom_ribbon(data=re,
                aes(x=Year, ymin=Northing.Est - Northing.SD, 
                    ymax=Northing.Est + Northing.SD,
                    fill=Quantile),
                alpha=0.2) +
    labs(x='Year', y='Northing (km)')
  
  # Plot Easting
  ggplot() + 
    geom_line(data=re, 
              aes(x=Year, y=Easting.Est, col=Quantile)) +
    geom_ribbon(data=re,
                aes(x=Year, ymin=Easting.Est - Easting.SD, 
                    ymax=Easting.Est + Easting.SD,
                    fill=Quantile),
                alpha=0.2) +
    labs(x='Year', y='Easting (km)')
  
  lm(Northing.Est ~ Year, data=re[re$Quantile == 'Quant05',])
  lm(Easting.Est ~ Year, data=re[re$Quantile == 'Quant05',])
  
  # plot in space
  re$Northing.Est <- re$Northing.Est * 1000
  re$Easting.Est <- re$Easting.Est * 1000
  
  re$Easting.Hold.05 <- mean(re$Easting.Est[re$Quantile == 'Quant05'])
  re$Northing.Hold.05 <- mean(re$Northing.Est[re$Quantile == 'Quant05'])
  
  re$Easting.Hold.50 <- mean(re$Easting.Est[re$Quantile == 'Quant50'])
  re$Northing.Hold.50 <- mean(re$Northing.Est[re$Quantile == 'Quant50'])
  
  re$Easting.Hold.95 <- mean(re$Easting.Est[re$Quantile == 'Quant95'])
  re$Northing.Hold.95 <- mean(re$Northing.Est[re$Quantile == 'Quant95'])
  
  re.sf.e05 <- st_as_sf(re, coords=c('Easting.Hold.05', 'Northing.Est'),
                    crs="EPSG:32619")
  re.sf.n05 <- st_as_sf(re, coords=c('Easting.Est', 'Northing.Hold.05'),
                        crs="EPSG:32619")
  
  re.sf.e50 <- st_as_sf(re, coords=c('Easting.Hold.50', 'Northing.Est'),
                        crs="EPSG:32619")
  re.sf.n50 <- st_as_sf(re, coords=c('Easting.Est', 'Northing.Hold.50'),
                        crs="EPSG:32619")
  
  re.sf.e95 <- st_as_sf(re, coords=c('Easting.Hold.95', 'Northing.Est'),
                        crs="EPSG:32619")
  re.sf.n95 <- st_as_sf(re, coords=c('Easting.Est', 'Northing.Hold.95'),
                        crs="EPSG:32619")

  coast <- st_transform(ecodata::coast, st_crs(re.sf.e05))  
  nafo <- st_read(here('Data/GIS/NAFO_5Y.shp'))
  nafo <- st_transform(nafo, st_crs(re.sf.e05))
  
  ggplot() +
    geom_sf(data=coast) +
    geom_sf(data=re.sf.e05[re.sf.e05$Quantile == 'Quant05',], 
            aes(col=Year)) +
    coord_sf(xlim=c(-71, -67),
             ylim=c(41.5, 45),
             crs="EPSG:4326")
  
  ggplot() +
    geom_sf(data=coast) +
    geom_sf(data=re.sf.n05[re.sf.n05$Quantile == 'Quant05',], 
            aes(col=Year)) +
    coord_sf(xlim=c(-71, -67),
             ylim=c(41.5, 45),
             crs="EPSG:4326")
  
  ggplot() +
    geom_sf(data=nafo, fill=NA) +
    geom_sf(data=coast, col=NA) +

    geom_sf(data=re.sf.n05[re.sf.n05$Quantile == 'Quant05',],
            aes(col=Quantile), cex=0.4) +
    geom_sf(data=re.sf.e05[re.sf.e05$Quantile == 'Quant05',],
            aes(col=Quantile), cex=0.4) +
    
    geom_sf(data=re.sf.n05[re.sf.n05$Quantile == 'Quant05' &
                             re.sf.n05$Year == 2022,],
             cex=0.4) +
    geom_sf(data=re.sf.e05[re.sf.e05$Quantile == 'Quant05'&
                             re.sf.n05$Year == 2022,],
            cex=0.4) +
    
    geom_sf(data=re.sf.n50[re.sf.n50$Quantile == 'Quant50',], 
            aes(col=Quantile), cex=0.4) +
    geom_sf(data=re.sf.e50[re.sf.e50$Quantile == 'Quant50',], 
            aes(col=Quantile), cex=0.4) +
    
    geom_sf(data=re.sf.n50[re.sf.n50$Quantile == 'Quant50'&
                           re.sf.n50$Year == 2022,], 
            cex=0.4) +
    geom_sf(data=re.sf.e50[re.sf.e50$Quantile == 'Quant50'&
                             re.sf.e50$Year == 2022,], 
            cex=0.4) +
    
    geom_sf(data=re.sf.n95[re.sf.n95$Quantile == 'Quant95',], 
            aes(col=Quantile), cex=0.4) +
    geom_sf(data=re.sf.e95[re.sf.e95$Quantile == 'Quant95',], 
            aes(col=Quantile), cex=0.4) +
    
    scale_color_discrete(labels=c('5%', '50%', '95%')) +
    guides(colour = guide_legend(override.aes = list(size=3))) +
    
    coord_sf(xlim=c(-71, -67),
             ylim=c(41.5, 45),
             crs="EPSG:4326") +
    labs(fill='NAFO\nUnit')
  
  
  