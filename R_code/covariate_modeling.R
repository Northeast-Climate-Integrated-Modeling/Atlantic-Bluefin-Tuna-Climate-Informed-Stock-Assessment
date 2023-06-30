rm(list=ls())

# Load libraries and functions
library(here)
library(tidyverse)
library(sf)
library(beepr)
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

# Load data
dat <- read.csv(here('Data/Clean/BFT_US_catch_VASTdata.csv'))

# Filter observations with no prey data
dat <- dat %>% 
  drop_na(prey)

# View
head(dat)
summary(dat)

# Pull survey values
survs <- dplyr::select(dat,
                       Size_class, catch, fhours, year, lon, lat)
colnames(survs) <- c('Category', 'Response_variable', 'Effort',
                     'Year', 'Lon', 'Lat')
survs$Category <- factor(survs$Category,
                         levels=c('small', 'large'))
survs$Category <- as.numeric(survs$Category) - 1
#survs$Response_variable <- as_units(as.numeric(survs$Response_variable), 'counts')
#survs$Effort <- as_units(survs$Effort, 'km^2')
survs$CPUE <- as_units(survs$Response_variable / survs$Effort, 'unitless')
survs$effort.unitless <- as_units(1, 'unitless')
str(survs)

# Pull covariate values
covars <- dplyr::select(dat,
                        year, lon, lat, sst, depth, slp, nao, amo, prey)
colnames(covars) <- c('Year', 'Lon', 'Lat', 'sst', 'depth', 'slp', 'nao', 'amo', 'prey')

# Scale covariate values
scaled.covars <- covars[,4:ncol(covars)] %>% 
  mutate(across(where(is.numeric), scale))
scaled.covars <- cbind(covars[,1:3], scaled.covars)
summary(scaled.covars)
scaled.covars <- data.frame(
  Lon         = as.numeric(scaled.covars$Lon),
  Lat         = as.numeric(scaled.covars$Lat),
  Year        = as.numeric(scaled.covars$Year),
  sst         = as.numeric(scaled.covars$sst),
  depth       = as.numeric(scaled.covars$depth),
  slp         = as.numeric(scaled.covars$slp),
  nao         = as.numeric(scaled.covars$nao),
  amo         = as.numeric(scaled.covars$amo), 
  prey        = as.numeric(scaled.covars$prey)
)
str(scaled.covars)

total <- merge(survs, scaled.covars, by=c('Year', 'Lon', 'Lat'))

library(splines)
summary(fm1 <- lm(Response_variable ~ 
                    bs(sst, degree = 3, intercept = FALSE) +
                    bs(depth, degree = 3, intercept = FALSE) +
                    bs(slp, degree = 3, intercept = FALSE) +
                    bs(nao, degree = 3, intercept = FALSE) +
                    bs(amo, degree = 3, intercept = FALSE) +
                    bs(prey, degree = 3, intercept = FALSE), 
                  data = total[total$Category == 0,]))
