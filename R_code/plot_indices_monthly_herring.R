rm(list=ls())
library(tidyverse)
library(here)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', linewidth =1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

ind <- read.csv(here('VAST_runs/herring_graham/Index.csv'))
vastdat <- load(here('VAST_runs/herring_graham/herring1.Rdata'))
strata_order <- fit$settings$strata.limits

year.labs <- (rep(seq(1989, 2022),nrow(strata_order)))
year.labs <- as.data.frame(year.labs[order(year.labs)])
colnames(year.labs) <- 'year'
year.labs$month <- rep(c('Jun', 'Jul', 'Aug', 'Sep', 'Oct'), 21)
year.labs$combo <- paste0(year.labs$month, '-1-', year.labs$year)
year.labs <- year.labs[
  year.labs$combo %notin%
    c('Jun-1-2020', 'Jul-1-2020'),
]
year.labs$Time <- seq(1:nrow(year.labs))

head(ind)

ind$upper <- ind$Estimate + ind$Std..Error.for.Estimate
ind$lower <- ind$Estimate - ind$Std..Error.for.Estimate

ind <- left_join(ind, year.labs, by=c('Time'))

for(i in 1:nrow(ind)){
  if(ind$lower[i]< 0){
    ind$lower[i] <- 0
  }
}

#ind$Stratum[ind$Stratum == 'Stratum_1'] <- 'Canada'
#ind$Stratum[ind$Stratum == 'Stratum_2'] <- 'US'
#ind$Stratum[ind$Stratum == 'Stratum_3'] <- 'Both'

head(ind)

library(ggplot2)

ind$month <- factor(ind$month,
                    levels = c('Jun', 'Jul', 'Aug', 'Sep', 'Oct'))
ind$combo <- as.Date(ind$combo,
                     format="%b-%d-%Y")
missing <- data.frame(
  Category = rep(c('herring', 'mackerel', 'menhaden'),2),
  Time = rep(NA, 6),
  Stratum=rep('Stratum_1', 6),
  Units = rep(NA, 6),
  Estimate=rep(NA, 6),
  Std..Error.for.Estimate = rep(NA, 6),
  Std..Error.for.ln.Estimate.=rep(NA, 6),
  upper=rep(NA, 6),
  lower=rep(NA, 6),
  year = rep(2020, 6),
  month=c(rep('Jun', 3), rep('Jul', 3))
)
missing$combo <- as.Date(
  paste0(missing$month, '-1-', missing$year),
  format="%b-%d-%Y"
)

ind <- rbind(ind, missing)
ind <- ind[with(ind, order(combo, Category)),]

ind.p <- ggplot() +
  geom_line(data=ind, 
            aes(x=Time, y=Estimate)) +
  geom_ribbon(data=ind, 
              alpha=0.2,
              aes(x=Time, ymin=lower, ymax=upper))+
  facet_wrap(vars(Category),
             scales = "free_y",
             nrow=3) +
  labs(x='Year', 
       y='Estimate') +
  theme(strip.text.x = element_text(size=12))

ind.p +  scale_x_continuous(breaks=as.numeric(unique(ind$Time[ind$month == 'Jun'])),
                         labels=as.character(unique(ind$year[ind$month == 'Jun'])))

ggsave(ind.p,
       filename=paste0(here(), "/Plot_Output/index_usonly.png"),
       width = 8, height = 4, units='in')
