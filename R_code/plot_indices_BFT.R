rm(list=ls())
library(tidyverse)
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

ind <- read.csv(here('VAST_runs/tuna8/Index.csv'))

head(ind)

ind$upper <- ind$Estimate + ind$Std..Error.for.Estimate
ind$lower <- ind$Estimate - ind$Std..Error.for.Estimate

for(i in 1:nrow(ind)){
  if(ind$lower[i]< 0){
    ind$lower[i] <- 0
  }
}

ind$Stratum[ind$Stratum == 'Stratum_1'] <- 'Canada'
ind$Stratum[ind$Stratum == 'Stratum_2'] <- 'US'
ind$Stratum[ind$Stratum == 'Stratum_3'] <- 'Both'

head(ind)

library(ggplot2)

ind.p <- ggplot() +
  geom_line(data=ind, 
            aes(x=Time, y=Estimate, col=Stratum)) +
  geom_ribbon(data=ind, alpha=0.2,
              aes(x=Time, ymin=lower, ymax=upper, fill=Stratum))+
  facet_wrap(vars(Category),
             scales = "free_y") +
  labs(x='Year', 
       y='Estimate') +
  theme(strip.text.x = element_text(size=12))

ggsave(ind.p,
       filename=paste0(here(), "/Plot_Output/index.png"),
       width = 8, height = 4, units='in')
