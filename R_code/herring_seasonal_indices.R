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

# Load data
ind <- read.csv(here('VAST_runs/herring_graham/Index.csv'))
vastdat <- load(here('VAST_runs/herring_graham/herring1.Rdata'))
strata_order <- fit$settings$strata.limits
strata_order$Stratum <- c('Stratum_1', 'Stratum_2', 'Stratum_3',
                          'Stratum_4', 'Stratum_5')
ind <- left_join(ind, strata_order, by=c('Stratum'))


# Remove intermediates
rm(list=setdiff(ls(), c("ind", "fit", "%notin%")))

ind$upper <- ind$Estimate + ind$Std..Error.for.Estimate
ind$lower <- ind$Estimate - ind$Std..Error.for.Estimate

ind$Estimate.ln <- log(ind$Estimate)
ind$upper.ln <- ind$Estimate.ln + ind$Std..Error.for.ln.Estimate.
ind$lower.ln <- ind$Estimate.ln - ind$Std..Error.for.ln.Estimate.

ind <- ind %>% 
  separate(Time, into=c('Year', 'Season'), sep=' ')

ind$Season[ind$Season == 'Jan-Jun'] <- 'Winter-Spring Feeding'
ind$Season[ind$Season == 'Jul-Dec'] <- 'Summer Feeding-Spawning'

ind <- ind %>% 
  mutate_at(c('Year'), as.numeric) %>% 
  mutate_at(c('Season','Stratum', 'STRATA'), as.factor)

ind$Season <- factor(ind$Season,
                     levels=c('Winter-Spring Feeding',
                              'Summer Feeding-Spawning'))

ind.p <- ggplot() +
  geom_line(data=ind[ind$STRATA %notin% c('ALL'),], 
            aes(x=Year, y=Estimate.ln, col=STRATA)) +
  geom_ribbon(data=ind[ind$STRATA %notin% c('ALL'),], 
              alpha=0.2,
              aes(x=Year, ymin=lower.ln, ymax=upper.ln, fill=STRATA))+
  facet_wrap(vars(Season),
             nrow=2) +
  labs(x='Year', 
       y='ln(Estimate)',
       col='Stratum', fill='Stratum') +
  theme(strip.text.x = element_text(size=12))

ind.p

ggsave(ind.p,
       filename=paste0(here('VAST_runs/herring_graham'), "/index.png"),
       width = 8, height = 4, units='in')
