rm(list=ls())

# Load libraries and functions
library(tidyverse)
library(here)
library(sf)
library(units)
library(TMB)
library(VAST)
# Add unitless back as possible unit (removed in units package update Mar 2023)
#install_unit(symbol='unitless', def='unitless', name='unitless')
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

# Load prey data
preyorig <- read.csv(here('Data/Prey_Data/bft_preyV3.csv'))
head(preyorig)
preyorig$X <- NULL

# Filter prey data
prey <- preyorig %>% 
  drop_na(Lon) %>% 
  drop_na(Lat) %>% 
  filter(MONTH %in% c(6,7,8,9,10)) %>% 
  filter(YEAR == 2023) %>% 
  filter(COMNAME != 'HERRING, BLUEBACK')

# Convert to fish groups (herring, menhaden, mackerels)
fishcat <- data.frame(
  COMNAME=c('MACKEREL, ATLANTIC', 'MACKEREL, SPANISH', 'MACKEREL, NK',
            'MACKEREL, ATLANTIC CHUB', 'MACKEREL, KING', 'MACKEREL, FRIGATE',
            'MACKEREL, SNAKE, NK',
            'HERRING, ATLANTIC',
            'MENHADEN, ATLANTIC'),
  fish =c(rep('mackerel', 7),
          'herring',
          'menhaden')
)
prey <- left_join(prey, fishcat, by=c('COMNAME'))
prey$COMNAME <- NULL

# Make gear legible
gearcat <- data.frame(
  OBGEARCAT = c('GG', 'OT', 'PR', 'SD', 'PS', 'TT', 'PT', 'LL', 'CD',
                'BS', 'GP', 'OB', 'ST'),
  gear =c('gillnet', 'otter trawl', 'pair trawl', 'scallop dredge',
          'purse seine', 'twin trawl', 'pots traps', 'longline',
          'unknown',
          'beach set', 'unknown', 'obsolete', 'scallop trawl')
)
prey <- left_join(prey, gearcat, by=c('OBGEARCAT'))
prey$OBGEARCAT <- NULL

# Convert lbs to kgs
prey$wt_kgs <- prey$wt_lbs * 0.45359237

# Remove columns
prey <- dplyr::select(prey, -wt_lbs, -CATDISP)

# Add padding 0s where needed
prey$MONTH   <- str_pad(prey$MONTH, 2, "left", "0")
prey$TRIPID <- paste0(prey$YEAR, prey$MONTH, prey$Lat, prey$Lon, prey$gear)
prey$TRIPID <- as.numeric(as.factor(prey$TRIPID))
prey$HAULNUM <- 1

# Order
prey <- prey[with(prey, order(YEAR, MONTH, TRIPID, HAULNUM, fish)),]
rownames(prey) <- NULL
head(prey)

# We want to condense all prey biomass of a single family
# taken in a single tow, since we don't care about
# separating species in our general prey field model.
# Currently, rows are separated by species. Let's try to figure out if any 
# are from the same exact haul.
prey$id <- paste0(prey$fish,
                  '-',
                  prey$YEAR, 
                  prey$MONTH,
                  "_", 
                  prey$gear, "-", 
                  prey$TRIPID, "-",
                  prey$HAULNUM,
                  "_", 
                  str_pad(prey$Lat, 17, "right", "0"), 
                  str_pad(prey$Lon, 17, "right", "0")
)
prey$id <- as.factor(as.numeric(as.factor(prey$id)))
head(prey)
length(unique(prey$id))
nrow(prey)
# There are instances of this.

# Group and sum
prey <- prey %>% 
  group_by(TRIPID, HAULNUM, YEAR, MONTH, Lat, Lon, fish, gear, id) %>% 
  summarise(wt_kgs = sum(wt_kgs))
prey <- as.data.frame(prey)

# Remove unusual gears
prey <- prey %>% 
  filter(gear %in% c('gillnet', 'otter trawl', 'pair trawl'))

# Split by fish
mackerel <- prey %>% filter(fish == 'mackerel')
menhaden <- prey %>% filter(fish == 'menhaden')
herring <- prey %>% filter(fish == 'herring')

# Create alternate datasets for all three fish with 0 kgs
prey.mackerel <- prey %>% 
  mutate(fish = 'mackerel') %>% 
  mutate(wt_kgs = 0) %>% 
  filter(id %notin% mackerel$id)
prey.menhaden <- prey %>% 
  mutate(fish ='menhaden') %>% 
  mutate(wt_kgs = 0) %>% 
  filter(id %notin% menhaden$id)
prey.herring <- prey %>% 
  mutate(fish = 'herring') %>% 
  mutate(wt_kgs = 0) %>% 
  filter(id %notin% herring$id)

# Rebind
prey2 <- rbind(mackerel, menhaden, herring,
               prey.mackerel, prey.menhaden, prey.herring)

# Check nrow makes sense
nrow(prey2) == 3 * nrow(prey)

# Order
prey2 <- prey2[with(prey2, order(
  YEAR, MONTH, TRIPID, HAULNUM, gear, fish
)),]
rownames(prey2) <- NULL

# Remove intermediates
rm(herring, mackerel, menhaden, prey, prey.herring, prey.mackerel,
   prey.menhaden)

# Add shapefiles
nafo <- st_read(here('Data/GIS/NAFO_Continental_Shelf_10kmBuff.shp'))
nafo <- nafo[nafo$Region == 'US',]
closed <- st_read(here('Data/GIS/Gillnet_Trawl_Closed_Areas_WGS.shp'))

# Convert to sf
prey.sf <- st_as_sf(prey2, coords=c('Lon', 'Lat'), crs="EPSG:4326")

# Project
coast <- ecodata::coast
coast <- st_transform(coast, st_crs(prey.sf))
nafo <- st_transform(nafo, st_crs(prey.sf))
closed <- st_transform(closed, st_crs(prey.sf))

new_bb <- st_bbox(nafo)

# Remove points outside spatial domain
nafo <- st_make_valid(nafo)
prey.sf <- st_intersection(prey.sf, nafo)

# Plot
ggplot() +
  
  geom_sf(data=nafo, 
          fill=NA, col='gray30', alpha=0.4) +
  #ggnewscale::new_scale_fill() + 
  geom_sf(data=coast, fill='gray30', col=NA) +
  
  #ggnewscale::new_scale_color() +
  geom_sf(data=prey.sf[prey.sf$wt_kgs != 0,],
          aes(col=YEAR), cex=0.2) +
  #scale_color_viridis_d()+
  facet_wrap(vars(fish, gear)) +
  coord_sf(xlim=c(new_bb[1], new_bb[3]),
           ylim=c(new_bb[2], new_bb[4]))

# Back to dataframe
prey <- sfheaders::sf_to_df(prey.sf, fill=TRUE)
prey <- prey %>% 
  dplyr::select(-sfg_id, -Region, -point_id, -id) %>% 
  rename(lon = x) %>% 
  rename(lat = y) %>% 
  rename(nafo.unit = Label)

colnames(prey) <- tolower(colnames(prey))

# Save
write.csv(prey,
          here('Data/Prey_Data/species_sep_prey_2023.csv'),
          row.names = F)
