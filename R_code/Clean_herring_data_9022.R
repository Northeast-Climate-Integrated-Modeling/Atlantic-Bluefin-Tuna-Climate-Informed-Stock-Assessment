rm(list=ls())

# Load libraries and functions
library(tidyverse)
library(here)
library(sf)

# Add unitless back as possible unit (removed in units package update Mar 2023)
#install_unit(symbol='unitless', def='unitless', name='unitless')
# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))
# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', linewidth=1, 
                                            fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Load prey data
preyorig <- read.csv(here('Data/Prey_Data/bft_preyV2.csv'))
preyorig$X <- NULL

# Load explanatory data
herring.area.names <- data.frame(
  area = c('Herring Area 1A', 'Herring Area 1B', 'Herring Area 2',
             'Herring Area 3'),
  strata = c('GOM Inshore', 'GOM Offshore', 'SNE', 'GB')
)

gear.types <- data.frame(
  obgearcat = c('GG', 'LL', 'OB', 'OT', 'PR', 
                'PS', 'PT', 'SD', 'ST', 
                'TT'),
  gear = c('gillnet', 'longline', 'obsolete', 'otter trawl', 'pair trawl',
           'purse seine', 'pots traps', 'scallop dredge', 'scallop trawl',
           'twin trawl')
)

# Filter prey data
prey <- preyorig %>% 
  drop_na(Lon) %>%                       # Need spatial info
  drop_na(Lat) %>% 
  filter(OBGEARCAT == 'OT') %>%          # Otter trawl only
  filter(YEAR >= 1989 & YEAR <= 2022)    # 1989 - 2022 seasons (complete)

# Separate herring and other fish
other <- prey[prey$COMNAME != 'HERRING, ATLANTIC',]
prey <- prey[prey$COMNAME == 'HERRING, ATLANTIC',]

# Assign seasons
# Summer feeding and spawning
# Fishery active in GoM and Georges Bank
other$season[as.numeric(other$MONTH) %in% c(7, 8, 9, 10, 11, 12)] <- 'B.SFS'
prey$season[as.numeric(prey$MONTH) %in% c(7, 8, 9, 10, 11, 12)] <- 'B.SFS'
# Winter and spring feeding
# Fishery active in SNE
other$season[as.numeric(other$MONTH) %in% c(1, 2, 3, 4, 5, 6)] <- 'A.WSF'
prey$season[as.numeric(prey$MONTH) %in% c(1, 2, 3, 4, 5, 6)] <- 'A.WSF'

# Convert lbs to kgs
other$wt_kgs <- other$wt_lbs * 0.45359237
prey$wt_kgs <- prey$wt_lbs * 0.45359237

# Remove columns
other <- dplyr::select(other, -wt_lbs, -CATDISP)
prey <- dplyr::select(prey, -wt_lbs, -CATDISP)

# Add padding 0s where needed
other$MONTH   <- str_pad(other$MONTH, 2, "left", "0")
other$HAULNUM <- str_pad(other$HAULNUM, 3, "left", "0")
prey$MONTH   <- str_pad(prey$MONTH, 2, "left", "0")
prey$HAULNUM <- str_pad(prey$HAULNUM, 3, "left", "0")

# Order
other <- other[with(other, order(YEAR, MONTH, TRIPID, HAULNUM, COMNAME)),]
rownames(other) <- NULL
prey <- prey[with(prey, order(YEAR, MONTH, TRIPID, HAULNUM, COMNAME)),]
rownames(prey) <- NULL

# We want to condense all prey biomass taken in a single tow, since we don't care about
# separating species in our general prey field model.
# Currently, rows are separated by species. Let's try to figure out if any are from the 
# same exact haul.
other$id <- paste0(other$YEAR, 
                   other$MONTH,
                  "_", 
                  other$OBGEARCAT, "-", 
                  other$TRIPID, "-",
                  other$HAULNUM,
                  "_", 
                  str_pad(other$Lat, 17, "right", "0"), 
                  str_pad(other$Lon, 17, "right", "0")
                  )
prey$id <- paste0(prey$YEAR, 
                  prey$MONTH,
                  "_", 
                  prey$OBGEARCAT, "-", 
                  prey$TRIPID, "-",
                  prey$HAULNUM,
                  "_", 
                  str_pad(prey$Lat, 17, "right", "0"), 
                  str_pad(prey$Lon, 17, "right", "0")
)

# So it looks like there are some hauls that list herring multiple times
# Split into list-based ID
idlist <- split(prey, f=prey$id)

# Combine all prey biomass for each haul, based on ID
for(i in 1:length(idlist)){
  #print(i)
  if(nrow(idlist[[i]]) > 1){
    idlist[[i]]$wt_kgs <- sum(idlist[[i]]$wt_kgs)
    
    idlist[[i]] <- idlist[[i]][1,]
  }
}
prey2 <- do.call(rbind, idlist)
prey2 <- prey2[with(prey2, order(YEAR, MONTH, TRIPID, HAULNUM, COMNAME)),]
rownames(prey2) <- NULL
length(unique(prey$id)) == nrow(prey2)
# Great. Moving on.

prey <- prey2
rm(idlist, prey2, i)

# We also need to know where otter trawling occurred and didn't get herring
noherring <- other[other$id %notin% prey$id,]
noherring$COMNAME <- 'HERRING, ATLANTIC'
noherring$wt_kgs <- 0
noherring <- unique(noherring)

# Combine
prey2 <- rbind(prey, noherring)

# Remove extraneous
prey <- prey2
rm(idtab, noherring, other, prey2)

# Add row column to do polygon cutting
prey$row <- seq(1, nrow(prey))

# Convert to sf
prey.sf <- st_as_sf(prey, coords=c('Lon', 'Lat'))
st_crs(prey.sf) <- 'EPSG:4326'

# Remove original dataframe- will convert sf results back to df
rm(prey)

# Cut to Herring management areas polygon
nwat <- st_read(here('Data/GIS/Prey_Management/Herring_Management_Areas.shp'))
nwat <- st_transform(nwat, st_crs(prey.sf))
nwat <- st_make_valid(nwat)
colnames(nwat)[1] <- 'area'

# Loss of 54 samples from outside spatial polygon
prey.sf <- st_intersection(prey.sf, nwat)

# Check seasonal occurrence in each area for funsies
seastab <- as.data.frame(table(prey.sf$MONTH[prey.sf$wt_kgs != 0], 
                               prey.sf$area[prey.sf$wt_kgs !=0]))
colnames(seastab) <- c('month', 'area','Freq')
# Summer feeding and spawning
# Fishery active in GoM and Georges Bank
seastab$season[as.numeric(seastab$month) %in% c(7, 8, 9, 10, 11, 12)] <- 'B.SFS'
# Winter and spring feeding
# Fishery active in SNE
seastab$season[as.numeric(seastab$month) %in% c(1, 2, 3, 4, 5, 6)] <- 'A.WSF'
# Plot
ggplot(data=seastab) +
  geom_col(aes(x=month, y=Freq, fill=season)) +
  facet_wrap(vars(area))

# Convert back to df
prey <- sfheaders::sf_to_df(prey.sf, fill=T)
colnames(prey) <- tolower(colnames(prey))
prey <- prey %>% 
  dplyr::select(id, tripid, haulnum, year, month, obgearcat, -comname, wt_kgs,
                area, x, y) %>% 
  rename(lat= y) %>% 
  rename(lon=x)

head(prey)

# Make easier to understand
prey <- left_join(prey, gear.types)
prey <- left_join(prey, herring.area.names)
prey <- dplyr::select(prey, -area, -obgearcat)

# Order and make ID column shorter
prey <- prey[with(prey, order(year, month, tripid, haulnum)),]
rownames(prey) <- NULL
prey$id <- as.numeric(as.factor(prey$id))

# Save
write.csv(prey, row.names = F, here('Data/Prey_Data/herring_data_9022.csv'))
