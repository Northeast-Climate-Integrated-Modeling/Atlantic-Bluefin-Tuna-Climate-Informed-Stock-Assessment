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
                panel.border = element_rect(color='black', size=1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Load prey data
preyorig <- read.csv(here('Data/Prey_Data/bft_preyV2.csv'))
head(preyorig)
preyorig$X <- NULL

# Filter prey data
prey <- preyorig %>% 
  drop_na(Lon) %>% 
  drop_na(Lat) %>% 
  filter(MONTH %in% c(6,7,8,9,10)) %>% 
  filter(YEAR >= 2002 & YEAR <= 2022)

# Convert lbs to kgs
prey$wt_kgs <- prey$wt_lbs * 0.45359237

# Remove columns
prey <- dplyr::select(prey, -wt_lbs, -CATDISP)

# Add padding 0s where needed
prey$MONTH   <- str_pad(prey$MONTH, 2, "left", "0")
prey$HAULNUM <- str_pad(prey$HAULNUM, 3, "left", "0")

# Order
prey <- prey[with(prey, order(YEAR, MONTH, TRIPID, HAULNUM, COMNAME)),]
rownames(prey) <- NULL
head(prey)

# We want to condense all prey biomass taken in a single tow, since we don't care about
# separating species in our general prey field model.
# Currently, rows are separated by species. Let's try to figure out if any are from the 
# same exact haul.
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
head(prey)
length(unique(prey$id))
nrow(prey)

# So it looks like there are some hauls that caught multiple species we care about.
# Create table of ID values, remove instances in which ID is unique
idtab <- as.data.frame(table(prey$id))
idtab <- subset(idtab, Freq > 1)
idtab <- idtab[with(idtab, order(Freq)),]
head(idtab)

# Create table of id vs. common name of species, remove rows that do not have their ID
# also within the duplicate ID table
spectab <- as.data.frame(table(prey$id, prey$COMNAME))
spectab <- subset(spectab, Freq != 0)
spectab <- spectab[spectab$Var1 %in% idtab$Var1,]

# Create subset of prey dataframe in which it looks like 2 or more hauls are noted
preycheck <- prey[prey$id %in% spectab$Var1,]

# Remove these observations from the overall prey dataframe (will be added back as combo hauls)
prey <- prey[prey$id %notin% preycheck$id,]

# Transform to list, make sure there are no list items with only one row (1 species obs per haul)
speclist <- split(preycheck, f=preycheck$id)
speclist <- Filter(function(dt) nrow(dt) > 1, speclist)
# If there are two or more unique species associated with an ID, combine.
for(i in 1:length(speclist)){
  intab <- table(speclist[[i]]$COMNAME)
  tablen <- length(intab)
  tabvals <- as.numeric(intab)
  if(tablen !=1 & all(tabvals == 1)){
    speclist[[i]]$wt_kgs <- sum(speclist[[i]]$wt_kgs)
    speclist[[i]]$COMNAME[1] <- 'combo'
    speclist[[i]] <- speclist[[i]][1,]
  }
  
  rm(intab, tablen, tabvals)
}
altlist.1 <- Filter(function(dt) nrow(dt) == 1, speclist) 
altlist.1 <- do.call(rbind, altlist.1)
rownames(altlist.1) <- NULL

# Add fixed results back to prey dataframe
prey <- rbind(prey, altlist.1)
prey <- prey[with(prey, order(YEAR, MONTH, TRIPID, HAULNUM, COMNAME)),]
rownames(prey) <- NULL
head(prey)

# Remove fixed results from preycheck
preycheck <- preycheck[preycheck$id %notin% prey$id,]
# Still a few weird things to look at.
# All the remaining rows have multiple entries for the same species within the same ID.
# There's really nothing I can do but assume it was some sort of entry error.
# Re start from the beginning, add all biomass together from same ID.

rm(preycheck, prey, altlist.1, speclist, spectab, idtab, i)

# Filter prey data
prey <- preyorig %>% 
  drop_na(Lon) %>% 
  drop_na(Lat) %>% 
  filter(MONTH %in% c(6,7,8,9,10)) %>% 
  filter(YEAR >= 2002 & YEAR <= 2022)

# Convert lbs to kgs
prey$wt_kgs <- prey$wt_lbs * 0.45359237

# Remove columns
prey <- dplyr::select(prey, -wt_lbs, -CATDISP)

# Add padding 0s where needed
prey$MONTH   <- str_pad(prey$MONTH, 2, "left", "0")
prey$HAULNUM <- str_pad(prey$HAULNUM, 3, "left", "0")

# Create ID
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

# Split into list-based ID
idlist <- split(prey, f=prey$id)

# Combine all prey biomass for each haul, based on ID
for(i in 1:length(idlist)){
  if(nrow(idlist[[i]]) > 1){
    idlist[[i]]$wt_kgs <- sum(idlist[[i]]$wt_kgs)
    if(length(table(idlist[[i]]$COMNAME)) > 1){
      idlist[[i]]$COMNAME <- 'COMBO'
    }
    
    idlist[[i]] <- idlist[[i]][1,]
  }
}
prey2 <- do.call(rbind, idlist)
prey2 <- prey2[with(prey2, order(YEAR, MONTH, TRIPID, HAULNUM, COMNAME)),]
rownames(prey2) <- NULL
head(prey2)
table(prey2$COMNAME)
length(unique(prey$id)) == nrow(prey2)
# Great. Moving on.

prey <- prey2; rm(idlist, prey2, i)

# Add row column to do polygon cutting
prey$row <- seq(1, nrow(prey))

# Convert to sf
prey.sf <- st_as_sf(prey, coords=c('Lon', 'Lat'))
st_crs(prey.sf) <- 'EPSG:4326'

# Remove original dataframe- will convert sf results back to df
rm(prey)

# Cut to tuna polygon
nwat <- st_read(here('Data/GIS/New_Extents2/US_120km_Buffer.shp'))
nwat <- st_transform(nwat, st_crs(prey.sf))
nwat <- st_make_valid(nwat)
prey.sf <- st_intersection(prey.sf, nwat)

# Remove locations on land
coast <- ecodata::coast
coast <- st_transform(coast, st_crs(prey.sf))
# onland <- st_intersection(prey.sf, coast)
# prey.sf <- prey.sf[prey.sf$row %notin% onland$row,]
# prey.sf$row <- NULL

# Plot to see locations
# Add tuna data
dat <- read.csv(here('Data/Clean/BFT_BothCountries_BothSizes_VAST.csv'))
dat <- dat[dat$location == 'us',]
dat <- dat[dat$Size == 'Large',]
dat.sf <- st_as_sf(dat, coords=c('lon', 'lat'))
st_crs(dat.sf) <- "EPSG:4326"
dat.sf <- st_intersection(dat.sf, nwat)


ggplot() +
  geom_sf(data=coast, fill='gray')+
  geom_sf(data=nwat, fill='lightgreen') +
  geom_sf(data=prey.sf, aes(col=YEAR)) +
  #geom_sf(data=dat.sf, col='red', pch=19, cex=0.25) +
  coord_sf(xlim=c(-76, -68),
           ylim=c(38, 45))

# Solid coverage. Slightly less in the south. Not sure what else to add.
# Convert back to df
prey <- sfheaders::sf_to_df(prey.sf, fill=T)
colnames(prey) <- tolower(colnames(prey))
colnames(prey)[8] <- 'ID'
prey <- prey %>% 
  dplyr::select(ID, tripid, haulnum, year, month, obgearcat, comname, wt_kgs,
                strata, x, y) %>% 
  rename(lat= y) %>% 
  rename(lon=x)

head(prey)

# Save
write.csv(prey, row.names = F, here('Data/Prey_Data/Clean_prey_data_0222.csv'))
