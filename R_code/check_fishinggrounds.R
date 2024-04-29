rm(list=ls())

# Load libraries and functions
library(here)
library(tidyverse)
library(sf)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', linewidth=1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Create dataframe of state codes
statecodes <- data.frame(
  stcode = c(9,10,23,24,25,33,34,36,44,51),
  state = c('CT', 'DE', 'ME', 'MD', 'MA', 'NH', 'NJ', 'NY', 'RI', 'VA')
)

# Create dataframe of species size codes
speccode <- data.frame(
  prim = c(4673, 4677, 4678, 4676, 4679, 4671, 4670, 4672),
  size = c('young school', 'school', 'large school', 'small med',
           'large med', 'giant', 'unknown', 'school or large school')
)

# Load data
bft <- read.csv(here('Data/Clean/LPS_compiled_0222_bfttargeted_allsizes.csv'))

# Clean
bft <- dplyr::select(bft,
                     id, day, month, year, 
                     stcode, county, location, new_location,
                     latddmm, londdmm, 
                     tourn_name,
                     tracker, intcode, siteno, site_no, sitetype,
                     prim_op, ppstate, prim1, prim2, tourn, tcode,
                     lines, fhours, party, miles, depth, sst, catch, measur)

# Calculate decimal degrees from degree-minutes
bft$lon <- (as.numeric(substr(bft$londdmm, start=1, stop=2)) +
                     (as.numeric(substr(bft$londdmm, start=3, stop = 4)) / 60)) * (-1)
bft$lat <- (as.numeric(substr(bft$latddmm, start=1, stop=2)) +
                     (as.numeric(substr(bft$latddmm, start=3, stop = 4)) / 60))

bft <- merge(bft, statecodes, by=c('stcode'))

# Clean
locs <- dplyr::select(bft, 
                      state, county, year, new_location, lon, lat,
                      londdmm, latddmm)

# Clean
locs <- locs %>% 
  filter(!is.na(lat)) %>% 
  filter(!is.na(lon)) 

coast <- ecodata::coast

locs <- dplyr::select(locs, -state, -county, -year)
locs <- unique(locs)
locs <- locs[with(locs, order(lat, lon)),]
locs$bullshitid <- seq(1:nrow(locs))
rownames(locs) <- NULL

locs.sf <- st_as_sf(locs, coords=c('lon', 'lat'), crs="EPSG:4326")
locs.sf <- st_transform(locs.sf, st_crs(coast))

onland <- st_intersection(locs.sf, coast)
bads <- locs.sf[locs.sf$bullshitid %in% onland$bullshitid,]
inwater <- locs.sf[locs.sf$bullshitid %notin% onland$bullshitid,]

for(i in 1:nrow(inwater)){
  inwater[i,] <- st_buffer(inwater[i,], dist=2000)
}

inwater <- st_make_valid(inwater)

onespot <- as.data.frame(table(inwater$new_location))
onespot <- onespot[onespot$Freq > 1,]

ggplot() + 
  geom_sf(data=coast) +
  geom_sf(data=inwater, aes(fill=new_location)) +
  coord_sf(xlim=c(-76, -67),
           ylim=c(35, 46)) +
  theme(legend.position = 'n')

hulls <- inwater %>% 
  filter(new_location %in% onespot$Var1) %>% 
  group_by(new_location) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  st_make_valid() %>% 
  st_convex_hull()

ggplot() + 
  geom_sf(data=coast) +
  geom_sf(data=hulls, aes(fill=new_location)) +
  coord_sf(xlim=c(-76, -67),
           ylim=c(35, 46)) +
  theme(legend.position = 'n')

mults <- locs[locs$new_location %in% hulls$new_location,]

mults.sf <- st_as_sf(mults, coords=c('lon', 'lat'), crs="EPSG:4326")
mults.sf <- st_transform(mults.sf, st_crs(coast))

for(i in 191:
    #length(unique(mults.sf$new_location))
    200
    ){
  nl <- unique(mults.sf$new_location)[i]
  
  print(
    ggplot() +
      geom_sf(data=coast) +
      geom_sf(data=mults.sf[mults.sf$new_location == nl,])+
      coord_sf(xlim=c(-76, -67),
               ylim=c(35, 46)) +
      theme(legend.position = 'n') +
      ggtitle(paste0(nl))
  )
  
}


hulls$area <- st_area(hulls)
library(FishStatsUtils)
hulls$area <- strip_units(hulls$area) 

toobig <- hulls[log(hulls$area) >=
                  quantile(log(hulls$area, 0.25)),]
