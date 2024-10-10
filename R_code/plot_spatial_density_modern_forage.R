rm(list=ls())          

# Load packages
library(here)
library(tidyverse)
library(VAST)
library(sf)

# Install unitless
install_unit(symbol='unitless', def='unitless', name='unitless')

# Load VAST fit data
load(here("VAST_runs/herring8_monthly/herring_8_monthly.Rdata"))
rm(list=setdiff(ls(), c("fit", "%notin%")))
panel_labels <- data.frame(
  year = c(rep(seq(2002,2022,1), 5)),
  month = c(rep('June', 21),
            rep('July', 21),
            rep('August', 21),
            rep('September', 21),
            rep('October', 21))
)
panel_labels <- panel_labels %>% 
  mutate(month = factor(month, levels=c('June', 'July', 'August', 
                                        'September', 'October')),
         year = as.numeric(year)) %>% 
panel_labels <- panel_labels[with(panel_labels,
                                  order(year, month)),]
panel_labels$combo <- paste0(panel_labels$month, ' ', panel_labels$year)
panel_labels <- panel_labels$combo
panel_labels <- panel_labels[panel_labels != 'June 2020' &
                               panel_labels != 'July 2020']

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

# Extract Data
Y_gt = fit$Report$D_gct[,,]
map_list = make_map_info(Region = fit$extrapolation_list$Area_km2_x,
                         spatial_list = fit$spatial_list,
                         Extrapolation_List = fit$extrapolation_list)
file_name = "density"
working_dir = here('VAST_runs/herring8_monthly')
setwd(working_dir)
fun = mean

Y_gt2 <- Y_gt[,1,] + Y_gt[,2,] + Y_gt[,3,]
rm(Y_gt)

zlim = range(Y_gt2, na.rm = TRUE)
MapSizeRatio = map_list$MapSizeRatio
Y_gt2 = Y_gt2[map_list$PlotDF[which(map_list$PlotDF[, "Include"] > 
                                    0), "x2i"], , drop = FALSE]
n_cells = nrow(Y_gt2)

# Call location list
loc_g = map_list$PlotDF[which(map_list$PlotDF[, "Include"] > 
                                0), c("Lon", "Lat")]
# Call projections
CRS_orig = sp::CRS("+proj=longlat")
CRS_proj = sp::CRS("EPSG:26919")

# Call other spatial data
coast <- st_transform(ecodata::coast, CRS_proj)
regions <- st_read(here("Data/GIS/NAFO_5Y.shp"), quiet=T)
regions <- st_transform(regions, CRS_proj)
regions <- st_make_valid(regions)

# Total spatial density
Y_total <- log(strip_units(Y_gt2))
Y_total <- as.data.frame(Y_total)
Y_total <- as_tibble(Y_total)
dimnames(Y_total)[[2]] <- panel_labels
Zlim = range(Y_total)

Y_total.june <- Y_total %>% dplyr::select(contains('June'))
Y_total.july <- Y_total %>% dplyr::select(contains('July'))
Y_total.aug <- Y_total %>% dplyr::select(contains('August'))
Y_total.sept <- Y_total %>% dplyr::select(contains('September'))
Y_total.oct <- Y_total %>% dplyr::select(contains('October'))

Y_total.june <- rowSums(Y_total.june)
Y_total.july <- rowSums(Y_total.july)
Y_total.aug <- rowSums(Y_total.aug)
Y_total.sept <- rowSums(Y_total.sept)
Y_total.oct <- rowSums(Y_total.oct)
Y_total = rowSums(Y_total)

Y_t <- matrix(Y_total, ncol=1)

Points_orig = sp::SpatialPointsDataFrame(coords = loc_g, 
                                         data = data.frame(y = Y_t[,1]), 
                                         proj4string = CRS_orig)
Points_LongLat = sp::spTransform(Points_orig, sp::CRS("+proj=longlat"))
Points_proj = sp::spTransform(Points_orig, CRS_proj)
xlim = Points_proj@bbox[1, ]
ylim = Points_proj@bbox[2, ]

cell.size = mean(diff(Points_proj@bbox[1, ]), diff(Points_proj@bbox[2, 
]))/floor(sqrt(n_cells))
Points_sf = sf::st_as_sf(Points_proj)
grid = sf::st_make_grid(Points_sf, cellsize = cell.size)
grid_i = sf::st_intersects(Points_sf, grid)
grid = sf::st_sf(grid, y = tapply(Points_sf$y, INDEX = factor(as.numeric(grid_i), 
                                                              levels = 1:length(grid)), FUN = mean, na.rm = TRUE))

grid2 <- st_intersection(grid, regions)
grid2$y <- scale(grid2$y)
Zlim = range(grid2$y, na.rm=TRUE)

tspd.s <- ggplot() +
  geom_sf(data=grid2, 
          aes(fill=y, col=y)) +
  scale_fill_viridis_c(option='viridis', alpha=0.8, direction=1,
                       na.value = 'transparent') +
  scale_color_viridis_c(option='viridis', alpha=0.8, direction=1,
                        na.value = 'transparent') +
  geom_sf(data=coast, fill='gray', col='darkgray') +

  geom_sf(data=regions, fill=NA, col='black', lwd=0.1) +
  
  coord_sf(xlim=c(-71, -67),
           ylim=c(41.75, 45),
           crs="EPSG:4326") +
  labs(col='Scaled\nlog(Abund.)', fill='Scaled\nlog(Abund.)') +
  theme(legend.position = 'right',
        axis.text.x = element_text(size=8, angle=20),
        axis.text.y = element_text(size=8),
        strip.text.x=element_text(margin=margin(0.1,0,0.1,0, "cm")),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8))
tspd.s

ggsave(plot=tspd.s,
       filename='summed_spatial_density_forage.png',
       width = 6.5, height = 4.25, units = 'in')
