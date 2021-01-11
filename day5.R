#https://datacarpentry.org/r-raster-vector-geospatial/03-raster-reproject-in-r/
#https://datacarpentry.org/r-raster-vector-geospatial/
#https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r
#possible data: https://sedac.ciesin.columbia.edu/data/collection/gpw-v4/sets/browse
#3d : https://www.rayshader.com/
# https://gigamove.rz.rwth-aachen.de/d/id/NCnNvHPRa4hPpv?0&id=NCnNvHPRa4hPpv&id=NCnNvHPRa4hPpv&id=NCnNvHPRa4hPpv&id=NCnNvHPRa4hPpv
#mapping: https://bookdown.org/robinlovelace/geocompr/adv-map.html
# https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/#relief
#theme: https://ggplot2-book.org/polishing.html
# annotation https://rdrr.io/cran/ggspatial/src/R/annotation-scale.R

library(sf)
library(ggplot2)
df_aggr <- read.csv("data/day2_data_energy_prod_EU_2020-08-03_2020-08-09_aggctr.csv")
ctr <- st_read("data/ne_10m_admin_0_countries/ne_10m_admin_0_countries_ctr_code.gpkg")
relief <- raster("./data/SR_LR/SR_LR.tif")
relief_br <- brick("./data/SR_LR/SR_LR.tif")

ctr_df <- merge(ctr, df_aggr, by = "ctr_code")

for(i in 5) {
  ctr[ ,i] <- as.character(ctr[ ,i])
}

ggplot(ctr_df) + geom_sf()

#crop
box <- st_bbox(c(xmin = -30, ymin = 35, xmax = 33, ymax = 81), 
               crs = st_crs(ctr_df))
ctr_df_eur <- st_crop(ctr_df, box)
ggplot(ctr_df_eur) + geom_sf()

crop <- crop(relief_br, extent(ctr_df_eur))
ggplot() + ggR(crop, stretch = "hist", geom_raster = T, ggLayer = T)


st_crs(ctr_df) 
st_crs(3035)

#reproject / transfrom unprojected Lation (WGS584) to LAEA(ETR589) 
ctr_df_eur_laea <- st_transform(ctr_df_eur, st_crs(3035))
ggplot(ctr_df_eur_laea) + geom_sf()

crop_prj <- projectRaster(crop, crs = crs(ctr_df_eur_laea))
ggplot() + ggR(crop_prj, stretch = "hist", geom_raster = T, ggLayer = T)

relief_spdf <- as(crop_prj, "SpatialPixelsDataFrame")
relief_df <- as.data.frame(relief_spdf)

##############################################################
### with pipes ###########
#####################################
df_aggr <- read.csv("data/day2_data_energy_prod_EU_2020-08-03_2020-08-09_aggctr.csv")
ctr <- st_read("data/ne_10m_admin_0_countries/ne_10m_admin_0_countries_ctr_code.gpkg")
box <- st_bbox(c(xmin = -30, ymin = 35, xmax = 33, ymax = 81), 
               crs = st_crs(ctr_df))
#join crop transform in one go using the pipe 
ctr_df <- merge(ctr, df_aggr, by ="ctr_code") %>% 
  st_crop(box) %>% 
  st_transform(st_crs(3035))

#same thing 
ctr_df <- merge(ctr, df_aggr, by ="ctr_code")
ctr_df <- st_crop(ctr_df, box) 
ctr_df <- st_transform(ctr_df, st_crs(3035))

#same thing 
ctr_df <- st_transform(st_crop(merge(ctr, df_aggr, by = "ctr_code")), box)


##############################################
###ggplot 
##############################################

relief_crop <- crop(relief, ctr_df_eur_laea)
box <- st_bbox(c(xmin = -30, ymin = 35, xmax = 33, ymax = 81), 
               crs = st_crs(ctr_df))
r_crop <- st_crop(relief_br, box)

# plot production output 
ggplot() + geom_sf(data = ctr_df_eur_laea, #ctr_df 
                   aes(colour = ActualGenerationOutput), 
                   alpha = 0.6) +
  scale_fill_gradientn(colours = viridis::inferno(9))
# scale_colour_gradientn(colours = viridis::inferno(8)) #grenzen 
ggplot() + geom_sf(data = ctr_df_eur_laea, 
                   aes(fill = ActualGenerationOutput), 
                   alpha = 0.6) +
  scale_fill_gradientn(colours = viridis::inferno(9))

viridis::inferno(8, alpha = 0.8)[2:6]
cols <- colorRampPalette(viridis::inferno(8, alpha = 0.8)[2:6])
cols <- colorRampPalette(c("red","yellow","green"))
cols(10)

ggplot() + geom_sf(data = ctr_df_eur_laea, 
                   aes(fill = ActualGenerationOutput), 
                   alpha = 0.6) +
  scale_fill_gradientn(colours = cols(8), 
                       name = "Generated Engery [Gw]",
                       guide = guide_colorbar(
                         direction = "horizontal", 
                         barheight = unit(2,"mm"),
                         barwidth = unit(50, "mm"), 
                         title.position = "top", 
                         title.hjust = 0.9, 
                         title.vjust = 0.1
                         )) + 
  #guides() +
  labs(title = "Our nice map") + 
  theme_bw() +  # theme_dark() # theme_light() + 
  theme(legend.position = c(0.5, -0.1))


## more tasks to do 
# Modifying legend optics, labels, breaks
# Themes in ggplot2
# Cartographic elements using ggspatial
# Adding raster backgrounds
# Saving ggplots

ggplot() + theme_grey() +
  geom_raster(relief_df, 
              mapping = aes(x=x, y=y, alpha= layer), 
              show.legend = F) +
  scale_alpha(name = "", range = c(0.8, 0), guide = F) +
  geom_sf(data = ctr_df_eur_laea, 
                   aes(fill = ActualGenerationOutput), 
                   alpha = 0.7) +
  scale_fill_gradientn(colours = cols(8), 
                       name = "Generated Engery [Gw]",
                       guide = guide_colorbar(
                         direction = "horizontal", 
                         barheight = unit(2,"mm"),
                         barwidth = unit(40, "mm"), 
                         title.position = "top", 
                         title.hjust = 0.5, 
                         title.vjust = 0.5,),
                       limits=c(0,6000),
                       breaks = c(0,1000,2000,3000,4000,5000,6000)) + 
  #guides(shape = guide_legend(override.aes = list(size = 0.1)),
  #       color = guide_legend(override.aes = list(size = 0.1))) +
  labs(title = "Generated Energy in Europe in the 1st week of August 2020",
       subtitle = "by European power plants connected to the national energy grids",
       caption = "Data: ENTSO-E, Natural Earth, Author: Hyeonmin Kang \n Data: 2020-11-20; Projection: LAEA, ETRS89",
       x = "Longitude", y = "Latitude") + 
  #theme_gray() + 
  theme(legend.position = c(0.85, -0.08),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 7, hjust = 0.5), 
        plot.caption = element_text(size = 6, hjust = 0.5, color = "darkgray")) +  
  coord_sf(expand = F, clip = "off") +
  annotation_north_arrow(height = unit(0.8, "cm"), width = unit(0.8, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(-1.55, "cm")) +
  annotation_scale(pad_x = unit(1.5, "cm"), pad_y = unit(-1.3, "cm"), 
                   height = unit(0.15, "cm"), width_hint = 0.15) 

####
#
#gsave("map.pdf")
#ggsave("map_web.png", width = 6, height = 6, dpi = "screen")
###########################################################################

#breaks <- seq(100, max(ctr_df_eur_laea$ActualGenerationOutput), length.out = 6)
#breaks <- round(breaks/1000, digits = 0)*1000 
breaks <- c(100, seq(1000, 6000, length.out = 6))
labels <- as.character(breaks)
labels[1] <- 0
#labels <- as.character(breaks)

gg <- ggplot() + geom_sf(data = ctr_df_eur_laea, 
                   aes(fill = ActualGenerationOutput), 
                   alpha = 0.6) +
  scale_fill_gradientn(colours = cols(8), 
                       name = "Generated Engery [Gw]",
                       breaks = breaks, labels = labels, 
                       guide = guide_colorbar(
                         direction = "horizontal", 
                         barheight = unit(2,"mm"),
                         barwidth = unit(50, "mm"), 
                         title.position = "top", 
                         title.hjust = 0.9, 
                         title.vjust = 0.1
                       )) + 
  theme(legend.position = c(0.8, -0.07),
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  labs(title = "Generated Energy in Europe in the 1st week of August 2020", 
       subtitle = "by European power plants connected to the grid")

png("map2.png", width = 900, height = 900, res = 100)
print(gg)
dev.off()
