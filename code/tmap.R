library(tmap)
library(sf)
library(raster)

# set workspace----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read shapefiles
df1 <- st_read("../output/clusters_1.shp")
df1$month <- 1
df2 <- st_read("../output/clusters_2.shp")
df2$month <- 2

#bind together
df <- rbind(df1, df2)

#read rasters
r1 <- raster("../output/rho_1.tif")
raster::crs(r1) <- crs(df1)
r2 <- raster("../output/rho_2.tif")
raster::crs(r2) <- crs(df2)

#class breaks
breaks <- c(-1.5, -1.0, -.5, 0, .5, 1.0, 1.5)

#tm_arrange
tm1 <- tm_shape(r1) +
  tm_raster(breaks = breaks,
            palette = "brewer.oranges") +
  tm_shape(df1) +
  tm_borders(col = "black", lwd = 2) +
  tm_text("ID", size = 1)  + 
  tm_layout(frame = FALSE,
            legend.show = FALSE,
            fontface = "Helvetica")
tm1
    
tm2 <- tm_shape(r2) +
  tm_raster() +
  tm_shape(df2) +
  tm_polygons(legend.whow = FALSE) +
  tm_text("ID", size = 1/2)
tm2

tm <- tmap_arrange(tm1, tm2)
tm
# #facets
# tm_shape(df) +
#   tm_polygons() +
#   tm_text("ID", size = 1/2) +
#   tm_facets("month")
