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
df3 <- st_read("../output/clusters_3.shp")
df3$month <- 3

df4 <- st_read("../output/clusters_4.shp")
df4$month <- 4
df5 <- st_read("../output/clusters_5.shp")
df5$month <- 5
df6 <- st_read("../output/clusters_6.shp")
df6$month <- 6

df7 <- st_read("../output/clusters_7.shp")
df7$month <- 7
df8 <- st_read("../output/clusters_8.shp")
df8$month <- 8
df9 <- st_read("../output/clusters_9.shp")
df9$month <- 9

df10 <- st_read("../output/clusters_10.shp")
df10$month <- 10
df11 <- st_read("../output/clusters_11.shp")
df11$month <- 11
df12 <- st_read("../output/clusters_12.shp")
df12$month <- 12

#bind together
df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

#read rasters
r1 <- raster("../output/rho_1.tif")
raster::crs(r1) <- crs(df1)
r2 <- raster("../output/rho_2.tif")
raster::crs(r2) <- crs(df2)
r3 <- raster("../output/rho_3.tif")
raster::crs(r3) <- crs(df3)

r4 <- raster("../output/rho_4.tif")
raster::crs(r4) <- crs(df4)
r5 <- raster("../output/rho_5.tif")
raster::crs(r5) <- crs(df5)
r6 <- raster("../output/rho_6.tif")
raster::crs(r6) <- crs(df6)

r7 <- raster("../output/rho_7.tif")
raster::crs(r7) <- crs(df7)
r8 <- raster("../output/rho_8.tif")
raster::crs(r8) <- crs(df8)
r9 <- raster("../output/rho_9.tif")
raster::crs(r9) <- crs(df9)

r10 <- raster("../output/rho_10.tif")
raster::crs(r10) <- crs(df10)
r11 <- raster("../output/rho_11.tif")
raster::crs(r11) <- crs(df11)
r12 <- raster("../output/rho_12.tif")
raster::crs(r12) <- crs(df12)

#class breaks
# breaks <- c(-1.5, -1.0, -.5, 0, .5, 1.0, 1.5)
breaks <- c(-1.5, -.9, -.3, .3, .9, 1.5)

#legend
leg <- tm_shape(r1) +
  tm_raster(col.scale = tm_scale(breaks = breaks,
                                 values = "brewer.oranges")) + 
  tm_layout(legend.only = TRUE, 
            legend.frame = FALSE)
leg

#tm_arrange
tm1 <- tm_shape(r1) +
  tm_raster(col.scale = tm_scale(breaks = breaks,
            values = "brewer.oranges")) +
  tm_shape(df1) +
  tm_borders(col = "black", lwd = 2) +
  #tm_text("ID", size = 1)  + 
  tm_layout(frame = FALSE,
            legend.show = FALSE,
            title = "Jan")

tm2 <- tm_shape(r2) +
  tm_raster(col.scale = tm_scale(breaks = breaks,
                                 values = "brewer.oranges")) +
  tm_shape(df2) +
  tm_borders(col = "black", lwd = 2) +
  #tm_text("ID", size = 1)  + 
  tm_layout(frame = FALSE,
            legend.show = FALSE,
            title = "Feb")

tm3 <- tm_shape(r3) +
  tm_raster(col.scale = tm_scale(breaks = breaks,
                                 values = "brewer.oranges")) +
  tm_shape(df3) +
  tm_borders(col = "black", lwd = 2) +
  #tm_text("ID", size = 1)  + 
  tm_layout(frame = FALSE,
            legend.show = FALSE,
            title = "Mar")

tm4 <- tm_shape(r4) +
  tm_raster(col.scale = tm_scale(breaks = breaks,
                                 values = "brewer.oranges")) +
  tm_shape(df4) +
  tm_borders(col = "black", lwd = 2) +
  #tm_text("ID", size = 1)  + 
  tm_layout(frame = FALSE,
            legend.show = FALSE,
            title = "Apr")

tm5 <- tm_shape(r5) +
  tm_raster(col.scale = tm_scale(breaks = breaks,
                                 values = "brewer.oranges")) +
  tm_shape(df5) +
  tm_borders(col = "black", lwd = 2) +
  #tm_text("ID", size = 1)  + 
  tm_layout(frame = FALSE,
            legend.show = FALSE,
            title = "May")

tm6 <- tm_shape(r6) +
  tm_raster(col.scale = tm_scale(breaks = breaks,
                                 values = "brewer.oranges")) +
  tm_shape(df6) +
  tm_borders(col = "black", lwd = 2) +
  #tm_text("ID", size = 1)  + 
  tm_layout(frame = FALSE,
            legend.show = FALSE,
            title = "Jun")

tm7 <- tm_shape(r7) +
  tm_raster(col.scale = tm_scale(breaks = breaks,
                                 values = "brewer.oranges")) +
  tm_shape(df7) +
  tm_borders(col = "black", lwd = 2) +
  #tm_text("ID", size = 1)  + 
  tm_layout(frame = FALSE,
            legend.show = FALSE,
            title = "Jul")

tm8 <- tm_shape(r8) +
  tm_raster(col.scale = tm_scale(breaks = breaks,
                                 values = "brewer.oranges")) +
  tm_shape(df8) +
  tm_borders(col = "black", lwd = 2) +
  #tm_text("ID", size = 1)  + 
  tm_layout(frame = FALSE,
            legend.show = FALSE,
            title = "Aug")

tm9 <- tm_shape(r9) +
  tm_raster(col.scale = tm_scale(breaks = breaks,
                                 values = "brewer.oranges")) +
  tm_shape(df9) +
  tm_borders(col = "black", lwd = 2) +
  #tm_text("ID", size = 1)  + 
  tm_layout(frame = FALSE,
            legend.show = FALSE,
            title = "Sep")

tm10 <- tm_shape(r10) +
  tm_raster(col.scale = tm_scale(breaks = breaks,
                                 values = "brewer.oranges")) +
  tm_shape(df10) +
  tm_borders(col = "black", lwd = 2) +
  #tm_text("ID", size = 1)  + 
  tm_layout(frame = FALSE,
            legend.show = FALSE,
            title = "Oct")

tm11 <- tm_shape(r11) +
  tm_raster(col.scale = tm_scale(breaks = breaks,
                                 values = "brewer.oranges")) +
  tm_shape(df11) +
  tm_borders(col = "black", lwd = 2) +
  #tm_text("ID", size = 1)  + 
  tm_layout(frame = FALSE,
            legend.show = FALSE,
            title = "Nov")

tm12 <- tm_shape(r12) +
  tm_raster(col.scale = tm_scale(breaks = breaks,
                                 values = "brewer.oranges")) +
  tm_shape(df12) +
  tm_borders(col = "black", lwd = 2) +
  #tm_text("ID", size = 1)  + 
  tm_layout(frame = FALSE,
            legend.show = FALSE,
            title = "Dec")


tm <- tmap_arrange(tm1, tm2, tm3, tm4, tm5, tm6, tm7, tm8, tm9, tm10, tm11, tm12, ncol = 4, nrow = 3)

tmap_save(tm, "../small_multiples.jpg", width = 6.5, height = 8)



# #facets
# tm_shape(df) +
#   tm_polygons() +
#   tm_text("ID", size = 1/2) +
#   tm_facets("month")
