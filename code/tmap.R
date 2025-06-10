# Install packages
#install.packages("ggplot2")
#install.packages("Kendall")
#install.packages("zyp")

# Load necessary libraries
library(ggplot2)
library(Kendall)
library(zyp)
library(tmap)
library(sf)
library(raster)

# Set workspace
dir_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir_path)

# Function to read shapefiles and assign month
read_shapefile <- function(i) {
  df <- st_read(sprintf("../output/clusters_%d.shp", i))
  df$month <- i
  return(df)
}

# Function to read raster files and set CRS
read_raster <- function(i, shapefile) {
  r <- raster(sprintf("../output/rho_%d.tif", i))
  crs(r) <- crs(shapefile)
  return(r)
}

# Read all shapefiles and rasters
dfs <- lapply(1:12, read_shapefile)
rasters <- mapply(read_raster, 1:12, dfs, SIMPLIFY = FALSE)

# Bind shapefiles together
df <- do.call(rbind, dfs)

# Class breaks
#breaks <- c(-1.6, -.9, -.3, .3, 1.0)

pix <- numeric(0)

for (i in 1:12) {
  
  # Extract pixel values and append to the vector
  values <- getValues(rasters[[i]])
  pix <- c(pix, values)
}

pix <- na.omit(pix) %>%
  sort(.)

breaks <- quantile(pix, probs = seq(0, 1, 0.2), names = FALSE)


# Create legend
leg <- tm_shape(rasters[[1]]) +
  tm_raster(col.scale = tm_scale(breaks = breaks, values = "brewer.oranges")) +
  tm_layout(legend.only = TRUE, legend.frame = FALSE)

# Function to generate thematic maps
generate_tm <- function(raster, shapefile, title) {
  tm_shape(raster) +
    tm_raster(col.scale = tm_scale(breaks = breaks, values = "brewer.oranges")) +
    tm_shape(shapefile) +
    tm_borders(col = "black", lwd = 2) +
    tm_layout(frame = FALSE, legend.show = FALSE, title = title)
}

# Generate maps for all months
titles <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
tm_list <- mapply(generate_tm, rasters, dfs, titles, SIMPLIFY = FALSE)

# Arrange maps in a grid
tm <- do.call(tmap_arrange, c(tm_list, list(ncol = 4, nrow = 3)))

#tmap_save(tm, "../small_multiples.jpg", width = 6.5, height = 8)

#Jaccard Index

#define sets of months
set1 <- dfs[1:11]
set2 <- dfs[2:12]

#Jaccard function: intersect area / union area
jac <- function(p1, p2) {
  int <- st_intersection(p1, p2) %>%
    st_area(.) %>%
    sum(.)

  uni <- st_union(st_union(p1), st_union(p2)) %>%
    st_area(.) %>%
    sum(.)

  return(int/uni)
}

#output array
out <- sapply(1:11, function(i) jac(set1[[i]]$geometry, set2[[i]]$geometry))
 
print(out)
  
# Perform the Mann-Kendall Trend Test
result <- MannKendall(out)

# Print the result
print(result)

