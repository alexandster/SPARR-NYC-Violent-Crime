library(sparr)
library(dplyr)
library(sf)
library(gt)
library(gtExtras)
library(raster)
library(lubridate)

# set workspace----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read table
df <- read.csv("NYPD_Complaints_2023.csv") %>%
  subset(., Y_COORD_CD != 0, select = c(CMPLNT_NUM, BORO_NM, CMPLNT_FR_DT, LAW_CAT_CD, OFNS_DESC, X_COORD_CD, Y_COORD_CD)) %>%
  na.omit(.)

#date format  
df$date <- as.Date(df$CMPLNT_FR_DT, "%m/%d/%Y")

#classify: offense
df$class <- ifelse(df$OFNS_DESC == "ASSAULT 3 & RELATED OFFENSES" | 
                     df$OFNS_DESC == "MURDER & NON-NEGL. MANSLAUGHTER" |
                     df$OFNS_DESC == "RAPE" |
                     df$OFNS_DESC == "ROBBERY"
                   , 1, 0)

#select borough
df <- df %>%
  subset(., BORO_NM == "MANHATTAN")

#read NYC boundary
geom <- st_read("manhattan_boundary.shp") %>%
  st_transform(., crs = 2263) %>%
  st_coordinates(.) %>%
  as.data.frame(.)

#ppp object window
w <- owin(poly=list(x=rev(geom[, "X"]),y=rev(geom[, "Y"]))) #window
plot(w)

#separate
df_0 <- subset(df, class == 0) 
df_1 <- subset(df, class == 1) 

#ppp
df_0_ppp <- ppp(df_0$X_COORD_CD, df_0$Y_COORD_CD, window = w)
df_1_ppp <- ppp(df_1$X_COORD_CD, df_1$Y_COORD_CD, window = w)


df$CMPLNT_FR_DT <- 

#monthly cluster maps
slices <- c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01", "2023-05-01", "2023-06-01", "2023-07-01", 
            "2023-08-01", "2023-09-01", "2023-10-01", "2023-11-01", "2023-12-01")

i <- 1
while (i < 12) {
  #print(i)
  
  #select month
  df_month <- df %>%
    subset(., month(date) == i)
  print(c(i, nrow(df_month)))
  
  i <- i + 1
}
  
  
  
  }  





#risk surface
hvrr2 <- risk(crjit,adapt=TRUE,h0=886.727,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 





# #risk surface
# r <- raster(rho$rr)
# crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# writeRaster(r, "rho_manhattan", format = "GTiff", overwrite=TRUE)
# 
# #classify points
# rho.class <- tol.classify(rho, cutoff = 0.05)
# 
# #plot
# #plot(rho)
# #points(rho.class$fin,col=2)
# #points(rho.class$fout)
# 
# #cluster report table
# ID <- 1:length(rho.class[["finsplit"]])       #cluster identifier
# Cases <- lengths(rho.class[["finsplit"]])     #case count
# Controls <- lengths(rho.class[["ginsplit"]])  #control count
# N <- Cases + Controls                         #point count
# Risk <- Cases/N                               #
# 
# #contours to sf
# pcpolys <- rho.class$pcpolys %>%
#   lapply(., FUN = st_as_sf) %>%
#   do.call(rbind, .) %>%
#   st_set_crs(4326)
# pcpolys$ID <- 1:length(rho.class[["finsplit"]])
# st_write(pcpolys,"manhattan_pcpolys.shp", append = FALSE)
# 
# Area <- st_area(pcpolys) #Take care of units
# Case_density <- Cases/Area                    #
# 
# #style it
# df_res <- data.frame(ID, N, Cases, Controls, Risk, Case_density, Area) %>%
#   gt() %>%
#   gt_theme_nytimes() %>%
#   tab_header(title = "Clusters of Violent Crime in Manhattan")
# df_res
# 
