library(sparr)
library(dplyr)
library(sf)
library(gt)
library(gtExtras)
library(raster)
library(ggplot2)
library(lubridate)

# set workspace----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read table
df <- read.csv("../data/NYPD_Arrests_Data__Historic__20241216.csv")   #source: https://s.cnmilf.com/user74170196/https/catalog.data.gov/dataset/nypd-arrest-data-year-to-date
df <- subset(df, Y_COORD_CD != 0 & ARREST_BORO == "M", select = c(ARREST_KEY, ARREST_DATE, OFNS_DESC, X_COORD_CD, Y_COORD_CD))
df<-  na.omit(df) 

#date format  
df$date <- as.Date(df$ARREST_DATE, "%m/%d/%Y")

#select 2023
df <- df %>%
  subset(., date >= "2023-01-01" & date <= "2023-12-31")

df$month <- month(df$date)

# time series plot
p <- ggplot(df, aes(x=date)) +
  geom_bar()
p

#offense frequency
table(df$OFNS_DESC)

#classify: offense
df$class <- ifelse(df$OFNS_DESC == "ASSAULT 3 & RELATED OFFENSES" | 
                     df$OFNS_DESC == "MURDER & NON-NEGL. MANSLAUGHTER" |
                     df$OFNS_DESC == "RAPE" |
                     df$OFNS_DESC == "ROBBERY"
                   , 1, 0)

#offense frequency
table(df$class)

#jitter
df$X_COORD_CD_J <- jitter(df$X_COORD_CD, amount = 30)
df$Y_COORD_CD_J <- jitter(df$Y_COORD_CD, amount = 30)

#read NYC boundary
geom <- st_read("../data/manhattan_boundary.shp") %>%
  st_transform(., crs = 2263) %>%
  st_coordinates(.) %>%
  as.data.frame(.) 

#ppp object window
w <- owin(poly=list(x=rev(geom[, "X"]),y=rev(geom[, "Y"]))) #window
plot(w)

#time slices
time_slice <- function(df){
  
  #separate
  df_0 <- subset(df, class == 0) 
  df_1 <- subset(df, class == 1) 

  #ppp
  df_0_ppp <- ppp(df_0$X_COORD_CD_J, df_0$Y_COORD_CD_J, window = w)
  df_1_ppp <- ppp(df_1$X_COORD_CD_J, df_1$Y_COORD_CD_J, window = w)

  # inspect raw data (with fixed window)
  df_ppp <- ppp(x=c(df_0_ppp$x,df_1_ppp$x),
             y=c(df_0_ppp$y,df_1_ppp$y),
             marks=factor(rep(c("control","case"),c(npoints(df_0_ppp),npoints(df_1_ppp)))),
             window=w)

    # compute SPARR
  rho <- risk(df_ppp,h0=1800, resolution = 1500, tolerate=TRUE)  #using the Davies bandwidth
  
  #risk surface
  r <- raster(rho$rr)
  #crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  crs(r) <- 2263
  writeRaster(r, paste0("../output/rho_", toString(df$month[1])), format = "GTiff", overwrite=TRUE)
  
  #classify points
  # rho.class <- tol.classify(rho, cutoff = 0.05)
  # 
  # #cluster report table
  # ID <- 1:length(rho.class[["finsplit"]])       #cluster identifier
  # Cases <- lengths(rho.class[["finsplit"]])     #case count
  # Controls <- lengths(rho.class[["ginsplit"]])  #control count
  # N <- Cases + Controls                         #point count
  # Risk <- Cases/N                               #
  
  # #contours to sf
  # pcpolys <- rho.class$pcpolys %>%
  #   lapply(., FUN = st_as_sf) %>%
  #   do.call(rbind, .) %>%
  #   st_set_crs(2263)
  # pcpolys$ID <- 1:length(rho.class[["finsplit"]])
  # st_write(pcpolys,paste0("../output/pcpolys_", toString(df$month[1]), ".shp"), append = FALSE)
  
  }



df[df$month > 7,] %>%
  group_by(month) %>%
  do(time_slice(.))
  
