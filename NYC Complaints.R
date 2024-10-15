library(sparr)
library(dplyr)
library(sf)
library(gt)
library(gtExtras)
library(raster)

# set workspace----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read table
df <- read.csv("NYPD_Complaint_Data_Current__Year_To_Date__20240531.csv") %>%
  subset(., Latitude != 0, select = c(CMPLNT_NUM, BORO_NM, CMPLNT_FR_DT, LAW_CAT_CD, OFNS_DESC, Latitude, Longitude)) %>%
  na.omit(.) 

#date format  
df$date <- as.Date(df$CMPLNT_FR_DT, "%m/%d/%Y")

#select 2023
df <- df %>%
  subset(., date >= "2023-01-01" & df$date >= "2024-01-01" )

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

#select borough
df <- df %>%
  subset(., BORO_NM == "MANHATTAN")

#separate
df_0 <- subset(df, class == 0)  # control, non-violent
df_1 <- subset(df, class == 1)  # case, violent

#read NYC boundary
geom <- st_read("nyc_boundary.shp") %>%
  st_coordinates(.) %>%
  as.data.frame(.) %>%
  subset(., L2 == 79)   #Manhattan

#ppp object window
w <- owin(poly=list(x=rev(geom[, "X"]),y=rev(geom[, "Y"]))) #window

#ppp
df_0_ppp <- ppp(df_0$Longitude, df_0$Latitude, window = w)
df_1_ppp <- ppp(df_1$Longitude, df_1$Latitude, window = w)

#kde 0: control
g_tilde <- bivariate.density(pp=df_0_ppp, h0=OS(df_0_ppp)/2, adapt=FALSE, resolution=1500, verbose=TRUE, parallelise = 7)
g_tilde

#kde 1: case
f_breve <- bivariate.density(pp=df_1_ppp, h0=OS(df_0_ppp)/2, adapt=FALSE, resolution=1500, verbose=TRUE, parallelise = 7)

#risk
rho <- risk(f_breve, g_tilde, tolerate = TRUE)

plot(rho, tol.show = TRUE)

#risk surface
r <- raster(rho$rr)
crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(r, "rho_manhattan", format = "GTiff", overwrite=TRUE)

#classify points
rho.class <- tol.classify(rho, cutoff = 0.05)

#cluster report table
ID <- 1:length(rho.class[["finsplit"]])       #cluster identifier
Cases <- lengths(rho.class[["finsplit"]])     #case count
Controls <- lengths(rho.class[["ginsplit"]])  #control count
N <- Cases + Controls                         #point count
Risk <- Cases/N                               #

#contours to sf
pcpolys <- rho.class$pcpolys %>%
  lapply(., FUN = st_as_sf) %>%
  do.call(rbind, .) %>%
  st_set_crs(4326)
pcpolys$ID <- 1:length(rho.class[["finsplit"]])
st_write(pcpolys,"manhattan_pcpolys.shp", append = FALSE)

Area <- st_area(pcpolys) #Take care of units
Case_density <- Cases/Area                    #

#style it
df_res <- data.frame(ID, N, Cases, Controls, Risk, Case_density, Area) %>%
  gt() %>%
  gt_theme_nytimes() %>%
  tab_header(title = "Clusters of Violent Crime in Manhattan")
df_res

