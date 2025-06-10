library(sparr)
library(dplyr)
library(sf)
library(gt)
library(gtExtras)
library(raster)
library(ggplot2)
library(geoR)  # for 'jitterDupCoords' function
library(lubridate) # for 'month' and 'year'
library(tmap)

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
                     df$OFNS_DESC == "ROBBERY" | 
                     df$OFNS_DESC == "FELONY ASSAULT"
                   , 1, 0)

#offense frequency
table(df$class)

dfjit <- df
set.seed(123) # Not strictly necessary, but gives the ability to replicate random jittering
t1 <- Sys.time()
jcoords <- jitterDupCoords(dfjit[,c("X_COORD_CD","Y_COORD_CD")],min=1,max=300)  # This only jitters duplicated coords. You could use the 'jitter2d' function to jitter *all* points
t2 <- Sys.time()
t2-t1 # this particular jitter takes around 90 secs
dfjit[,c("X_COORD_CD","Y_COORD_CD")] <- jcoords

#read NYC boundary
geom <- st_read("../data/manhattan_boundary.shp") %>%
  st_transform(., crs = 2263) %>%
  st_coordinates(.) %>%
  as.data.frame(.) 

#ppp object window
w <- owin(poly=list(x=rev(geom[, "X"]),y=rev(geom[, "Y"]))) #window
plot(w)

# jittering will result in a small number of data points lost over the edge of the boundary. We need to identify these and remove them.
sum(!inside.owin(x=dfjit$X_COORD_CD,y=dfjit$Y_COORD_CD,w=w)) # this many points fell outside
dfjit <- dfjit[inside.owin(x=dfjit$X_COORD_CD,y=dfjit$Y_COORD_CD,w=w),] # keep only 'inside' points

write.csv(dfjit, "../output/arrests_jittered_2023.csv", row.names = FALSE)

# separate classes
dfjit_0 <- subset(dfjit, class == 0) 
dfjit_1 <- subset(dfjit, class == 1) 
# dfjit01 <- rbind(dfjit_0,dfjit_1)

#ppp
df_0_ppp <- ppp(dfjit_0[, "X_COORD_CD"], dfjit_0[, "Y_COORD_CD"], window = w)
df_1_ppp <- ppp(dfjit_1[, "X_COORD_CD"], dfjit_1[, "Y_COORD_CD"], window = w)

# create point pattern object
lyppp <- superimpose(df_0_ppp,df_1_ppp)
marks(lyppp)<- factor(rep(c("control","case"),c(npoints(df_0_ppp),npoints(df_1_ppp))))
plot(lyppp)
summary(lyppp)

#compute SPARR
lyrr <- risk(lyppp, h0=2000, resolution = 500, tolerate=TRUE, pilot.symmetry="none") 

#plotting
plot(lyrr,main="adaptive asymmetric, h0=7200")
points(df_0_ppp,pch=3,col="peachpuff4")
points(df_1_ppp,pch=19,col="seagreen3")

#classify points
rho.class <- tol.classify(lyrr, cutoff = 0.05)

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
  st_set_crs(2263)
pcpolys$ID <- 1:length(rho.class[["finsplit"]])
#st_write(pcpolys,"../output/pcpolys_timeless.shp", append = FALSE)

Area <- st_area(pcpolys) %>%
  units::set_units(., value = km^2) #Takes care of units #Take care of units
Case_density <- Cases/Area                    #

#cluster report table
df_res <- data.frame(ID, N, Cases, Controls, Risk, Case_density, Area)
#write.csv(df_res, "../output/clusters_timeless.csv", row.names = FALSE)

#risk surface to raster
r <- raster(lyrr$rr)
crs(r) <- crs(pcpolys)
# writeRaster(r, "../output/rho_timeless", format = "GTiff", overwrite=TRUE)


#map it
tm <- tm_shape(r) +
  tm_raster(col.scale = tm_scale(style = "quantile", 
                                 values = "brewer.oranges")) +
  tm_shape(pcpolys) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(frame = FALSE, legend.show = TRUE)
tm



