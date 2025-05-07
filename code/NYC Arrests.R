library(sparr)
library(dplyr)
library(sf)
library(gt)
library(gtExtras)
library(raster)
library(ggplot2)
library(geoR)  # for 'jitterDupCoords' function
library(lubridate) # for 'month' and 'year'

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

# separate classes
dfjit_0 <- subset(dfjit, class == 0) 
dfjit_1 <- subset(dfjit, class == 1) 
dfjit01 <- rbind(dfjit_0,dfjit_1)

# per-month split of data
dfjit01_monthly <- list() 
for(i in 1:12) dfjit01_monthly[[i]] <- dfjit01[month(dfjit01$date)==i,]
names(dfjit01_monthly) <- month.abb

# creating separate monthly data lists for cases and controls
dfjit0_monthly <- lapply(dfjit01_monthly,subset,subset=class==0)
dfjit1_monthly <- lapply(dfjit01_monthly,subset,subset=class==1)

# ppp objects of monthly cases and controls together (01)
ppjit01_monthly <- list() 
for(i in 1:12){
  ppjit01_monthly[[i]] <- ppp(x=c(dfjit0_monthly[[i]]$X_COORD_CD,dfjit1_monthly[[i]]$X_COORD_CD),
                              y=c(dfjit0_monthly[[i]]$Y_COORD_CD,dfjit1_monthly[[i]]$Y_COORD_CD),
                              marks=factor(rep(c("control","case"),c(nrow(dfjit0_monthly[[i]]),nrow(dfjit1_monthly[[i]])))),
                              window=w)
}
names(ppjit01_monthly) <- month.abb

# ppp objects of monthly cases and controls separately
ppjit0_monthly <- ppjit1_monthly <- list() 
for(i in 1:12){
  ppsplit_temp <- split(ppjit01_monthly[[i]])
  ppjit0_monthly[[i]] <- ppsplit_temp$control
  ppjit1_monthly[[i]] <- ppsplit_temp$case
}
names(ppjit0_monthly) <- names(ppjit1_monthly) <- month.abb

# Compute risk surface for each month
rr_monthly <- lapply(ppjit01_monthly,risk,h0=2000,tolerate=TRUE, res = 500)

# small multiples plot: monthly clusters
df_res = data.frame()

for(i in 1:12){
  print(i)
  
  #risk surface
  r <- raster(rr_monthly[[i]]$rr)
  crs(r) <- 2263
  
  #writeRaster(r, paste0("../output/rho_", i), format = "GTiff", overwrite=TRUE)
  
  # SPARR plotting
  #plot(rr_monthly[[i]],main=names(rr_monthly)[i],tol.show=FALSE)
  #tol.contour(rr_monthly[[i]]$P,levels=0.05,col="seagreen4",drawlabels=FALSE,add=TRUE,lwd=2)
  
  # get tolerance contours  
  rho.class <- tol.classify(rr_monthly[[i]], cutoff = 0.05)
  
  # filter out clusters without cases
  indices <- lengths(rho.class[["finsplit"]]) > 0 
  rho.class[["finsplit"]] <- rho.class[["finsplit"]][indices]  #filter out clusters without cases
  rho.class[["ginsplit"]] <- rho.class[["ginsplit"]][indices]  #filter out clusters without cases
  
  # cluster identifier
  ID <- 1:length(rho.class[["finsplit"]])   
  
  # case count
  Cases <- lengths(rho.class[["finsplit"]])
  
  # control count
  Controls <- lengths(rho.class[["ginsplit"]])[1:length(rho.class[["finsplit"]])]
  
  # point count
  N <- Cases + Controls
  
  # Risk
  Risk <- Cases/N                             
  
  # contours to sf
  pcpolys <- rho.class$pcpolys %>%
    lapply(., FUN = st_as_sf) %>%
    do.call(rbind, .) %>%
    st_set_crs(2263)
  
  # filter polygons (clusters without cases)
  pcpolys <- pcpolys[indices, ]
  
  # polygon ID
  pcpolys$ID <- 1:length(rho.class[["finsplit"]])
  
  # polygon month
  pcpolys$month <- i
  
  # write polygons to file
  #st_write(pcpolys, paste0("../output/clusters_", i, ".shp"), append=FALSE)
  
  # polygon area
  Area <- st_area(pcpolys) %>%
    units::set_units(., value = km^2) #Takes care of units
  
  # case density
  Case_density <- Cases/Area
  
  # attach to table
  df_res <- rbind(df_res, data.frame(i, ID, N, Cases, Controls, Risk, Case_density, Area))
  
}
#write.csv(df_res, "../output/clusters.csv", row.names = FALSE)

