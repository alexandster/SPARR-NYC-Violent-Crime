#################################
## Copy of Alex's read-in code ##
#################################
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
#################################
#################################
#################################



# Tilman's scratchpad

# inspect and fix window
W <- Window(df_1_ppp)
wd <- as.data.frame(W)
m <- max(wd$id)
m
par(mfrow=c(3,3)) # plot all components separately
for(i in 1:m){
  if(wd$sign[wd$id==i][1]==1){
    plot(owin(poly=list(x=wd$x[wd$id==i],y=wd$y[wd$id==i])),main=paste("Component",i,"(regular)"))
  } else {
    plot(owin(poly=list(x=rev(wd$x[wd$id==i]),y=rev(wd$y[wd$id==i]))),main=paste("Component",i,"(HOLE)"))
  }
}
par(mfrow=c(1,1)) # show all holes
plot(W)
for(i in 2:m) plot(owin(poly=list(x=rev(wd$x[wd$id==i]),y=rev(wd$y[wd$id==i]))),col="red",add=TRUE)
# We can simply work with the first component of 'W'
WFIX <- owin(poly=list(x=wd$x[wd$id==1],y=wd$y[wd$id==1]))
plot(WFIX)


# inspect raw data (with fixed window)
crppp <- ppp(x=c(df_0_ppp$x,df_1_ppp$x),
             y=c(df_0_ppp$y,df_1_ppp$y),
             marks=factor(rep(c("control","case"),c(npoints(df_0_ppp),npoints(df_1_ppp)))),
             window=WFIX)
# marks(crppp) <- factor(rep(c("control","case"),c(npoints(df_0_ppp),npoints(df_1_ppp))))
npoints(crppp) # Total number of points
npoints(unique(unmark(crppp))) # Number of *unique* locations. Heavily tied dataset.


# create a slightly jittered version of the dataset (30-60 secs)
crjit <- rshift(crppp,group=factor(1:npoints(crppp)),radius=0.001,edge="none")
npoints(unique(unmark(crjit))) # Much better

# examine and plot both objects (both have fixed window)
crppp
crjit  # number of points is slightly less here compared to unjittered version -- this is a natural consequence of jittering
par(mfrow=c(1,2))
plot(crppp,cex=0.5,main="unjittered")
plot(crjit,cex=0.5,main="jittered")


# get an idea of suggested bandwidths by running several different selectors
OS(crppp)
NS(crjit) # OS bandwidths basically the same. Sample size is slightly different, but these are effectively equivalent

# SLIK.adapt(crjit,para=5) # leave-one out/CV bandwidths will perform poorly for heavily tied datasets. Only running on jittered version

hk <- LSCV.risk(crjit,method="kelsall-diggle");hk  
hh <- LSCV.risk(crjit,method="hazelton");hh 
hd <- LSCV.risk(crjit,method="davies");hd  # K-D bandwidth is around double the other two here.


# I suggest a bandwidth somewhere in the range 0.002 - 0.006. Less than 0.002 seems too small if you experiment with it...


hvrr1 <- risk(crjit,h0=0.00278,tolerate=TRUE)  #using the kelsall-diggle bandwidth for illustration
hvrr2 <- risk(crjit,adapt=TRUE,h0=0.00278,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 

hvrr1org <- risk(crppp,h0=0.00278,tolerate=TRUE) # repeat the above using the original/unjittered data so we can compare the effect of the jittering
hvrr2org <- risk(crppp,adapt=TRUE,h0=0.00278,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 

par(mfrow=c(2,2))
plot(hvrr1,main="fixed; h=0.00278")
plot(hvrr2,main="symmetric (pooled) adaptive; h0=hp=0.00278; data jittered")
plot(hvrr1org,main="(fixed using unjittered data)")
plot(hvrr2org,main="(adaptive using unjittered data)")



# repeat these four estimates, using a larger bandwidth (around the size of the suggested OS bandwidth)

hvrr1 <- risk(crjit,h0=0.006,tolerate=TRUE)  #using the kelsall-diggle bandwidth for illustration
hvrr2 <- risk(crjit,adapt=TRUE,h0=0.006,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 

hvrr1org <- risk(crppp,h0=0.006,tolerate=TRUE) # repeat the above using the original/unjittered data so we can compare the effect of the jittering
hvrr2org <- risk(crppp,adapt=TRUE,h0=0.006,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 

par(mfrow=c(2,2))
plot(hvrr1,main="fixed; h=0.006")
plot(hvrr2,main="symmetric (pooled) adaptive; h0=hp=0.006; data jittered")
plot(hvrr1org,main="(fixed using unjittered data)")
plot(hvrr2org,main="(adaptive using unjittered data)")




# In conclusion, it doesn't look like the jittering has had a major effect on the final estimates here. However, heavily tied observations can hurt the performance of bandwidth selectors, so even if we end up using the unjittered data to produce the final estimates, I think we should probably used the jittered data when running bandwidth selectors

###