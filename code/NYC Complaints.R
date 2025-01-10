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
  subset(., Y_COORD_CD != 0, select = c(CMPLNT_NUM, BORO_NM, CMPLNT_FR_DT, LAW_CAT_CD, OFNS_DESC, X_COORD_CD, Y_COORD_CD)) %>%
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
df_0 <- subset(df, class == 0) 
df_1 <- subset(df, class == 1) 

#read NYC boundary
geom <- st_read("nyc_boundary.shp") %>%
  st_transform(., crs = 2263) %>%
  st_coordinates(.) %>%
  as.data.frame(.) %>%
  subset(., L2 == 79)   #Manhattan

#ppp object window
w <- owin(poly=list(x=rev(geom[, "X"]),y=rev(geom[, "Y"]))) #window
plot(w)

#ppp
df_0_ppp <- ppp(df_0$X_COORD_CD, df_0$Y_COORD_CD, window = w)
df_1_ppp <- ppp(df_1$X_COORD_CD, df_1$Y_COORD_CD, window = w)

# inspect and fix window
W <- Window(df_1_ppp)
wd <- as.data.frame(W)
WFIX <- owin(poly=list(x=wd$x[wd$id==1],y=wd$y[wd$id==1]))
plot(WFIX)

# inspect raw data (with fixed window)
crppp <- ppp(x=c(df_0_ppp$x,df_1_ppp$x),
             y=c(df_0_ppp$y,df_1_ppp$y),
             marks=factor(rep(c("control","case"),c(npoints(df_0_ppp),npoints(df_1_ppp)))),
             window=WFIX)

# create a slightly jittered version of the dataset (30-60 secs)
crjit <- rshift(crppp,group=factor(1:npoints(crppp)),radius=0.001,edge="none")
npoints(unique(unmark(crjit))) # Much better

# examine and plot both objects (both have fixed window)
par(mfrow=c(1,2))
plot(crppp,cex=0.5,main="unjittered")
plot(crjit,cex=0.5,main="jittered")

# get an idea of suggested bandwidths by running several different selectors
OS(crppp)
NS(crjit) # OS bandwidths basically the same. Sample size is slightly different, but these are effectively equivalent
#Comment AH:OS(crppp) = 2120.827 ftUS, NS(crjit) = 1955.452 ftUS. Are they still "basically the same"?

hk <- LSCV.risk(crjit,method="kelsall-diggle");hk #   886.727 ftUS
hh <- LSCV.risk(crjit,method="hazelton");hh # 558.4516 ftUS
hd <- LSCV.risk(crjit,method="davies");hd  # 408.6685 ftUS

hvrr1 <- risk(crjit,h0=886.727,tolerate=TRUE)  #using the Kelsall-Diggle bandwidth
hvrr2 <- risk(crjit,adapt=TRUE,h0=886.727,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 
hvrr1org <- risk(crppp,h0=886.727,tolerate=TRUE) # repeat the above using the original/unjittered data so we can compare the effect of the jittering
hvrr2org <- risk(crppp,adapt=TRUE,h0=886.727,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 

#plot using Kelsall-Diggle
par(mfrow=c(2,2))
plot(hvrr1,main="fixed; h=886.727")
plot(hvrr2,main="symmetric (pooled) adaptive; h0=hp=886.727; data jittered")
plot(hvrr1org,main="(fixed using unjittered data)")
plot(hvrr2org,main="(adaptive using unjittered data)")

# repeat these four estimates, using a larger bandwidth (around the size of the suggested OS bandwidth)
hvrr1 <- risk(crjit,h0=2000,tolerate=TRUE)  #using the kelsall-diggle bandwidth for illustration
hvrr2 <- risk(crjit,adapt=TRUE,h0=2000,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 
hvrr1org <- risk(crppp,h0=2000,tolerate=TRUE) # repeat the above using the original/unjittered data so we can compare the effect of the jittering
hvrr2org <- risk(crppp,adapt=TRUE,h0=2000,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 

#plot using larger bandwidth
par(mfrow=c(2,2))
plot(hvrr1,main="fixed; h=2000")
plot(hvrr2,main="symmetric (pooled) adaptive; h0=hp=2000; data jittered")
plot(hvrr1org,main="(fixed using unjittered data)")
plot(hvrr2org,main="(adaptive using unjittered data)")

#the larger bandwidth too large for my taste. The Kelsall-Diggle bandwidth or even the Hazelton or Davies bandwidths looks better.  
