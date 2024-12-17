library(sparr)
library(dplyr)
library(sf)
library(gt)
library(gtExtras)
library(raster)
library(ggplot2)

# set workspace----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read table
df <- read.csv("NYPD_Arrests_Data__Historic__20241216.csv")   #source: https://s.cnmilf.com/user74170196/https/catalog.data.gov/dataset/nypd-arrest-data-year-to-date
df <- subset(df, Y_COORD_CD != 0 & ARREST_BORO == "M", select = c(ARREST_KEY, ARREST_DATE, OFNS_DESC, X_COORD_CD, Y_COORD_CD))
df<-  na.omit(df) 

#date format  
df$date <- as.Date(df$ARREST_DATE, "%m/%d/%Y")

#select 2023
df <- df %>%
  subset(., date >= "2023-01-01" & date <= "2023-12-31")

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

#separate
df_0 <- subset(df, class == 0) 
df_1 <- subset(df, class == 1) 

#read NYC boundary
geom <- st_read("manhattan_boundary.shp") %>%
  st_transform(., crs = 2263) %>%
  st_coordinates(.) %>%
  as.data.frame(.) 

#ppp object window
w <- owin(poly=list(x=rev(geom[, "X"]),y=rev(geom[, "Y"]))) #window
plot(w)

#ppp
df_0_ppp <- ppp(df_0$X_COORD_CD, df_0$Y_COORD_CD, window = w)
df_1_ppp <- ppp(df_1$X_COORD_CD, df_1$Y_COORD_CD, window = w)

# inspect raw data (with fixed window)
crppp <- ppp(x=c(df_0_ppp$x,df_1_ppp$x),
             y=c(df_0_ppp$y,df_1_ppp$y),
             marks=factor(rep(c("control","case"),c(npoints(df_0_ppp),npoints(df_1_ppp)))),
             window=w)

# create a slightly jittered version of the dataset (30-60 secs)
crjit <- rshift(crppp,group=factor(1:npoints(crppp)),radius=300,edge="none")
npoints(unique(unmark(crjit))) # Much better

#jittered vs. unjittered
par(mfrow=c(1,2))
plot(crppp,cex=0.5,main="unjittered")
plot(crjit,cex=0.5,main="jittered")

# get an idea of suggested bandwidths by running several different selectors
OS(crppp) # 1941.747 ftUS
NS(crjit) # 1790.382 ftUS

hk <- LSCV.risk(crjit,method="kelsall-diggle");hk #   402.0214 ftUS
hh <- LSCV.risk(crjit,method="hazelton");hh # 408.7517 ftUS
hd <- LSCV.risk(crjit,method="davies");hd  # 442.8772 ftUS

#################################################################
##@Tilman: run from here

##uncomment and run the line below
# load("Arrests.RData")
#################################################################

# compute SPARR
hvrr1 <- risk(crjit,h0=hd,tolerate=TRUE)  #using the Davies bandwidth
hvrr2 <- risk(crjit,adapt=TRUE,h0=hd,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 
hvrr1org <- risk(crppp,h0=hd,tolerate=TRUE) # repeat the above using the original/unjittered data so we can compare the effect of the jittering
hvrr2org <- risk(crppp,adapt=TRUE,h0=hd,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 

#plot using Davies
par(mfrow=c(2,2))
plot(hvrr1,main="fixed; h=hd")
plot(hvrr2,main="symmetric (pooled) adaptive; h0=hp=hd; data jittered")
plot(hvrr1org,main="(fixed using unjittered data)")
plot(hvrr2org,main="(adaptive using unjittered data)")

# repeat these four estimates, using a larger bandwidth (NS bandwidth)
hvrr1 <- risk(crjit,h0=1790.382,tolerate=TRUE)  
hvrr2 <- risk(crjit,adapt=TRUE,h0=1790.382,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 
hvrr1org <- risk(crppp,h0=1790.382,tolerate=TRUE) # repeat the above using the original/unjittered data so we can compare the effect of the jittering
hvrr2org <- risk(crppp,adapt=TRUE,h0=1790.382,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 

#plot using larger bandwidth
par(mfrow=c(2,2))
plot(hvrr1,main="fixed; h=1790.382")
plot(hvrr2,main="symmetric (pooled) adaptive; h0=hp=1790.382; data jittered")
plot(hvrr1org,main="(fixed using unjittered data)")
plot(hvrr2org,main="(adaptive using unjittered data)")

#save.image(file='Arrests.RData')

