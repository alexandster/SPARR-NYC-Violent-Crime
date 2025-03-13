library(sparr)
library(dplyr)
library(sf)
library(gt)
library(gtExtras)
library(raster)
library(ggplot2)
library("geoR")  # for 'jitterDupCoords' function
library("lubridate") # for 'month' and 'year'


# set workspace----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("../data/Arrests.RData")

dfjit <- df
set.seed(123) # Not strictly necessary, but gives the ability to replicate random jittering
t1 <- Sys.time()
jcoords <- jitterDupCoords(dfjit[,c("X_COORD_CD","Y_COORD_CD")],min=1,max=300)  # This only jitters duplicated coords. You could use the 'jitter2d' function to jitter *all* points
t2 <- Sys.time()
t2-t1 # this particular jitter takes around 90 secs
dfjit[,c("X_COORD_CD","Y_COORD_CD")] <- jcoords


# jittering will result in a small number of data points lost over the edge of the boundary. We need to identify these and remove them.
sum(!inside.owin(x=dfjit$X_COORD_CD,y=dfjit$Y_COORD_CD,w=w)) # this many points fell outside
dfjit <- dfjit[inside.owin(x=dfjit$X_COORD_CD,y=dfjit$Y_COORD_CD,w=w),] # keep only 'inside' points

dfjit_0 <- subset(dfjit, class == 0) 
dfjit_1 <- subset(dfjit, class == 1) 
dfjit01 <- rbind(dfjit_0,dfjit_1)


unique(year(dfjit01$date)) # just checking only 1 year is represented
table(month(dfjit01$date)) # all months are represented
dfjit01_monthly <- list() # per-month split of data
for(i in 1:12) dfjit01_monthly[[i]] <- dfjit01[month(dfjit01$date)==i,]
names(dfjit01_monthly) <- month.abb

# creating separate monthly data lists for cases and controls
dfjit0_monthly <- lapply(dfjit01_monthly,subset,subset=class==0)
dfjit1_monthly <- lapply(dfjit01_monthly,subset,subset=class==1)

# sanity check
sapply(dfjit01_monthly,nrow)
sapply(dfjit1_monthly,nrow)+sapply(dfjit0_monthly,nrow)  # all good here


ppjit01_monthly <- list() # ppp objects of monthly cases and controls together (01)
for(i in 1:12){
  ppjit01_monthly[[i]] <- ppp(x=c(dfjit0_monthly[[i]]$X_COORD_CD,dfjit1_monthly[[i]]$X_COORD_CD),
                              y=c(dfjit0_monthly[[i]]$Y_COORD_CD,dfjit1_monthly[[i]]$Y_COORD_CD),
                              marks=factor(rep(c("control","case"),c(nrow(dfjit0_monthly[[i]]),nrow(dfjit1_monthly[[i]])))),
                              window=w)
}
names(ppjit01_monthly) <- month.abb

ppjit0_monthly <- ppjit1_monthly <- list() # ppp objects of monthly cases and controls separately
for(i in 1:12){
  ppsplit_temp <- split(ppjit01_monthly[[i]])
  ppjit0_monthly[[i]] <- ppsplit_temp$control
  ppjit1_monthly[[i]] <- ppsplit_temp$case
}
names(ppjit0_monthly) <- names(ppjit1_monthly) <- month.abb

sapply(ppjit0_monthly,OS) # around 3000 http://127.0.0.1:9581/graphics/plot_zoom_png?width=1572&height=805
sapply(ppjit1_monthly,OS) # around 4000
sapply(ppjit0_monthly,NS) # around 2800
sapply(ppjit1_monthly,NS) # around 3500
sapply(ppjit01_monthly,LSCV.risk,method="kelsall-diggle") #  around 900

rr_monthly <- lapply(ppjit01_monthly,risk,h0=2000,tolerate=TRUE)

#small multiples plot: monthly clusters
dev.off()
#par(mfrow=c(4,3))
for(i in 4:6){
  print(i)
  plot(rr_monthly[[i]],main=names(rr_monthly)[i],tol.show=FALSE)
  tol.contour(rr_monthly[[i]]$P,levels=0.05,col="seagreen4",drawlabels=FALSE,add=TRUE,lwd=2)
  rho.class <- tol.classify(rr_monthly[[i]], cutoff = 0.05)
  # ID <- 1:length(rho.class[["finsplit"]])       #cluster identifier
  # Cases <- lengths(rho.class[["finsplit"]])     #case count
  # Controls <- lengths(rho.class[["ginsplit"]])  #control count
  # N <- Cases + Controls                         #point count
  # Risk <- Cases/N                               #
}


# #read table
# df <- read.csv("../data/NYPD_Arrests_Data__Historic__20241216.csv")   #source: https://s.cnmilf.com/user74170196/https/catalog.data.gov/dataset/nypd-arrest-data-year-to-date
# df <- subset(df, Y_COORD_CD != 0 & ARREST_BORO == "M", select = c(ARREST_KEY, ARREST_DATE, OFNS_DESC, X_COORD_CD, Y_COORD_CD))
# df<-  na.omit(df) 
# 
# #date format  
# df$date <- as.Date(df$ARREST_DATE, "%m/%d/%Y")
# 
# #select 2023
# df <- df %>%
#   subset(., date >= "2023-01-01" & date <= "2023-12-31")
# 
# # time series plot
# p <- ggplot(df, aes(x=date)) +
#   geom_bar()
# p
# 
# #offense frequency
# table(df$OFNS_DESC)
# 
# #classify: offense
# df$class <- ifelse(df$OFNS_DESC == "ASSAULT 3 & RELATED OFFENSES" | 
#                      df$OFNS_DESC == "MURDER & NON-NEGL. MANSLAUGHTER" |
#                      df$OFNS_DESC == "RAPE" |
#                      df$OFNS_DESC == "ROBBERY"
#                    , 1, 0)
# 
# #offense frequency
# table(df$class)

# #separate
# df_0 <- subset(df, class == 0) 
# df_1 <- subset(df, class == 1) 
# 
# #read NYC boundary
# geom <- st_read("../data/manhattan_boundary.shp") %>%
#   st_transform(., crs = 2263) %>%
#   st_coordinates(.) %>%
#   as.data.frame(.) 
# 
# #ppp object window
# w <- owin(poly=list(x=rev(geom[, "X"]),y=rev(geom[, "Y"]))) #window
# plot(w)
# 
# #ppp
# df_0_ppp <- ppp(df_0$X_COORD_CD, df_0$Y_COORD_CD, window = w)
# df_1_ppp <- ppp(df_1$X_COORD_CD, df_1$Y_COORD_CD, window = w)
# 
# # inspect raw data (with fixed window)
# crppp <- ppp(x=c(df_0_ppp$x,df_1_ppp$x),
#              y=c(df_0_ppp$y,df_1_ppp$y),
#              marks=factor(rep(c("control","case"),c(npoints(df_0_ppp),npoints(df_1_ppp)))),
#              window=w)
# 
# # create a slightly jittered version of the dataset (30-60 secs)
# crjit <- rshift(crppp,group=factor(1:npoints(crppp)),radius=300,edge="none")
# npoints(unique(unmark(crjit))) # Much better
# 
# #jittered vs. unjittered
# par(mfrow=c(1,2))
# plot(crppp,cex=0.5,main="unjittered")
# plot(crjit,cex=0.5,main="jittered")
# 
# # get an idea of suggested bandwidths by running several different selectors
# OS(crppp) # 1941.747 ftUS
# NS(crjit) # 1790.382 ftUS
# 
# hk <- LSCV.risk(crjit,method="kelsall-diggle");hk #   402.0214 ftUS
# hh <- LSCV.risk(crjit,method="hazelton");hh # 408.7517 ftUS
# hd <- LSCV.risk(crjit,method="davies");hd  # 442.8772 ftUS
# 
# #################################################################
# ##@Tilman: run from here
# 
# ##uncomment and run the line below
# # load("../data/Arrests.RData")
# #################################################################
# 
# # compute SPARR
# hvrr1 <- risk(crjit,h0=hd,tolerate=TRUE)  #using the Davies bandwidth
# hvrr2 <- risk(crjit,adapt=TRUE,h0=hd,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 
# hvrr1org <- risk(crppp,h0=hd,tolerate=TRUE) # repeat the above using the original/unjittered data so we can compare the effect of the jittering
# hvrr2org <- risk(crppp,adapt=TRUE,h0=hd,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 
# 
# #plot using Davies
# par(mfrow=c(2,2))
# plot(hvrr1,main="fixed; h=hd")
# plot(hvrr2,main="symmetric (pooled) adaptive; h0=hp=hd; data jittered")
# plot(hvrr1org,main="(fixed using unjittered data)")
# plot(hvrr2org,main="(adaptive using unjittered data)")
# 
# # repeat these four estimates, using a larger bandwidth (NS bandwidth)
# hvrr1 <- risk(crjit,h0=1790.382,tolerate=TRUE)  
# hvrr2 <- risk(crjit,adapt=TRUE,h0=1790.382,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 
# hvrr1org <- risk(crppp,h0=1790.382,tolerate=TRUE) # repeat the above using the original/unjittered data so we can compare the effect of the jittering
# hvrr2org <- risk(crppp,adapt=TRUE,h0=1790.382,tolerate=TRUE,pilot.symmetry="pooled", davies.baddeley=0.01) 
# 
# #plot using larger bandwidth
# par(mfrow=c(2,2))
# plot(hvrr1,main="fixed; h=1790.382")
# plot(hvrr2,main="symmetric (pooled) adaptive; h0=hp=1790.382; data jittered")
# plot(hvrr1org,main="(fixed using unjittered data)")
# plot(hvrr2org,main="(adaptive using unjittered data)")
# 
# #save.image(file='../data/Arrests.RData')



####### tilman's scratchpad ######


# # just playing with a fully continuous spatiotemporal rr
# range(as.numeric(dfjit$date))
# dfjit_0_pppst <- ppp(dfjit_0$X_COORD_CD, dfjit_0$Y_COORD_CD, marks=as.numeric(dfjit_0$date),window = w)
# dfjit_1_pppst <- ppp(dfjit_1$X_COORD_CD, dfjit_1$Y_COORD_CD, marks=as.numeric(dfjit_1$date),window = w)
# OS.spattemp(dfjit_0_pppst)
# OS.spattemp(dfjit_1_pppst)
# 
# t1 <- Sys.time()
# stden0 <- spattemp.density(dfjit_0_pppst,h=2000,lambda=14,tlim=c(19350,19730))
# stden1 <- spattemp.density(dfjit_1_pppst,h=2000,lambda=14,tlim=c(19350,19730))
# t2 <- Sys.time()
# t2 - t1  # around 5 mins
# 
# dev.new()
# plot(stden0,sleep=0.01)
# Sys.sleep(1)
# plot(stden1,sleep=0.01)
# dev.off()
# 
# t1 <- Sys.time()
# strr <- spattemp.risk(stden1,stden0,tolerate=TRUE)
# t2 <- Sys.time()
# t2 - t1  # around 5 mins
# 
# dev.new()
# plot(strr,sleep=0.1,fix.range=TRUE)
# dev.off()

