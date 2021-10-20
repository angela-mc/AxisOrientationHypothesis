rm(list=ls())

require(geosphere)
library(gstat)
require(rworldmap)
require(classInt)
require(RgoogleMaps)
require(GISTools)
library(RColorBrewer)
require(maps)
require(letsR)

require(rgdal)
library(sf)
library(raster)
library(rgeos)
library(maptools)
library(purrr)

########################################################
########## cost maps - what is the min and max overall #
########################################################

# grand max = max of all
# grand min = 0

# get pairs
read.csv("data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
glotto[glotto$Island_check%in%1,]$EAsoc2->issoc
load("data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0 (same origins for dfi2500)

## Temp maxall & distribution

load(file="data/Re_analysisGEbarriers/TempHarshness_raster.rds") # values already with -1 etc
# # scale them: (xi-xmean) / SD (i.e function scale) 
TempHarshness_rasterscaled<-TempHarshness_raster
values(TempHarshness_rasterscaled)<-scale(values(TempHarshness_raster))

soci1<-unique(as.character(dfi0$soci))
rm(maxall) ; rm(distr)
for(i in 1: length(soci1))
  {  socO<-soci1[i]
  coordsO<-data.frame(lon=glotto[glotto$EAsoc2%in%socO,]$EAlong, lat=glotto[glotto$EAsoc2%in%socO,]$EAlat)
  coordinates(coordsO)<-c("lon","lat")
  crs(coordsO)<-crs(TempHarshness_rasterscaled)
  
  valueO<-raster::extract(x=TempHarshness_rasterscaled, y=coordsO)
  if(!is.na(valueO))
  {raster_costO<-abs(TempHarshness_rasterscaled - valueO)
  if(i==1) {maxall<-max(values(raster_costO), na.rm=T)
  distr<-values(raster_costO)[!is.na(values(raster_costO))]} # i=1 has a nonNA value
  if(i>1)  {if(max(values(raster_costO), na.rm=T)>maxall) maxall<-max(values(raster_costO), na.rm=T)
  distr<-c(distr,values(raster_costO)[!is.na(values(raster_costO))] )}
  }
  print(i)
}
maxall # 3.482619
length(distr)
#save(distr, file="data/Re_analysisGEbarriers/P2_datamodel/CostDistrTemp.rds")

load(file="data/Re_analysisGEbarriers/P2_datamodel/CostDistrTemp.rds")
hist(distr)
range(distr) ; mean(distr) ; sd(distr)
quantile(distr,  probs = c(0.1, 0.5, 1, 2, 5, 10, 25, 50, 75,95)/100)
# 0.1%        0.5%          1%          2%          5%         10%         25%         50%         75%         95% 
# 0.001946615 0.009869271 0.019776243 0.039522438 0.099262461 0.201266327 0.554651811 1.325543953 2.123251637 2.805729618 


## Xeric maxall & distribution

load(file="data/Re_analysisGEbarriers/XericHarshness_raster.rds") # values already with -1 etc
# # scale them: (xi-xmean) / SD (i.e function scale) 
XericHarshness_rasterscaled<-XericHarshness_raster
values(XericHarshness_rasterscaled)<-scale(values(XericHarshness_raster))

soci1<-unique(as.character(dfi0$soci))
rm(maxall) ; rm(distr)
for(i in 1: length(soci1))
  {  socO<-soci1[i]
  coordsO<-data.frame(lon=glotto[glotto$EAsoc2%in%socO,]$EAlong, lat=glotto[glotto$EAsoc2%in%socO,]$EAlat)
  coordinates(coordsO)<-c("lon","lat")
  crs(coordsO)<-crs(XericHarshness_rasterscaled)
  
  valueO<-raster::extract(x=XericHarshness_rasterscaled, y=coordsO)
  if(!is.na(valueO))
  {raster_costO<-abs(XericHarshness_rasterscaled - valueO)
  if(i==1) {maxall<-max(values(raster_costO), na.rm=T)
  distr<-values(raster_costO)[!is.na(values(raster_costO))]} # i=1 has a nonNA value
  if(i>1)  {if(max(values(raster_costO), na.rm=T)>maxall) maxall<-max(values(raster_costO), na.rm=T)
  distr<-c(distr,values(raster_costO)[!is.na(values(raster_costO))] )}
  }
  print(i)
}
maxall #5.216021
length(distr)
save(distr, file="data/Re_analysisGEbarriers/P2_datamodel/CostDistrXeric.rds")

load(file="data/Re_analysisGEbarriers/P2_datamodel/CostDistrXeric.rds")
hist(distr)
range(distr) ; mean(distr) ; sd(distr)
quantile(distr,  probs = c(0.1, 0.5, 1, 2, 5, 10, 25, 50, 75,95)/100)
# 0.1%        0.5%          1%          2%          5%         10%         25%         50%         75%         95% 
# 0.001153409 0.005853877 0.011727415 0.023487223 0.058625596 0.117706632 0.300283651 0.659676293 1.206312944 2.581704984 

