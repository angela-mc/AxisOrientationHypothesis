
# ##############################
# ########## build dfs ######### temp and xeric
# ##############################

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


read.csv("data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
glotto[glotto$Island_check%in%1,]$EAsoc2->issoc
coords<-data.frame(lon=glotto$EAlong, lat=glotto$EAlat)
coordinates(coords)<-c("lon","lat")
load(paste0("data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceTemp5QT/aban1242.rds"))
proj4string(coords)<-proj4string(unifiedPolygons2)
load("data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0 (same origins for dfi2500)
soci1<-unique(as.character(dfi0$soci))
dfSocSimE<-data.frame("soci"=soci1)


# TEMP

load(file="data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceTemp5QT/nonb.rds")

dfSocSimE$NBsimchv1_temp<--999
dfSocSimE$NBsimchv2_temp<--999
for(i in 1:length(soci1))
  {socO<-as.character(soci1)[i]
  if(socO%in%nonb) {dfSocSimE[dfSocSimE$soci%in%socO,]$NBsimchv1_temp<- 0 ; dfSocSimE[dfSocSimE$soci%in%socO,]$NBsimchv2_temp<- 0 }
  if(!socO%in%nonb & socO%in%list.files("data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceTemp5QT/")) # if not there - no Envdata
    { load(paste0("data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceTemp5QT/",socO))
      pgeo2 <- spTransform(unifiedPolygons2, CRS('+proj=longlat +datum=WGS84'))
      ext2 <- floor(extent(pgeo2))
      rr2 <- raster(ext2, res=0.5)
      rr2 <- rasterize(pgeo2, rr2, field=1)
      
      #v1
      v1l<-length(unique(glotto[!is.na(over(coords,unifiedPolygons2)),]$EAsoc2))-1 # exclude self
      
      #v2
      vals<-raster::extract(x=rr2, y=coords)
      vals[is.na(vals)]<-0
      v2l<-length(unique(glotto$EAsoc2[vals%in%1]))-1
      
      dfSocSimE$NBsimchv1_temp[i]<-v1l
      dfSocSimE$NBsimchv2_temp[i]<-v2l
    }
  print(i)
}


# XERIC

load(file="data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceXeric5QT/nonb.rds")

dfSocSimE$NBsimchv1_xeric<--999
dfSocSimE$NBsimchv2_xeric<--999
for(i in 1:length(soci1))
  {socO<-as.character(soci1)[i]
  if(socO%in%nonb) {dfSocSimE[dfSocSimE$soci%in%socO,]$NBsimchv1_xeric<- 0 ; dfSocSimE[dfSocSimE$soci%in%socO,]$NBsimchv2_xeric<- 0 }
  if(!socO%in%nonb & socO%in%list.files("data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceXeric5QT/")) # if not there - no Envdata
    { load(paste0("data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceXeric5QT/",socO))
      pgeo2 <- spTransform(unifiedPolygons2, CRS('+proj=longlat +datum=WGS84'))
      ext2 <- floor(extent(pgeo2))
      rr2 <- raster(ext2, res=0.5)
      rr2 <- rasterize(pgeo2, rr2, field=1)
      
      #v1
      v1l<-length(unique(glotto[!is.na(over(coords,unifiedPolygons2)),]$EAsoc2))-1 # exclude self
      
      #v2
      vals<-raster::extract(x=rr2, y=coords)
      vals[is.na(vals)]<-0
      v2l<-length(unique(glotto$EAsoc2[vals%in%1]))-1
      
      dfSocSimE$NBsimchv1_xeric[i]<-v1l
      dfSocSimE$NBsimchv2_xeric[i]<-v2l
    }
  print(i)
}

write.csv(dfSocSimE, file="data/Re_analysisGEbarriers/P2_datamodel/dfSocSimE.csv")

