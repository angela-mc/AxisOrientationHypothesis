rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)
source("scripts/10_rLCP_MCostFXN.R")

# get pairs
read.csv("data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
glotto[glotto$Island_check%in%1,]$EAsoc2->issoc
load("data/originsagric/dfnb_100_threshold_2500_noislands.rda") # dfi2500
dfi2500->dfi0 # so the code stays the same
load(file="data/Re_analysisGEbarriers/Elev_raster.rds")

soci1<-unique(as.character(dfi0$soci))

for(i in 1:length(soci1))
  {   socO<-soci1[i]
  socDs<-as.character(dfi0[dfi0$soci%in%socO,]$nb)
  
  coordsO<-data.frame(lon=glotto[glotto$EAsoc2%in%socO,]$EAlong, lat=glotto[glotto$EAsoc2%in%socO,]$EAlat)
  coordinates(coordsO)<-c("lon","lat")
  crs(coordsO)<-crs(Elev_raster)
  
  gd<-glotto[glotto$EAsoc2%in%socDs,]
  gd<-gd[match(socDs, gd$EAsoc2),] # same order as socDs
  coordsD<-data.frame(lon=gd$EAlong, lat=gd$EAlat)
  coordinates(coordsD)<-c("lon","lat")
  crs(coordsD)<-crs(Elev_raster)
  
  raster_costO<-Elev_raster
  print(paste("starting:",socO))
  result <- movecost_elev(dtm=raster_costO,origin=coordsO, destin=coordsD) # list(sPath=sPath,mcost=mcost, plen=plen)
  
  # add list of Ds
  result$namesDs<-socDs
  
  plot(result$sPath)
  
  save(result, file=paste0("data/Re_analysisGEbarriers/P2_elevation2500/elevation_", socO,".rds"))
  print(paste("done:",socO))
  
  
  print(i)
  
  }




