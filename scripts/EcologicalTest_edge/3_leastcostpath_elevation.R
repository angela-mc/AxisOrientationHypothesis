rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)
source("scripts/10_rLCP_MCostFXN.R")

# get pairs
read.csv("data/latlong_soc_from_dplace-societies-2018-03-16.csv", stringsAsFactors = F)-> longlat
load("data/predictors/preds3april/dfiEA_noislands.rds") # list of pairs - input data
load("data/Re_analysisGEbarriers/Elev_raster.rds")

# need to do each soci with its nb but also reverse each nb with its nb
soci1<-unique(dfiEA$soci)

for(i in 1:length(soci1))
{   socO<-soci1[i]
    socDs<-as.character(dfiEA[dfiEA$soci%in%socO,]$nb)
    raster_costO<-Elev_raster
    
    coordsO<-data.frame(lon=longlat[longlat$Society.id%in%socO,]$Revised.longitude, lat=longlat[longlat$Society.id%in%socO,]$Revised.latitude)
    coordinates(coordsO)<-c("lon","lat")
    crs(coordsO)<-crs(raster_costO)
    
    gd<-longlat[longlat$Society.id%in%socDs,]
    gd<-gd[match(socDs, gd$Society.id),] # same order as socDs
    
    coordsD<- data.frame(lon=gd$Revised.longitude, lat=gd$Revised.latitude)
    coordinates(coordsD)<-c("lon","lat")
    crs(coordsD)<-crs(raster_costO)
    
    print(paste("starting:",socO))
    result <- movecost_elev(dtm=raster_costO,origin=coordsO, destin=coordsD) # list(sPath=sPath,mcost=mcost, plen=plen)
    
    # add list of Ds
    result$namesDs<-socDs
    
    save(result, file=paste0("data/Re_analysisGEbarriers/P1_elevation_soci/elevation_", socO,".rds"))
    print(paste("done:",socO))
    print(i)
  
}
  
