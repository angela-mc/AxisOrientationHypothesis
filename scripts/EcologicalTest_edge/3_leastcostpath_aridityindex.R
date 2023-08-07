rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)
source("scripts/10_rLCP_MCostFXN.R")

# get pairs
read.csv("data/latlong_soc_from_dplace-societies-2018-03-16.csv", stringsAsFactors = F)-> longlat
load("data/dfiEA_noislands.rds") # list of pairs - input data

load(file="data/Re_analysisGEbarriers/XericHarshness_raster.rds") # values already with -1 
# # scale them: (xi-xmean) / SD (i.e function scale) 
XericHarshness_rasterscaled<-XericHarshness_raster
values(XericHarshness_rasterscaled)<-scale(values(XericHarshness_raster))


# need to do each soci with its nb but also reverse each nb with its nb
soci1<-unique(dfiEA$soci)

for(i in 1:length(soci1))
  {   socO<-soci1[i]
  socDs<-as.character(dfiEA[dfiEA$soci%in%socO,]$nb)
  
  coordsO<-data.frame(lon=longlat[longlat$Society.id%in%socO,]$Revised.longitude, lat=longlat[longlat$Society.id%in%socO,]$Revised.latitude)
  coordinates(coordsO)<-c("lon","lat")
  crs(coordsO)<-crs(XericHarshness_rasterscaled)
  
  gd<-longlat[longlat$Society.id%in%socDs,]
  gd<-gd[match(socDs, gd$Society.id),] # same order as socDs
  
  coordsD<- data.frame(lon=gd$Revised.longitude, lat=gd$Revised.latitude)
  coordinates(coordsD)<-c("lon","lat")
  crs(coordsD)<-crs(XericHarshness_rasterscaled)
  
  valueO<-raster::extract(x=XericHarshness_rasterscaled, y=coordsO)
  if(!is.na(valueO))
    {raster_costO<-abs(XericHarshness_rasterscaled - valueO)
    print(paste("starting:",socO))
    result <- movecostAngela_vOCTDontAdapt_multdestin(dtm=raster_costO,origin=coordsO, destin=coordsD) # list(sPath=sPath,mcost=mcost, plen=plen)
    
    # add list of Ds
    result$namesDs<-socDs
    
    save(result, file=paste0("data/Re_analysisGEbarriers/P1_xeric_soci/temp_", socO,".rds")) # named temp for simplicity (code duplicated from temperature analyses)
    print(paste("done:",socO))
    }
  
  print(i)
  
}




