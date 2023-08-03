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

load(file="data/Re_analysisGEbarriers/XericHarshness_raster.rds") # values already with -1 etc
# # scale them: (xi-xmean) / SD (i.e function scale) 
XericHarshness_rasterscaled<-XericHarshness_raster
values(XericHarshness_rasterscaled)<-scale(values(XericHarshness_raster))


soci1<-unique(as.character(dfi0$soci))

for(i in 1:length(soci1))
  {   socO<-soci1[i]
  socDs<-as.character(dfi0[dfi0$soci%in%socO,]$nb)
  
  coordsO<-data.frame(lon=glotto[glotto$EAsoc2%in%socO,]$EAlong, lat=glotto[glotto$EAsoc2%in%socO,]$EAlat)
  coordinates(coordsO)<-c("lon","lat")
  crs(coordsO)<-crs(XericHarshness_rasterscaled)
  
  gd<-glotto[glotto$EAsoc2%in%socDs,]
  gd<-gd[match(socDs, gd$EAsoc2),] # same order as socDs
  coordsD<-data.frame(lon=gd$EAlong, lat=gd$EAlat)
  coordinates(coordsD)<-c("lon","lat")
  crs(coordsD)<-crs(XericHarshness_rasterscaled)
  
  valueO<-raster::extract(x=XericHarshness_rasterscaled, y=coordsO)
  if(!is.na(valueO))
  {raster_costO<-abs(XericHarshness_rasterscaled - valueO)
  print(paste("starting:",socO))
  result <- movecostAngela_vOCTDontAdapt_multdestin(dtm=raster_costO,origin=coordsO, destin=coordsD) # list(sPath=sPath,mcost=mcost, plen=plen)
  
  # add list of Ds
  result$namesDs<-socDs
  
  save(result, file=paste0("data/Re_analysisGEbarriers/P2_xeric2500/xeric_", socO,".rds"))
  print(paste("done:",socO))
  }
  
  print(i)
  
}




