rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)
source("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/scripts/10_rLCP_MCostFXN.R")

# get pairs
read.csv("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
glotto[glotto$Island_check%in%1,]$EAsoc2->issoc
load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_2500_noislands.rda") # dfi2500
dfi2500->dfi0 # so the code stays the same

load(file="/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/TempHarshness_raster.rds") # values already with -1 etc
# # scale them: (xi-xmean) / SD (i.e function scale) 
TempHarshness_rasterscaled<-TempHarshness_raster
values(TempHarshness_rasterscaled)<-scale(values(TempHarshness_raster))


soci1<-unique(as.character(dfi0$soci))

for(i in 1:length(soci1))
{   socO<-soci1[i]
socDs<-as.character(dfi0[dfi0$soci%in%socO,]$nb)

coordsO<-data.frame(lon=glotto[glotto$EAsoc2%in%socO,]$EAlong, lat=glotto[glotto$EAsoc2%in%socO,]$EAlat)
coordinates(coordsO)<-c("lon","lat")
crs(coordsO)<-crs(TempHarshness_rasterscaled)

gd<-glotto[glotto$EAsoc2%in%socDs,]
gd<-gd[match(socDs, gd$EAsoc2),] # same order as socDs
coordsD<-data.frame(lon=gd$EAlong, lat=gd$EAlat)
coordinates(coordsD)<-c("lon","lat")
crs(coordsD)<-crs(TempHarshness_rasterscaled)

valueO<-raster::extract(x=TempHarshness_rasterscaled, y=coordsO)
if(!is.na(valueO))
{raster_costO<-abs(TempHarshness_rasterscaled - valueO)
print(paste("starting:",socO))
result <- movecostAngela_vOCTDontAdapt_multdestin(dtm=raster_costO,origin=coordsO, destin=coordsD) # list(sPath=sPath,mcost=mcost, plen=plen)

# add list of Ds
result$namesDs<-socDs

save(result, file=paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/P2_temp2500/temp_", socO,".rds"))
print(paste("done:",socO))
}

print(i)

}




