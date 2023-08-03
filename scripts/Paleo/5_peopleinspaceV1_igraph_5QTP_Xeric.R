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

library(igraph)
library(spdep)

#######################
########## rcost maps # xeric
#######################

# get pairs
read.csv("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
glotto[glotto$Island_check%in%1,]$EAsoc2->issoc
load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0 (same origins for dfi2500)

whichk<-"8k" ; threshold5q<-0.0464783927
whichk<-"12k" ; threshold5q<-0.0405217564
whichk<-"4k" ; threshold5q<-0.0445149720
load(file=paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/XericHarshness_raster_", whichk,".rds")) # values already with -1 etc
TempHarshness_rasterscaled<-XericHarshness_raster # # scale them: (xi-xmean) / SD (i.e function scale) 
values(TempHarshness_rasterscaled)<-scale(values(XericHarshness_raster))

## FOR SiMPLICITY OF code copied from temperature code: named TempHarshness_rasterscaled what in reality is xeric

soci1<-unique(as.character(dfi0$soci))

fxnx<-function(x, polO) {to_ret<-0 ; if(polO%in%x) to_ret<-1 ; return(to_ret)}

multiple0<-list()
nonb<-list()
mtargetgr<-list()
i=1
for(i in 1: length(soci1))
  {  socO<-soci1[i]
  coordsO<-data.frame(lon=glotto[glotto$EAsoc2%in%socO,]$EAlong, lat=glotto[glotto$EAsoc2%in%socO,]$EAlat)
  coordinates(coordsO)<-c("lon","lat")
  crs(coordsO)<-crs(TempHarshness_rasterscaled)
  
  valueO<-raster::extract(x=TempHarshness_rasterscaled, y=coordsO)
  if(!is.na(valueO))
    {raster_costO<-abs(TempHarshness_rasterscaled - valueO)
    values(raster_costO)[values(raster_costO)>=threshold5q]<-NA # 5% q
    if(length(coordinates(raster_costO)[which(values(raster_costO)%in%0),])>2) multiple0<-c(multiple0,soci1[i])
    if(length(coordinates(raster_costO)[which(values(raster_costO)%in%0),])==2) # if ok
    {print(paste("rtoP",print(Sys.time())))
      pol25 <- rasterToPolygons(raster_costO, fun=function(x){!is.na(x)}, dissolve = T) # time-consuming fxn1
      print(Sys.time())
      
      # select polygon with origin
      polO<-(1: length(pol25))[pol25$layer%in%0] # add condition if more than one with 0
      
      # nb
      nb <- poly2nb(pol25) # time-consuming fxn2
      mat <- nb2mat(nb, style="B",zero.policy=T) # max[n,n] is 0, but it is ok, the group of adjacent pols will contain origin polygon
      colnames(mat) <- rownames(mat)
      if(length(mat[polO,][mat[polO,]%in%1])<1) nonb<-c(nonb,soci1[i])
      if(length(mat[polO,][mat[polO,]%in%1])>1) 
      {
        g  <- graph.adjacency(mat)
        clu <- components(g)
        grclu<-igraph::groups(clu)
        targegtgrclu<-grclu[ lapply(grclu,fxnx, polO=polO)%in%1]
        if(length(targegtgrclu)>1) mtargetgr<-c(mtargetgr,soci1[i])
        if(length(targegtgrclu)==1) 
        {touni<-as.numeric(unlist(targegtgrclu))
        unifiedPolygons2 <- aggregate(pol25[touni,],dissolve=T) # save & follow area of origin societies to find out how many societies from glottolog fall into this polygon
        save(unifiedPolygons2, file=paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/peopleinspaceXeric5QT_", whichk,"/", soci1[i], ".rds"))
        } # from mtargetgr
      } # from nonb
      
    }# from multiple0
    } #     if(!is.na(valueO))
  print(paste(i,"/n",print(Sys.time())))}


nonb<-unlist(nonb)
multiple0<-unlist(multiple0)
mtargetgr<-unlist(mtargetgr)

save(nonb, file=paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/peopleinspaceXeric5QT_", whichk,"/nonb.rds"))
save(multiple0, file=paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/peopleinspaceXeric5QT_", whichk,"/multiple0.rds"))
save(mtargetgr, file=paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/peopleinspaceXeric5QT_", whichk,"/mtargetgr.rds"))

