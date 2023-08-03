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

whichk<-"8k"
whichk<-"12k"
whichk<-"4k"

########################################################
########## cost maps - what is the min and max overall #
########################################################

# grand max = max of all
# grand min = 0

# get pairs
read.csv("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
glotto[glotto$Island_check%in%1,]$EAsoc2->issoc
load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0 (same origins for dfi2500)

## Temp maxall & distribution
load(file=paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/TempHarshness_raster_",whichk,".rds")) # values already with -1 etc
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
maxall
length(distr)
save(distr, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/peopleinspace_costdistrib_maps/CostDistrTemp_",whichk,".rds"))

load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/peopleinspace_costdistrib_maps/CostDistrTemp_",whichk,".rds"))
hist(distr)
range(distr) ; mean(distr) ; sd(distr)
quantile(distr,  probs = c(0.1, 0.5, 1, 2, 5, 10, 25, 50, 75,95)/100)
# whichk = 8k
  # 0.1%        0.5%          1%          2%          5%         10%         25%         50%         75%         95% 
  # 0.001281373 0.006431451 0.012887188 0.025873698 0.065437155 0.135615544 0.380125264 1.121783649 2.033358643 2.686658850 

# whichk = 12k 
  # 0.1%        0.5%          1%          2%          5%         10%         25%         50%         75%         95% 
  # 0.001337209 0.006733593 0.013482183 0.027011575 0.068214172 0.140312520 0.386699319 1.151967758 2.047065252 2.633275676 

# whichk = 4k 
  # 0.1%        0.5%          1%          2%          5%         10%         25%         50%         75%         95% 
  # 0.001291926 0.006525597 0.013076003 0.026196498 0.066218187 0.137264977 0.384164316 1.111331811 2.023322710 2.703967607 


## Xeric maxall & distribution

load(file=paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/XericHarshness_raster_", whichk,".rds")) # values already with -1 etc
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
maxall
length(distr)
save(distr, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/peopleinspace_costdistrib_maps/CostDistrXeric_",whichk,".rds"))

load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/peopleinspace_costdistrib_maps/CostDistrXeric_",whichk,".rds"))
hist(distr)
range(distr) ; mean(distr) ; sd(distr)
quantile(distr,  probs = c(0.1, 0.5, 1, 2, 5, 10, 25, 50, 75,95)/100)
# whichk = 8k
  # 0.1%         0.5%           1%           2%           5%          10%          25%          50%          75%          95% 
  # 0.0009266606 0.0046572674 0.0093029743 0.0185868497 0.0464783927 0.0932516440 0.2357117210 0.4931856832 0.8807587653 2.4899366869 

# whichk = 12k 
  # 0.1%         0.5%           1%           2%           5%          10%          25%          50%          75%          95% 
  # 0.0007918062 0.0040214071 0.0080547336 0.0161337868 0.0405217564 0.0813741632 0.2068157992 0.4456729265 0.8421002036 2.4406022689 

# whichk = 4k 
  # 0.1%         0.5%           1%           2%           5%          10%          25%          50%          75%          95% 
  # 0.0008735388 0.0044108630 0.0088607612 0.0177930346 0.0445149720 0.0888889668 0.2251533774 0.4755704810 0.8708613768 2.4661337581 

# ##############################
# ########## maps of polygons ## temperature
# ##############################
# 
# rm(list=ls())
# 
# require(geosphere)
# library(gstat)
# require(rworldmap)
# require(classInt)
# require(RgoogleMaps)
# require(GISTools)
# library(RColorBrewer)
# require(maps)
# require(letsR)
# 
# require(rgdal)
# library(sf)
# library(raster)
# library(rgeos)
# library(maptools)
# library(purrr)
# 
# list.files("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceTemp5QT/")->lfiles
# lfiles<-lfiles[!lfiles%in%c("nonb.rds","multiple0.rds","mtargetgr.rds")]
# 
# read.csv("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
# load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_0_noislands.rda") 
# # load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_2500_noislands.rda") 
# # soci1<-unique(as.character(dfi0$soci)) # dfi0 (same origins for dfi2500) but not same destinations - because get closest 100 nb THEN weed out societies - this way it is not the case that societies close to water will have disproportionately further away nbs, will just have fewer nbs (if close by island socs are available)
# # soci12<-unique(as.character(dfi2500$soci))
# 
# # some chekc-ups  
# # glotto[!glotto$oragr_which%in%-999,]->gor
# # table(gor$oragr_which) # includes isladnds and NewGuinea etc
# # soci1<-unique(as.character(dfi0$soci))
# # table(glotto[glotto$EAsoc2%in%soci1,]$oragr_which) # all have an origin: OK
# # a<-dfi0[!duplicated(dfi0$soci),]
# # b<-glotto[glotto$EAsoc2%in%soci1,]
# # a<-a[match(b$EAsoc2, a$soci),]
# # plot(a$lat~b$EAlat)
# # plot(a$long~b$EAlong) # same
# 
# load(file="/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceTemp5QT/nonb.rds")
# load(file="/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/TempHarshness_raster.rds") # values already with -1 etc
# TempHarshness_rasterscaled<-TempHarshness_raster # # scale them: (xi-xmean) / SD (i.e function scale) 
# values(TempHarshness_rasterscaled)<-scale(values(TempHarshness_raster))
# 
# soci1<-unique(as.character(dfi0$soci))
# dfi0[dfi0$soci%in%nonb,]$oragr # Mesoamerica
# # sort done by area
# namesC<-c("West Africa T", "Sudanic_Savan","W_African_Sav", "Ethipian plat",
#           "E_North_Ameri",
#           "Mesoamerica",
#           "Southwes amaz" ,"C/S_Andes","NW_Lowland_SA", "N_Lowland_SA" ,
#           "Fertile_Cresc" , "W_Yuman_E_Tib", "Chinese_loess", "South trop ch" , "Ganges_E_Indi" , "Lower-MiddleY"  , "Sava_W_India" ,"S_India")
# namesC%in%dfi0$oragr
# 
# done<-lfiles
# names(done)<-rep(-999, length(done))
# for(i in 1: length(done))
# { socO<-strsplit(done[i], split=".rds")[[1]][1]
# names(done)[i]<-dfi0[dfi0$soci%in%socO,]$oragr
# #print(i)
# }
# -999%in%done #F
# table(names(done))
# table(dfi0$oragr_which)
# 
# ordered_done<-c(done[names(done)%in%"West Africa T"], done[names(done)%in%"Sudanic_Savan"],done[names(done)%in%"W_African_Sav"],done[names(done)%in%"Ethipian plat"],
#                 done[names(done)%in%"E_North_Ameri"],
#                 done[names(done)%in%"Mesoamerica"],
#                 done[names(done)%in%"Southwes amaz"],done[names(done)%in%"C/S_Andes"],done[names(done)%in%"NW_Lowland_SA"],done[names(done)%in%"N_Lowland_SA"],
#                 done[names(done)%in%"Fertile_Cresc"],
#                 done[names(done)%in%"W_Yuman_E_Tib"],done[names(done)%in%"Chinese_loess"],done[names(done)%in%"South trop ch"],done[names(done)%in%"Ganges_E_Indi"],
#                 done[names(done)%in%"Lower-MiddleY"],done[names(done)%in%"Sava_W_India"],done[names(done)%in%"S_India"])
# #ordered_done<-unlist(ordered_done)
# table(names(ordered_done))
# 
# 
# threshold<-0.099262461
# pdf( paste0("data/Re_analysisGEbarriers/P2_datamodel/SocSimE_mapTemp.pdf"), height=10, width=10)
# for(i in 1: length(ordered_done))
# { load(paste0("data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceTemp5QT/", ordered_done[i]))
#   
#   socO<-strsplit(ordered_done[i], split="_")[[1]][1]
#   coordsO<-data.frame(lon=glotto[glotto$EAsoc2%in%socO,]$EAlong, lat=glotto[glotto$EAsoc2%in%socO,]$EAlat)
#   coordinates(coordsO)<-c("lon","lat")
#   crs(coordsO)<-crs(TempHarshness_rasterscaled)
#   
#   valueO<-raster::extract(x=TempHarshness_rasterscaled, y=coordsO)
#   raster_costO<-abs(TempHarshness_rasterscaled - valueO)
#   #plot(raster_costO)
#   values(raster_costO)[values(raster_costO)>=threshold]<-NA # 5% q
#   #plot(raster_costO)
#   
#   maps::map(database = "world", fill=F, col="light gray", border="gray")
#   plot(raster_costO,add=T)
#   plot(unifiedPolygons2,add=T, col="red")
#   abline(h=coordinates(raster_costO)[which(values(raster_costO)%in%0),][2], lwd=0.5, lty=3) ; abline(v=coordinates(raster_costO)[which(values(raster_costO)%in%0),][1],lwd=0.5, lty=3)
#   title(main=paste(socO, " ", names(ordered_done)[i]))
#   print(i)
# }
# dev.off()
# 
# 
# 
# ## figure for supplement (pannel A)
# 
# rm(list=ls())
# 
# require(geosphere)
# library(gstat)
# require(rworldmap)
# require(classInt)
# require(RgoogleMaps)
# require(GISTools)
# library(RColorBrewer)
# require(maps)
# require(letsR)
# 
# require(rgdal)
# library(sf)
# library(raster)
# library(rgeos)
# library(maptools)
# library(purrr)
# 
# list.files("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceTemp5QT/")->lfiles
# lfiles<-lfiles[!lfiles%in%c("nonb.rds","multiple0.rds","mtargetgr.rds")]
# 
# read.csv("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
# load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_0_noislands.rda") 
# 
# load(file="/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceTemp5QT/nonb.rds")
# load(file="/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/TempHarshness_raster.rds") # values already with -1 etc
# TempHarshness_rasterscaled<-TempHarshness_raster # # scale them: (xi-xmean) / SD (i.e function scale) 
# values(TempHarshness_rasterscaled)<-scale(values(TempHarshness_raster))
# 
# done<-lfiles
# names(done)<-rep(-999, length(done))
# for(i in 1: length(done))
# { socO<-strsplit(done[i], split=".rds")[[1]][1] ; names(done)[i]<-dfi0[dfi0$soci%in%socO,]$oragr}
# -999%in%done #F
# 
# which(done%in%"acat1239.rds")
# socO<-"acat1239"
# socO<-"sanf1262"
# 
# load(paste0("data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceTemp5QT/",socO,".rds"))
# 
# coordsO<-data.frame(lon=glotto[glotto$EAsoc2%in%socO,]$EAlong, lat=glotto[glotto$EAsoc2%in%socO,]$EAlat)
# coordinates(coordsO)<-c("lon","lat")
# crs(coordsO)<-crs(TempHarshness_rasterscaled)
# 
# valueO<-raster::extract(x=TempHarshness_rasterscaled, y=coordsO)
# raster_costO<-abs(TempHarshness_rasterscaled - valueO)
# 
# threshold<-0.099262461
# values(raster_costO)[values(raster_costO)>=threshold]<-NA # 5% q
# 
# pdf("data/Re_analysisGEbarriers/P2_datamodel/temp_ex.pdf", height = 5, width=7)
# par(mfrow=c(1,2))
# a<-maps::map(database = "world",xlim=c(-130,-45), ylim = c(-25,75), fill=T, col=scales::alpha("gray90", 1), border="gray90")
# mtext("A", side=3, adj=0, font=2,cex=1.5, line=+0.5)#ps=9,
# #title(main=paste("Acatepec Me'phaa", " "))
# #a<-maps::map(database = "world",xlim=c(-175,175), ylim = c(-75,75), fill=T, col=scales::alpha("gray90", 1), border="gray90")
# plot(raster_costO,add=T, legend=F, col="black")
# plot(unifiedPolygons2,add=T, col="red", border="red")
# plot(coordsO,cex=1,add=T)
# 
# load(file="data/Re_analysisGEbarriers/P2_datamodel/CostDistrTemp.rds")
# a<-hist(distr, main="", xlab="Temperature differences", col="gray90",border="gray90",axes=F)
# axis(side=2, tick=F,at=c(0,1800000,3000000), labels=c(0,"1.8e+06","3e+06"))  # 3 left
# axis(side=1, tick=F,at=c(0.000000, 1.74131,3.482619), labels=c(0,1.75,3.5))  
# abline(v=threshold, col="red", lwd=2)
# text("5% quantile", x=threshold+0.7, y=max(a$counts))
# mtext("B", side=3, adj=0, font=2,cex=1.5, line=+0.5)#ps=9,
# 
# dev.off()
# 
# ##############################
# ########## maps of polygons ## xeric
# ##############################
# 
# rm(list=ls())
# 
# require(geosphere)
# library(gstat)
# require(rworldmap)
# require(classInt)
# require(RgoogleMaps)
# require(GISTools)
# library(RColorBrewer)
# require(maps)
# require(letsR)
# 
# require(rgdal)
# library(sf)
# library(raster)
# library(rgeos)
# library(maptools)
# library(purrr)
# 
# list.files("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceXeric5QT/")->lfiles
# lfiles<-lfiles[!lfiles%in%c("nonb.rds","multiple0.rds","mtargetgr.rds")]
# 
# read.csv("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
# load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0 (same origins for dfi2500)
# 
# load(file="/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceXeric5QT/nonb.rds")
# load(file="/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/XericHarshness_raster.rds") # values already with -1 etc
# XericHarshness_rasterscaled<-XericHarshness_raster # # scale them: (xi-xmean) / SD (i.e function scale) 
# values(XericHarshness_rasterscaled)<-scale(values(XericHarshness_raster))
# 
# soci1<-unique(as.character(dfi0$soci))
# unique(dfi0[dfi0$soci%in%nonb,]$oragr) # "Mesoamerica"   "Fertile_Cresc" "C/S_Andes"     "Sudanic_Savan" "W_Yuman_E_Tib" "Ethipian plat" "N_Lowland_SA"
# # sort done by area
# namesC<-c("West Africa T", "Sudanic_Savan","W_African_Sav", "Ethipian plat",
#           "E_North_Ameri",
#           "Mesoamerica",
#           "Southwes amaz" ,"C/S_Andes","NW_Lowland_SA", "N_Lowland_SA" ,
#           "Fertile_Cresc" , "W_Yuman_E_Tib", "Chinese_loess", "South trop ch" , "Ganges_E_Indi" , "Lower-MiddleY"  , "Sava_W_India" ,"S_India")
# namesC%in%dfi0$oragr
# 
# done<-lfiles
# names(done)<-rep(-999, length(done))
# for(i in 1: length(done))
# { socO<-done[i]
# names(done)[i]<-dfi0[dfi0$soci%in%socO,]$oragr
# #print(i)
# }
# -999%in%done #F
# table(names(done))
# table(dfi0$oragr)
# 
# ordered_done<-c(done[names(done)%in%"West Africa T"], done[names(done)%in%"Sudanic_Savan"],done[names(done)%in%"W_African_Sav"],done[names(done)%in%"Ethipian plat"],
#                 done[names(done)%in%"E_North_Ameri"],
#                 done[names(done)%in%"Mesoamerica"],
#                 done[names(done)%in%"Southwes amaz"],done[names(done)%in%"C/S_Andes"],done[names(done)%in%"NW_Lowland_SA"],done[names(done)%in%"N_Lowland_SA"],
#                 done[names(done)%in%"Fertile_Cresc"],
#                 done[names(done)%in%"W_Yuman_E_Tib"],done[names(done)%in%"Chinese_loess"],done[names(done)%in%"South trop ch"],done[names(done)%in%"Ganges_E_Indi"],
#                 done[names(done)%in%"Lower-MiddleY"],done[names(done)%in%"Sava_W_India"],done[names(done)%in%"S_India"])
# #ordered_done<-unlist(ordered_done)
# table(names(ordered_done))
# 
# 
# threshold<-0.058625596
# pdf( paste0("data/Re_analysisGEbarriers/P2_datamodel/SocSimE_mapXeric.pdf"), height=10, width=10)
# for(i in 1: length(ordered_done))
# { load(paste0("data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceXeric5QT/", ordered_done[i]))
#   
#   socO<-strsplit(ordered_done[i], split=".rds")[[1]][1]
#   coordsO<-data.frame(lon=glotto[glotto$EAsoc2%in%socO,]$EAlong, lat=glotto[glotto$EAsoc2%in%socO,]$EAlat)
#   coordinates(coordsO)<-c("lon","lat")
#   crs(coordsO)<-crs(XericHarshness_rasterscaled)
#   
#   valueO<-raster::extract(x=XericHarshness_rasterscaled, y=coordsO)
#   raster_costO<-abs(XericHarshness_rasterscaled - valueO)
#   #plot(raster_costO)
#   values(raster_costO)[values(raster_costO)>=threshold]<-NA # 5% q
#   #plot(raster_costO)
#   
#   maps::map(database = "world", fill=F, col="light gray", border="gray")
#   plot(raster_costO,add=T)
#   plot(unifiedPolygons2,add=T, col="red")
#   abline(h=coordinates(raster_costO)[which(values(raster_costO)%in%0),][2], lwd=0.5, lty=3) ; abline(v=coordinates(raster_costO)[which(values(raster_costO)%in%0),][1],lwd=0.5, lty=3)
#   title(main=paste(socO, " ", names(ordered_done)[i]))
#   print(i)
# }
# dev.off()
# 
# 
# 
# ## figure for supplement (pannel B)
# 
# rm(list=ls())
# 
# require(geosphere)
# library(gstat)
# require(rworldmap)
# require(classInt)
# require(RgoogleMaps)
# require(GISTools)
# library(RColorBrewer)
# require(maps)
# require(letsR)
# 
# require(rgdal)
# library(sf)
# library(raster)
# library(rgeos)
# library(maptools)
# library(purrr)
# 
# list.files("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceXeric5QT/")->lfiles
# lfiles<-lfiles[!lfiles%in%c("nonb.rds","multiple0.rds","mtargetgr.rds")]
# 
# read.csv("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
# load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0 (same origins for dfi2500)
# 
# load(file="/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceXeric5QT/nonb.rds")
# load(file="/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/XericHarshness_raster.rds") # values already with -1 etc
# XericHarshness_rasterscaled<-XericHarshness_raster # # scale them: (xi-xmean) / SD (i.e function scale) 
# values(XericHarshness_rasterscaled)<-scale(values(XericHarshness_raster))
# 
# done<-lfiles
# 
# which(done%in%"acat1239.rds")
# socO<-"acat1239"
# socO<-"Nj8"
# socO<-"sanf1262"
# 
# load(paste0("data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceXeric5QT/",socO,".rds"))
# coordsO<-data.frame(lon=glotto[glotto$EAsoc2%in%socO,]$EAlong, lat=glotto[glotto$EAsoc2%in%socO,]$EAlat)
# coordinates(coordsO)<-c("lon","lat")
# crs(coordsO)<-crs(XericHarshness_rasterscaled)
# 
# valueO<-raster::extract(x=XericHarshness_rasterscaled, y=coordsO)
# raster_costO<-abs(XericHarshness_rasterscaled - valueO)
# 
# threshold<-0.058625596
# values(raster_costO)[values(raster_costO)>=threshold]<-NA # 5% q
# 
# pdf("data/Re_analysisGEbarriers/P2_datamodel/xeric_ex.pdf", height = 5, width=7)
# par(mfrow=c(1,2))
# a<-maps::map(database = "world",xlim=c(-130,-45), ylim = c(-25,75), fill=T, col=scales::alpha("gray90", 1), border="gray90")
# mtext("C", side=3, adj=0, font=2,cex=1.5, line=+0.5)#ps=9,
# #title(main=paste("Acatepec Me'phaa", " "))
# #a<-maps::map(database = "world",xlim=c(-175,175), ylim = c(-75,75), fill=T, col=scales::alpha("gray90", 1), border="gray90")
# plot(raster_costO,add=T, legend=F, col="black")
# plot(unifiedPolygons2,add=T, col="red", border="red")
# plot(coordsO,cex=1,add=T)
# 
# load(file="data/Re_analysisGEbarriers/P2_datamodel/CostDistrXeric.rds")
# a<-hist(distr, main="", xlab="Aridity differences", col="gray90",border="gray90",axes=F)
# axis(side=2, tick=F,at=c(0,6000000,10000000), labels=c(0,"6e+06","12e+06"))  # 3 left
# axis(side=1, tick=F,at=c(0.000000, 2.5,5), labels=c(0,2.5,5.0))  
# abline(v=threshold, col="red", lwd=2)
# text("5% quantile", x=threshold+1.5, y=max(a$counts))
# mtext("D", side=3, adj=0, font=2,cex=1.5, line=+0.5)#ps=9,
# 
# dev.off()


# ##############################
# ########## build dfs ######### temp and xeric - to be ran after the calculations are done
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

whichk<-"8k"
whichk<-"12k"
whichk<-"4k"

read.csv("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
glotto[glotto$Island_check%in%1,]$EAsoc2->issoc
coords<-data.frame(lon=glotto$EAlong, lat=glotto$EAlat)
coordinates(coords)<-c("lon","lat")
load(paste0("data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceTemp5QT/aban1242.rds"))
proj4string(coords)<-proj4string(unifiedPolygons2)
load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0 (same origins for dfi2500)
soci1<-unique(as.character(dfi0$soci))
dfSocSimE<-data.frame("soci"=soci1)


# TEMP

load(file=paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/peopleinspaceTemp5QT_",whichk,"/nonb.rds"))
## NONB should not be excluded, NONB shuld be 0 !! 0 socs in 5%

dfSocSimE$NBsimchv1_temp<--999
dfSocSimE$NBsimchv2_temp<--999
for(i in 1:length(soci1))
  {socO<-as.character(soci1)[i]
  if(socO%in%nonb) {dfSocSimE[dfSocSimE$soci%in%socO,]$NBsimchv1_temp<- 0 ; dfSocSimE[dfSocSimE$soci%in%socO,]$NBsimchv2_temp<- 0 }
  if(!socO%in%nonb & paste0(socO,".rds")%in%list.files(paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/peopleinspaceTemp5QT_",whichk,"/"))) # if not there - no Envdata
    { load(paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/peopleinspaceTemp5QT_",whichk,"/", socO,".rds"))
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
      
      # # beyond 2500km? - yes but in which dir? does it count to have 500 socs 2500 away in the E dir for the W spread??
      # v1l_2500<-unique(glotto[!is.na(over(coords,unifiedPolygons2)),]$EAsoc2)
      # cO<-c(glotto[glotto$EAsoc2%in%socO,]$EAlong,glotto[glotto$EAsoc2%in%socO,]$EAlat)
      # #cD<-cbind(glotto[glotto$EAsoc2%in%v1l_2500,]$EAlong,glotto[glotto$EAsoc2%in%v1l_2500,]$EAlat)
      # 
      dfSocSimE$NBsimchv1_temp[i]<-v1l
      dfSocSimE$NBsimchv2_temp[i]<-v2l
    }
  print(i)
}


# XERIC 
load(file=paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/peopleinspaceXeric5QT_",whichk,"/nonb.rds"))
## NONB should not be excluded, NONB shuld be 0 !! 0 socs in 5%

dfSocSimE$NBsimchv1_xeric<--999
dfSocSimE$NBsimchv2_xeric<--999
for(i in 1:length(soci1))
  {socO<-as.character(soci1)[i]
  if(socO%in%nonb) {dfSocSimE[dfSocSimE$soci%in%socO,]$NBsimchv1_xeric<- 0 ; dfSocSimE[dfSocSimE$soci%in%socO,]$NBsimchv2_xeric<- 0 }
  if(!socO%in%nonb & paste0(socO,".rds")%in%list.files("data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceXeric5QT/")) # if not there - no Envdata
    { load(paste0("data/Re_analysisGEbarriers/P2_datamodel/peopleinspaceXeric5QT/",socO,".rds"))
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

write.csv(dfSocSimE, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/peopleinspace_costdistrib_maps/dfSocSimE_", whichk,".csv"))

