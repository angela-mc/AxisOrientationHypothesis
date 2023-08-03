rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)
source("scripts/10_rLCP_MCostFXN.R")
source("scripts/1getdata_fxn.R")


whichk<-"8k"
whichk<-"12k"
whichk<-"4k"

######################
####### dfbarr ####### dfi0
######################

load("data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0
read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfP2barriers0.csv"), stringsAsFactors = F)->dfB0 # barriers
read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/peopleinspace_costdistrib_maps/dfSocSimE_", whichk,".csv"), stringsAsFactors = F)->dfSocSimE # SocSimE
read.csv(file = paste0("data/Re_analysisGEbarriers/paleoclim2021/P2EnvDis_", whichk,"/dfi0ED.csv"), stringsAsFactors = F)->dfEDiss

dfEDiss<-dfEDiss[(!dfEDiss$edTemp%in%-999 & !dfEDiss$edXeric%in%-999),]

c_socs<-intersect(dfi0$pair_soc_unsorted, dfEDiss$pair_soc_unsorted)
c_socs<-intersect(c_socs, dfB0$pairsoc_unsor)

dfEDiss<-dfEDiss[dfEDiss$pair_soc_unsorted%in%c_socs,]
dfB0<-dfB0[dfB0$pairsoc_unsor%in%c_socs,]
dfB0<-dfB0[match(dfEDiss$pair_soc_unsorted, dfB0$pairsoc_unsor),]

dfB0$edTemp<-dfEDiss$edTemp
dfB0$edXeric<-dfEDiss$edXeric

# add SocSimE
dfB0$NBsimchv1_temp<- -999
dfB0$NBsimchv1_xeric<- -999

for(i in 1:length(dfSocSimE[,1])) # per origin
  if(dfSocSimE$soci[i]%in%dfB0$Soci) # if it was not pruned out by lack of other barriers
  { socO<-dfSocSimE$soci[i]
  dfB0[dfB0$Soci%in%socO,]$NBsimchv1_temp<-dfSocSimE$NBsimchv1_temp[i]
  dfB0[dfB0$Soci%in%socO,]$NBsimchv1_xeric<-dfSocSimE$NBsimchv1_xeric[i]
  print(i)
  }

dfB0<-dfB0[!dfB0$NBsimchv1_temp%in%-999 & !dfB0$NBsimchv1_xeric%in%-999,]
colnames(dfB0)

dfi0<-dfi0[dfi0$pair_soc_unsorted%in%dfB0$pairsoc_unsor,]
dfi0<-dfi0[match(dfB0$pairsoc_unsor, dfi0$pair_soc_unsorted),]
dfB0$GeoD<-dfi0$dist
dfB0$oragr<-dfi0$oragr
dfB0$oragr_whichSOC2<-dfi0$oragr_whichSOC2

write.csv(dfB0, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfB0_NOCLIMSIMNB.csv"))



# ######################
# ####### dfbarr ####### dfi2500 - see the other one for this one
# ######################
# 
