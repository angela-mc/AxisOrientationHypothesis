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

c_socs<-intersect(dfB0_current$pairsoc_unsor, dfB0_past$pairsoc_unsor)
dfB0_current<-dfB0_current[dfB0_current$pairsoc_unsor%in%c_socs,]
dfB0_past<-dfB0_past[dfB0_past$pairsoc_unsor%in%c_socs,]
dfB0_past<-dfB0_past[match(dfB0_current$pairsoc_unsor, dfB0_past$pairsoc_unsor),]

# colour areas
table(dfB0$oragr)
dfB0_past$cola<-"black"
dfB0_past[dfB0_past$oragr%in%"Chinese_loess",]$cola<-"coral"
dfB0_past[dfB0_past$oragr%in%"Fertile_Cresc",]$cola<-"orange" # FC bigger in the past, plot1
dfB0_past[dfB0_past$oragr%in%"Ganges_E_Indi",]$cola<-"chocolate"
dfB0_past[dfB0_past$oragr%in%"Lower-MiddleY",]$cola<-"brown"
dfB0_past[dfB0_past$oragr%in%"Lower-S_India",]$cola<-"darkred"
dfB0_past[dfB0_past$oragr%in%"Sava_W_India",]$cola<-"red"
dfB0_past[dfB0_past$oragr%in%"South trop ch",]$cola<-"coral3"
dfB0_past[dfB0_past$oragr%in%"W_Yuman_E_Tib",]$cola<-"brown3"

dfB0_past[dfB0_past$oragr%in%"E_North_Ameri",]$cola<-"blue"
dfB0_past[dfB0_past$oragr%in%"Mesoamerica",]$cola<-"yellow"

dfB0_past[dfB0_past$oragr%in%"Ethipian plat",]$cola<-"chartreuse"
dfB0_past[dfB0_past$oragr%in%"Sudanic_Savan",]$cola<-"chartreuse3"
dfB0_past[dfB0_past$oragr%in%"W_African_Sav",]$cola<-"darkgreen" # smaller in the current plot1
dfB0_past[dfB0_past$oragr%in%"West Africa T",]$cola<-"chartreuse4" # bigger in past ie less barrier plot 3

dfB0_past[dfB0_past$oragr%in%"C/S_Andes",]$cola<-"darkmagenta"
dfB0_past[dfB0_past$oragr%in%"N_Lowland_SA",]$cola<-"salmon"
dfB0_past[dfB0_past$oragr%in%"NW_Lowland_SA",]$cola<-"salmon"
dfB0_past[dfB0_past$oragr%in%"Southwes amaz",]$cola<-"deeppink"

dfB0_current$cola=dfB0_past$cola

plot(dfB0_past$sqrtedTemp~dfB0_current$sqrtedTemp,col=scales::alpha(dfB0_past$cola,0.75), pch=20) ; abline(a=0,b=1)
plot(dfB0_past$sqrtedXeric~dfB0_current$sqrtedXeric,col=scales::alpha(dfB0_past$cola,0.75), pch=20)  ; abline(a=0,b=1)
plot(dfB0_past$NBsimchv1_temp~dfB0_current$NBsimchv1_temp,col=scales::alpha(dfB0_past$cola,0.75), pch=20) ; abline(a=0,b=1)
plot(dfB0_past$NBsimchv1_xeric~dfB0_current$NBsimchv1_xeric) ; abline(a=0,b=1)
plot(dfB0_past$logMCOSTt~dfB0_current$logMCOSTt,col=scales::alpha(dfB0_past$cola,0.75), pch=20) ; abline(a=0,b=1)





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

write.csv(dfB0, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfB0.csv"))



######################
####### dfbarr ####### dfi2500
######################

load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_2500_noislands.rda") 
dfi2500->dfi0 # easier for code
read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfP2barriers2500.csv"), stringsAsFactors = F)->dfB0 # barriers - dfB0 easier for code
read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/peopleinspace_costdistrib_maps/dfSocSimE_", whichk,".csv"), stringsAsFactors = F)->dfSocSimE # SocSimE
read.csv(file = paste0("data/Re_analysisGEbarriers/paleoclim2021/P2EnvDis_", whichk,"/dfi2500ED.csv"), stringsAsFactors = F)->dfEDiss

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

write.csv(dfB0, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfB2500.csv")) # the variable is named dfB0 but it is for simplicity of code, it refers to dfB2500

