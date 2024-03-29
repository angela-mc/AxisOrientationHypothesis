rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)
library(rgeos)
library(maptools) 
library(rworldmap)
library(maps)
library(ggplot2)
library(dplyr)

source("scripts/10_rLCP_MCostFXN.R")

read.csv("data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
load(file="data/Re_analysisGEbarriers/TempHarshness_raster.rds") # values already with -1 etc
TempHarshness_rasterscaled<-TempHarshness_raster # # scale them: (xi-xmean) / SD (i.e function scale) 
values(TempHarshness_rasterscaled)<-scale(values(TempHarshness_raster))

load(file="data/Re_analysisGEbarriers/XericHarshness_raster.rds") # values already with -1 etc
XericHarshness_rasterscaled<-XericHarshness_raster # # scale them: (xi-xmean) / SD (i.e function scale) 
values(XericHarshness_rasterscaled)<-scale(values(XericHarshness_raster))

# get all socs involved in P2
load("data/originsagric/dfnb_100_threshold_0_noislands.rda") 
load("data/originsagric/dfnb_100_threshold_2500_noislands.rda") 
allsocs<-unique(unlist(c(as.character(dfi0$soci), as.character(dfi0$nb), as.character(dfi2500$soci), as.character(dfi2500$nb))))
glotto<-glotto[glotto$EAsoc2%in%allsocs,]


coords<-data.frame(lon=glotto$EAlong, lat=glotto$EAlat)
coordinates(coords)<-c("lon","lat")

# verify coordinates
# plot(TempHarshness_rasterscaled) ; points(coords, pch=16, cex=0.5)
# abline(v=-12.27) ; abline (h=-37.11)
# plot(XericHarshness_rasterscaled) ; points(coords, pch=16, cex=0.5)
# abline(v=-12.27) ; abline (h=-37.11)
# load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/Elev_raster.rds")
# plot(Elev_raster) ; points(coords, pch=16, cex=0.5)
# abline(v=-12.27) ; abline (h=-37.11)

glotto$EnvPC1_relcp<-raster::extract(x=TempHarshness_rasterscaled, y=coords)
glotto$EnvPC2_relcp<-raster::extract(x=XericHarshness_rasterscaled, y=coords)

glottoLL<- glotto[complete.cases(glotto$EnvPC1_relcp), ] # remove NAs from env no data
library(spatstat)
library(robustHD)

edTemp<-matrix(data=NA, nrow = length(glottoLL$EAsoc2), ncol = length(glottoLL$EAsoc2 ))
edXeric<-matrix(data=NA, nrow = length(glottoLL$EAsoc2), ncol = length(glottoLL$EAsoc2 ))
rownames(edTemp)<-glottoLL$EAsoc2 ; colnames(edTemp)<-glottoLL$EAsoc2
rownames(edXeric)<-glottoLL$EAsoc2 ; colnames(edXeric)<-glottoLL$EAsoc2

for(i in 1:length(glottoLL$EAsoc2)) # order of glottoLL
  { soci<-glottoLL$EAsoc2[i]
    edTemp[i,]<-abs(glottoLL$EnvPC1_relcp - glottoLL$EnvPC1_relcp[i])
    edXeric[i,]<-abs(glottoLL$EnvPC2_relcp - glottoLL$EnvPC2_relcp[i])
    print(i)
}

edTemp[1:10,1:10]
edXeric[1:10,1:10]

save(glottoLL, file="data/Re_analysisGEbarriers/P2EnvDis/glottoLL_newEnvData.rds")
save(edTemp, file="data/Re_analysisGEbarriers/P2EnvDis/edTemp_newEnvData.rds")
save(edXeric, file="data/Re_analysisGEbarriers/P2EnvDis/edXeric_newEnvData.rds")


# data transformation
load("data/Re_analysisGEbarriers/P2EnvDis/edTemp_newEnvData.rds")
tempEDvalues<-edTemp[upper.tri(edTemp, diag = FALSE)]
hist(tempEDvalues)
hist(sqrt(tempEDvalues)) # good
hist(log(tempEDvalues+1)) # not as good

load("data/Re_analysisGEbarriers/P2EnvDis/edXeric_newEnvData.rds")
tempEDvalues<-edXeric[upper.tri(edXeric, diag = FALSE)]
hist(tempEDvalues)
hist(sqrt(tempEDvalues)) # good
hist(log(tempEDvalues+1)) # not as good
## --------------------------------------------------------------------------------------------------


# Edissim - Temp - extract values per pairs - AB = BA because this is EDissim
load("data/Re_analysisGEbarriers/P2EnvDis/edTemp_newEnvData.rds")
load("data/Re_analysisGEbarriers/P2EnvDis/edXeric_newEnvData.rds")
load("data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0

identical(rownames(edTemp), colnames(edTemp)) #T
identical(rownames(edXeric), colnames(edXeric)) #T
identical(rownames(edTemp), colnames(edXeric)) #T
socnames<-rownames(edTemp)

dfi0$edTemp<- -999
dfi0$edXeric<- -999

for(i in 1:length(dfi0[,1])) # rather long but not horrible
  if(dfi0$soci[i]%in%socnames & dfi0$nb[i]%in%socnames) # if not - their Edata was NA
  { r<-which(socnames%in%dfi0$soci[i])
    c<-which(socnames%in%dfi0$nb[i])
    dfi0$edTemp[i]<-edTemp[r,c]
    dfi0$edXeric[i]<-edXeric[r,c]
    print(i)
  }
# table(dfi0[dfi0$envD==0,]$edTemp) # 9 are -999
colnames(dfi0)
dfi0<-dfi0[,colnames(dfi0)%in%c("soci","nb","pair_soc","oragr","pair_soc_unsorted","edTemp","edXeric")]
write.csv(dfi0, file = "data/Re_analysisGEbarriers/P2EnvDis/dfi0ED.csv")


# dfi2500
load("data/originsagric/dfnb_100_threshold_2500_noislands.rda") 

identical(rownames(edTemp), colnames(edTemp)) #T
identical(rownames(edXeric), colnames(edXeric)) #T
identical(rownames(edTemp), colnames(edXeric)) #T
socnames<-rownames(edTemp)

dfi2500$edTemp<- -999
dfi2500$edXeric<- -999

for(i in 1:length(dfi2500[,1])) # rather long but not horrible
  if(dfi2500$soci[i]%in%socnames & dfi2500$nb[i]%in%socnames) # if not - their Edata was NA
  { r<-which(socnames%in%dfi2500$soci[i])
  c<-which(socnames%in%dfi2500$nb[i])
  dfi2500$edTemp[i]<-edTemp[r,c]
  dfi2500$edXeric[i]<-edXeric[r,c]
  print(i)
  }
# table(dfi0[dfi0$envD==0,]$edTemp) # 9 are -999
colnames(dfi2500)
dfi2500<-dfi2500[,colnames(dfi2500)%in%c("soci","nb","pair_soc","oragr","pair_soc_unsorted","edTemp","edXeric")]
write.csv(dfi2500, file = "data/Re_analysisGEbarriers/P2EnvDis/dfi2500ED.csv")


# data transformation - sqrt seems ok
read.csv(file = "data/Re_analysisGEbarriers/P2EnvDis/dfi0ED.csv", stringsAsFactors = F)->dfi0ED
par(mfrow=c(1,3))
hist(dfi0ED[dfi0ED$edTemp>-999,]$edTemp) ; hist(log(dfi0ED[dfi0ED$edTemp>-999,]$edTemp+1)) ; hist(sqrt(dfi0ED[dfi0ED$edTemp>-999,]$edTemp))
#hist(myBCtransform(dfi0ED[dfi0ED$edTemp>-999,]$edTemp))
hist(dfi0ED[dfi0ED$edXeric>-999,]$edXeric) ; hist(log(dfi0ED[dfi0ED$edXeric>-999,]$edXeric+1)) ; hist(sqrt(dfi0ED[dfi0ED$edXeric>-999,]$edXeric))

# data transformation - sqrt seems ok
read.csv(file = "data/Re_analysisGEbarriers/P2EnvDis/dfi2500ED.csv", stringsAsFactors = F)->dfi0ED # leave name for code
par(mfrow=c(1,3))
hist(dfi0ED[dfi0ED$edTemp>-999,]$edTemp) ; hist(log(dfi0ED[dfi0ED$edTemp>-999,]$edTemp+1)) ; hist(sqrt(dfi0ED[dfi0ED$edTemp>-999,]$edTemp))
#hist(myBCtransform(dfi0ED[dfi0ED$edTemp>-999,]$edTemp))
hist(dfi0ED[dfi0ED$edXeric>-999,]$edXeric) ; hist(log(dfi0ED[dfi0ED$edXeric>-999,]$edXeric+1)) ; hist(sqrt(dfi0ED[dfi0ED$edXeric>-999,]$edXeric))

