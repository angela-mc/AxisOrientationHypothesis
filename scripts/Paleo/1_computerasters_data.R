rm(list=ls())

require(rgdal)
require(maptools)
require(raster)
require(geosphere)
library(gstat)
library(rgeos)
require(rworldmap)
require(classInt)
require(RgoogleMaps)
require(GISTools)
library(RColorBrewer)
require(maps)
require(letsR)

source("scripts/6_OrAgr_paleoclim_SF.R") # support functions


######################################
### Load temp and precip rasters  ####
######################################


read.csv("data/Re_analysisGEbarriers/paleoclim2021/Domestication dates_PK.csv", stringsAsFactors = F)->domestimes
domestimes$domestimesToUse<-c("10k","10k","10k","11k","9k","4k","5k","10k","MEAN","8k","4k","9k")
  #  W African savanna --> 4500 and no first proto date, so use 5k and 4k, mean it and do RPCA on that
load("data/Re_analysisGEbarriers/paleoclim2021/complete.rdata") 
#complete[[1]] ; #names(complete)[grepl("_9k", names(complete))]

# which climate layer to use?
mean(domestimes[!domestimes$Location%in%"New Guinea",]$First.proper.date) # 8113.636 - use 9/8KYA
whichk<-"12k"
whichk<-"8k" 
whichk<-"4k"

MeanT_raster<-complete[[which(names(complete)%in%paste0("MeanT_",whichk))]]
VarT_raster<-complete[[which(names(complete)%in%paste0("VarT_",whichk))]]
MeanP_raster<-complete[[which(names(complete)%in%paste0("MeanP_",whichk))]]
CvP_raster<-complete[[which(names(complete)%in%paste0("VarP",whichk))]] # replace this by CV = (SD/mean)*100 = (sqrt(var) / mean) *100
values(CvP_raster)<-( sqrt(values( complete[[which(names(complete)%in%paste0("VarP",whichk))]] )) /values(complete[[which(names(complete)%in%paste0("MeanP_",whichk))]]) )*100

par(mfrow=c(2,2))
plot(MeanT_raster)
plot(VarT_raster)
plot(MeanP_raster)
plot(CvP_raster)
par(mfrow=c(1,1))

crs(MeanT_raster)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crs(VarT_raster)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crs(MeanP_raster)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crs(CvP_raster)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


### Initiate raster data holder
length_mrastdata<-203595

# myRastData <- as.data.frame(matrix(NA,length_mrastdata,4)) 
#   # this is the value after being pruned by Wolrd!
# names(myRastData) <- c("MeanT_raster", "VarT_raster",
#                        "MeanP_raster", "CvP_raster")

myRastData <- as.data.frame(matrix(NA,length_mrastdata,4)) ## this is the value after being pruned by Wolrd!
names(myRastData) <- c("MeanT_raster", "VarT_raster",
                       "MeanP_raster", "CvP_raster") # no elevation


###### Trim rasters to terrestrial areas
World <- getMap()
### Remove antarctica
World <- World[which(World$continent != 'Antarctica'), ]

for (i in 1:length(names(myRastData))) {
  eval(parse(text = paste0(names(myRastData)[i]," <- mask(", 
                           names(myRastData)[i], ", World)")))
  eval(parse(text = paste0(names(myRastData)[i]," <- projectRaster(", 
                           names(myRastData)[i], ", crs = '+proj=wag4 +lon_0=0')")))
}

length(MeanT_raster)

### Fill in raster data holder
for ( i in names(myRastData)) {
  eval(parse(text = paste('myRastData$', i, "<- values(", i,")", sep ='')))
}

myRastData <- na.omit(myRastData)
RastDataRaw <- myRastData

transforms <- list()

## Normalize (uses Bruno Vilela's code)
for(i in 1:ncol(myRastData)) {
  curtran <- normalizer(myRastData[, i], plotit = T)
  myRastData[, i] <- curtran[[1]]
  transforms[[i]] <- curtran[[2]]
}
transforms

PCAdata <- na.omit(myRastData)

# Determine Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(PCAdata)) # get eigenEstimates
ap <- parallel(subject=nrow(na.omit(PCAdata)),var=ncol(na.omit(PCAdata)),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) # this suggests we stick with 2 - 90% var
## --------------------------------------------------------------------------------------------------------

## only BOX OX transformation on main analyses

myPCA <- principal(PCAdata, nfactors = 2, rotate = "varimax", scores = T) # tp only nfactors=2
myPCA 
row.names(myPCA)
myPCA $loadings

# do we need to transform values to make them into Temp harshness and Xeric harshness? (these,as well as teh RC1/2 will change depending on layer)
if(whichk%in%"12k"){
  TempHarshness_raster <- XericHarshness_raster <- MeanT_raster
  values(TempHarshness_raster) <- rep(NA, times = length_mrastdata)
  values(TempHarshness_raster)[as.numeric(names(myPCA$scores[,"RC1"]))] <- (-1)*myPCA$scores[,"RC1"]
  plot(TempHarshness_raster)
  
  values(XericHarshness_raster) <- rep(NA, times = length_mrastdata)
  values(XericHarshness_raster)[as.numeric(names(myPCA$scores[,"RC2"]))] <-  myPCA$scores[,"RC2"] # no (-1) CV positive
  plot(XericHarshness_raster)
  
}

if(whichk%in%"8k"){
  TempHarshness_raster <- XericHarshness_raster <- MeanT_raster
  values(TempHarshness_raster) <- rep(NA, times = length_mrastdata)
  values(TempHarshness_raster)[as.numeric(names(myPCA$scores[,"RC1"]))] <- (-1)*myPCA$scores[,"RC1"]
  plot(TempHarshness_raster)
  
  values(XericHarshness_raster) <- rep(NA, times = length_mrastdata)
  values(XericHarshness_raster)[as.numeric(names(myPCA$scores[,"RC2"]))] <-  myPCA$scores[,"RC2"] # no (-1) CV positive
  plot(XericHarshness_raster)
  
}

if(whichk%in%"4k"){
  TempHarshness_raster <- XericHarshness_raster <- MeanT_raster
  values(TempHarshness_raster) <- rep(NA, times = length_mrastdata)
  values(TempHarshness_raster)[as.numeric(names(myPCA$scores[,"RC1"]))] <- (-1)*myPCA$scores[,"RC1"]
  plot(TempHarshness_raster)
  
  values(XericHarshness_raster) <- rep(NA, times = length_mrastdata)
  values(XericHarshness_raster)[as.numeric(names(myPCA$scores[,"RC2"]))] <-  myPCA$scores[,"RC2"] # no (-1) CV positive
  plot(XericHarshness_raster)
  
}


# make rasters with the longlat projection
load(file= "data/predictors/preds3april/paleoclim/Elev_raster.rds") #newElev_raster
projectRaster(from = TempHarshness_raster, to = Elev_raster)-> TempHarshness_raster
projectRaster(from = XericHarshness_raster, to = Elev_raster)-> XericHarshness_raster

plot(TempHarshness_raster)
plot(XericHarshness_raster)

# save them
save(TempHarshness_raster, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/TempHarshness_raster_",whichk,".rds"))
save(XericHarshness_raster, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/XericHarshness_raster_",whichk,".rds"))


# load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/TempHarshness_raster_","12k",".rds")) ; TempHarshness_raster->TempHarshness_raster12
# load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/TempHarshness_raster_","8k",".rds")) ; TempHarshness_raster->TempHarshness_raster8
# load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/TempHarshness_raster_","4k",".rds")) ; TempHarshness_raster->TempHarshness_raster4
# 
# plot(values(TempHarshness_raster12)~values(TempHarshness_raster8), pch=20, col=scales::alpha("black",0.25))
# plot(values(TempHarshness_raster8)~values(TempHarshness_raster4), pch=20, col=scales::alpha("black",0.25))
# plot(values(TempHarshness_raster12)~values(TempHarshness_raster4), pch=20, col=scales::alpha("black",0.25))
