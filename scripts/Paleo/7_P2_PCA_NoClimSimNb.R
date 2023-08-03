rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)

whichk<-"8k"
whichk<-"12k"
whichk<-"4k"

#################
####### PCA ##### dfi0
#################

load("data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0
hist(dfi0$dist) ; hist(log(dfi0$dist+1)) ; hist(sqrt(dfi0$dist)) # sqrt workds best

read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfB0_NOCLIMSIMNB.csv"), stringsAsFactors = F)->dfB0 # use for socs

# # average using median per society of origin
# unique(dfB0$Soci)->socs
# for(i in 1:length(socs))
#   {dfB0[dfB0$Soci%in%socs[i],]$MCOSTe<-median(dfB0[dfB0$Soci%in%socs[i],]$MCOSTe) ; dfB0[dfB0$Soci%in%socs[i],]$LCPe<-median( dfB0[dfB0$Soci%in%socs[i],]$LCPe)
#    dfB0[dfB0$Soci%in%socs[i],]$MCOSTt<-median( dfB0[dfB0$Soci%in%socs[i],]$MCOSTt) ; dfB0[dfB0$Soci%in%socs[i],]$LCPt<-median( dfB0[dfB0$Soci%in%socs[i],]$LCPt)
#    dfB0[dfB0$Soci%in%socs[i],]$MCOSTx<-median( dfB0[dfB0$Soci%in%socs[i],]$MCOSTx) ; dfB0[dfB0$Soci%in%socs[i],]$LCPx<-median( dfB0[dfB0$Soci%in%socs[i],]$LCPx)
#    dfB0[dfB0$Soci%in%socs[i],]$edTemp<-median( dfB0[dfB0$Soci%in%socs[i],]$edTemp) ; dfB0[dfB0$Soci%in%socs[i],]$edXeric<-median( dfB0[dfB0$Soci%in%socs[i],]$edXeric)
#    # dfB0[dfB0$Soci%in%socs[i],]$NBsimchv1_temp<-median( dfB0[dfB0$Soci%in%socs[i],]$NBsimchv1_temp) ; dfB0[dfB0$Soci%in%socs[i],]$NBsimchv1_xeric<-median( dfB0[dfB0$Soci%in%socs[i],]$NBsimchv1_xeric)
#    dfB0[dfB0$Soci%in%socs[i],]$GeoD<-median( dfB0[dfB0$Soci%in%socs[i],]$GeoD)
#    print(i)
#   }
# dfB0<-dfB0[ (!duplicated(dfB0$Soci)),]
# write.csv(dfB0, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfB0_perSO_av_NOSIMCLIMNB.csv"))

read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfB0_perSO_av_NOSIMCLIMNB.csv"), stringsAsFactors = F)->dfB0
namesC<-c("West Africa T", "Sudanic_Savan","W_African_Sav", "Ethipian plat","E_North_Ameri","Mesoamerica","Southwes amaz" ,"C/S_Andes","NW_Lowland_SA", "N_Lowland_SA" ,"Fertile_Cresc" , "W_Yuman_E_Tib", "Chinese_loess", "South trop ch" , "Ganges_E_Indi" , "Lower-MiddleY"  , "Sava_W_India" ,"S_India")
conts1<-c("Africa", "Africa","Africa", "Africa","NorthAmerica","Mesoamerica","SouthAmerica" ,"SouthAmerica","SouthAmerica", "SouthAmerica" ,"Asia" , "Asia", "Asia", "Asia" , "Asia" , "Asia"  , "Asia" ,"Asia")
dfB0$cont<- -999
for(i in 1: length(namesC)) dfB0[dfB0$oragr%in%namesC[i],]$cont<-conts1[i]
table(dfB0$cont)

# data transformations
dfB0$sqrtGeoD<-sqrt(dfB0$GeoD)
dfB0$logMCOSTe<-log(dfB0$MCOSTe+1) ; dfB0$logLCPe<-log(dfB0$LCPe+1)
dfB0$logMCOSTt<-log(dfB0$MCOSTt+1) ; dfB0$logLCPt<-log(dfB0$LCPt+1)
dfB0$logMCOSTx<-log(dfB0$MCOSTx+1) ; dfB0$logLCPx<-log(dfB0$LCPx+1)
dfB0$sqrtedTemp <-sqrt(dfB0$edTemp)
dfB0$sqrtedXeric <-sqrt(dfB0$edXeric)
# leave SocSimE alone

# PCA
dataPCA<-dfB0[,colnames(dfB0)%in%c("sqrtGeoD", "logMCOSTe","logLCPe","logMCOSTt","logLCPt","logMCOSTx","logLCPx",
                                   "sqrtedTemp" , "sqrtedXeric")]#, "NBsimchv1_temp","NBsimchv1_xeric")]

pcaik<-prcomp(dataPCA, center = TRUE,scale. = TRUE) # predictors get standardized
summary(pcaik)
pcaik$rotation
library(nFactors)
ev <- eigen(cor(dataPCA)) # get eigenEstimates, suggests 3 PCs
ev

library(psych)
myPCA<-principal(dataPCA, nfactors=3, rotate="varimax") # this function scales variables before doing PCA
row.names(myPCA)
myPCA$loadings

save(myPCA, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/myPCA_perSO_av_NOSIMCLIMNB.rds"))
dfB0$RC1<- myPCA$scores[,colnames(myPCA$scores)%in%"RC1"]
dfB0$RC2<- myPCA$scores[,colnames(myPCA$scores)%in%"RC2"]
dfB0$RC3<- myPCA$scores[,colnames(myPCA$scores)%in%"RC3"]
write.csv(dfB0, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/dfB_PCA_perSO_av_NOSIMCLIMNB.csv"))


#################
####### PCA ##### dfi2500
#################

# see the one with climsim as this part is the same