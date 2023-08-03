rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)

#################
####### PCA ##### dfi0
#################

load("data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0
hist(dfi0$dist) ; hist(log(dfi0$dist+1)) ; hist(sqrt(dfi0$dist)) # sqrt workds best

#read.csv(file="data/Re_analysisGEbarriers/P2_datamodel/dfB0_NOCLIMSIMNB.csv", stringsAsFactors = F)->dfB0
#colnames(dfB0)

# # average using median per society of origin
# unique(dfB0$Soci)->socs
# for(i in 1:length(socs))
#   {dfB0[dfB0$Soci%in%socs[i],]$MCOSTe<-median(dfB0[dfB0$Soci%in%socs[i],]$MCOSTe) ; dfB0[dfB0$Soci%in%socs[i],]$LCPe<-median( dfB0[dfB0$Soci%in%socs[i],]$LCPe)
#    dfB0[dfB0$Soci%in%socs[i],]$MCOSTt<-median( dfB0[dfB0$Soci%in%socs[i],]$MCOSTt) ; dfB0[dfB0$Soci%in%socs[i],]$LCPt<-median( dfB0[dfB0$Soci%in%socs[i],]$LCPt)
#    dfB0[dfB0$Soci%in%socs[i],]$MCOSTx<-median( dfB0[dfB0$Soci%in%socs[i],]$MCOSTx) ; dfB0[dfB0$Soci%in%socs[i],]$LCPx<-median( dfB0[dfB0$Soci%in%socs[i],]$LCPx)
#    dfB0[dfB0$Soci%in%socs[i],]$edTemp<-median( dfB0[dfB0$Soci%in%socs[i],]$edTemp) ; dfB0[dfB0$Soci%in%socs[i],]$edXeric<-median( dfB0[dfB0$Soci%in%socs[i],]$edXeric)
#    dfB0[dfB0$Soci%in%socs[i],]$GeoD<-median( dfB0[dfB0$Soci%in%socs[i],]$GeoD)
#    print(i)
#   }
# dfB0<-dfB0[ (!duplicated(dfB0$Soci)),]
# write.csv(dfB0, file="data/Re_analysisGEbarriers/P2_datamodel/dfB0_perSO_av_NOCLIMSIMNB.csv")

read.csv(file="data/Re_analysisGEbarriers/P2_datamodel/dfB0_perSO_av_NOCLIMSIMNB.csv", stringsAsFactors = F)->dfB0 # average using median per society of origin
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

# PCA
dataPCA<-dfB0[,colnames(dfB0)%in%c("sqrtGeoD", "logMCOSTe","logLCPe","logMCOSTt","logLCPt","logMCOSTx","logLCPx",
                                   "sqrtedTemp" , "sqrtedXeric")]

pcaik<-prcomp(dataPCA, center = TRUE,scale. = TRUE) # predictors get standardized
summary(pcaik)
pcaik$rotation
library(nFactors)
ev <- eigen(cor(dataPCA))
ev

library(psych)
myPCA<-principal(dataPCA, nfactors=3, rotate="varimax") # this function scales variables before doing PCA
row.names(myPCA)
myPCA$loadings # all pos loadings

#save(myPCA, file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/myPCA_perSO_av_NOCLIMSIMNB.rds")
dfB0$RC1<- myPCA$scores[,colnames(myPCA$scores)%in%"RC1"]
dfB0$RC2<- myPCA$scores[,colnames(myPCA$scores)%in%"RC2"]
dfB0$RC3<- myPCA$scores[,colnames(myPCA$scores)%in%"RC3"]
#write.csv(dfB0, file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/dfB_PCA_perSO_av_NOCLIMSIMNB.csv")


#################
####### PCA ##### this one is the same as that variable was not included in the long-analyses anyway
#################
