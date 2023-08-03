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

load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0
hist(dfi0$dist) ; hist(log(dfi0$dist+1)) ; hist(sqrt(dfi0$dist)) # sqrt workds best

read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfB0.csv"), stringsAsFactors = F)->dfB0 # use for socs

#read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfB0.csv"), stringsAsFactors = F)->dfB0
#colnames(dfB0)

# # average using median per society of origin
# unique(dfB0$Soci)->socs
# for(i in 1:length(socs))
#   {dfB0[dfB0$Soci%in%socs[i],]$MCOSTe<-median(dfB0[dfB0$Soci%in%socs[i],]$MCOSTe) ; dfB0[dfB0$Soci%in%socs[i],]$LCPe<-median( dfB0[dfB0$Soci%in%socs[i],]$LCPe)
#    dfB0[dfB0$Soci%in%socs[i],]$MCOSTt<-median( dfB0[dfB0$Soci%in%socs[i],]$MCOSTt) ; dfB0[dfB0$Soci%in%socs[i],]$LCPt<-median( dfB0[dfB0$Soci%in%socs[i],]$LCPt)
#    dfB0[dfB0$Soci%in%socs[i],]$MCOSTx<-median( dfB0[dfB0$Soci%in%socs[i],]$MCOSTx) ; dfB0[dfB0$Soci%in%socs[i],]$LCPx<-median( dfB0[dfB0$Soci%in%socs[i],]$LCPx)
#    dfB0[dfB0$Soci%in%socs[i],]$edTemp<-median( dfB0[dfB0$Soci%in%socs[i],]$edTemp) ; dfB0[dfB0$Soci%in%socs[i],]$edXeric<-median( dfB0[dfB0$Soci%in%socs[i],]$edXeric)
#    dfB0[dfB0$Soci%in%socs[i],]$NBsimchv1_temp<-median( dfB0[dfB0$Soci%in%socs[i],]$NBsimchv1_temp) ; dfB0[dfB0$Soci%in%socs[i],]$NBsimchv1_xeric<-median( dfB0[dfB0$Soci%in%socs[i],]$NBsimchv1_xeric)
#    dfB0[dfB0$Soci%in%socs[i],]$GeoD<-median( dfB0[dfB0$Soci%in%socs[i],]$GeoD)
#    print(i)
#   }
# dfB0<-dfB0[ (!duplicated(dfB0$Soci)),]
# write.csv(dfB0, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfB0_perSO_av.csv"))

read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfB0_perSO_av.csv"), stringsAsFactors = F)->dfB0

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
                                   "sqrtedTemp" , "sqrtedXeric", "NBsimchv1_temp","NBsimchv1_xeric")]

# # hypothetical y to calculate vif
# hypy<-rnorm(n=length(dataPCA[,1]), mean=0, sd=0.5)
# model <- lm(hypy ~ sqrtGeoD + logMCOSTe + logLCPe + logMCOSTt + logLCPt +logMCOSTx + logLCPx +
#                             + sqrtedTemp + sqrtedXeric + NBsimchv1_temp + NBsimchv1_xeric,
#                             data = dataPCA)
# library(car)
# vif(model) # high vif

pcaik<-prcomp(dataPCA, center = TRUE,scale. = TRUE) # predictors get standardized
summary(pcaik)
pcaik$rotation
library(nFactors)
ev <- eigen(cor(dataPCA)) # get eigenEstimates, suggests 3 PCs

# do 4 if you want: (1) all Ds+ MCOSTe, (2)  xeric , (3) temp, (4) allSimSocE - nice because temp & prec are separated

library(psych)
myPCA<-principal(dataPCA, nfactors=4, rotate="varimax") # this function scales variables before doing PCA
row.names(myPCA)
myPCA$loadings

save(myPCA, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/myPCA_perSO_av.rds"))
dfB0$RC1<- myPCA$scores[,colnames(myPCA$scores)%in%"RC1"]
dfB0$RC2<- myPCA$scores[,colnames(myPCA$scores)%in%"RC2"]
dfB0$RC3<- myPCA$scores[,colnames(myPCA$scores)%in%"RC3"]
dfB0$RC4<- myPCA$scores[,colnames(myPCA$scores)%in%"RC4"]
write.csv(dfB0, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/dfB_PCA_perSO_av.csv"))


#################
####### PCA ##### dfi2500
#################


load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_2500_noislands.rda") 
hist(dfi2500$dist) ; hist(log(dfi2500$dist+1)) ; hist(sqrt(dfi2500$dist)) # sqrt workds best

read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfB2500.csv"), stringsAsFactors = F)->dfB2500 # for socs

# # average using median per society of origin
# unique(dfB2500$Soci)->socs
# for(i in 1:length(socs))
#   {dfB2500[dfB2500$Soci%in%socs[i],]$MCOSTe<-median(dfB2500[dfB2500$Soci%in%socs[i],]$MCOSTe) ; dfB2500[dfB2500$Soci%in%socs[i],]$LCPe<-median( dfB2500[dfB2500$Soci%in%socs[i],]$LCPe)
#    dfB2500[dfB2500$Soci%in%socs[i],]$MCOSTt<-median( dfB2500[dfB2500$Soci%in%socs[i],]$MCOSTt) ; dfB2500[dfB2500$Soci%in%socs[i],]$LCPt<-median( dfB2500[dfB2500$Soci%in%socs[i],]$LCPt)
#    dfB2500[dfB2500$Soci%in%socs[i],]$MCOSTx<-median( dfB2500[dfB2500$Soci%in%socs[i],]$MCOSTx) ; dfB2500[dfB2500$Soci%in%socs[i],]$LCPx<-median( dfB2500[dfB2500$Soci%in%socs[i],]$LCPx)
#    dfB2500[dfB2500$Soci%in%socs[i],]$edTemp<-median( dfB2500[dfB2500$Soci%in%socs[i],]$edTemp) ; dfB2500[dfB2500$Soci%in%socs[i],]$edXeric<-median( dfB2500[dfB2500$Soci%in%socs[i],]$edXeric)
#    dfB2500[dfB2500$Soci%in%socs[i],]$NBsimchv1_temp<-median( dfB2500[dfB2500$Soci%in%socs[i],]$NBsimchv1_temp) ; dfB2500[dfB2500$Soci%in%socs[i],]$NBsimchv1_xeric<-median( dfB2500[dfB2500$Soci%in%socs[i],]$NBsimchv1_xeric)
#    dfB2500[dfB2500$Soci%in%socs[i],]$GeoD<-median( dfB2500[dfB2500$Soci%in%socs[i],]$GeoD)
#    print(i)
#   }
# dfB2500<-dfB2500[ (!duplicated(dfB2500$Soci)),]
# write.csv(dfB2500, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_",whichk,"/dfB2500_perSO_av.csv"))

read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_",whichk,"/dfB2500_perSO_av.csv"), stringsAsFactors = F)->dfB2500
colnames(dfB2500)

namesC<-c("West Africa T", "Sudanic_Savan","W_African_Sav", "Ethipian plat","E_North_Ameri","Mesoamerica","Southwes amaz" ,"C/S_Andes","NW_Lowland_SA", "N_Lowland_SA" ,"Fertile_Cresc" , "W_Yuman_E_Tib", "Chinese_loess", "South trop ch" , "Ganges_E_Indi" , "Lower-MiddleY"  , "Sava_W_India" ,"S_India")
conts1<-c("Africa", "Africa","Africa", "Africa","NorthAmerica","Mesoamerica","SouthAmerica" ,"SouthAmerica","SouthAmerica", "SouthAmerica" ,"Asia" , "Asia", "Asia", "Asia" , "Asia" , "Asia"  , "Asia" ,"Asia")
dfB2500$cont<- -999
for(i in 1: length(namesC)) dfB2500[dfB2500$oragr%in%namesC[i],]$cont<-conts1[i]
table(dfB2500$cont)

# data transformations
dfB2500$sqrtGeoD<-sqrt(dfB2500$GeoD)
dfB2500$logMCOSTe<-log(dfB2500$MCOSTe+1) ; dfB2500$logLCPe<-log(dfB2500$LCPe+1)
dfB2500$logMCOSTt<-log(dfB2500$MCOSTt+1) ; dfB2500$logLCPt<-log(dfB2500$LCPt+1)
dfB2500$logMCOSTx<-log(dfB2500$MCOSTx+1) ; dfB2500$logLCPx<-log(dfB2500$LCPx+1)
dfB2500$sqrtedTemp <-sqrt(dfB2500$edTemp)
dfB2500$sqrtedXeric <-sqrt(dfB2500$edXeric)
# leave SocSimE alone 

# PCA
# dataPCA<-dfB2500[,colnames(dfB2500)%in%c("sqrtGeoD", "logMCOSTe","logLCPe","logMCOSTt","logLCPt","logMCOSTx","logLCPx",
#                                    "sqrtedTemp" , "sqrtedXeric", "NBsimchv1_temp","NBsimchv1_xeric")]

# PCA
dataPCA<-dfB2500[,colnames(dfB2500)%in%c("sqrtGeoD", "logMCOSTe","logLCPe","logMCOSTt","logLCPt","logMCOSTx","logLCPx",
                                         "sqrtedTemp" , "sqrtedXeric")] # exclude "NBsimchv1_temp","NBsimchv1_xeric"

# # hypothetical y to calculate vif
# hypy<-rnorm(n=length(dataPCA[,1]), mean=0, sd=0.5)
# model <- lm(hypy ~ sqrtGeoD + logMCOSTe + logLCPe + logMCOSTt + logLCPt +logMCOSTx + logLCPx +
#                             + sqrtedTemp + sqrtedXeric,
#                             data = dataPCA)
# library(car)
# vif(model) # high but not as high as dfi0

pcaik<-prcomp(dataPCA, center = TRUE,scale. = TRUE) # predictors get standardized
summary(pcaik)
pcaik$rotation
library(nFactors)
ev <- eigen(cor(dataPCA)) # get eigenEstimates, suggests 3PCs (no_av)

# temperature & xeric paths separate from geoD&elevation

library(psych)
myPCA<-principal(dataPCA, nfactors=4, rotate="varimax") # this function scales variables before doing PCA
row.names(myPCA)
myPCA$loadings

# do 4 if you want: (1) geoD + elevation cost, (2) all xeric(Turnover&Diss), (4) temp diss&turnover, (3) LCPl xeric&temp

save(myPCA, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi2500_PCA_nf4/myPCA_perSO_av.rds")) # rename this as di25--_PCA_nf4

dfB2500$RC1<- myPCA$scores[,colnames(myPCA$scores)%in%"RC1"]
dfB2500$RC2<- myPCA$scores[,colnames(myPCA$scores)%in%"RC2"]
dfB2500$RC3<- myPCA$scores[,colnames(myPCA$scores)%in%"RC3"]
dfB2500$RC4<- myPCA$scores[,colnames(myPCA$scores)%in%"RC4"]

write.csv(dfB2500, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi2500_PCA_nf4/dfB_PCA_perSO_av.csv"))

