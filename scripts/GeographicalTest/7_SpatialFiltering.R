
rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)

read.csv(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/dfB_PCA_perSO_av.csv", stringsAsFactors = F)-> dfb0
load("data/originsagric/dfnb_100_threshold_0_noislands.rda") 
dfi0<-dfi0[!duplicated(dfi0$soci),]
dfi0<-dfi0[dfi0$soci%in%dfb0$Soci,]
dfi0<-dfi0[match(dfb0$Soci, dfi0$soci),]
dfb0$lat<-dfi0$lat ; dfb0$long<-dfi0$long

# distGeo(p1=c(dfb0$long[1],dfb0$lat[1]), p2=c(dfb0$long[2],dfb0$lat[2])) # p long lat
# distGeo(p1=c(dfb0$long[1],dfb0$lat[1]), p2=c(dfb0$long[1],dfb0$lat[1])) # p long lat
# 
# distm<-matrix(ncol=length(dfb0[,1]), nrow = length(dfb0[,1]))
# rownames(distm)<-dfb0$Soci ; colnames(distm)<-dfb0$Soci
# 
# # col names coords
# extract_coord_cols<-function(x) {return(c(dfb0$long[x], dfb0$lat[x]))}
# list_coords<-lapply((1:dim(distm)[2]), FUN=extract_coord_cols)
# 
# for(i in 1: dim(distm)[1])
#   { soci<-rownames(distm)[i]
#     coordi<-list_coords[[i]]
#     distm[i,]<-unlist(lapply(list_coords,distGeo,p1=coordi) )
#     print(i)
# }
# #save(distm, file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/DistGeo_dfB_perSO_av.rds")
# 
## ------------------------------------------------------------------------------------------------------------------------------------

#################
####### PCA ##### dfi0
#################

load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/myPCA_perSO_av.rds")
read.csv(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/dfB_PCA_perSO_av.csv", stringsAsFactors = F)-> dfb0
RC_names<-c("GeoD_LCPsl_MCOSTe", "NrSocSimE", "TempD_MCOSTt","XericD_MCOSTx")
RCs<-c("RC1", "RC2","RC3","RC4") # NrSocSimE has negative loading
myPCA$loadings
## ------------------------------------------------------------------------------------------------------------------------------------

# prep data
dfb0$contF<-factor(dfb0$cont,levels=c("Asia","Africa","SouthAmerica","Mesoamerica","NorthAmerica"))
dfb0[dfb0$oragr%in%"Lower-MiddleY",]$oragr<-"Lower_MiddleY"
dfb0[dfb0$oragr%in%"NW_Lowland_SA",]$oragr<-"N_Lowland_SA" # merge SAM Lowlands
dfb0<-dfb0[!dfb0$oragr%in%"S_India",] # rem 1 soc area
lvls<- c("South trop ch", "Lower_MiddleY" ,"Chinese_loess", "W_Yuman_E_Tib","Fertile_Cresc", "Sava_W_India","Ganges_E_Indi"  ,
         "West Africa T", "W_African_Sav","Sudanic_Savan", "Ethipian plat", 
         "N_Lowland_SA","C/S_Andes","Southwes amaz" ,
         "Mesoamerica","E_North_Ameri")
dfb0$oragrF<-factor(dfb0$oragr,levels=lvls)
## ------------------------------------------------------------------------------------------------------------------------------------

load("data/originsagric/dfnb_100_threshold_0_noislands.rda") 
dfi0<-dfi0[!duplicated(dfi0$soci),]
dfi0<-dfi0[dfi0$soci%in%dfb0$Soci,]
dfi0<-dfi0[match(dfb0$Soci, dfi0$soci),]
dfb0$lat<-dfi0$lat ; dfb0$long<-dfi0$long
load( file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/DistGeo_dfB_perSO_av.rds")
distm<-distm[dfb0$Soci, dfb0$Soci]
identical(rownames(distm), dfb0$Soci)
# ------------------------------------------------------------------------------------------------------------------------------------

# models / area
library(letsR)
library(spdep)


lmbase <- lm(RC1~oragrF , data=dfb0)
coords<-cbind(dfb0$long,dfb0$lat) #Make a matrix of coordinates (X and Y coordinates)
coords<-as.matrix(coords)
row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
sarcol <- SpatialFiltering(RC1 ~  oragrF, data=dfb0, nb=nb, style="W", ExactEV=TRUE)
save(sarcol, file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/sac/SpFilter/spf1500_RC1.rds")


lmbase <- lm(RC2~oragrF , data=dfb0)
coords<-cbind(dfb0$long,dfb0$lat) #Make a matrix of coordinates (X and Y coordinates)
coords<-as.matrix(coords)
row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
sarcol <- SpatialFiltering(RC2 ~  oragrF, data=dfb0, nb=nb, style="W", ExactEV=TRUE)
save(sarcol, file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/sac/SpFilter/spf1500_RC2.rds")


lmbase <- lm(RC3~oragrF , data=dfb0)
coords<-cbind(dfb0$long,dfb0$lat) #Make a matrix of coordinates (X and Y coordinates)
coords<-as.matrix(coords)
row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
sarcol <- SpatialFiltering(RC3 ~  oragrF, data=dfb0, nb=nb, style="W", ExactEV=TRUE)
save(sarcol, file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/sac/SpFilter/spf1500_RC3.rds")


lmbase <- lm(RC4~oragrF , data=dfb0)
coords<-cbind(dfb0$long,dfb0$lat) #Make a matrix of coordinates (X and Y coordinates)
coords<-as.matrix(coords)
row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
sarcol <- SpatialFiltering(RC4 ~  oragrF, data=dfb0, nb=nb, style="W", ExactEV=TRUE)
save(sarcol, file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/sac/SpFilter/spf1500_RC4.rds")
# ------------------------------------------------------------------------------------------------------------------------------------


#################
####### PCA ##### dfi2500
#################

rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)

load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/myPCA_perSO_av.rds")
read.csv(file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/dfB_PCA_perSO_av.csv", stringsAsFactors = F)-> dfb0
RC_names<-c("TempD_MCOSTt","LPCtLCPx","XericD_MCOSTx","GeoD_LCPsl_MCOSTe")
RCs<-c("RC1", "RC2","RC3","RC4") 
myPCA$loadings
## ------------------------------------------------------------------------------------------------------------------------------------

# prep data
dfb0$contF<-factor(dfb0$cont,levels=c("Asia","Africa","SouthAmerica","Mesoamerica","NorthAmerica"))
dfb0[dfb0$oragr%in%"Lower-MiddleY",]$oragr<-"Lower_MiddleY"
dfb0[dfb0$oragr%in%"NW_Lowland_SA",]$oragr<-"N_Lowland_SA" # merge SAM Lowlands
dfb0<-dfb0[!dfb0$oragr%in%"S_India",] # rem 1 soc area
lvls<- c("South trop ch", "Lower_MiddleY" ,"Chinese_loess", "W_Yuman_E_Tib","Fertile_Cresc", "Sava_W_India","Ganges_E_Indi"  ,
         "West Africa T", "W_African_Sav","Sudanic_Savan", "Ethipian plat", 
         "N_Lowland_SA","C/S_Andes","Southwes amaz" ,
         "Mesoamerica","E_North_Ameri")
dfb0$oragrF<-factor(dfb0$oragr,levels=lvls)
## ------------------------------------------------------------------------------------------------------------------------------------

load("data/originsagric/dfnb_100_threshold_2500_noislands.rda") 
dfi0<-dfi2500
dfi0<-dfi0[!duplicated(dfi0$soci),]
dfi0<-dfi0[dfi0$soci%in%dfb0$Soci,]
dfi0<-dfi0[match(dfb0$Soci, dfi0$soci),]
dfb0$lat<-dfi0$lat ; dfb0$long<-dfi0$long
load( file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/DistGeo_dfB_perSO_av.rds")
distm<-distm[dfb0$Soci, dfb0$Soci]
identical(rownames(distm), dfb0$Soci)
# ------------------------------------------------------------------------------------------------------------------------------------


# models / area
library(letsR)
library(spdep)

lmbase <- lm(RC2~oragrF , data=dfb0)
coords<-cbind(dfb0$long,dfb0$lat) #Make a matrix of coordinates (X and Y coordinates)
coords<-as.matrix(coords)
row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
sarcol <- SpatialFiltering(RC2 ~  oragrF, data=dfb0, nb=nb, style="W", ExactEV=TRUE)
save(sarcol, file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/sac/SoFilter/spf1500_RC2.rds")


lmbase <- lm(RC3~oragrF , data=dfb0)
coords<-cbind(dfb0$long,dfb0$lat) #Make a matrix of coordinates (X and Y coordinates)
coords<-as.matrix(coords)
row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
sarcol <- SpatialFiltering(RC3 ~  oragrF, data=dfb0, nb=nb, style="W", ExactEV=TRUE)
save(sarcol, file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/sac/SoFilter/spf1500_RC3.rds")

lmbase <- lm(RC1~oragrF , data=dfb0)
coords<-cbind(dfb0$long,dfb0$lat) #Make a matrix of coordinates (X and Y coordinates)
coords<-as.matrix(coords)
row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
sarcol <- SpatialFiltering(RC1 ~  oragrF, data=dfb0, nb=nb, style="W", ExactEV=TRUE)
save(sarcol, file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/sac/SoFilter/spf1500_RC1.rds")

lmbase <- lm(RC4~oragrF , data=dfb0)
coords<-cbind(dfb0$long,dfb0$lat) #Make a matrix of coordinates (X and Y coordinates)
coords<-as.matrix(coords)
row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
sarcol <- SpatialFiltering(RC4 ~  oragrF, data=dfb0, nb=nb, style="W", ExactEV=TRUE)
save(sarcol, file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/sac/SoFilter/spf1500_RC4.rds")
