rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)
library(geosphere)

whichk<-"8k"
whichk<-"12k"
whichk<-"4k"

# setwd("/Volumes/Angela_5TB/WashUDesktop/other/BOX_15march/GeogrAxes") # for running terminal


#################
####### PCA ##### dfi0
#################

load( file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/myPCA_perSO_av_NOSIMCLIMNB.rds"))
read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/dfB_PCA_perSO_av_NOSIMCLIMNB.csv"), stringsAsFactors = F)->dfb0
myPCA$loadings
RC_names<-c("GeoD_LCPsl_MCOSTe", "XericD_MCOSTx", "TempD_MCOSTt_NrSocSimET")
RCs<-c("RC1", "RC2","RC3")

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

load( file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/DistGeo_dfB_perSO_av_NOCLIMSIMNB.rds") # this is the same regardless of climate
identical(colnames(distm), rownames(distm))
#distm<-distm[dfb0$Soci, dfb0$Soci]
sum(!rownames(distm)%in%dfb0$Soci)
sum(!dfb0$Soci%in%rownames(distm))

if(!identical(rownames(distm), dfb0$Soci)){
  # add to distm and get rid if too many
  which(!rownames(distm)%in%dfb0$Soci)
  which(!colnames(distm)%in%dfb0$Soci)
  tokick<- which(!rownames(distm)%in%dfb0$Soci)
  distm<-distm[-tokick,-tokick]
  sum(!rownames(distm)%in%dfb0$Soci)
  
  if(sum(!dfb0$Soci%in%rownames(distm))>0){ # add these ones...!!
    toadd<-dfb0$Soci[!dfb0$Soci%in%rownames(distm)]
    rown<-rownames(distm)
    for(j in 1:length(toadd)){
      tj<-toadd[j]
      
      extract_coord_cols<-function(x) {return(c(dfi0[dfi0$soci%in%x,]$long, dfi0[dfi0$soci%in%x,]$lat))}
      list_coords<-lapply(rownames(distm), FUN=extract_coord_cols)
      coordi<-c(dfi0[dfi0$soci%in%tj,]$long,dfi0[dfi0$soci%in%tj,]$lat)
      #list_coords<-c(list_coords, list(coordi))
      to_save<-unlist(lapply(list_coords,distGeo,p1=coordi) )
      distm<-rbind(distm,to_save)
      distm<-cbind(distm,c(to_save,0))
      rownames(distm)[dim(distm)[1]]<-tj
      colnames(distm)[dim(distm)[1]]<-tj
      print(j)
    }
  }
  
}

dim(distm)
distm[991:1003,991:1003] 

# order distm by dfb0 or vice versa
dfb0<-dfb0[match(rownames(distm), dfb0$Soci),]
dim(dfb0)
# ------------------------------------------------------------------------------------------------------------------------------------

# models / area

library(letsR)
library(spdep)
source("scripts/10_rLCP_P2_PCA_modelsFXN.R")

varlist<-calc_var_per_cont(dfplot=dfb0, column="oragrF")
wcol<-weightcol(dfplot=dfb0, column="oragrF", varlist)

library(letsR)
library(devtools)
library(spdep)
library(spatialreg)


if(whichk%in%c("8k", "12k","4k")){
  
  # model 1
  lmbase <- lm(RC1~oragrF , data=dfb0)
  coords<-cbind(dfb0$long,dfb0$lat) #Make a matrix of coordinates (X and Y coordinates)
  coords<-as.matrix(coords)
  row.names(coords)<-dfb0$Soci
  nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
  sarcol <- SpatialFiltering(RC1 ~  oragrF, data=dfb0, nb=nb, style="W", ExactEV=TRUE)
  save(sarcol, file=paste0("/Volumes/Angela_5TB/WashUDesktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/spf1500_RC1_NOSIMCLIMNB.rds"))
  print(whichk)
  print("model1")
  
  # model 3
  if(whichk%in%c("12k","4k")){
    lmbase <- lm(RC3~oragrF , data=dfb0)
    coords<-cbind(dfb0$long,dfb0$lat) #Make a matrix of coordinates (X and Y coordinates)
    coords<-as.matrix(coords)
    row.names(coords)<-dfb0$Soci
    nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
    sarcol <- SpatialFiltering(RC3 ~  oragrF, data=dfb0, nb=nb, style="W", ExactEV=TRUE)
    save(sarcol, file=paste0("/Volumes/Angela_5TB/WashUDesktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/spf1500_RC3_NOSIMCLIMNB.rds"))
    
  }
print(whichk)
print("model3")
}


#################
####### PCA ##### dfi2500
#################
