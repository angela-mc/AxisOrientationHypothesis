
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
source("scripts/10_rLCP_P2_PCA_modelsFXN.R")

varlist<-calc_var_per_cont(dfplot=dfb0, column="oragrF")
wcol<-weightcol(dfplot=dfb0, column="oragrF", varlist)


# only xeric needs it by MI test, but plot looks ok 
RC_names ; RCs

# model 1 - no need for SAC
#load("data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/sac/SoFilter/spf1500_RC1.rds")
mod1<-lm( RC1 ~ oragrF, data = dfb0, weights = wcol[[1]]) 
save(mod1, file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/models_space/mod1_area.rds")


# model2
mod2 <- lm(RC2 ~ oragrF, data = dfb0, weights = wcol[[2]]) 
summary(mod2)
save(mod2, file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/models_space/mod2_area.rds")


# model3
mod3 <- lm(RC3 ~ oragrF, data = dfb0, weights = wcol[[3]]) 
summary(mod3)

load("data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/sac/SoFilter/spf1500_RC3.rds")
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci 
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
(42+ dim(fitted(sarcol))[2]-1)
dfb0[,42: (42+ dim(fitted(sarcol))[2]-1)]<-fitted(sarcol)
mod3_2<-lm(RC3 ~ oragrF +V42,data = dfb0,weights=wcol[[3]] ) # double check same results (only one diff ChLoess marginally sign in space, marginally not sign in no-space)
mi1<-moran.test(residuals(mod3_2), nb2listw(nb)) 
mi1
save(mod3, file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/models_space/mod3_area.rds")


mod4<-lm( RC4 ~ oragrF, data = dfb0, weights = wcol[[4]]) 
save(mod4, file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/models_space/mod4_area.rds")

