rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)

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
source("scripts/10_rLCP_P2_PCA_modelsFXN.R")

varlist<-calc_var_per_cont(dfplot=dfb0, column="oragrF")
wcol<-weightcol(dfplot=dfb0, column="oragrF", varlist)

# model 1
# coords <- as.matrix(cbind(dfb0$long, dfb0$lat) ) # define cell coordinates
# ac <- autocov_dist(as.numeric(dfb0$RC1), coords, nbs = 1500, longlat = TRUE, zero.policy = T) # 1000 warning, 1500 ok ie 1500 km(?)
# f2 <- RC1 ~ oragrF  + ac  # specify new formula
# mod1 <- lm(f2, data = dfb0,weights=wcol[[1]]) # run new model
# summary(mod1)
load("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/sac/SpFilter/spf1500_RC1.rds")
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci # verify how many needed for Moran's I test
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
(42+ dim(fitted(sarcol))[2]-1)
dfb0[,42: (42+ dim(fitted(sarcol))[2]-1)]<-fitted(sarcol)
mod1<-lm(RC1 ~ oragrF +V42,data = dfb0,weights=wcol[[1]] )
mi1<-moran.test(residuals(mod1), nb2listw(nb)) 
mi1

ANOVA=aov(mod1) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95)
par(mar=c(5,15,4,4)) ; plot(TUKEY , las=1 , col="brown")
#TUKEY$cont2F->dfres
save(mod1, file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/models_space/mod1_area.rds")



# model 2 - according to plot does not need it, but according to MI test it does (respect plot)
load("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/sac/SpFilter/spf1500_RC2.rds")
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci # verify how many needed for Moran's I test
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
(42+ dim(fitted(sarcol))[2]-1)
dfb0[,42: (42+ dim(fitted(sarcol))[2]-1)]<-fitted(sarcol)
mod2<-lm(RC2 ~ oragrF +V42,data = dfb0,weights=wcol[[2]] )
mi1<-moran.test(residuals(mod2), nb2listw(nb)) 
mi1

mod2_2<-lm(RC2 ~ oragrF,data = dfb0,weights=wcol[[2]] ) # double check - same results 

ANOVA=aov(mod2) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95)
par(mar=c(5,15,4,4)) ; plot(TUKEY , las=1 , col="brown")
mod2<-lm(RC2 ~ oragrF,data = dfb0,weights=wcol[[2]] ) 
save(mod2, file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/models_space/mod2_area.rds")


# model 3
# load("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/sac/SpFilter/spf1500_RC3.rds")
# coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci # verify how many needed for Moran's I test
# nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
# (42+ dim(fitted(sarcol))[2]-1)
# dfb0[,42: (42+ dim(fitted(sarcol))[2]-1)]<-fitted(sarcol)
# mod2<-lm(RC3 ~ oragrF +V42,data = dfb0,weights=wcol[[3]] )
# mi1<-moran.test(residuals(mod3), nb2listw(nb)) 
# mi1

mod3<-lm(RC3 ~ oragrF,data = dfb0,weights=wcol[[3]] ) # does nto need spf

ANOVA=aov(mod3) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95)
par(mar=c(5,15,4,4)) ; plot(TUKEY , las=1 , col="brown")
save(mod3, file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/models_space/mod3_area.rds")


# model 4 - according to plot does not need it, but according to MI test it does (check res are the same, if they are - respect plot)
load("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/sac/SpFilter/spf1500_RC4.rds")
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci # verify how many needed for Moran's I test
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
(42+ dim(fitted(sarcol))[2]-1)
dfb0[,42: (42+ dim(fitted(sarcol))[2]-1)]<-fitted(sarcol)
mod4<-lm(RC4 ~ oragrF +V42,data = dfb0,weights=wcol[[4]] )
mi1<-moran.test(residuals(mod4), nb2listw(nb)) 
mi1

mod4_2<-lm(RC4 ~ oragrF,data = dfb0,weights=wcol[[4]] ) # almost identical results -> respect plot

ANOVA=aov(mod4) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95)
par(mar=c(5,15,4,4)) ; plot(TUKEY , las=1 , col="brown")
mod4<-lm(RC4 ~ oragrF,data = dfb0,weights=wcol[[4]] ) 
save(mod4, file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/models_space/mod4_area.rds")




