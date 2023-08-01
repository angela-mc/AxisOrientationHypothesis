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

pdf(paste0("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/sac/MI_nospace_",fulldata,"_oragr.pdf"), height = 15, width = 10)
par(mfrow=c(4,3))

# model 1
mod1<-lm(RC1~oragrF , data=dfb0,weights=wcol[[1]])
MI_plot(model=mod1,dfb0,distm)
title(main=paste0(RC_names[RCs%in%"RC1"], "\nnon-spatial"))
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mi1<-moran.test(residuals(mod1), nb2listw(nb)) # same res
mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

# # ac
# coords <- as.matrix(cbind(dfb0$long, dfb0$lat) ) # define cell coordinates
# ac <- autocov_dist(as.numeric(dfb0$RC1), coords, nbs = 1500, longlat = TRUE, zero.policy = T) # 1000 warning, 1500 ok ie 1500 km(?)
# f2 <- RC1 ~ oragrF  + ac
# mod1 <- lm(f2, data = dfb0, weights = wcol[[1]])
# MI_plot(model=mod1,dfb0,distm)
# title(main=paste0(RC_names[RCs%in%"RC1"], "\n ac"))
# mi1<-moran.test(residuals(mod1), nb2listw(nb)) # same res
# mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

# sp filtering
load("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/sac/SpFilter/spf1500_RC1.rds")
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci # verify how many needed for Moran's I test
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mod1<-lm(RC1 ~ oragrF +fitted(sarcol) ,data = dfb0,weights = wcol[[1]]) # add all for this part
MI_plot(model=mod1,dfb0,distm)
title(main=paste0(RC_names[RCs%in%"RC1"], "\n SpFiltering"))
mi1<-moran.test(residuals(mod1), nb2listw(nb))
mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))


# model 2
mod2<-lm(RC2~oragrF , data=dfb0,weights= wcol[[2]]) 
MI_plot(model=mod2,dfb0,distm)
title(main=paste0(RC_names[RCs%in%"RC2"], "\nnon-spatial"))
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mi1<-moran.test(residuals(mod2), nb2listw(nb)) # same res
mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

# # ac
# coords <- as.matrix(cbind(dfb0$long, dfb0$lat) ) # define cell coordinates
# ac <- autocov_dist(as.numeric(dfb0$RC2), coords, nbs = 1500, longlat = TRUE, zero.policy = T) # 1000 warning, 1500 ok ie 1500 km(?)
# f2 <- RC2 ~ oragrF  + ac
# mod2 <- lm(f2, data = dfb0, weights = wcol[[2]])
# MI_plot(model=mod2,dfb0,distm)
# title(main=paste0(RC_names[RCs%in%"RC2"], "\n ac"))
# mi1<-moran.test(residuals(mod2), nb2listw(nb)) # same res
# mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

# sp filtering
load("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/sac/SpFilter/spf1500_RC2.rds")
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci # verify how many needed for Moran's I test
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mod2<-lm(RC2 ~ oragrF +fitted(sarcol) ,data = dfb0,weights = wcol[[2]]) # add all for this part
MI_plot(model=mod2,dfb0,distm)
title(main=paste0(RC_names[RCs%in%"RC2"], "\n SpFiltering"))
mi1<-moran.test(residuals(mod2), nb2listw(nb))
mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))


# model 3
mod3<-lm(RC3~oragrF , data=dfb0,weights= wcol[[3]]) 
MI_plot(model=mod3,dfb0,distm)
title(main=paste0(RC_names[RCs%in%"RC3"], "\nnon-spatial"))
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mi1<-moran.test(residuals(mod3), nb2listw(nb)) # same res
mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

# # ac
# coords <- as.matrix(cbind(dfb0$long, dfb0$lat) ) # define cell coordinates
# ac <- autocov_dist(as.numeric(dfb0$RC3), coords, nbs = 1500, longlat = TRUE, zero.policy = T) # 1000 warning, 1500 ok ie 1500 km(?)
# f2 <- RC3 ~ oragrF  + ac
# mod3 <- lm(f2, data = dfb0, weights = wcol[[3]])
# MI_plot(model=mod3,dfb0,distm)
# title(main=paste0(RC_names[RCs%in%"RC3"], "\n ac"))
# mi1<-moran.test(residuals(mod3), nb2listw(nb)) # same res
# mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

# sp filtering
load("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/sac/SpFilter/spf1500_RC3.rds")
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci # verify how many needed for Moran's I test
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mod3<-lm(RC3 ~ oragrF +fitted(sarcol) ,data = dfb0,weights = wcol[[3]]) # add all for this part
MI_plot(model=mod3,dfb0,distm)
title(main=paste0(RC_names[RCs%in%"RC3"], "\n SpFiltering"))
mi1<-moran.test(residuals(mod3), nb2listw(nb))
mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))


# model 4
mod4<-lm(RC4~oragrF , data=dfb0, weights = wcol[[4]]) # needs addig var mix - for residuals same thing
MI_plot(model=mod4,dfb0,distm)
title(main=paste0(RC_names[RCs%in%"RC4"], "\nnon-spatial"))
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mi1<-moran.test(residuals(mod4), nb2listw(nb)) # same res
mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

# # ac
# coords <- as.matrix(cbind(dfb0$long, dfb0$lat) ) # define cell coordinates
# ac <- autocov_dist(as.numeric(dfb0$RC4), coords, nbs = 1500, longlat = TRUE, zero.policy = T) # 1000 warning, 1500 ok ie 1500 km(?)
# f2 <- RC4 ~ oragrF  + ac
# mod4 <- lm(f2, data = dfb0, weights = wcol[[4]])
# MI_plot(model=mod4,dfb0,distm)
# title(main=paste0(RC_names[RCs%in%"RC4"], "\n ac"))
# mi1<-moran.test(residuals(mod4), nb2listw(nb)) # same res
# mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

# sp filtering
load("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/sac/SpFilter/spf1500_RC4.rds")
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci # verify how many needed for Moran's I test
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mod4<-lm(RC4 ~ oragrF +fitted(sarcol) ,data = dfb0,weights = wcol[[4]]) # add all for this part
MI_plot(model=mod4,dfb0,distm)
title(main=paste0(RC_names[RCs%in%"RC4"], "\n SpFiltering"))
mi1<-moran.test(residuals(mod4), nb2listw(nb))
mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))
dev.off()



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

pdf(paste0("data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/sac/MI_nospace_",fulldata,"_oragr.pdf"), height = 15, width = 10)
par(mfrow=c(4,3)) # 4 models

# model 1 - order for plot
mod4<-lm(RC4~oragrF , data=dfb0, weights = wcol[[4]]) # needs addig var mix - for residuals same thing
MI_plot(model=mod4,dfb0,distm)
title(main=paste0(RC_names[RCs%in%"RC4"], "\nnon-spatial"))
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mi1<-moran.test(residuals(mod4), nb2listw(nb)) # same res
mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

# # ac
# coords <- as.matrix(cbind(dfb0$long, dfb0$lat) ) # define cell coordinates
# ac <- autocov_dist(as.numeric(dfb0$RC4), coords, nbs = 1500, longlat = TRUE, zero.policy = T) # 1000 warning, 1500 ok ie 1500 km(?)
# f2 <- RC4 ~ oragrF  + ac
# mod4 <- lm(f2, data = dfb0, weights = wcol[[4]])
# MI_plot(model=mod4,dfb0,distm)
# title(main=paste0(RC_names[RCs%in%"RC4"], "\n ac"))
# mi1<-moran.test(residuals(mod4), nb2listw(nb)) # same res
# mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

#sp filtering
load("data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/sac/SoFilter/")
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci # verify how many needed for Moran's I test
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mod4<-lm(RC4 ~ oragrF +fitted(sarcol) ,data = dfb0,weights = wcol[[4]]) # add all for this part
MI_plot(model=mod4,dfb0,distm)
title(main=paste0(RC_names[RCs%in%"RC4"], "\n SpFiltering"))
mi1<-moran.test(residuals(mod4), nb2listw(nb))
mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

#plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')


# model 2
mod2<-lm(RC2~oragrF , data=dfb0,weights= wcol[[2]]) 
MI_plot(model=mod2,dfb0,distm)
title(main=paste0(RC_names[RCs%in%"RC2"], "\nnon-spatial"))
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mi1<-moran.test(residuals(mod2), nb2listw(nb)) # same res
mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

# # ac
# coords <- as.matrix(cbind(dfb0$long, dfb0$lat) ) # define cell coordinates
# ac <- autocov_dist(as.numeric(dfb0$RC2), coords, nbs = 1500, longlat = TRUE, zero.policy = T) # 1000 warning, 1500 ok ie 1500 km(?)
# f2 <- RC2 ~ oragrF  + ac
# mod2 <- lm(f2, data = dfb0, weights = wcol[[2]])
# MI_plot(model=mod2,dfb0,distm)
# title(main=paste0(RC_names[RCs%in%"RC2"], "\n ac"))
# mi1<-moran.test(residuals(mod2), nb2listw(nb)) # same res
# mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

# sp filtering
load("data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/sac/SoFilter/spf1500_RC2.rds")
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci # verify how many needed for Moran's I test
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mod2<-lm(RC2 ~ oragrF +fitted(sarcol) ,data = dfb0,weights = wcol[[2]]) # add all for this part
MI_plot(model=mod2,dfb0,distm)
title(main=paste0(RC_names[RCs%in%"RC2"], "\n SpFiltering"))
mi1<-moran.test(residuals(mod2), nb2listw(nb))
mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))


# model 3
mod3<-lm(RC3~oragrF , data=dfb0,weights= wcol[[3]]) 
MI_plot(model=mod3,dfb0,distm)
title(main=paste0(RC_names[RCs%in%"RC3"], "\nnon-spatial"))
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mi1<-moran.test(residuals(mod3), nb2listw(nb)) # same res
mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

# # ac
# coords <- as.matrix(cbind(dfb0$long, dfb0$lat) ) # define cell coordinates
# ac <- autocov_dist(as.numeric(dfb0$RC3), coords, nbs = 1500, longlat = TRUE, zero.policy = T) # 1000 warning, 1500 ok ie 1500 km(?)
# f2 <- RC3 ~ oragrF  + ac
# mod3 <- lm(f2, data = dfb0, weights = wcol[[3]])
# MI_plot(model=mod3,dfb0,distm)
# title(main=paste0(RC_names[RCs%in%"RC3"], "\n ac"))
# mi1<-moran.test(residuals(mod3), nb2listw(nb)) # same res
# mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

# sp filtering
load("data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/sac/SoFilter/spf1500_RC3.rds")
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci # verify how many needed for Moran's I test
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mod3<-lm(RC3 ~ oragrF +fitted(sarcol) ,data = dfb0,weights = wcol[[3]]) # add all for this part
MI_plot(model=mod3,dfb0,distm)
title(main=paste0(RC_names[RCs%in%"RC3"], "\n SpFiltering"))
mi1<-moran.test(residuals(mod3), nb2listw(nb))
mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))


# model 4
mod1<-lm(RC1~oragrF , data=dfb0,weights= wcol[[1]]) 
MI_plot(model=mod1,dfb0,distm)
title(main=paste0(RC_names[RCs%in%"RC1"], "\nnon-spatial"))
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mi1<-moran.test(residuals(mod1), nb2listw(nb)) # same res
mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

# # ac
# coords <- as.matrix(cbind(dfb0$long, dfb0$lat) ) # define cell coordinates
# ac <- autocov_dist(as.numeric(dfb0$RC1), coords, nbs = 1500, longlat = TRUE, zero.policy = T) # 1000 warning, 1500 ok ie 1500 km(?)
# f2 <- RC1 ~ oragrF  + ac
# mod1 <- lm(f2, data = dfb0, weights = wcol[[1]])
# MI_plot(model=mod1,dfb0,distm)
# title(main=paste0(RC_names[RCs%in%"RC1"], "\n ac"))
# mi1<-moran.test(residuals(mod1), nb2listw(nb)) # same res
# mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

#sp filtering
load("data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/sac/SoFilter/")
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci # verify how many needed for Moran's I test
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mod1<-lm(RC1 ~ oragrF +fitted(sarcol) ,data = dfb0,weights = wcol[[1]]) # add all for this part
MI_plot(model=mod1,dfb0,distm)
title(main=paste0(RC_names[RCs%in%"RC1"], "\n SpFiltering"))
mi1<-moran.test(residuals(mod1), nb2listw(nb))
mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))
#plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')

dev.off()




