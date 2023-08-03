rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)

#################
####### PCA ##### dfi0
#################

load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/myPCA_perSO_av_NOCLIMSIMNB.rds")
read.csv(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/dfB_PCA_perSO_av_NOCLIMSIMNB.csv", stringsAsFactors = F)-> dfb0
myPCA$loadings
RC_names<-c("GeoD_LCPsl_MCOSTe","XericD_MCOSTx","TempD_MCOSTt")
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

  load( file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/DistGeo_dfB_perSO_av_NOCLIMSIMNB.rds") # built in 7_SpatialFiltering
  dim(distm)
  distm<-distm[dfb0$Soci, dfb0$Soci]
  identical(rownames(distm), dfb0$Soci)
  
# ------------------------------------------------------------------------------------------------------------------------------------

# models / area
library(letsR)
library(spdep)
source("scripts/10_rLCP_P2_PCA_modelsFXN.R")

varlist<-calc_var_per_cont(dfplot=dfb0, column="oragrF")
wcol<-weightcol(dfplot=dfb0, column="oragrF", varlist)

pdf(paste0("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/sac/MI_nospace_","_oragr_NOCLIMSIMNB.pdf"), height = 3.5, width = 10)
# pdf(paste0("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/sac/MI_FIgS_nospace_",fulldata,"_cont.pdf"), height = 3.5, width = 10)
par(mfrow=c(1,4))

# model 1
mod1<-lm(RC1~oragrF , data=dfb0,weights=wcol[[1]])
MI_plot(model=mod1,dfb0,distm)
# title(main=paste0(RC_names[RCs%in%"RC1"], "\nnon-spatial"))
title(main=paste0("Topographic travel costs\n&climate-related path lengths"))
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mi1<-moran.test(residuals(mod1), nb2listw(nb)) # same res
# mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

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
load("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/sac/SpFilter/spf1500_RC1_NOSIMCLIMNB.rds") # built in 7_SpatialFiltering
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci # verify how many needed for Moran's I test
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
# mod1<-lm(RC1 ~ oragrF +fitted(sarcol) ,data = dfb0,weights = wcol[[1]]) # add all for this part
mod1<-lm(RC1 ~ oragrF +sarcol$dataset[,1]+sarcol$dataset[,2] ,data = dfb0,weights = wcol[[1]]) # add all for this part
matx<-as.matrix(mod1$residuals) # this is the one with sp filtering
rownames(matx)<-dfb0$Soci
c<-lets.correl(x = matx, y=distm, z=12,equidistant = T, plot = F) # for P2 the data are clumper per regions so I do not want #socs equal in bins, I want per distance
toplot<-nrow(c)/2
points(c[1:toplot,1] ~ c[1:toplot,5], ylim=c(-1,1), pch=19, col="red")
lines(c[1:toplot,1] ~ c[1:toplot,5], ylim=c(-1,1), lty=2, col="red")
for(j in 1:toplot) # add SE
{arrows(x0= c[j,5], y0=c[j,1], x1=c[j,5], y1= (c[j,1]+ c[j,2]), angle = 180, col=scales::alpha("red",0.5))
  arrows(x0= c[j,5], y0=c[j,1], x1=c[j,5], y1= (c[j,1]- c[j,2]), angle = 180,col=scales::alpha("red",0.5))}
#MI_plot(model=mod1,dfb0,distm)
#title(main=paste0(RC_names[RCs%in%"RC1"], "\n SpFiltering"))
#mi1<-moran.test(residuals(mod1), nb2listw(nb))
#mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

# model 2
mod2<-lm(RC2~oragrF , data=dfb0,weights= wcol[[2]]) 
MI_plot(model=mod2,dfb0,distm)
# title(main=paste0(RC_names[RCs%in%"RC2"], "\nnon-spatial"))
title(main=paste0("Aridity turnover"))
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mi1<-moran.test(residuals(mod2), nb2listw(nb)) # same res
# mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))

# model 3
mod3<-lm(RC3~oragrF , data=dfb0,weights= wcol[[3]]) 
MI_plot(model=mod3,dfb0,distm)
# title(main=paste0(RC_names[RCs%in%"RC3"], "\nnon-spatial"))
title(main=paste0("Temperature turnover"))
coords<-cbind(dfb0$long,dfb0$lat)  ; coords<-as.matrix(coords) ; row.names(coords)<-dfb0$Soci
nb<-dnearneigh(coords,row.names = row.names(coords), d1=0,d2=1500,longlat=T)
mi1<-moran.test(residuals(mod3), nb2listw(nb)) # same res
# mtext( text = paste0("p = ",round(mi1$p.value, digits=2)),  side=3, line = -2, adj=0.5, font = ifelse(mi1$p.value<0.05,2,1))
dev.off()

#################
####### PCA ##### dfi2500 - see analyses with CLIMSIMNB (that predictor was never for long-distance, so those analyses are same)
#################

