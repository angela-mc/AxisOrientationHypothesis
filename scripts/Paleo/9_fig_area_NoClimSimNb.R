rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)
library(geosphere)

library(letsR)
library(spdep)
source("scripts/10_rLCP_P2_PCA_modelsFXN.R")

whichk<-"8k"
whichk<-"12k"
whichk<-"4k"


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


#####################
#### ordered means ## by continent
#####################

# cr
pdf(paste0("manuscript/Figures_EDGE/Paleo_fig10_", whichk,"_areasM_ordered_cr_NOSIMCLIMNB.pdf"), height = 8,width = 13)
par(mfrow=c(2,2))
par(mar=c(c(2, 7, 2, 2)))
find_order_barriers_venom(names_lvls,lvls, dfb0, whichRC="RC1", title="Topographic travel costs\n&climate-related path lengths", add_to_lim1=0.7, add_to_axis=0.5,cexp = "equal")
par(mar=c(c(2, 7, 2, 2)))
find_order_barriers_venom(names_lvls,lvls, dfb0, whichRC="RC3", title="Temperature turnover", add_to_lim1=1.25, add_to_lim2=1.25,add_to_axis=0.5,cexp = "equal")
par(mar=c(c(2, 7, 2, 2)))
find_order_barriers_venom(names_lvls,lvls, dfb0, whichRC="RC2", title="Aridity turnover", add_to_lim1=3.5, add_to_lim2=0,add_to_axis=0.5,cexp = "equal")
dev.off()



#####################
#### Tukey matrices # by continent
#####################

# cr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/mod1_area_NOCLIMSIMNB.rds"))
mod1->mod1cr;rm(mod1)
ANOVA=aov(mod1cr) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
TUKEY$oragrF->dfres

pdf(paste0("manuscript/Figures_EDGE/Paleo_fig10_", whichk,"_cr_Tukey_areas1_","_matrix_NOCLIMSIMNB.pdf"), height = 5, width = 5)
par(mar=c(2,2,2,2))
mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb0,whichRC="RC1",dfres,title="")
dev.off()

load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/mod2_area_NOSIMCLIMNB.rds"))
mod2->mod2cr;rm(mod2)
ANOVA=aov(mod2cr) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
TUKEY$oragrF->dfres

pdf(paste0("manuscript/Figures_EDGE/Paleo_fig10_", whichk,"_cr_Tukey_areas2_","_matrix_NOCLIMSIMNB.pdf"), height = 5, width = 5)
par(mar=c(2,2,2,2))
mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb0,whichRC="RC2",dfres,title="")
dev.off()

load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/mod3_area_NOSIMCLIMNB.rds"))
mod3->mod3cr;rm(mod3)
ANOVA=aov(mod3cr) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
TUKEY$oragrF->dfres

pdf(paste0("manuscript/Figures_EDGE/Paleo_fig10_", whichk,"_cr_Tukey_areas3_","_matrix_NOCLIMSIMNB.pdf"), height = 5, width = 5)
par(mar=c(2,2,2,2))
mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb0,whichRC="RC3",dfres,title="")
dev.off()



