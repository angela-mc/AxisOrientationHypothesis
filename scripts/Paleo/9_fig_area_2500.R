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

##################################
####### read data and models ##### dfi2500
##################################


# df2500
load( file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi2500_PCA_nf4/myPCA_perSO_av.rds"))
read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi2500_PCA_nf4/dfB_PCA_perSO_av.csv"), stringsAsFactors = F)->dfb2500
myPCA$loadings
if(whichk%in%"8k") RC_names<-c("TempD_MCOSTt","LPCtLCPx","XericD_MCOSTx","GeoD_LCPsl_MCOSTe") ; RCs<-c("RC1", "RC2","RC3","RC4")
# logLCPe      0.669  0.617              between RC1 and RC4 - leave it on RC4
if(whichk%in%"12k") RC_names<-c("TempD_MCOSTt","LPCtLCPx","XericD_MCOSTx","GeoD_LCPsl_MCOSTe") ; RCs<-c("RC1", "RC2","RC3","RC4") 
# logLCPe      0.708  0.576      between RC1 and RC4 - leave it on RC4
if(whichk%in%"4k")RC_names<-c("TempD_MCOSTt","LPCtLCPx","XericD_MCOSTx","GeoD_LCPsl_MCOSTe") ; RCs<-c("RC1", "RC2","RC3","RC4") 

# prep data
dfb2500$contF<-factor(dfb2500$cont,levels=c("Asia","Africa","SouthAmerica","Mesoamerica","NorthAmerica"))
dfb2500[dfb2500$oragr%in%"Lower-MiddleY",]$oragr<-"Lower_MiddleY"
dfb2500[dfb2500$oragr%in%"NW_Lowland_SA",]$oragr<-"N_Lowland_SA" # merge SAM Lowlands
dfb2500<-dfb2500[!dfb2500$oragr%in%"S_India",] # rem 1 soc area
lvls<- c("South trop ch", "Lower_MiddleY" ,"Chinese_loess", "W_Yuman_E_Tib","Fertile_Cresc", "Sava_W_India","Ganges_E_Indi"  ,
         "West Africa T", "W_African_Sav","Sudanic_Savan", "Ethipian plat",
         "N_Lowland_SA","C/S_Andes","Southwes amaz" ,
         "Mesoamerica","E_North_Ameri")
dfb2500$oragrF<-factor(dfb2500$oragr,levels=lvls)

# read models ~ area from lr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi2500_PCA_nf4/mod1_area.rds")) ; mod1->mod1lr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi2500_PCA_nf4/mod2_area.rds")) ; mod2->mod2lr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi2500_PCA_nf4/mod3_area.rds")) ; mod3->mod3lr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi2500_PCA_nf4/mod4_area.rds")) ; mod4->mod4lr
## ------------------------------------------------------------------------------------------------------------------------------------
 
rm(mod1) ; rm(mod2) ;rm(mod3) ;rm(mod4)
#identical(levels(dfb2500$oragrF),levels(dfb0$oragrF))
names_lvls<-c("South Tropical China", "Lower-Middle Yangtze" ,"Chinese loess plateau", "West Yunan & East Tibet","Fertile Crescent", "Sava West India","Ganges of East India",
              "West Africa", "West African Savannah","Sudanic savannah", "Ethiopian plateau", 
              "Northern Lowlands of South America","Central/Southern Andes","Southwest Amazon" ,
              "Mesoamerica","East North America")

# 
# #####################
# #### ordered means ##
# #####################
# 
# # lr
# pdf(paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/areasM_ordered_lr.pdf"), height = 8,width = 13)
# 
# par(mfrow=c(2,2))
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers(names_lvls,lvls, dfb2500, whichRC="RC4", title="Topographic travel costs", add_to_lim=0.3, add_to_axis=0.5,cexp = "equal")
# if(whichk%in%"12k") mtext("d", font=2, side=3,adj=0)
# if(whichk%in%"8k") mtext("d", font=2, side=3,adj=0)
# if(whichk%in%"4k") mtext("f", font=2, side=3,adj=0)
# 
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers(names_lvls,lvls, dfb2500, whichRC="RC2", title="Climate-related path lengths", add_to_lim=0.3, add_to_axis=0.5,cexp = "equal")
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers(names_lvls,lvls, dfb2500, whichRC="RC1", title="Temperature turnover", add_to_lim=0.3, add_to_axis=0.5,cexp = "equal")
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers(names_lvls,lvls, dfb2500, whichRC="RC3", title="Aridity turnover", add_to_lim=0.3, add_to_axis=0.5,cexp = "equal")
# dev.off()
# # ------------------------------------------------------------------------------------------------------------------------------------


#####################
#### ordered means ## continent
#####################

# lr
pdf(paste0("manuscript/1FinalSub_JuneNature/Fig3/ExtD_fig10_", whichk,"_areasM_ordered_lr.pdf"), height = 8,width = 13)

par(mfrow=c(2,2))
par(mar=c(c(2, 7, 2, 2)))
find_order_barriers_venom(names_lvls,lvls, dfb2500, whichRC="RC4", title="Topographic travel costs", add_to_lim1=0, add_to_lim2=0.8,add_to_axis=0.5,cexp = "equal")
# if(whichk%in%"12k") mtext("d", font=2, side=3,adj=0)
# if(whichk%in%"8k") mtext("d", font=2, side=3,adj=0)
# if(whichk%in%"4k") mtext("f", font=2, side=3,adj=0)

par(mar=c(c(2, 7, 2, 2)))
find_order_barriers_venom(names_lvls,lvls, dfb2500, whichRC="RC2", title="Climate-related path lengths", add_to_lim1=0.3, add_to_lim2=0,add_to_axis=0.5,cexp = "equal")
par(mar=c(c(2, 7, 2, 2)))
find_order_barriers_venom(names_lvls,lvls, dfb2500, whichRC="RC1", title="Temperature turnover", add_to_lim1=0.3, add_to_axis=0.5,cexp = "equal")
par(mar=c(c(2, 7, 2, 2)))
find_order_barriers_venom(names_lvls,lvls, dfb2500, whichRC="RC3", title="Aridity turnover", add_to_lim1=4, add_to_lim2=0.3,add_to_axis=0.5,cexp = "equal")
dev.off()
# ------------------------------------------------------------------------------------------------------------------------------------

# 
# #####################
# #### Tukey matrices # 
# #####################
# 
# # lr
# ANOVA=aov(mod1lr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/lr_Tukey_areas1","_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered(names_lvls,lvls, dfb2500,whichRC="RC1",dfres,title="")
# dev.off()
# 
# ANOVA=aov(mod2lr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/lr_Tukey_areas2","_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered(names_lvls,lvls, dfb2500,whichRC="RC2",dfres,title="")
# dev.off()
# 
# ANOVA=aov(mod3lr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/lr_Tukey_areas3","_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered(names_lvls,lvls, dfb2500,whichRC="RC3",dfres,title="")
# dev.off()
# 
# ANOVA=aov(mod4lr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/lr_Tukey_areas4","_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered(names_lvls,lvls, dfb2500,whichRC="RC4",dfres,title="")
# dev.off()
# 



#####################
#### Tukey matrices # continent
#####################

fulldata<-"no_av"
# lr
ANOVA=aov(mod1lr) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
TUKEY$oragrF->dfres

pdf(paste0("manuscript/1FinalSub_JuneNature/Fig3/ExtD_fig10_", whichk,"_lr_Tukey_areas1_",fulldata,"_matrix.pdf"), height = 5, width = 5)
par(mar=c(2,2,2,2))
mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb2500,whichRC="RC1",dfres,title="")
dev.off()

ANOVA=aov(mod2lr) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
TUKEY$oragrF->dfres

pdf(paste0("manuscript/1FinalSub_JuneNature/Fig3/ExtD_fig10_", whichk,"_lr_Tukey_areas2_",fulldata,"_matrix.pdf"), height = 5, width = 5)
par(mar=c(2,2,2,2))
mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb2500,whichRC="RC2",dfres,title="")
dev.off()

ANOVA=aov(mod3lr) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
TUKEY$oragrF->dfres

pdf(paste0("manuscript/1FinalSub_JuneNature/Fig3/ExtD_fig10_", whichk,"_lr_Tukey_areas3_",fulldata,"_matrix.pdf"), height = 5, width = 5)
par(mar=c(2,2,2,2))
mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb2500,whichRC="RC3",dfres,title="")
dev.off()

ANOVA=aov(mod4lr) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
TUKEY$oragrF->dfres

pdf(paste0("manuscript/1FinalSub_JuneNature/Fig3/ExtD_fig10_", whichk,"_lr_Tukey_areas4_",fulldata,"_matrix.pdf"), height = 5, width = 5)
par(mar=c(2,2,2,2))
mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb2500,whichRC="RC4",dfres,title="")
dev.off()


