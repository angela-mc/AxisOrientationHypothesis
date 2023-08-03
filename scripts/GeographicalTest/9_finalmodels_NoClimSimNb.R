rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)
library(letsR)
library(spdep)
source("scripts/10_rLCP_P2_PCA_modelsFXN.R")


#####################
#### read data ######
#####################


# read models ~ cont from cr
load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/models_space/mod1_area_NOCLIMSIMNB.rds") ; mod1->mod1cr
load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/models_space/mod2_area_NOCLIMSIMNB.rds") ; mod2->mod2cr
load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/models_space/mod3_area_NOCLIMSIMNB.rds") ; mod3->mod3cr

summary(mod2cr)

load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/myPCA_perSO_av_NOCLIMSIMNB.rds")
read.csv(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/dfB_PCA_perSO_av_NOCLIMSIMNB.csv", stringsAsFactors = F)-> dfb0
myPCA$loadings
RC_names<-c("GeoD_LCPsl_MCOSTe","XericD_MCOSTx","TempD_MCOSTt")
RCs<-c("RC1", "RC2","RC3")

# read models ~ cont lr
load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/models_space/mod1_area.rds") ; mod1->mod1lr
load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/models_space/mod2_area.rds") ; mod2->mod2lr
load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/models_space/mod3_area.rds") ; mod3->mod3lr
load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/models_space/mod4_area.rds") ; mod4->mod4lr

read.csv(file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/dfB_PCA_perSO_av.csv", stringsAsFactors = F)-> dfb2500
RC_nameslr<-c("TempD_MCOSTt","LPCtLCPx","XericD_MCOSTx","GeoD_LCPsl_MCOSTe")
RCslr<-c("RC1", "RC2","RC3","RC4") 

rm(mod1) ; rm(mod2) ;rm(mod3) ;rm(mod4)
# ------------------------------------------------------------------------------------------------------------------------------------

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
dfb0$cont2<-dfb0$cont
dfb0[dfb0$cont%in%"Asia" & dfb0$oragr%in%c("Chinese_loess","Lower-MiddleY","Fertile_Cresc"),]$cont2<-"Asia2"
dfb0$cont2F<-factor(dfb0$cont2,levels=c("Asia","Asia2","Africa","SouthAmerica","Mesoamerica","NorthAmerica"))

dfb2500$contF<-factor(dfb2500$cont,levels=c("Asia","Africa","SouthAmerica","Mesoamerica","NorthAmerica"))
dfb2500[dfb2500$oragr%in%"Lower-MiddleY",]$oragr<-"Lower_MiddleY"
dfb2500[dfb2500$oragr%in%"NW_Lowland_SA",]$oragr<-"N_Lowland_SA" # merge SAM Lowlands
dfb0<-dfb0[!dfb0$oragr%in%"S_India",] # rem 1 soc area
dfb2500$oragrF<-factor(dfb2500$oragr,levels=lvls)

#identical(levels(dfb2500$oragrF),levels(dfb0$oragrF))
lvls<-levels(dfb2500$oragrF)
names_lvls<-c("South Tropical China", "Lower-Middle Yangtze" ,"Chinese loess plateau", "West Yunan & East Tibet","Fertile Crescent", "Sava West India","Ganges of East India",
             "West Africa", "West African Savannah","Sudanic savannah", "Ethiopian plateau", 
             "Northern Lowlands of South America","Central/Southern Andes","Southwest Amazon" ,
             "Mesoamerica","East North America")
## ------------------------------------------------------------------------------------------------------------------------------------
mean(dfb0[dfb0$oragr%in%"Chinese_loess",]$RC3) #TTurn
mean(dfb0[dfb0$oragr%in%"Fertile_Cresc",]$RC3) # smaller - ok
# ------------------------------------------------------------------------------------------------------------------------------------

# 
# #####################
# #### ordered means ##
# #####################
# 
# # cr
# pdf("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/figures/areasM_ordered_cr.pdf", height = 8,width = 13)
# par(mfrow=c(2,2))
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers(names_lvls,lvls, dfb0, whichRC="RC1", title="Topographic travel costs\n&climate-related path lengths", add_to_lim=0.7, add_to_axis=0.5,cexp = "equal")
# mtext("A", font=2, side=3,adj=0)
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers(names_lvls,lvls, dfb0, whichRC="RC2", title="Scarcity of nodes in the\necological neighborhood", add_to_lim=0.3, add_to_axis=0.5,cexp = "equal")
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers(names_lvls,lvls, dfb0, whichRC="RC3", title="Temperature turnover", add_to_lim=0.3, add_to_axis=0.5,cexp = "equal")
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers(names_lvls,lvls, dfb0, whichRC="RC4", title="Aridity turnover", add_to_lim=1.25, add_to_axis=0.5,cexp = "equal")
# dev.off()
# 
# # lr
# pdf("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/figures/areasM_ordered_lr.pdf", height = 8,width = 13)
# 
# par(mfrow=c(2,2))
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers(names_lvls,lvls, dfb2500, whichRC="RC4", title="Topographic travel costs", add_to_lim=0.3, add_to_axis=0.5,cexp = "equal")
# mtext("B", font=2, side=3,adj=0)
# 
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers(names_lvls,lvls, dfb2500, whichRC="RC2", title="Climate-related path lengths", add_to_lim=0.3, add_to_axis=0.5,cexp = "equal")
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers(names_lvls,lvls, dfb2500, whichRC="RC1", title="Temperature turnover", add_to_lim=0.3, add_to_axis=0.5,cexp = "equal")
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers(names_lvls,lvls, dfb2500, whichRC="RC3", title="Aridity turnover", add_to_lim=0.3, add_to_axis=0.5,cexp = "equal")
# dev.off()
# # ------------------------------------------------------------------------------------------------------------------------------------
# mean(dfb2500[dfb2500$oragr%in%"Lower_MiddleY",]$RC3) 
# mean(dfb2500[dfb2500$oragr%in%"South trop ch",]$RC3)
# mean(dfb2500[dfb2500$oragr%in%"C/S_Andes",]$RC3) 
# 
# mean(dfb2500[dfb2500$oragr%in%"Lower_MiddleY",]$RC1) 
# mean(dfb2500[dfb2500$oragr%in%"South trop ch",]$RC1)
# mean(dfb2500[dfb2500$oragr%in%"C/S_Andes",]$RC1) 


#####################
#### ordered means ## by continent
#####################


# cr
pdf("manuscript/Figures_EDGE/Fig3a_altorder_NOCLIMSIMNB.pdf", height = 8,width = 13)
par(mfrow=c(2,2))
par(mar=c(c(2, 7, 2, 2)))
find_order_barriers_venom(names_lvls,lvls, dfb0, whichRC="RC1", title="Topographic travel costs\n&climate-related path lengths", add_to_lim1=0.7, add_to_axis=0.5,cexp = "equal")
mtext("a", font=2, side=3,adj=0)
plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
par(mar=c(c(2, 7, 2, 2)))
find_order_barriers_venom(names_lvls,lvls, dfb0, whichRC="RC3", title="Temperature turnover", add_to_lim1=3,add_to_lim2 = 1, add_to_axis=0.5,cexp = "equal")
par(mar=c(c(2, 7, 2, 2)))
find_order_barriers_venom(names_lvls,lvls, dfb0, whichRC="RC2", title="Aridity turnover", add_to_lim1=0,add_to_lim2=0.3, add_to_axis=0.5,cexp = "equal")
dev.off()

# # lr
# pdf("manuscript/1FinalSub_JuneNature/Fig3/Fig3b_altorder.pdf", height = 8,width = 13)
# 
# par(mfrow=c(2,2))
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers_venom(names_lvls,lvls, dfb2500, whichRC="RC4", title="Topographic travel costs", add_to_lim1=0.3, add_to_axis=0.5,cexp = "equal")
# mtext("b", font=2, side=3,adj=0)
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers_venom(names_lvls,lvls, dfb2500, whichRC="RC2", title="Climate-related path lengths", add_to_lim1=1, add_to_lim=3, add_to_axis=0.5,cexp = "equal")
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers_venom(names_lvls,lvls, dfb2500, whichRC="RC1", title="Temperature turnover", add_to_lim1=0.7, add_to_axis=0.5,cexp = "equal")
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers_venom(names_lvls,lvls, dfb2500, whichRC="RC3", title="Aridity turnover", add_to_lim1=3.5, add_to_lim2 = 1, add_to_axis=0.5,cexp = "equal")
# dev.off()
# # ------------------------------------------------------------------------------------------------------------------------------------



# #####################
# #### Tukey matrices #
# #####################
# 
# # cr
# ANOVA=aov(mod1cr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/figures/cr_Tukey_areas1_",fulldata,"_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered(names_lvls,lvls, dfb0,whichRC="RC1",dfres,title="")
# dev.off()
# 
# ANOVA=aov(mod2cr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/figures/cr_Tukey_areas2_",fulldata,"_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered(names_lvls,lvls, dfb0,whichRC="RC2",dfres,title="")
# dev.off()
# 
# ANOVA=aov(mod3cr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/figures/cr_Tukey_areas3_",fulldata,"_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered(names_lvls,lvls, dfb0,whichRC="RC3",dfres,title="")
# dev.off()
# 
# ANOVA=aov(mod4cr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/figures/cr_Tukey_areas4_",fulldata,"_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered(names_lvls,lvls, dfb0,whichRC="RC4",dfres,title="")
# dev.off()
# 
# # lr
# ANOVA=aov(mod1lr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/figures/lr_Tukey_areas1_",fulldata,"_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered(names_lvls,lvls, dfb2500,whichRC="RC1",dfres,title="")
# dev.off()
# 
# ANOVA=aov(mod2lr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/figures/lr_Tukey_areas2_",fulldata,"_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered(names_lvls,lvls, dfb2500,whichRC="RC2",dfres,title="")
# dev.off()
# 
# ANOVA=aov(mod3lr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/figures/lr_Tukey_areas3_",fulldata,"_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered(names_lvls,lvls, dfb2500,whichRC="RC3",dfres,title="")
# dev.off()
# 
# ANOVA=aov(mod4lr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/figures/lr_Tukey_areas4_",fulldata,"_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered(names_lvls,lvls, dfb2500,whichRC="RC4",dfres,title="")
# dev.off()
# 
# # ------------------------------------------------------------------------------------------------------------------------------------


#####################
#### Tukey matrices # by continent
#####################

# cr
ANOVA=aov(mod1cr) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
TUKEY$oragrF->dfres

pdf(paste0("manuscript/Figures_EDGE/cr_Tukey_areas1_","","_matrix.pdf"), height = 5, width = 5)
par(mar=c(2,2,2,2))
mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb0,whichRC="RC1",dfres,title="")
dev.off()

ANOVA=aov(mod2cr) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
TUKEY$oragrF->dfres

pdf(paste0("manuscript/Figures_EDGE/cr_Tukey_areas2_","","_matrix.pdf"), height = 5, width = 5)
par(mar=c(2,2,2,2))
mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb0,whichRC="RC2",dfres,title="")
dev.off()

ANOVA=aov(mod3cr) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
TUKEY$oragrF->dfres

pdf(paste0("manuscript/Figures_EDGE/cr_Tukey_areas3_","","_matrix.pdf"), height = 5, width = 5)
par(mar=c(2,2,2,2))
mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb0,whichRC="RC3",dfres,title="")
dev.off()


# # lr
# ANOVA=aov(mod1lr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("manuscript/1FinalSub_JuneNature/Fig3/lr_Tukey_areas1_",fulldata,"_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb2500,whichRC="RC1",dfres,title="")
# dev.off()
# 
# ANOVA=aov(mod2lr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("manuscript/1FinalSub_JuneNature/Fig3/lr_Tukey_areas2_",fulldata,"_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb2500,whichRC="RC2",dfres,title="")
# dev.off()
# 
# ANOVA=aov(mod3lr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("manuscript/1FinalSub_JuneNature/Fig3/lr_Tukey_areas3_",fulldata,"_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb2500,whichRC="RC3",dfres,title="")
# dev.off()
# 
# ANOVA=aov(mod4lr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("manuscript/1FinalSub_JuneNature/Fig3/lr_Tukey_areas4_",fulldata,"_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb2500,whichRC="RC4",dfres,title="")
# dev.off()
# 
# ------------------------------------------------------------------------------------------------------------------------------------

# pdf(paste0("data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/figures/Tukey/ac/Tukey_areas4_",fulldata,"_legend.pdf"), height = 2, width = 3)
# matFigure_diffTukey_key(dfres)
# dev.off()


