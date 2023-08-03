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
####### read data and models ##### dfi0
##################################


# df0
load( file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/myPCA_perSO_av.rds"))
read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/dfB_PCA_perSO_av.csv"), stringsAsFactors = F)->dfb0
myPCA$loadings
if(whichk%in%"8k") RC_names<-c("GeoD_LCPsl_MCOSTe", "NrSocSimEX","XericD_MCOSTx", "TempD_MCOSTt_NrSocSimET") ; RCs<-c("RC1", "RC2","RC3","RC4") # NrSocSimE has negative loading
# NBtemp loads on RC4 and RC2 but more on RC4
if(whichk%in%"12k") RC_names<-c("GeoD_LCPsl_MCOSTe", "NrSocSimE","XericD_MCOSTx", "TempD_MCOSTt") ; RCs<-c("RC1", "RC2","RC3","RC4") # NrSocSimE has negative loading
if(whichk%in%"4k") RC_names<-c("GeoD_LCPsl_MCOSTe", "NrSocSimEX","XericD_MCOSTx", "TempD_MCOSTt_NrSocSimET") ; RCs<-c("RC1", "RC2","RC3","RC4") # NrSocSimE has negative loading

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

# read models ~ area from cr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/mod1_area.rds")) ; mod1->mod1cr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/mod2_area.rds")) ; mod2->mod2cr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/mod3_area.rds")) ; mod3->mod3cr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/mod4_area.rds")) ; mod4->mod4cr
## ------------------------------------------------------------------------------------------------------------------------------------

rm(mod1) ; rm(mod2) ;rm(mod3) ;rm(mod4)
#identical(levels(dfb2500$oragrF),levels(dfb0$oragrF))
names_lvls<-c("South Tropical China", "Lower-Middle Yangtze" ,"Chinese loess plateau", "West Yunan & East Tibet","Fertile Crescent", "Sava West India","Ganges of East India",
              "West Africa", "West African Savannah","Sudanic savannah", "Ethiopian plateau", 
              "Northern Lowlands of South America","Central/Southern Andes","Southwest Amazon" ,
              "Mesoamerica","East North America")


# #####################
# #### ordered means ##
# #####################
# 
# # cr
# pdf(paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/areasM_ordered_cr.pdf"), height = 8,width = 13)
# par(mfrow=c(2,2))
# par(mar=c(c(2, 7, 2, 2)))
# find_order_barriers(names_lvls,lvls, dfb0, whichRC="RC1", title="Topographic travel costs\n&climate-related path lengths", add_to_lim=0.7, add_to_axis=0.5,cexp = "equal")
# if(whichk%in%"12k") mtext("A", font=2, side=3,adj=0)
# if(whichk%in%"8k") mtext("C", font=2, side=3,adj=0)
# if(whichk%in%"4k") mtext("E", font=2, side=3,adj=0)
# par(mar=c(c(2, 7, 2, 2)))
# if(whichk%in%"8k") find_order_barriers(names_lvls,lvls, dfb0, whichRC="RC2", title="Scarcity of nodes in the\naridity neighborhood", add_to_lim=0.3, add_to_axis=0.5,cexp = "equal")
# if(whichk%in%"12k") find_order_barriers(names_lvls,lvls, dfb0, whichRC="RC2", title="Scarcity of nodes in the\necological neighborhood", add_to_lim=0.3, add_to_axis=0.5,cexp = "equal")
# if(whichk%in%"4k") find_order_barriers(names_lvls,lvls, dfb0, whichRC="RC2", title="Scarcity of nodes in the\naridity neighborhood", add_to_lim=0.3, add_to_axis=0.5,cexp = "equal")
# 
# par(mar=c(c(2, 7, 2, 2)))
# if(whichk%in%"8k") find_order_barriers(names_lvls,lvls, dfb0, whichRC="RC4", title="Temperature turnover\n&scarcity of nodes in neighborhood", add_to_lim=1.25, add_to_axis=0.5,cexp = "equal")
# if(whichk%in%"12k") find_order_barriers(names_lvls,lvls, dfb0, whichRC="RC4", title="Temperature turnover", add_to_lim=1.5, add_to_axis=0.5,cexp = "equal")
# if(whichk%in%"4k") find_order_barriers(names_lvls,lvls, dfb0, whichRC="RC4", title="Temperature turnover\n&scarcity of nodes in neighborhood", add_to_lim=1.25, add_to_axis=0.5,cexp = "equal")
# 
# par(mar=c(c(2, 7, 2, 2)))
# #if(whichk%in%c("4k")) find_order_barriers(names_lvls,lvls, dfb0, whichRC="RC3", title="Aridity turnover", add_to_lim=2, add_to_axis=0.5,cexp = "equal")
# 
# if(whichk%in%c("12k", "8k","4k")) #find_order_barriers(names_lvls,lvls, dfb0, whichRC="RC3", title="Aridity turnover", add_to_lim=2, add_to_axis=0.5,cexp = "equal")
#   { whichRC="RC3"
#     title="Aridity turnover"
#     add_to_lim=2
#     add_to_axis=0.5
#     cexp = "equal"
#     means1<-numeric() ; sds1<-numeric() ; sr<-numeric()# mean & sd
#     for(j in 1:length(lvls))
#       {means1[j]<-mean(dfb0[dfb0$oragr%in%lvls[j],colnames(dfb0)%in%whichRC])
#       sds1[j]<-sd(dfb0[dfb0$oragr%in%lvls[j],colnames(dfb0)%in%whichRC]) 
#       sr[j]<-length(dfb0[dfb0$oragr%in%lvls[j],colnames(dfb0)%in%whichRC])}
#     
#     names(means1)<-lvls ; names(sds1)<-lvls ; names(sr)<-lvls
#     ordm<-names(means1)[order(means1)]
#     noarea<-seq(from=1, to=length(lvls),by=1) ; names(noarea)<-lvls
#     noarea<-noarea[order(factor(names(noarea), levels =ordm))]
#     
#     x<-seq(from=1, to=length(lvls),by=1)
#     y<- means1[order(means1)]
#     
#     cols_vplots<-c("#00A08A","#EBCC2A","deeppink","darkmagenta","blue3")
#     cols_vplots<-c("#5B1A18","darkgreen","goldenrod1","violetred","dodgerblue1")
#     cols<- c(rep(cols_vplots[1],7),rep(cols_vplots[2],4), rep(cols_vplots[3],3) , cols_vplots[4],cols_vplots[5])
#     names(cols)<-lvls
#     cols<-cols[order(factor(names(cols), levels =ordm))]
#     
#     sr<-sr[order(factor(names(sr), levels =ordm))]
#     scaledsr<-log(sr)+0.5
# 
#     rylim<-range(dfb0[,colnames(dfb0)%in%whichRC])
#     if(cexp=="equal") plot( y~x, col=cols, xlab="", ylab="", xaxt='n',yaxt='n', pch=18, cex=4.5,ylim=c(rylim[1]-6, rylim[2]+add_to_lim))
#     if(cexp=="sr") plot( y~x, col=cols, xlab="", ylab="", xaxt='n',yaxt='n', pch=18, cex=scaledsr, ylim=c(rylim[1]-add_to_lim, rylim[2]+add_to_lim))
#     
#     mtext(side=2, text = title, line=+2.5, font=2,adj=0.5,cex=1.2) # x axis label
#     axis(side=2, at =  c(rylim[1]+add_to_axis,0, rylim[2]-add_to_axis), labels=c( round(rylim[1]+add_to_axis, digits=1),0, round(rylim[2]-add_to_axis, digits=1)), tick = F, cex.axis=1)
#     #abline(h=0, lty=2, col="gray")
#     
#     # add SD
#     sds1<-sds1[order(factor(names(sds1), levels =ordm))]
#     means1<-means1[order(factor(names(means1), levels =ordm))]
#     
#     for(j in 1:length(sds1))
#     { arrows(x0= x[j], y0=means1[j], x1=x[j], y1= (means1[j]+ sds1[j]), angle = 180,col=cols[j])
#       arrows(x0= x[j], y0=means1[j], x1=x[j], y1= (means1[j]- sds1[j]), angle = 180,col=cols[j])}
#     
#     text(x=x, y=means1, labels=noarea, font=2,col="white")  
#   }
# dev.off()


#####################
#### ordered means ## by continent
#####################

# cr
pdf(paste0("manuscript/1FinalSub_JuneNature/Fig3/ExtD_fig10_", whichk,"_areasM_ordered_cr.pdf"), height = 8,width = 13)
par(mfrow=c(2,2))
par(mar=c(c(2, 7, 2, 2)))
find_order_barriers_venom(names_lvls,lvls, dfb0, whichRC="RC1", title="Topographic travel costs\n&climate-related path lengths", add_to_lim1=0.7, add_to_axis=0.5,cexp = "equal")
# if(whichk%in%"12k") mtext("a", font=2, side=3,adj=0)
# if(whichk%in%"8k") mtext("c", font=2, side=3,adj=0)
# if(whichk%in%"4k") mtext("e", font=2, side=3,adj=0)
par(mar=c(c(2, 7, 2, 2)))
if(whichk%in%"8k") find_order_barriers_venom(names_lvls,lvls, dfb0, whichRC="RC2", title="Scarcity of nodes in the\naridity neighborhood", add_to_lim1=0, add_to_lim2=0.3,add_to_axis=0.5,cexp = "equal")
if(whichk%in%"12k") find_order_barriers_venom(names_lvls,lvls, dfb0, whichRC="RC2", title="Scarcity of nodes in the\necological neighborhood", add_to_lim1=0, add_to_lim2=0.3, add_to_axis=0.5,cexp = "equal")
if(whichk%in%"4k") find_order_barriers_venom(names_lvls,lvls, dfb0, whichRC="RC2", title="Scarcity of nodes in the\naridity neighborhood", add_to_lim1=0, add_to_lim2=0.3, add_to_axis=0.5,cexp = "equal")

par(mar=c(c(2, 7, 2, 2)))
if(whichk%in%"8k") find_order_barriers_venom(names_lvls,lvls, dfb0, whichRC="RC4", title="Temperature turnover\n&scarcity of nodes in neighborhood", add_to_lim1=1.25, add_to_lim2=1.25,add_to_axis=0.5,cexp = "equal")
if(whichk%in%"12k") find_order_barriers_venom(names_lvls,lvls, dfb0, whichRC="RC4", title="Temperature turnover", add_to_lim1=0.3,add_to_lim2=0, add_to_axis=0.5,cexp = "equal")
if(whichk%in%"4k") find_order_barriers_venom(names_lvls,lvls, dfb0, whichRC="RC4", title="Temperature turnover\n&scarcity of nodes in neighborhood", add_to_lim1=1.25, add_to_lim2=1.25,add_to_axis=0.5,cexp = "equal")

par(mar=c(c(2, 7, 2, 2)))
find_order_barriers_venom(names_lvls,lvls, dfb0, whichRC="RC3", title="Aridity turnover", add_to_lim1=3.5, add_to_lim2=0,add_to_axis=0.5,cexp = "equal")

dev.off()


# #####################
# #### Tukey matrices # 
# #####################
# 
# # cr
# ANOVA=aov(mod1cr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/cr_Tukey_areas1","_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered(names_lvls,lvls, dfb0,whichRC="RC1",dfres,title="")
# dev.off()
# 
# ANOVA=aov(mod2cr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/cr_Tukey_areas2","_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered(names_lvls,lvls, dfb0,whichRC="RC2",dfres,title="")
# dev.off()
# 
# ANOVA=aov(mod3cr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/cr_Tukey_areas3","_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered(names_lvls,lvls, dfb0,whichRC="RC3",dfres,title="")
# dev.off()
# 
# ANOVA=aov(mod4cr) ; summary(ANOVA)
# TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
# TUKEY$oragrF->dfres
# 
# pdf(paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/cr_Tukey_areas4","_matrix.pdf"), height = 5, width = 5)
# par(mar=c(2,2,2,2))
# mats<-matFigure_diffTukey_allcomp_halfMordered(names_lvls,lvls, dfb0,whichRC="RC4",dfres,title="")
# dev.off()
# 
# 
# 

#####################
#### Tukey matrices # by continent
#####################
fulldata<-"no_av" # yes no no_av

# cr
ANOVA=aov(mod1cr) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
TUKEY$oragrF->dfres

pdf(paste0("manuscript/1FinalSub_JuneNature/Fig3/ExtD_fig10_", whichk,"_cr_Tukey_areas1_",fulldata,"_matrix.pdf"), height = 5, width = 5)
par(mar=c(2,2,2,2))
mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb0,whichRC="RC1",dfres,title="")
dev.off()

ANOVA=aov(mod2cr) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
TUKEY$oragrF->dfres

pdf(paste0("manuscript/1FinalSub_JuneNature/Fig3/ExtD_fig10_", whichk,"_cr_Tukey_areas2_",fulldata,"_matrix.pdf"), height = 5, width = 5)
par(mar=c(2,2,2,2))
mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb0,whichRC="RC2",dfres,title="")
dev.off()

ANOVA=aov(mod3cr) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
TUKEY$oragrF->dfres

pdf(paste0("manuscript/1FinalSub_JuneNature/Fig3/ExtD_fig10_", whichk,"_cr_Tukey_areas3_",fulldata,"_matrix.pdf"), height = 5, width = 5)
par(mar=c(2,2,2,2))
mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb0,whichRC="RC3",dfres,title="")
dev.off()

ANOVA=aov(mod4cr) ; summary(ANOVA)
TUKEY<- TukeyHSD(x=ANOVA, 'oragrF', conf.level=0.95) #plot(TUKEY , las=1 , col="brown")
TUKEY$oragrF->dfres

pdf(paste0("manuscript/1FinalSub_JuneNature/Fig3/ExtD_fig10_", whichk,"_cr_Tukey_areas4_",fulldata,"_matrix.pdf"), height = 5, width = 5)
par(mar=c(2,2,2,2))
mats<-matFigure_diffTukey_allcomp_halfMordered_venom(names_lvls,lvls, dfb0,whichRC="RC4",dfres,title="")
dev.off()


