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


####################
#### loadings ######
####################

# close range
load( file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/myPCA_perSO_av.rds"))
read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/dfB_PCA_perSO_av.csv"), stringsAsFactors = F)->dfb0
myPCA$loadings
if(whichk%in%"8k") RC_names<-c("GeoD_LCPsl_MCOSTe", "NrSocSimEX","XericD_MCOSTx", "TempD_MCOSTt_NrSocSimET") ; RCs<-c("RC1", "RC2","RC3","RC4") # NrSocSimE has negative loading
  # NBtemp loads on RC4 and RC2 but more on RC4
if(whichk%in%"12k") RC_names<-c("GeoD_LCPsl_MCOSTe", "NrSocSimE","XericD_MCOSTx", "TempD_MCOSTt") ; RCs<-c("RC1", "RC2","RC3","RC4") # NrSocSimE has negative loading
if(whichk%in%"4k") RC_names<-c("GeoD_LCPsl_MCOSTe", "NrSocSimEX","XericD_MCOSTx", "TempD_MCOSTt_NrSocSimET") ; RCs<-c("RC1", "RC2","RC3","RC4") # NrSocSimE has negative loading

rownames(myPCA$loadings)
if(whichk%in%"12k") df_l<-cbind(round(myPCA$loadings[,1],2),round(myPCA$loadings[,4],2),
                          round(myPCA$loadings[,2],2),round(myPCA$loadings[,3],2)) 
if(whichk%in%"8k") df_l<-cbind(round(myPCA$loadings[,1],2),round(myPCA$loadings[,4],2),
                                round(myPCA$loadings[,3],2),round(myPCA$loadings[,4],2)) # 
if(whichk%in%"4k") df_l<-cbind(round(myPCA$loadings[,1],2),round(myPCA$loadings[,4],2),
                               round(myPCA$loadings[,3],2),round(myPCA$loadings[,4],2)) # 

colnames(df_l)<-c("crRC1 loadings","crRC2 loadings","crRC3 loadings","crRC4 loadings")
rownames(df_l)<-c("Number of nodes in the temperature neighborhood",
                  "Number of nodes in the aridity neighborhood",
                  "Sqrt Geodesic distance", "Log topographic travel accumulated cost","Log length topographic travel least-cost path",
                  "Log temperature-related accumulated cost","Log length temperature-related least-cost path",
                  "Log aridity-related accumulated cost","Log length aridity-related least-cost path",
                  "Sqrt Temperature dissimilarity", "Sqrt Aridity dissimilarity")
#df_l$Barrier<-rownames(df_l)

# # if you named the number of people as scarcity - multiply loadings by -1
# df_l[1,]<-(-1)*df_l[1,]
# df_l[2,]<-(-1)*df_l[2,]

if(whichk%in%"12k") df_l<-rbind(df_l, c(0.46, 0.15, 0.18, 0.17))
if(whichk%in%"8k") df_l<-rbind(df_l, c(0.48, 0.14, 0.18, 0.21))
if(whichk%in%"4k") df_l<-rbind(df_l, c(0.48, 0.96, 0.83, 0.66))

rownames(df_l)[12]<-"Proportion Variation"
#caption<-"Table S3. Rotated PCA variable loadings, capturing variation across candidate geo-environmental drivers of cultural spread. Each data point reflects a society located in one of the centers of independent domestication, for which we compute and average barriers to cultural diffusion across (A) close-range (i.e. nearest 100), and (B) long-range (i.e. nearest 100 societies located at least 2,500 km away) networks of neighbors"
#df_l<-rbind(c(caption,"","",""),df_l )
if(whichk%in%"12k") write.csv(df_l,file = "manuscript/Tables/TableS5A_pclim.csv") 
if(whichk%in%"8k") write.csv(df_l,file = "manuscript/Tables/TableS5C_pclim.csv") 
if(whichk%in%"4k") write.csv(df_l,file = "manuscript/Tables/TableS5E_pclim.csv") 



# long range
load( file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi2500_PCA_nf4/myPCA_perSO_av.rds"))
read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi2500_PCA_nf4/dfB_PCA_perSO_av.csv"), stringsAsFactors = F)->dfb2500
myPCA$loadings
if(whichk%in%"8k") RC_names<-c("TempD_MCOSTt","LPCtLCPx","XericD_MCOSTx","GeoD_LCPsl_MCOSTe") ; RCs<-c("RC1", "RC2","RC3","RC4")
# logLCPe      0.669  0.617              between RC1 and RC4 - leave it on RC4
if(whichk%in%"12k") RC_names<-c("TempD_MCOSTt","LPCtLCPx","XericD_MCOSTx","GeoD_LCPsl_MCOSTe") ; RCs<-c("RC1", "RC2","RC3","RC4") 
# logLCPe      0.708  0.576      between RC1 and RC4 - leave it on RC4
if(whichk%in%"4k")RC_names<-c("TempD_MCOSTt","LPCtLCPx","XericD_MCOSTx","GeoD_LCPsl_MCOSTe") ; RCs<-c("RC1", "RC2","RC3","RC4") 
rownames(myPCA$loadings)

if(whichk%in%"12k") df_l<-cbind(round(myPCA$loadings[,1],2),round(myPCA$loadings[,4],2),
            round(myPCA$loadings[,3],2),round(myPCA$loadings[,1],2)) # order is 4-1-3-2 so re-order
if(whichk%in%"8k") df_l<-cbind(round(myPCA$loadings[,1],2),round(myPCA$loadings[,4],2),
                                round(myPCA$loadings[,3],2),round(myPCA$loadings[,2],2)) # order is 4-1-3-2 so re-order
if(whichk%in%"4k") df_l<-cbind(round(myPCA$loadings[,1],2),round(myPCA$loadings[,4],2),
                               round(myPCA$loadings[,3],2),round(myPCA$loadings[,2],2)) # order is 4-1-3-2 so re-order

colnames(df_l)<-c("lrRC1 loadings","lrRC2 loadings","lrRC3 loadings","lrRC4 loadings")
rownames(df_l)<-c("Sqrt Geodesic distance", "Log topographic travel accumulated cost","Log length topographic travel least-cost path",
                  "Log temperature-related accumulated cost","Log length temperature-related least-cost path",
                  "Log aridity-related accumulated cost","Log length aridity-related least-cost path",
                  "Sqrt Temperature dissimilarity", "Sqrt Aridity dissimilarity")
#df_l$Barrier<-rownames(df_l)
if(whichk%in%"12k") df_l<-rbind(df_l, c(0.31,0.20,0.80,0.58))
if(whichk%in%"8k") df_l<-rbind(df_l, c(0.30,0.20,0.22,0.28))
if(whichk%in%"4k") df_l<-rbind(df_l, c(0.26,0.89,0.71,0.52))

rownames(df_l)[10]<-"Proportion Variation"
if(whichk%in%"12k")  write.csv(df_l,file = "manuscript/Tables/TableS5B_pclim.csv")
if(whichk%in%"8k")  write.csv(df_l,file = "manuscript/Tables/TableS5D_pclim.csv")
if(whichk%in%"4k")  write.csv(df_l,file = "manuscript/Tables/TableS5F_pclim.csv")



# #################################
# #### model parameters cont ######
# #################################
# 
# rm(list=ls())
# 
# require(movecost)
# require(sp)
# require(raster)
# require(gdistance)
# library(letsR)
# library(spdep)
# source("scripts/10_rLCP_P2_PCA_modelsFXN.R")
# 
# fulldata<-"no_av" # yes no no_av
# 
# # read models ~ cont from cr
# load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/models_space/mod11.rds") ; mod11->mod11cr
# load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/models_space/mod12.rds") ; mod12->mod12cr
# load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/models_space/mod1_all.rds") ; mod1->mod1cr
# load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/models_space/mod2.rds") ; mod2->mod2cr
# load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/models_space/mod3.rds") ; mod3->mod3cr
# load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/models_space/mod4.rds") ; mod4->mod4cr
# 
# #if(fulldata%in%c("no_av")) load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/myPCA_perSO_av.rds") 
# if(fulldata%in%"no_av") read.csv(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/dfB_PCA_perSO_av.csv", stringsAsFactors = F)-> dfb0
# if(fulldata%in%c("no_av"))
# {RC_namescr<-c("GeoD_LCPsl_MCOSTe", "NrSocSimE", "TempD_MCOSTt","XericD_MCOSTx") ; RCscr<-c("RC1", "RC2","RC3","RC4")} # NrSocSimE has negative loading
# 
# 
# # read models ~ cont lr
# load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/models_space/mod1.rds") ; mod1->mod1lr
# load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/models_space/mod2.rds") ; mod2->mod2lr
# load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/models_space/mod3.rds") ; mod3->mod3lr
# load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/models_space/mod4.rds") ; mod4->mod4lr
# 
# #if(fulldata%in%c("no_av")) load(file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/myPCA_perSO_av.rds")
# if(fulldata%in%"no_av") read.csv(file="data/Re_analysisGEbarriers/P2_datamodel/dfi2500_PCA_nf5/dfB_PCA_perSO_av.csv", stringsAsFactors = F)-> dfb2500
# if(fulldata%in%c("no_av"))
# {RC_nameslr<-c("TempD_MCOSTt","LPCtLCPx","XericD_MCOSTx","GeoD_LCPsl_MCOSTe") ; RCslr<-c("RC1", "RC2","RC3","RC4") }
# 
# rm(mod1) ; rm(mod2) ;rm(mod3) ;rm(mod4) ;rm(mod11) ;rm(mod12) 
# 
# 
# # DF of them
# rm(dfcr)
# 
# # mod11cr
# df_out<-data.frame("model"=rep('crPC1',5),"Levels"=rep(' ',5),"Estimate"=rep("",5), "SE"=rep("",5), "p-value"=rep("",5))
# df_out$Levels<-c("Intercept","Africa","South America","Mesoamerica","North America") #"FC + CL + LMY", 
# summary(mod1cr)->summc
# summc$coefficients->summc
# df_out$Estimate<-round(summc[1:5,1],digits=3)
# df_out$SE<-round(summc[1:5,2],digits=3)
# df_out$p.value<-round(summc[1:5,4],digits=3)
# summary(mod1cr)->summc
# 
# mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
#            ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod1cr), digits=2) )
# to_add<-data.frame(mp,"","","","")
# colnames(to_add)<-colnames(df_out)
# df_out<-rbind(df_out,to_add)
# 
# df_out-> dfcr
# 
# # # mod12cr
# # df_out<-data.frame("model"=rep('crPC1_2',6),"Levels"=rep(' ',6),"Estimate"=rep("",6), "SE"=rep("",6), "p-value"=rep("",6))
# # df_out$Levels<-c("Intercept","Asia - (FC + CL + LMY)", "Africa","South America","Mesoamerica","North America")
# # summary(mod12cr)->summc
# # summc$coefficients->summc
# # df_out$Estimate<-round(summc[1:6,1],digits=3)
# # df_out$SE<-round(summc[1:6,2],digits=3)
# # df_out$p.value<-round(summc[1:6,4],digits=3)
# # summary(mod12cr)->summc
# # 
# # mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
# #            ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod12cr), digits=2) )
# # to_add<-data.frame(mp,"","","","")
# # colnames(to_add)<-colnames(df_out)
# # df_out<-rbind(df_out,to_add)
# # 
# # dfcr<-rbind(dfcr,df_out)
# 
# 
# # mod2cr
# df_out<-data.frame("model"=rep('crPC2',5),"Levels"=rep(' ',5),"Estimate"=rep("",5), "SE"=rep("",5), "p-value"=rep("",5))
# df_out$Levels<-c("Intercept", "Africa","South America","Mesoamerica","North America")
# summary(mod2cr)->summc
# summc$coefficients->summc
# df_out$Estimate<-round(summc[1:5,1],digits=3)
# df_out$SE<-round(summc[1:5,2],digits=3)
# df_out$p.value<-round(summc[1:5,4],digits=3)
# summary(mod2cr)->summc
# 
# mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
#            ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod2cr), digits=2) )
# to_add<-data.frame(mp,"","","","")
# colnames(to_add)<-colnames(df_out)
# df_out<-rbind(df_out,to_add)
# 
# dfcr<-rbind(dfcr,df_out)
# 
# 
# # mod3cr
# df_out<-data.frame("model"=rep('crPC3',5),"Levels"=rep(' ',5),"Estimate"=rep("",5), "SE"=rep("",5), "p-value"=rep("",5))
# df_out$Levels<-c("Intercept", "Africa","South America","Mesoamerica","North America")
# summary(mod3cr)->summc
# summc$coefficients->summc
# df_out$Estimate<-round(summc[1:5,1],digits=3)
# df_out$SE<-round(summc[1:5,2],digits=3)
# df_out$p.value<-round(summc[1:5,4],digits=3)
# summary(mod3cr)->summc
# 
# mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
#            ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod3cr), digits=2) )
# to_add<-data.frame(mp,"","","","")
# colnames(to_add)<-colnames(df_out)
# df_out<-rbind(df_out,to_add)
# 
# dfcr<-rbind(dfcr,df_out)
# 
# 
# # mod4cr
# df_out<-data.frame("model"=rep('crPC4',5),"Levels"=rep(' ',5),"Estimate"=rep("",5), "SE"=rep("",5), "p-value"=rep("",5))
# df_out$Levels<-c("Intercept", "Africa","South America","Mesoamerica","North America")
# summary(mod4cr)->summc
# summc$coefficients->summc
# df_out$Estimate<-round(summc[1:5,1],digits=3)
# df_out$SE<-round(summc[1:5,2],digits=3)
# df_out$p.value<-round(summc[1:5,4],digits=3)
# summary(mod4cr)->summc
# 
# mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
#            ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod4cr), digits=2) )
# to_add<-data.frame(mp,"","","","")
# colnames(to_add)<-colnames(df_out)
# df_out<-rbind(df_out,to_add)
# 
# dfcr<-rbind(dfcr,df_out)
# 
# 
# 
# rm(dflr) # do in order of preds: 4 (geo) - 2 (paths) - 1 (temp) - 3 (xeric)
# 
# # mod4lr 
# df_out<-data.frame("model"=rep('lrPC4',5),"Levels"=rep(' ',5),"Estimate"=rep("",5), "SE"=rep("",5), "p-value"=rep("",5))
# df_out$Levels<-c("Intercept", "Africa","South America","Mesoamerica","North America")
# summary(mod4lr)->summc
# summc$coefficients->summc
# df_out$Estimate<-round(summc[1:5,1],digits=3)
# df_out$SE<-round(summc[1:5,2],digits=3)
# df_out$p.value<-round(summc[1:5,4],digits=3)
# summary(mod4lr)->summc
# 
# mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
#            ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod4lr), digits=2) )
# to_add<-data.frame(mp,"","","","")
# colnames(to_add)<-colnames(df_out)
# df_out<-rbind(df_out,to_add)
# 
# dflr<-df_out
# 
# 
# # mod2lr 
# df_out<-data.frame("model"=rep('lrPC2',5),"Levels"=rep(' ',5),"Estimate"=rep("",5), "SE"=rep("",5), "p-value"=rep("",5))
# df_out$Levels<-c("Intercept", "Africa","South America","Mesoamerica","North America")
# summary(mod2lr)->summc
# summc$coefficients->summc
# df_out$Estimate<-round(summc[1:5,1],digits=3)
# df_out$SE<-round(summc[1:5,2],digits=3)
# df_out$p.value<-round(summc[1:5,4],digits=3)
# summary(mod2lr)->summc
# 
# mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
#            ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod2lr), digits=2) )
# to_add<-data.frame(mp,"","","","")
# colnames(to_add)<-colnames(df_out)
# df_out<-rbind(df_out,to_add)
# 
# dflr<-rbind(dflr,df_out)
# 
# 
# # mod1lr 
# df_out<-data.frame("model"=rep('lrPC1',5),"Levels"=rep(' ',5),"Estimate"=rep("",5), "SE"=rep("",5), "p-value"=rep("",5))
# df_out$Levels<-c("Intercept", "Africa","South America","Mesoamerica","North America")
# summary(mod1lr)->summc
# summc$coefficients->summc
# df_out$Estimate<-round(summc[1:5,1],digits=3)
# df_out$SE<-round(summc[1:5,2],digits=3)
# df_out$p.value<-round(summc[1:5,4],digits=3)
# summary(mod1lr)->summc
# 
# mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
#            ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod1lr), digits=2) )
# to_add<-data.frame(mp,"","","","")
# colnames(to_add)<-colnames(df_out)
# df_out<-rbind(df_out,to_add)
# 
# dflr<-rbind(dflr,df_out)
# 
# 
# 
# # mod3lr 
# df_out<-data.frame("model"=rep('lrPC3',5),"Levels"=rep(' ',5),"Estimate"=rep("",5), "SE"=rep("",5), "p-value"=rep("",5))
# df_out$Levels<-c("Intercept", "Africa","South America","Mesoamerica","North America")
# summary(mod3lr)->summc
# summc$coefficients->summc
# df_out$Estimate<-round(summc[1:5,1],digits=3)
# df_out$SE<-round(summc[1:5,2],digits=3)
# df_out$p.value<-round(summc[1:5,4],digits=3)
# summary(mod3lr)->summc
# 
# mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
#            ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod3lr), digits=2) )
# to_add<-data.frame(mp,"","","","")
# colnames(to_add)<-colnames(df_out)
# df_out<-rbind(df_out,to_add)
# 
# dflr<-rbind(dflr,df_out)
# 
# dft<-rbind(dfcr,dflr)
# write.csv(dft,file = "manuscript/Tables/TableS5.csv")



#################################
#### model parameters area ######
#################################

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


# read models ~ area from cr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/mod1_area.rds")) ; mod1->mod1cr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/mod2_area.rds")) ; mod2->mod2cr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/mod3_area.rds")) ; mod3->mod3cr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/mod4_area.rds")) ; mod4->mod4cr

if(whichk%in%"8k") RC_names<-c("GeoD_LCPsl_MCOSTe", "NrSocSimEX","XericD_MCOSTx", "TempD_MCOSTt_NrSocSimET") ; RCs<-c("RC1", "RC2","RC3","RC4") # NrSocSimE has negative loading
# NBtemp loads on RC4 and RC2 but more on RC4
if(whichk%in%"12k") RC_names<-c("GeoD_LCPsl_MCOSTe", "NrSocSimE","XericD_MCOSTx", "TempD_MCOSTt") ; RCs<-c("RC1", "RC2","RC3","RC4") # NrSocSimE has negative loading
if(whichk%in%"4k") RC_names<-c("GeoD_LCPsl_MCOSTe", "NrSocSimEX","XericD_MCOSTx", "TempD_MCOSTt_NrSocSimET") ; RCs<-c("RC1", "RC2","RC3","RC4") # NrSocSimE has negative loading

# read models ~ area from lr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi2500_PCA_nf4/mod1_area.rds")) ; mod1->mod1lr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi2500_PCA_nf4/mod2_area.rds")) ; mod2->mod2lr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi2500_PCA_nf4/mod3_area.rds")) ; mod3->mod3lr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi2500_PCA_nf4/mod4_area.rds")) ; mod4->mod4lr

if(whichk%in%"8k") RC_names<-c("TempD_MCOSTt","LPCtLCPx","XericD_MCOSTx","GeoD_LCPsl_MCOSTe") ; RCs<-c("RC1", "RC2","RC3","RC4")
# logLCPe      0.669  0.617              between RC1 and RC4 - leave it on RC4
if(whichk%in%"12k") RC_names<-c("TempD_MCOSTt","LPCtLCPx","XericD_MCOSTx","GeoD_LCPsl_MCOSTe") ; RCs<-c("RC1", "RC2","RC3","RC4") 
# logLCPe      0.708  0.576      between RC1 and RC4 - leave it on RC4
if(whichk%in%"4k")RC_names<-c("TempD_MCOSTt","LPCtLCPx","XericD_MCOSTx","GeoD_LCPsl_MCOSTe") ; RCs<-c("RC1", "RC2","RC3","RC4") 

rm(mod1) ; rm(mod2) ;rm(mod3) ;rm(mod4)
# ------------------------------------------------------------------------------------------------------------------------------------

names_lvls<-c("South Tropical China", "Lower-Middle Yangtze" ,"Chinese loess plateau", "West Yunan & East Tibet","Fertile Crescent", "Sava West India","Ganges of East India",
              "West Africa", "West African Savannah","Sudanic savannah", "Ethiopian plateau", 
              "Northern Lowlands of South America","Central/Southern Andes","Southwest Amazon" ,
              "Mesoamerica","East North America")


# mod1cr
df_out<-data.frame("model"=rep('crPC1',16),"Levels"=rep(' ',16),"Estimate"=rep("",16), "SE"=rep("",16), "p-value"=rep("",16))
df_out$Levels<-c("Intercept",names_lvls[2:16])
summary(mod1cr)->summc
summc$coefficients->summc
df_out$Estimate<-round(summc[1:16,1],digits=3)
df_out$SE<-round(summc[1:16,2],digits=3)
df_out$p.value<-round(summc[1:16,4],digits=3)
summary(mod1cr)->summc

mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
           ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod1cr), digits=2) )
to_add<-data.frame(mp,"","","","")
colnames(to_add)<-colnames(df_out)
df_out<-rbind(df_out,to_add)

df_out-> dfcr


# mod2cr
df_out<-data.frame("model"=rep('crPC2',16),"Levels"=rep(' ',16),"Estimate"=rep("",16), "SE"=rep("",16), "p-value"=rep("",16))
df_out$Levels<-c("Intercept",names_lvls[2:16])
summary(mod2cr)->summc
summc$coefficients->summc
df_out$Estimate<-round(summc[1:16,1],digits=3)
df_out$SE<-round(summc[1:16,2],digits=3)
df_out$p.value<-round(summc[1:16,4],digits=3)
summary(mod2cr)->summc

mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
           ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod2cr), digits=2) )
to_add<-data.frame(mp,"","","","")
colnames(to_add)<-colnames(df_out)
df_out<-rbind(df_out,to_add)

dfcr<-rbind(dfcr,df_out)


# mod3cr
df_out<-data.frame("model"=rep('crPC3',16),"Levels"=rep(' ',16),"Estimate"=rep("",16), "SE"=rep("",16), "p-value"=rep("",16))
df_out$Levels<-c("Intercept",names_lvls[2:16])
summary(mod3cr)->summc
summc$coefficients->summc
df_out$Estimate<-round(summc[1:16,1],digits=3)
df_out$SE<-round(summc[1:16,2],digits=3)
df_out$p.value<-round(summc[1:16,4],digits=3)
summary(mod3cr)->summc

mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
           ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod3cr), digits=2) )
to_add<-data.frame(mp,"","","","")
colnames(to_add)<-colnames(df_out)
df_out<-rbind(df_out,to_add)

dfcr<-rbind(dfcr,df_out)

# mod4cr
df_out<-data.frame("model"=rep('crPC4',16),"Levels"=rep(' ',16),"Estimate"=rep("",16), "SE"=rep("",16), "p-value"=rep("",16))
df_out$Levels<-c("Intercept",names_lvls[2:16])
summary(mod4cr)->summc
summc$coefficients->summc
df_out$Estimate<-round(summc[1:16,1],digits=3)
df_out$SE<-round(summc[1:16,2],digits=3)
df_out$p.value<-round(summc[1:16,4],digits=3)
summary(mod4cr)->summc

mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
           ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod4cr), digits=2) )
to_add<-data.frame(mp,"","","","")
colnames(to_add)<-colnames(df_out)
df_out<-rbind(df_out,to_add)

dfcr<-rbind(dfcr,df_out)



rm(dflr) # do in order of preds: PC 1,2,3,4 (easier)

# mod1lr 
df_out<-data.frame("model"=rep('lrPC1',16),"Levels"=rep(' ',16),"Estimate"=rep("",16), "SE"=rep("",16), "p-value"=rep("",16))
df_out$Levels<-c("Intercept",names_lvls[2:16])
summary(mod1lr)->summc
summc$coefficients->summc
df_out$Estimate<-round(summc[1:16,1],digits=3)
df_out$SE<-round(summc[1:16,2],digits=3)
df_out$p.value<-round(summc[1:16,4],digits=3)
summary(mod1lr)->summc

mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
           ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod1lr), digits=2) )
to_add<-data.frame(mp,"","","","")
colnames(to_add)<-colnames(df_out)
df_out<-rbind(df_out,to_add)

dflr<-df_out

# mod2lr 
df_out<-data.frame("model"=rep('lrPC2',16),"Levels"=rep(' ',16),"Estimate"=rep("",16), "SE"=rep("",16), "p-value"=rep("",16))
df_out$Levels<-c("Intercept",names_lvls[2:16])
summary(mod2lr)->summc
summc$coefficients->summc
df_out$Estimate<-round(summc[1:16,1],digits=3)
df_out$SE<-round(summc[1:16,2],digits=3)
df_out$p.value<-round(summc[1:16,4],digits=3)
summary(mod2lr)->summc

mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
           ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod2lr), digits=2) )
to_add<-data.frame(mp,"","","","")
colnames(to_add)<-colnames(df_out)
df_out<-rbind(df_out,to_add)

dflr<-rbind(dflr,df_out)



# mod3lr 
df_out<-data.frame("model"=rep('lrPC3',16),"Levels"=rep(' ',16),"Estimate"=rep("",16), "SE"=rep("",16), "p-value"=rep("",16))
df_out$Levels<-c("Intercept",names_lvls[2:16])
summary(mod3lr)->summc
summc$coefficients->summc
df_out$Estimate<-round(summc[1:16,1],digits=3)
df_out$SE<-round(summc[1:16,2],digits=3)
df_out$p.value<-round(summc[1:16,4],digits=3)
summary(mod3lr)->summc

mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
           ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod3lr), digits=2) )
to_add<-data.frame(mp,"","","","")
colnames(to_add)<-colnames(df_out)
df_out<-rbind(df_out,to_add)

dflr<-rbind(dflr,df_out)

# mod4lr 
df_out<-data.frame("model"=rep('lrPC4',16),"Levels"=rep(' ',16),"Estimate"=rep("",16), "SE"=rep("",16), "p-value"=rep("",16))
df_out$Levels<-c("Intercept",names_lvls[2:16])
summary(mod4lr)->summc
summc$coefficients->summc
df_out$Estimate<-round(summc[1:16,1],digits=3)
df_out$SE<-round(summc[1:16,2],digits=3)
df_out$p.value<-round(summc[1:16,4],digits=3)
summary(mod4lr)->summc

mp<-paste0("Model parameters: adjusted R-sq = ",round(summc$adj.r.squared, digits=2), ", F-statistic = ", round(summc$fstatistic[1], digits=0),
           ", df = ", summc$df[1]-1,",",summc$df[2], ", AIC = ",round(AIC(mod4lr), digits=2) )
to_add<-data.frame(mp,"","","","")
colnames(to_add)<-colnames(df_out)
df_out<-rbind(df_out,to_add)

dflr<-rbind(dflr,df_out)


dft<-rbind(dfcr,dflr)
if(whichk%in%"12k") write.csv(dft,file = "manuscript/Tables/TableS6_pclim_12k.csv")
if(whichk%in%"8k") write.csv(dft,file = "manuscript/Tables/TableS6_pclim_8k.csv")
if(whichk%in%"4k") write.csv(dft,file = "manuscript/Tables/TableS6_pclim_4k.csv")

