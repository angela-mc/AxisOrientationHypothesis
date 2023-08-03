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
load( file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/myPCA_perSO_av_NOSIMCLIMNB.rds"))
read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/dfB_PCA_perSO_av_NOSIMCLIMNB.csv"), stringsAsFactors = F)->dfb0
myPCA$loadings
RC_names<-c("GeoD_LCPsl_MCOSTe", "XericD_MCOSTx", "TempD_MCOSTt_NrSocSimET")
RCs<-c("RC1", "RC2","RC3")

rownames(myPCA$loadings)
df_l<-cbind(round(myPCA$loadings[,1],2),round(myPCA$loadings[,2],2),round(myPCA$loadings[,3],2)) 
colnames(df_l)<-c("crRC1 loadings","crRC2 loadings","crRC3 loadings")
rownames(df_l)<-c( "Sqrt Geodesic distance", "Log topographic travel accumulated cost","Log length topographic travel least-cost path",
                  "Log temperature-related accumulated cost","Log length temperature-related least-cost path",
                  "Log aridity-related accumulated cost","Log length aridity-related least-cost path",
                  "Sqrt Temperature dissimilarity", "Sqrt Aridity dissimilarity")

if(whichk%in%"12k") df_l<-rbind(df_l, c(0.532, 0.230, 0.221))
if(whichk%in%"8k")  df_l<-rbind(df_l, c(0.532, 0.226, 0.224))
if(whichk%in%"4k") df_l<-rbind(df_l, c(0.532, 0.226, 0.224))

rownames(df_l)[10]<-"Proportion Variation"
write.csv(df_l,file = paste0("manuscript/Tables_EDGE/Paleo_loadings_", whichk,".csv"))


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
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/mod1_area_NOCLIMSIMNB.rds")); mod1->mod1cr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/mod2_area_NOSIMCLIMNB.rds")) ; mod2->mod2cr
load(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/mod3_area_NOSIMCLIMNB.rds")); mod3->mod3cr
load( file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/myPCA_perSO_av_NOSIMCLIMNB.rds"))
read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2_datamodel_", whichk,"/dfi0_PCA_nf4/dfB_PCA_perSO_av_NOSIMCLIMNB.csv"), stringsAsFactors = F)->dfb0
myPCA$loadings
RC_names<-c("GeoD_LCPsl_MCOSTe", "XericD_MCOSTx", "TempD_MCOSTt_NrSocSimET")
RCs<-c("RC1", "RC2","RC3")
rm(mod1) ; rm(mod2) ;rm(mod3) 
# ------------------------------------------------------------------------------------------------------------------------------------
names_lvls<-c("South Tropical China", "Lower-Middle Yangtze" ,"Chinese loess plateau", "West Yunan & East Tibet","Fertile Crescent", "Sava West India","Ganges of East India",
              "West Africa", "West African Savannah","Sudanic savannah", "Ethiopian plateau", 
              "Northern Lowlands of South America","Central/Southern Andes","Southwest Amazon" ,
              "Mesoamerica","East North America")

# mod1cr
rm(dft)
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
dft<-df_out

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
dft<-rbind(dft,df_out)

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
dft<-rbind(dft,df_out)

write.csv(dft,file = paste0("manuscript/Tables_EDGE/Paleo_model_", whichk,".csv"))

