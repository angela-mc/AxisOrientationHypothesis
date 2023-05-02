rm(list=ls())

read.csv("data/latlong_soc_from_dplace-societies-2018-03-16.csv", stringsAsFactors = F)-> latlong_soc
read.csv("data/EA/EA/variables.csv", stringsAsFactors = F)-> vars
read.csv("data/EA/EA/codes_wallnewcodes.csv", stringsAsFactors = F)-> codes
read.csv("data/EA/EA/data_wallnewcodes.csv", stringsAsFactors = F)-> data
read.csv("data/EA/EA/societies.csv", stringsAsFactors = F)-> soc


library(pscl)
library(rcompanion)
library(DescTools)
library(corrplot)
library(rcompanion)
library(factoextra)
library(stats)

read.csv("data/newcodesFINAL.csv", stringsAsFactors = F)->newcodes
unique(newcodes$Trait)->alltraits
traits_of_interest<-list() # which traits can we consider?

for(i in 1: length(alltraits))
{ctrait<-alltraits[i]
if(sum(!is.na(newcodes[newcodes$Trait%in%ctrait,]$New.code))>0) traits_of_interest<-c(traits_of_interest, ctrait)}
traits_of_interest<-unlist(traits_of_interest)


###################
#### run GLMER ####
###################

library(car)
library(pROC)
library(lme4)

load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/","listw",".rds"))
#load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/","nbhoodV210","/","pcaik_","_", traits_of_interest[j],".rds"))
name_folders<-c("nbhoodV1","nbhoodV210") ##  the analysis will be separate for each nbhood structure ie 67 traits * 3 nbs types
round_dec<-3
diffRCorder<-list()

for(k in 1: 2)
  {name_folder<-name_folders[k]
  
  for(j in 1: length(traits_of_interest))
    #if(!(k==2 & j==34))
      { trait_of_interest<-traits_of_interest[j]
      if(!paste(trait_of_interest, name_folder)%in%listw & paste0("dfmS2_", "_",trait_of_interest,".csv") %in% list.files(paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/",name_folder)))  
        # for now the ones that 100% work
        {read.csv(file =paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/",name_folder,"/","dfmS2_", "_",trait_of_interest,".csv"), stringsAsFactors = F)->dfm
          
          load(paste0("data/coordinates/dfnb_EA_",100,".rda")) # dfiEA
          dfm[dfm$pairsoc_S%in%dfiEA$pair_soc,]->dfmSC # this condition is already done, here because before we had different spatial scales
          
          print(paste(k,j)) 
          model <- glmer(share ~ RC1 + RC2 + RC3 + RC4 + RC5 + 
                           (1 | soc1) + (1|soc2), data = dfmSC, family = binomial, 
                         control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
         
          save(model, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/models/",name_folder,"/model_",trait_of_interest,"_",100,".rds"))
          
          vifm<-vif(model) # futile now since RPCAs
          summary(model)->sumM
          sumM$coefficients-> sumMc
          
          nagelkerke(model)->nagel
          predpr <- predict(model,type=c("response"))
          roccurve <- pROC::roc(response = dfmSC$share , predictor = predpr)
          #plot(roccurve)
          aucroc<-pROC::auc(roccurve)
          
          df_out<-data.frame("Trait"=trait_of_interest, "Name_tr"=vars[vars$id%in%trait_of_interest,]$title, 
                             "Nsoc"=length(unique(c(dfmSC$soc1, dfmSC$soc2))),"Npairs"=length(dfmSC$pairsoc_S), 
                             "Count0"=table(dfmSC$share)[names(table(dfmSC$share))%in%"0"],
                             "Count1"=table(dfmSC$share)[names(table(dfmSC$share))%in%"1"],
                             "Percentage1" = round(table(dfmSC$share)[names(table(dfmSC$share))%in%"1"] / length(dfmSC[,1]),2),
                             "DFsRes"=sumM$AICtab[5], 
                             #"DFsNull"=model$df.null,
                             "AIC"=sumM$AICtab[1],
                             #"McFadden"=nagel$Pseudo.R.squared.for.model.vs.null[1],
                             #"Cox_Snell_ML"= nagel$Pseudo.R.squared.for.model.vs.null[2],
                             #"Nagelkerke"=nagel$Pseudo.R.squared.for.model.vs.null[3],
                             "AUC" = aucroc,
                             "Deviance"=sumM$AICtab[4], 
                             # "Null_deviance"=sumM$null.deviance,
                             "MaxVIF"=max(vifm),
                             "Intercept"=round(sumM$coefficients[1,1], digits=round_dec),
                             "RC1"= round(sumM$coefficients[2,1], digits=round_dec),
                             "RC1_pval"= round(sumM$coefficients[2,4],digits=round_dec),
                             "RC2"=round(sumM$coefficients[3,1],digits=round_dec),
                             "RC2_pval"=round(sumM$coefficients[3,4],digits=round_dec),
                             "RC3"=round(sumM$coefficients[4,1],digits=round_dec),
                             "RC3_pval"=round(sumM$coefficients[4,4], digits=round_dec),
                             "RC4"=round(sumM$coefficients[5,1], digits=round_dec),
                             "RC4_pval"=round(sumM$coefficients[5,4], digits=round_dec),
                             "RC5" = round(sumM$coefficients[6,1], digits=round_dec),
                             "RC5_pval" = round(sumM$coefficients[6,4], digits=round_dec)
                             #"RC6" = round(sumM$coefficients[7,1], digits=round_dec),
                             #"RC6_pval" = round(sumM$coefficients[7,4], digits=round_dec)
          )
          save(df_out, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/dfoutput/",name_folder,"/model_",trait_of_interest,".rds"))
          
          # replace the names of RCs with real predictor name
          load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/",name_folder,"/","RCtraits_","_", trait_of_interest,".rds")) # RCtraits
          RCs<-character()
          
          RCs[1]<-RCtraits[rownames(RCtraits)%in%"nbhood",]$whichRC[1]
          RCs[2]<-RCtraits[rownames(RCtraits)%in%"X.quali",]$whichRC[1]
          RCs[3]<-RCtraits[rownames(RCtraits)%in%"sqrtgeod",]$whichRC[1]
          RCs[4]<-RCtraits[rownames(RCtraits)%in%"sqrtEDtemp",]$whichRC[1]
          RCs[5]<-RCtraits[rownames(RCtraits)%in%"sqrtEDxeric",]$whichRC[1]
          names(RCs)<-c("Nbhood", "ShareFam", "GeoD_LCPsl_MCOSTe", "TempD_MCOSTt", "XericD_MCOSTx")
          
          colnames(df_out)[colnames(df_out)%in%"RC1"]<-names(RCs)[RCs%in%"dim1.rot"] ; colnames(df_out)[colnames(df_out)%in%"RC1_pval"]<-paste0(names(RCs)[RCs%in%"dim1.rot"], "_pval")
          colnames(df_out)[colnames(df_out)%in%"RC2"]<-names(RCs)[RCs%in%"dim2.rot"] ; colnames(df_out)[colnames(df_out)%in%"RC2_pval"]<-paste0(names(RCs)[RCs%in%"dim2.rot"], "_pval")
          colnames(df_out)[colnames(df_out)%in%"RC3"]<-names(RCs)[RCs%in%"dim3.rot"] ; colnames(df_out)[colnames(df_out)%in%"RC3_pval"]<-paste0(names(RCs)[RCs%in%"dim3.rot"], "_pval")
          colnames(df_out)[colnames(df_out)%in%"RC4"]<-names(RCs)[RCs%in%"dim4.rot"] ; colnames(df_out)[colnames(df_out)%in%"RC4_pval"]<-paste0(names(RCs)[RCs%in%"dim4.rot"], "_pval")
          colnames(df_out)[colnames(df_out)%in%"RC5"]<-names(RCs)[RCs%in%"dim5.rot"] ; colnames(df_out)[colnames(df_out)%in%"RC5_pval"]<-paste0(names(RCs)[RCs%in%"dim5.rot"], "_pval")
          if(!(names(RCs)[RCs%in%"dim1.rot"] %in% "GeoD_LCPsl_MCOSTe" & names(RCs)[RCs%in%"dim2.rot"] %in%"XericD_MCOSTx" & names(RCs)[RCs%in%"dim3.rot"] %in% "ShareFam" & 
               names(RCs)[RCs%in%"dim4.rot"]%in%"Nbhood" &  names(RCs)[RCs%in%"dim5.rot"]%in%"TempD_MCOSTt")) diffRCorder<-c(diffRCorder, paste0(trait_of_interest, "_", name_folder))
          
          save(df_out, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/dfoutput/",name_folder,"/dfoutNamesPred_",trait_of_interest,".rds"))
          
        } # from if(!paste(trait_of_interest, name_folder)%in%listw)
      
      
      } # from j = traits_of_interest
} # from k = name_folders


# ###########################
# #### big df ############### 
# ###########################

name_folders<-c("nbhoodV1","nbhoodV210") ##  the analysis will be separate for each nbhood structure ie 67 traits 

m1<-list.files(paste0("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/dfoutput/",name_folders[1],"/"))
m1<-m1[grep("dfoutNamesPred", m1)]
m2<-list.files(paste0("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/dfoutput/",name_folders[2],"/"))
m2<-m2[grep("dfoutNamesPred", m2)]

rm(dfm1)
for(i in 1:length(m1))
    { load(paste0("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/dfoutput/",name_folders[1],"/",m1[i]))
      
      if(i==1) dfm1<-df_out
      if(i>1) dfm1<-rbind(dfm1, df_out)
  }


rm(dfm2)
for(i in 1:length(m2))
  { load(paste0("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/dfoutput/",name_folders[2],"/",m2[i]))
    if(i==1) dfm2<-df_out
    if(i>1) dfm2<-rbind(dfm2, df_out)
}

write.csv(dfm1, file="data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/dfm1.csv")  # these pvalues are NOT adjusted
write.csv(dfm2, file="data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/dfm2.csv")  # these pvalues are NOT adjusted




# ###########################
# #### Moran s I ############ 
# ###########################

rm(list=ls())

library(car)
library(pROC)
library(lme4)

load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/","listw",".rds"))
name_folders<-c("nbhoodV1","nbhoodV210") ##  the analysis will be separate for each nbhood structure ie 67 traits 
load("data/coordinates/distmidpoints_P1.rds") # this is a 3 GB file - don't upload on GIT
load(paste0("data/coordinates/dfnb_EA_",100,".rda"))

m1<-list.files(paste0("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/models/",name_folders[1],"/"))
m2<-list.files(paste0("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/models/",name_folders[2],"/"))

fxn_sort<-function(x)
  {return( paste( sort(strsplit(x, split="_")[[1]]) ,collapse ="_")) }

library(letsR)
list.files("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/")


pdf("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/MoransI_nbhoodV1_forS.pdf", height = 10, width = 10)
par(mfrow=c(4,4))
par(mar=c(4,5,0.5,0.5))

for(i in 1: length(m1))
  { load(paste0("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/models/",name_folders[1],"/",m1[i]))
    matx<-as.matrix(residuals(model))
    socs<-paste(model@frame$soc1,model@frame$soc2,sep="_")
    
    rownames(matx)<-unlist(lapply(socs,fxn_sort))
    distmi<-distm[rownames(matx),rownames(matx)]
    
    c<-lets.correl(x = matx, y=distmi, z=12,equidistant = T, plot = F) 
    toplot<-nrow(c)/2
    plot(c[1:toplot,1] ~ c[1:toplot,5], ylim=c(-1,1), pch=19, ylab="Moran's I", xlab="Distance",cex.lab=1.5, yaxt="n", xaxt="n")
    
    axis(side=2, at=c(-0.75,0,0.75), labels=c(-0.75,0,0.75),cex=1,tick=F)
    axis(side=1, at=c[1:toplot,5], labels=format(round(c[1:toplot,5]), scientific=T) ,cex=1,tick=F)
    
    lines(c[1:toplot,1] ~ c[1:toplot,5], ylim=c(-1,1), lty=2, col="gray75")
    abline(h=0, lty=2, col="red")
    for(j in 1:toplot) # add SE
    {arrows(x0= c[j,5], y0=c[j,1], x1=c[j,5], y1= (c[j,1]+ c[j,2]), angle = 180)
      arrows(x0= c[j,5], y0=c[j,1], x1=c[j,5], y1= (c[j,1]- c[j,2]), angle = 180)}
    codevar<-strsplit(m1[i], split=".rds")[[1]][1]
    codevar<-strsplit(codevar,split="_")[[1]][2]
    title(main=codevar, line = -2,cex=2)
    
    
    print(i)
}
dev.off()


pdf("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/MoransI_nbhoodV210_forS.pdf", height = 10, width = 10)
par(mfrow=c(4,4))
par(mar=c(4,5,0.5,0.5))

for(i in 1: length(m2))
  { load(paste0("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/models/",name_folders[2],"/",m2[i]))
    matx<-as.matrix(residuals(model))
    socs<-paste(model@frame$soc1,model@frame$soc2,sep="_")
    
    rownames(matx)<-unlist(lapply(socs,fxn_sort))
    distmi<-distm[rownames(matx),rownames(matx)]
    
    c<-lets.correl(x = matx, y=distmi, z=12,equidistant = T, plot = F) 
    toplot<-nrow(c)/2
    plot(c[1:toplot,1] ~ c[1:toplot,5], ylim=c(-1,1), pch=19, ylab="Moran's I", xlab="Distance",cex.lab=1.5, yaxt="n", xaxt="n")
    
    axis(side=2, at=c(-0.75,0,0.75), labels=c(-0.75,0,0.75),cex=1,tick=F)
    axis(side=1, at=c[1:toplot,5], labels=format(round(c[1:toplot,5]), scientific=T) ,cex=1,tick=F)
    
    lines(c[1:toplot,1] ~ c[1:toplot,5], ylim=c(-1,1), lty=2, col="gray75")
    abline(h=0, lty=2, col="red")
    for(j in 1:toplot) # add SE
    {arrows(x0= c[j,5], y0=c[j,1], x1=c[j,5], y1= (c[j,1]+ c[j,2]), angle = 180)
      arrows(x0= c[j,5], y0=c[j,1], x1=c[j,5], y1= (c[j,1]- c[j,2]), angle = 180)}
    codevar<-strsplit(m2[i], split=".rds")[[1]][1]
    codevar<-strsplit(codevar,split="_")[[1]][2]
    title(main=codevar, line = -2,cex=2)
    
    
    print(i)
}
dev.off()


# LISTW
load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/","listw",".rds"))
listw[which(names(listw)%in%"not_sameERCs")]

name_folder<-"nbhoodV1"
trait_of_interest<-"EA068" 

name_folder<-"nbhoodV210"
trait_of_interest<-"EA048" 

name_folder<-"nbhoodV210"
trait_of_interest<-"EA068" 

read.csv(file =paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/",name_folder,"/","dfmS2_", "_",trait_of_interest,".csv"), stringsAsFactors = F)->dfm
load(paste0("data/coordinates/dfnb_EA_",100,".rda")) # dfiEA
dfm[dfm$pairsoc_S%in%dfiEA$pair_soc,]->dfmSC # this condition is already done, here because before we had different spatial scales

model <- glmer(share ~ RC1 + RC2 + RC3 + RC4 + RC5 + 
                 (1 | soc1) + (1|soc2), data = dfmSC, family = binomial, 
               control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
# don t save model - confusing
vifm<-vif(model) # futile now since RPCAs
summary(model)->sumM
sumM$coefficients-> sumMc
nagelkerke(model)->nagel
predpr <- predict(model,type=c("response"))
roccurve <- pROC::roc(response = dfmSC$share , predictor = predpr)
aucroc<-pROC::auc(roccurve)

df_out<-data.frame("Trait"=trait_of_interest, "Name_tr"=vars[vars$id%in%trait_of_interest,]$title, 
                   "Nsoc"=length(unique(c(dfmSC$soc1, dfmSC$soc2))),"Npairs"=length(dfmSC$pairsoc_S), 
                   "Count0"=table(dfmSC$share)[names(table(dfmSC$share))%in%"0"],
                   "Count1"=table(dfmSC$share)[names(table(dfmSC$share))%in%"1"],
                   "Percentage1" = round(table(dfmSC$share)[names(table(dfmSC$share))%in%"1"] / length(dfmSC[,1]),2),
                   "DFsRes"=sumM$AICtab[5], 
                   #"DFsNull"=model$df.null,
                   "AIC"=sumM$AICtab[1],
                   #"McFadden"=nagel$Pseudo.R.squared.for.model.vs.null[1],
                   #"Cox_Snell_ML"= nagel$Pseudo.R.squared.for.model.vs.null[2],
                   #"Nagelkerke"=nagel$Pseudo.R.squared.for.model.vs.null[3],
                   "AUC" = aucroc,
                   "Deviance"=sumM$AICtab[4], 
                   # "Null_deviance"=sumM$null.deviance,
                   "MaxVIF"=max(vifm),
                   "Intercept"=round(sumM$coefficients[1,1], digits=round_dec),
                   "RC1"= round(sumM$coefficients[2,1], digits=round_dec),
                   "RC1_pval"= round(sumM$coefficients[2,4],digits=round_dec),
                   "RC2"=round(sumM$coefficients[3,1],digits=round_dec),
                   "RC2_pval"=round(sumM$coefficients[3,4],digits=round_dec),
                   "RC3"=round(sumM$coefficients[4,1],digits=round_dec),
                   "RC3_pval"=round(sumM$coefficients[4,4], digits=round_dec),
                   "RC4"=round(sumM$coefficients[5,1], digits=round_dec),
                   "RC4_pval"=round(sumM$coefficients[5,4], digits=round_dec),
                   "RC5" = round(sumM$coefficients[6,1], digits=round_dec),
                   "RC5_pval" = round(sumM$coefficients[6,4], digits=round_dec)
                   #"RC6" = round(sumM$coefficients[7,1], digits=round_dec),
                   #"RC6_pval" = round(sumM$coefficients[7,4], digits=round_dec)
)
# don t save df out - confusing
summary(model)
df_out
write.csv(df_out, file=paste0("manuscript/Tables_newtree/TableS4d_", trait_of_interest,".csv")) 
# modify table name

load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/",name_folder,"/","RCtraits_","_", trait_of_interest,".rds"))
RCtraits
rownames(RCtraits)<-c("Indirect exposure", "Sqrt geodesic distance",
                      "Sqrt temperature harshness dissimilarity",
                      "Sqrt aridity index dissimilarity", "log travel least-cost path cost",
                      "log temperatureleast-cost path cost", "log aridity least-cost path cost",
                      "log travel least-cost path length", "log temperature least-cost path length",
                      "log aridity least-cost path length", "Shared Fam")
write.csv(RCtraits, file=paste0("manuscript/Tables_newtree/TableS2d_", trait_of_interest,".csv"))
# modify table name
