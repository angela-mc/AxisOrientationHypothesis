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
library(car)
library(pROC)
library(lme4)

read.csv("data/newcodesFINAL.csv", stringsAsFactors = F)->newcodes
unique(newcodes$Trait)->alltraits
traits_of_interest<-list() # which traits can we consider?

for(i in 1: length(alltraits))
{ctrait<-alltraits[i]
if(sum(!is.na(newcodes[newcodes$Trait%in%ctrait,]$New.code))>0) traits_of_interest<-c(traits_of_interest, ctrait)}
traits_of_interest<-unlist(traits_of_interest)


#################
#### run RPCA  ## RPCA per trait because the value of nb are different for trait
#################

name_folders<-c("nbhoodV1", "nbhoodV210") ##  the analysis will be separate for each nbhood structure ie 67 traits * 2 nbs types
listw<-list()
#rm(dfVIF)

for(k in 1: 2)
{ name_folder<-name_folders[k]
#if(k==2) rm(dfVIF)

for(i in 1: length(traits_of_interest))
   {trait_of_interest<-traits_of_interest[i]
   read.csv(paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdS1/dfmS1_", trait_of_interest, ".csv"), stringsAsFactors = F)->dfm # 
   
   # subset for specific nbhood structure ; some will have NAs -> subset for that
   nbcols<-colnames(dfm)[grepl("nbhood", colnames(dfm))]
   nbcolsout<-nbcols[!nbcols%in%name_folder]
   dfm<-dfm[,!colnames(dfm)%in%nbcolsout]
   dfm<-dfm[complete.cases(dfm),] # after nbcols out since NA differ depending on nbhood structure
   colnames(dfm)[colnames(dfm)%in%name_folder]<-"nbhood"
   
   # data transformation
   dfm$sqrtgeod<-sqrt(dfm$geod) # used dfiEA to get the distribution of all geo distances and see that sqrt produces the best transformation
   dfm$sqrtEDtemp<-sqrt(dfm$edTemp) # used distribution of all to see that sqrt() works
   dfm$sqrtEDxeric<-sqrt(dfm$edXeric) # used distribution of all to see that sqrt() works
   dfm$logMCOSTe<-log(dfm$MCOSTe+1) ;  dfm$logMCOSTt<-log(dfm$MCOSTt+1) ;  dfm$logMCOSTx<-log(dfm$MCOSTx+1)
   dfm$logLCPle<-log(dfm$LCPle+1) ;  dfm$logLCPlt<-log(dfm$LCPlt+1) ;  dfm$logLCPlx<-log(dfm$LCPlx+1)
   
   # (R)PCA analysis
   
   per1 = sum( dfm$share%in%1) / length(dfm$share)  # don t consider models with data points and imbalance of >10%/90% and the ones with nbhood = 1 value
   if(length(dfm$share)>=50 & per1>=0.1 & per1<=0.9 & length(unique(dfm$nbhood))>1)
      { dataPCA<-dfm[,colnames(dfm)%in%c("famid", "sqrtgeod","nbhood","sqrtEDtemp", "sqrtEDxeric",
                                         "logMCOSTe", "logMCOSTt", "logMCOSTx", "logLCPle", "logLCPlt", "logLCPlx"),]
      
    
      library(PCAmixdata)
      dataPCA$shareFAM<-as.factor(dataPCA$famid)
      
      # does PCAmix scale variables before hand? Don't know, categorical variables are OK categorical, but continous? scale beforehand
      dataPCA$nbhood<-scale(dataPCA$nbhood)
      dataPCA$sqrtgeod<-scale(dataPCA$sqrtgeod)
      dataPCA$sqrtEDtemp<-scale(dataPCA$sqrtEDtemp)
      dataPCA$sqrtEDxeric<-scale(dataPCA$sqrtEDxeric)
      
      dataPCA$logMCOSTe<-scale(dataPCA$logMCOSTe)
      dataPCA$logMCOSTt<-scale(dataPCA$logMCOSTt)
      dataPCA$logMCOSTx<-scale(dataPCA$logMCOSTx)
      dataPCA$logLCPle<-scale(dataPCA$logLCPle)
      dataPCA$logLCPlt<-scale(dataPCA$logLCPlt)
      dataPCA$logLCPlx<-scale(dataPCA$logLCPlx)
      
      #dataPCA[,colnames(dataPCA)%in%"shareFAM"]
      
      res.pcamix<-PCAmix(X.quanti = dataPCA[,colnames(dataPCA)%in%c("nbhood","sqrtgeod" , "sqrtEDtemp" ,"sqrtEDxeric", 
                                                                    "logMCOSTe","logMCOSTt","logMCOSTx",
                                                                    "logLCPle","logLCPlt","logLCPlx")],
                         X.quali = dataPCA$shareFAM,graph=FALSE) 
      res.pcamix$eig 
      res.pcarot <- PCArot(res.pcamix,dim=5,graph=FALSE)
      res.pcarot$eig #variance of the rotated PCs
      #round(res.pcamix$sqload[,1:3],digit=2)
      round(res.pcarot$sqload,digit=2)
      res.pcarot$ind$coord
      colnames( res.pcarot$ind$coord)
      
      if("dim1.rot" %in% colnames(res.pcarot$ind$coord))  dfm$RC1<-res.pcarot$ind$coord[,colnames( res.pcarot$ind$coord)%in%"dim1.rot"]
      if(!"dim1.rot" %in% colnames(res.pcarot$ind$coord)) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC1"}
      if("dim2.rot" %in% colnames(res.pcarot$ind$coord))  dfm$RC2<-res.pcarot$ind$coord[,colnames( res.pcarot$ind$coord)%in%"dim2.rot"]
      if(!"dim2.rot" %in% colnames(res.pcarot$ind$coord)) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC2"}
      if("dim3.rot" %in% colnames(res.pcarot$ind$coord))  dfm$RC3<-res.pcarot$ind$coord[,colnames( res.pcarot$ind$coord)%in%"dim3.rot"]
      if(!"dim3.rot" %in% colnames(res.pcarot$ind$coord)) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC3"}
      if("dim4.rot" %in% colnames(res.pcarot$ind$coord))  dfm$RC4<-res.pcarot$ind$coord[,colnames( res.pcarot$ind$coord)%in%"dim4.rot"]
      if(!"dim4.rot" %in% colnames(res.pcarot$ind$coord)) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC4"}
      if("dim5.rot" %in% colnames(res.pcarot$ind$coord))  dfm$RC5<-res.pcarot$ind$coord[,colnames( res.pcarot$ind$coord)%in%"dim5.rot"]
      if(!"dim5.rot" %in% colnames(res.pcarot$ind$coord)) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC5"}
      
      ## multiply with sign of loadings so that every time the effect size means a positive thing i.e increase in X = increase in Y
      round(res.pcarot$sqload,digit=2)
      RCtraits<-rbind(res.pcarot$sqload[1,],res.pcarot$sqload[2,],res.pcarot$sqload[3,],res.pcarot$sqload[4,],res.pcarot$sqload[5,],
                      res.pcarot$sqload[6,],res.pcarot$sqload[7,],res.pcarot$sqload[8,],res.pcarot$sqload[8,],res.pcarot$sqload[10,],
                      res.pcarot$sqload[11,])
      RCtraits<-as.data.frame(RCtraits)        
      rownames(RCtraits)<-rownames(res.pcarot$sqload) # these are the traits in order above
      
      # map variables on loadings
      RCtraits$whichRC<- NA
      RCtraits$loadingRC<- NA
      for(jj in 1: length(rownames(res.pcarot$sqload) ))
      { macRC<-max( abs(as.numeric(RCtraits[jj,])), na.rm=T)
        RCtraits$whichRC[jj]<-colnames(RCtraits)[1:5] [abs(as.numeric(RCtraits[jj,]))%in%macRC]
        RCtraits$loadingRC[jj]<- RCtraits[ RCtraits$whichRC[jj]] [jj,]}
      
      
      if(length(unique(sign(RCtraits[RCtraits$whichRC%in%"dim1.rot",]$loadingRC)))>1) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"RC1_msigns"}
      if(length(unique(sign(RCtraits[RCtraits$whichRC%in%"dim2.rot",]$loadingRC)))>1) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"RC2_msigns"}
      if("dim3.rot"%in% RCtraits$whichRC & length(unique(sign(RCtraits[RCtraits$whichRC%in%"dim3.rot",]$loadingRC)))>1) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"RC3_msigns"}
      if("dim4.rot"%in% RCtraits$whichRC & length(unique(sign(RCtraits[RCtraits$whichRC%in%"dim4.rot",]$loadingRC)))>1) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"RC4_msigns"}
      if("dim5.rot"%in% RCtraits$whichRC & length(unique(sign(RCtraits[RCtraits$whichRC%in%"dim5.rot",]$loadingRC)))>1) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"RC5_msigns"}
      
      RCtraits$sign<-sign(RCtraits$loadingRC)
      dfm$RC1<-dfm$RC1 *unique(RCtraits[RCtraits$whichRC%in%"dim1.rot",]$sign) # multiply scores in df_model with sign
      dfm$RC2<-dfm$RC2 *unique(RCtraits[RCtraits$whichRC%in%"dim2.rot",]$sign)
      if("dim3.rot"%in% RCtraits$whichRC) {dfm$RC3<-dfm$RC3 *unique(RCtraits[RCtraits$whichRC%in%"dim3.rot",]$sign)
      } else {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC3"}
      
      if("dim4.rot"%in% RCtraits$whichRC) {dfm$RC4<-dfm$RC4 *unique(RCtraits[RCtraits$whichRC%in%"dim4.rot",]$sign)
      } else {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC4"}
      if("dim5.rot"%in% RCtraits$whichRC) {dfm$RC5<-dfm$RC5 *unique(RCtraits[RCtraits$whichRC%in%"dim5.rot",]$sign)
      } else {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC5"}
      # if("RC6"%in% RCtraits$whichRC) {dfm$RC6<-dfm$RC6 *unique(RCtraits[RCtraits$whichRC%in%"RC6",]$sign)
      # } else {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC6"}
      #
      
      
      save(res.pcarot, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/",name_folder,"/","res.pcarot_","_", trait_of_interest,".rds"))
      save(res.pcamix, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/",name_folder,"/","res_pcamix_","_", trait_of_interest,".rds"))
      
      write.csv(dfm, file =paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/",name_folder,"/","dfmS2_","_", trait_of_interest,".csv") )
      save(RCtraits, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/",name_folder,"/","RCtraits_","_", trait_of_interest,".rds"))
      
      print(paste(name_folder,i))
      
      if(length(unique(RCtraits[rownames(RCtraits)%in%c("sqrtgeod","logMCOSTe","logLCPle","logLCPlt","logLCPlx"),]$whichRC))>1 |
         length(unique(RCtraits[rownames(RCtraits)%in%c("X.quali"),]$whichRC))>1 |
         length(unique(RCtraits[rownames(RCtraits)%in%c("nbhood"),]$whichRC))>1 |
         length(unique(RCtraits[rownames(RCtraits)%in%c("sqrtEDtemp","logMCOSTt"),]$whichRC))>1 |
         length(unique(RCtraits[rownames(RCtraits)%in%c("sqrtEDxeric","logMCOSTx"),]$whichRC))>1)
      {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"not_sameERCs"}
      
      } # if(length(dfm$share>=50) & per1>=0.1 & per1<=0.9 & length(unique(dfm$nbhood))>1)
   if(!(length(dfm$share>=50) & per1>=0.1 & per1<=0.9 & length(unique(dfm$nbhood))>1)) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"conditions"}
   
   print(paste(k,i))
} # from i

#write.csv(dfVIF, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJagerRPCA/dfVIF_", name_folder,".csv"))
} # from k


listw<-unlist(listw)
table(names(listw))
listw[!names(listw)%in%"conditions"]
# not_sameERCs       not_sameERCs 
# "EA068 nbhoodV1" "EA048 nbhoodV210" "EA068 nbhoodV210" 

#save(listw, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/","listw",".rds"))

# see listw - this list has some traits with a different mapping - added as separate columns in tables

load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/","listw",".rds"))
load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/","nbhoodV1","/","RCtraits_","_", "EA068",".rds"))
RCtraits

load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/","nbhoodV210","/","RCtraits_","_", "EA068",".rds"))
RCtraits

load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/","nbhoodV210","/","RCtraits_","_", "EA048",".rds"))
RCtraits
