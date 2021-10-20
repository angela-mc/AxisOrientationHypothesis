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
    read.csv(paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJagerS1/dfmS1_", trait_of_interest, ".csv"), stringsAsFactors = F)->dfm # 
    
    # subset for specific nbhood structure ; some will have NAs -> subset for that
    nbcols<-colnames(dfm)[grepl("nbhood", colnames(dfm))]
    nbcolsout<-nbcols[!nbcols%in%name_folder]
    dfm<-dfm[,!colnames(dfm)%in%nbcolsout]
    dfm<-dfm[complete.cases(dfm),] # after nbcols out since NA differ depending on nbhood structure
    colnames(dfm)[colnames(dfm)%in%name_folder]<-"nbhood"
    
    # data transformation
    dfm$logphyd<-log(dfm$phyd) # fam share for famID
    dfm$sqrtgeod<-sqrt(dfm$geod) # used dfiEA to get the distribution of all geo distances and see that sqrt produces the best transformation
    dfm$sqrtEDtemp<-sqrt(dfm$edTemp) # used distribution of all to see that sqrt() works
    dfm$sqrtEDxeric<-sqrt(dfm$edXeric) # used distribution of all to see that sqrt() works
    dfm$logMCOSTe<-log(dfm$MCOSTe+1) ;  dfm$logMCOSTt<-log(dfm$MCOSTt+1) ;  dfm$logMCOSTx<-log(dfm$MCOSTx+1)
    dfm$logLCPle<-log(dfm$LCPle+1) ;  dfm$logLCPlt<-log(dfm$LCPlt+1) ;  dfm$logLCPlx<-log(dfm$LCPlx+1)
    
    # nb struct transformation - not needed
    
    
    # (R)PCA analysis
   
    per1 = sum( dfm$share%in%1) / length(dfm$share)  # don t consider models with data points and imbalance of >10%/90% and the ones with nbhood = 1 value
    if(length(dfm$share)>=50 & per1>=0.1 & per1<=0.9 & length(unique(dfm$nbhood))>1)
      { dataPCA<-dfm[,colnames(dfm)%in%c("logphyd", "sqrtgeod","nbhood","sqrtEDtemp", "sqrtEDxeric",
                                       "logMCOSTe", "logMCOSTt", "logMCOSTx", "logLCPle", "logLCPlt", "logLCPlx"),]
      

        library(psych)
        myPCA<-principal(dataPCA, nfactors=5, rotate="varimax") # this function scales variables before doing PCA
        row.names(myPCA)
        myPCA$scores
        myPCA$Vaccounted
        myPCA$loadings
        
        if("RC1" %in% colnames(myPCA$scores))  dfm$RC1<-myPCA$scores[,colnames(myPCA$scores)%in%"RC1"]
        if(!"RC1" %in% colnames(myPCA$scores)) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC1"}
        if("RC2" %in% colnames(myPCA$scores))  dfm$RC2<-myPCA$scores[,colnames(myPCA$scores)%in%"RC2"]
        if(!"RC2" %in% colnames(myPCA$scores)) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC2"}
        if("RC3" %in% colnames(myPCA$scores))  dfm$RC3<-myPCA$scores[,colnames(myPCA$scores)%in%"RC3"]
        if(!"RC3" %in% colnames(myPCA$scores)) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC3"}
        if("RC4" %in% colnames(myPCA$scores))  dfm$RC4<-myPCA$scores[,colnames(myPCA$scores)%in%"RC4"]
        if(!"RC4" %in% colnames(myPCA$scores)) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC4"}
        if("RC5" %in% colnames(myPCA$scores))  dfm$RC5<-myPCA$scores[,colnames(myPCA$scores)%in%"RC5"]
        if(!"RC5" %in% colnames(myPCA$scores)) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC5"}
        
        ## multiply with sign of loadings so that every time the effect size means a positive thing i.e increase in X = increase in Y
        myPCA$loadings
        RCtraits<-rbind(myPCA$loadings[1,],myPCA$loadings[2,],myPCA$loadings[3,],myPCA$loadings[4,],myPCA$loadings[5,],
                        myPCA$loadings[6,], myPCA$loadings[7,], myPCA$loadings[8,], myPCA$loadings[9,], myPCA$loadings[10,], myPCA$loadings[11,])
        RCtraits<-as.data.frame(RCtraits)
        rownames(RCtraits)<-names(myPCA$communality) # these are the traits in order above
        
        # map variables on loadings
        RCtraits$whichRC<- NA
        RCtraits$loadingRC<- NA
        for(jj in 1: length(names(myPCA$communality)))
          { macRC<-max( abs(as.numeric(RCtraits[jj,])), na.rm=T)
          RCtraits$whichRC[jj]<-colnames(RCtraits)[1:5] [abs(as.numeric(RCtraits[jj,]))%in%macRC]
          RCtraits$loadingRC[jj]<- RCtraits[ RCtraits$whichRC[jj]] [jj,]}
        
        if(length(unique(sign(RCtraits[RCtraits$whichRC%in%"RC1",]$loadingRC)))>1) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"RC1_msigns"}
        if(length(unique(sign(RCtraits[RCtraits$whichRC%in%"RC2",]$loadingRC)))>1) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"RC2_msigns"}
        if("RC3"%in% RCtraits$whichRC & length(unique(sign(RCtraits[RCtraits$whichRC%in%"RC3",]$loadingRC)))>1) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"RC3_msigns"}
        if("RC4"%in% RCtraits$whichRC & length(unique(sign(RCtraits[RCtraits$whichRC%in%"RC4",]$loadingRC)))>1) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"RC4_msigns"}
        if("RC5"%in% RCtraits$whichRC & length(unique(sign(RCtraits[RCtraits$whichRC%in%"RC5",]$loadingRC)))>1) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"RC5_msigns"}
        #if("RC6"%in% RCtraits$whichRC & length(unique(sign(RCtraits[RCtraits$whichRC%in%"RC6",]$loadingRC)))>1) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"RC6_msigns"}
        
        RCtraits$sign<-sign(RCtraits$loadingRC)
        dfm$RC1<-dfm$RC1 *unique(RCtraits[RCtraits$whichRC%in%"RC1",]$sign) # multiply scores in df_model with sign
        dfm$RC2<-dfm$RC2 *unique(RCtraits[RCtraits$whichRC%in%"RC2",]$sign)
        if("RC3"%in% RCtraits$whichRC) {dfm$RC3<-dfm$RC3 *unique(RCtraits[RCtraits$whichRC%in%"RC3",]$sign)
        } else {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC3"}
        
        if("RC4"%in% RCtraits$whichRC) {dfm$RC4<-dfm$RC4 *unique(RCtraits[RCtraits$whichRC%in%"RC4",]$sign)
        } else {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC4"}
        if("RC5"%in% RCtraits$whichRC) {dfm$RC5<-dfm$RC5 *unique(RCtraits[RCtraits$whichRC%in%"RC5",]$sign)
        } else {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC5"}
        # if("RC6"%in% RCtraits$whichRC) {dfm$RC6<-dfm$RC6 *unique(RCtraits[RCtraits$whichRC%in%"RC6",]$sign)
        # } else {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC6"}
        #
        save(myPCA, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJagerRPCA/",name_folder,"/","pcaik_","_", trait_of_interest,".rds"))
        write.csv(dfm, file =paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJagerRPCA/",name_folder,"/","dfmS2_","_", trait_of_interest,".csv") )
        save(RCtraits, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJagerRPCA/",name_folder,"/","RCtraits_","_", trait_of_interest,".rds"))
        print(paste(name_folder,i))
        
        if(length(unique(RCtraits[rownames(RCtraits)%in%c("sqrtgeod","logMCOSTe","logLCPle","logLCPlt","logLCPlx"),]$whichRC))>1 |
           length(unique(RCtraits[rownames(RCtraits)%in%c("logphyd"),]$whichRC))>1 |
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
  # "EA037 nbhoodV210" "EA048 nbhoodV210" 
 
save(listw, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJagerRPCA/","listw",".rds"))


# see listw - this list has some traits with a different mapping - added as separate columns in tables

load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJagerRPCA/","listw",".rds"))
table(names(listw))
listw[!names(listw)%in%"conditions"] # "EA037 nbhoodV210" "EA048 nbhoodV210" 
traits_of_interest

load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJagerRPCA/","nbhoodV210","/","pcaik_","_", "EA037",".rds"))
myPCA$loadings
load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJagerRPCA/","nbhoodV210","/","RCtraits_","_", "EA037",".rds"))

# # from _calibUAgeo
# # Loadings:
# #   RC1    RC4    RC5    RC3    RC2   
# # nbhood      -0.123 -0.158        -0.973       
# # logphyd                                  0.998
# # sqrtgeod     0.894  0.328  0.255  0.110       
# # sqrtEDtemp   0.588  0.391  0.679  0.169       
# # sqrtEDxeric  0.362  0.888  0.197  0.186       
# # logMCOSTe    0.917  0.298  0.214  0.116       
# # logMCOSTt    0.692  0.295  0.630  0.139       
# # logMCOSTx    0.615  0.738  0.220  0.125       
# # logLCPle     0.914  0.305  0.229  0.121       
# # logLCPlt     0.904  0.298  0.231              
# # logLCPlx     0.955  0.189  0.130              
# # 
# # RC1   RC4   RC5   RC3   RC2
# # SS loadings    5.556 2.012 1.187 1.099 1.003
# # Proportion Var 0.505 0.183 0.108 0.100 0.091
# # Cumulative Var 0.505 0.688 0.796 0.896 0.987
load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJagerRPCA/","nbhoodV210","/","pcaik_","_", "EA048",".rds"))
myPCA$loadings
load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJagerRPCA/","nbhoodV210","/","RCtraits_","_", "EA048",".rds"))
# 
# # Loadings:
# #   RC1    RC2    RC3    RC4    RC5   
# # nbhood      -0.241 -0.243        -0.927       
# # logphyd      0.389  0.358                0.838
# # sqrtgeod     0.896  0.216         0.146  0.295
# # sqrtEDtemp   0.180  0.874         0.291  0.274
# # sqrtEDxeric  0.155         0.972              
# # logMCOSTe    0.897  0.309  0.212         0.163
# # logMCOSTt    0.499  0.817  0.179  0.107  0.141
# # logMCOSTx    0.695  0.317  0.596              
# # logLCPle     0.950  0.121  0.137  0.145  0.125
# # logLCPlt     0.941  0.174  0.135  0.163  0.123
# # logLCPlx     0.838  0.345  0.111  0.222  0.205
# # 
# # RC1   RC2   RC3   RC4   RC5
# # SS loadings    5.097 2.029 1.444 1.088 0.989
# # Proportion Var 0.463 0.184 0.131 0.099 0.090
# # Cumulative Var 0.463 0.648 0.779 0.878 0.968
