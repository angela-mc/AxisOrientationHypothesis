
calcSElines<-function(dtrial, name_folder, coldown="royalblue1", colup="indianred1")
  { dtrial$SEGeoD_LCPsl_MCOSTe<--999 ; dtrial$SEXericD_MCOSTx<- -999 ; dtrial$SETempD_MCOSTt<--999 ; 
    dtrial$SEPhyD<- -999  ; dtrial$SENbhood<- -999
  
  for(i in 1: length(dtrial[,1]))
    { tvar<-dtrial$Trait[i]
    load(paste0("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_Jager/models/",name_folder,"/model_",tvar,"_","100",".rds"))
    summary(model)->sumM
    sumM$coefficients-> sumMc
    
    # which RC is what?
    load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJagerRPCA/",name_folder,"/","RCtraits_","_", tvar,".rds")) # RCtraits
    RCs<-character()
    
    RCs[1]<-RCtraits[rownames(RCtraits)%in%"nbhood",]$whichRC[1]
    RCs[2]<-RCtraits[rownames(RCtraits)%in%"logphyd",]$whichRC[1]
    RCs[3]<-RCtraits[rownames(RCtraits)%in%"sqrtgeod",]$whichRC[1]
    RCs[4]<-RCtraits[rownames(RCtraits)%in%"sqrtEDtemp",]$whichRC[1]
    RCs[5]<-RCtraits[rownames(RCtraits)%in%"sqrtEDxeric",]$whichRC[1]
    names(RCs)<-c("Nbhood", "PhyD", "GeoD_LCPsl_MCOSTe", "TempD_MCOSTt", "XericD_MCOSTx")
    
    dtrial$SEGeoD_LCPsl_MCOSTe[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"GeoD_LCPsl_MCOSTe"],][2]
    dtrial$SEXericD_MCOSTx[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"XericD_MCOSTx"],][2]
    dtrial$SETempD_MCOSTt[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"TempD_MCOSTt"],][2]
    dtrial$SEPhyD[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"PhyD"],][2]
    dtrial$SENbhood[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"Nbhood"],][2]
    
    }
  return(dtrial) 
}


calcSElines_newtree<-function(dtrial, name_folder, coldown="royalblue1", colup="indianred1")
  { dtrial$SEGeoD_LCPsl_MCOSTe<--999 ; dtrial$SEXericD_MCOSTx<- -999 ; dtrial$SETempD_MCOSTt<--999 ; 
  dtrial$SEPhyD<- -999  ; dtrial$SENbhood<- -999
  
  for(i in 1: length(dtrial[,1]))
    { tvar<-dtrial$Trait[i]
    load(paste0("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_Jager_newtree/models/",name_folder,"/model_",tvar,"_","100",".rds"))
    summary(model)->sumM
    sumM$coefficients-> sumMc
    
    # which RC is what?
    load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJagerRPCA_newtree/",name_folder,"/","RCtraits_","_", tvar,".rds")) # RCtraits
    RCs<-character()
    
    RCs[1]<-RCtraits[rownames(RCtraits)%in%"nbhood",]$whichRC[1]
    RCs[2]<-RCtraits[rownames(RCtraits)%in%"logphyd",]$whichRC[1]
    RCs[3]<-RCtraits[rownames(RCtraits)%in%"sqrtgeod",]$whichRC[1]
    RCs[4]<-RCtraits[rownames(RCtraits)%in%"sqrtEDtemp",]$whichRC[1]
    RCs[5]<-RCtraits[rownames(RCtraits)%in%"sqrtEDxeric",]$whichRC[1]
    names(RCs)<-c("Nbhood", "PhyD", "GeoD_LCPsl_MCOSTe", "TempD_MCOSTt", "XericD_MCOSTx")
    
    dtrial$SEGeoD_LCPsl_MCOSTe[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"GeoD_LCPsl_MCOSTe"],][2]
    dtrial$SEXericD_MCOSTx[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"XericD_MCOSTx"],][2]
    dtrial$SETempD_MCOSTt[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"TempD_MCOSTt"],][2]
    dtrial$SEPhyD[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"PhyD"],][2]
    dtrial$SENbhood[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"Nbhood"],][2]
    
    }
  return(dtrial) 
}


calcSElines_EDGE<-function(dtrial, name_folder, coldown="royalblue1", colup="indianred1")
  { dtrial$SEGeoD_LCPsl_MCOSTe<--999 ; dtrial$SEXericD_MCOSTx<- -999 ; dtrial$SETempD_MCOSTt<--999 ; 
  dtrial$SEPhyD<- -999  ; dtrial$SENbhood<- -999
  
  for(i in 1: length(dtrial[,1]))
    { tvar<-dtrial$Trait[i]
    load(paste0("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_EDGE/models/",name_folder,"/model_",tvar,"_","100",".rds"))
    summary(model)->sumM
    sumM$coefficients-> sumMc
    
    # which RC is what?
    load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmEDGERPCA/",name_folder,"/","RCtraits_","_", tvar,".rds")) # RCtraits
    RCs<-character()
    
    RCs[1]<-RCtraits[rownames(RCtraits)%in%"nbhood",]$whichRC[1]
    RCs[2]<-RCtraits[rownames(RCtraits)%in%"logphyd",]$whichRC[1]
    RCs[3]<-RCtraits[rownames(RCtraits)%in%"sqrtgeod",]$whichRC[1]
    RCs[4]<-RCtraits[rownames(RCtraits)%in%"sqrtEDtemp",]$whichRC[1]
    RCs[5]<-RCtraits[rownames(RCtraits)%in%"sqrtEDxeric",]$whichRC[1]
    names(RCs)<-c("Nbhood", "PhyD", "GeoD_LCPsl_MCOSTe", "TempD_MCOSTt", "XericD_MCOSTx")
    
    dtrial$SEGeoD_LCPsl_MCOSTe[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"GeoD_LCPsl_MCOSTe"],][2]
    dtrial$SEXericD_MCOSTx[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"XericD_MCOSTx"],][2]
    dtrial$SETempD_MCOSTt[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"TempD_MCOSTt"],][2]
    dtrial$SEPhyD[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"PhyD"],][2]
    dtrial$SENbhood[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"Nbhood"],][2]
    
    }
  return(dtrial) 
}

df_moreadds<-function(workingdf, name_folder,method_adj,vars, coldown="royalblue1", colup="indianred1") 
  {# adjust pvalues
  workingdf$GeoD_LCPsl_MCOSTe_pvalue<-p.adjust (workingdf$GeoD_LCPsl_MCOSTe_pval, method = method_adj)
  workingdf$XericD_MCOSTx_pvalue<-p.adjust (workingdf$XericD_MCOSTx_pval, method = method_adj)
  workingdf$TempD_MCOSTt_pvalue<-p.adjust (workingdf$TempD_MCOSTt_pval, method = method_adj)
  workingdf$PhyD_pvalue<-p.adjust (workingdf$PhyD_pval, method = method_adj)
  workingdf$Nbhood_pvalue<-p.adjust (workingdf$Nbhood_pval, method = method_adj)
  
    # sign col
    workingdf$signGeoD_LCPsl_MCOSTe<- "grey72" ; workingdf$signXericD_MCOSTx<- "grey72" ;  workingdf$signTempD_MCOSTt<- "grey72" #ns
    workingdf$signNbhood<- "grey72" ; workingdf$signPhyD<- "grey72" ; 
    
    if(sum(workingdf$GeoD_LCPsl_MCOSTe_pvalue<0.05 & workingdf$GeoD_LCPsl_MCOSTe<0)>0) workingdf[workingdf$GeoD_LCPsl_MCOSTe_pvalue<0.05 & workingdf$GeoD_LCPsl_MCOSTe<0,]$signGeoD_LCPsl_MCOSTe<-coldown
    if(sum(workingdf$GeoD_LCPsl_MCOSTe_pvalue<0.05 & workingdf$GeoD_LCPsl_MCOSTe>0)>0) workingdf[workingdf$GeoD_LCPsl_MCOSTe_pvalue<0.05 & workingdf$GeoD_LCPsl_MCOSTe>0,]$signGeoD_LCPsl_MCOSTe<-colup
    
    if(sum(workingdf$XericD_MCOSTx_pvalue<0.05 & workingdf$XericD_MCOSTx<0)>0) workingdf[workingdf$XericD_MCOSTx_pvalue<0.05 & workingdf$XericD_MCOSTx<0,]$signXericD_MCOSTx<-coldown
    if(sum(workingdf$XericD_MCOSTx_pvalue<0.05 & workingdf$XericD_MCOSTx>0)>0) workingdf[workingdf$XericD_MCOSTx_pvalue<0.05 & workingdf$XericD_MCOSTx>0,]$signXericD_MCOSTx<-colup
    
    if(sum(workingdf$TempD_MCOSTt_pvalue<0.05 & workingdf$TempD_MCOSTt<0)>0) workingdf[workingdf$TempD_MCOSTt_pvalue<0.05 & workingdf$TempD_MCOSTt<0,]$signTempD_MCOSTt<-coldown
    if(sum(workingdf$TempD_MCOSTt_pvalue<0.05 & workingdf$TempD_MCOSTt>0)>0) workingdf[workingdf$TempD_MCOSTt_pvalue<0.05 & workingdf$TempD_MCOSTt>0,]$signTempD_MCOSTt<-colup
    
    if(sum(workingdf$Nbhood_pvalue<0.05 & workingdf$Nbhood<0)>0) workingdf[workingdf$Nbhood_pvalue<0.05 & workingdf$Nbhood<0,]$signNbhood<-coldown
    if(sum(workingdf$Nbhood_pvalue<0.05 & workingdf$Nbhood>0)>0) workingdf[workingdf$Nbhood_pvalue<0.05 & workingdf$Nbhood>0,]$signNbhood<-colup
    
    if(sum(workingdf$PhyD_pvalue<0.05 & workingdf$PhyD<0)>0) workingdf[workingdf$PhyD_pvalue<0.05 & workingdf$PhyD<0,]$signPhyD<-coldown
    if(sum(workingdf$PhyD_pvalue<0.05 & workingdf$PhyD>0)>0) workingdf[workingdf$PhyD_pvalue<0.05 & workingdf$PhyD>0,]$signPhyD<-colup
    
    # add trait category
    vars[vars$category%in%c("Community organization, Housing, Settlement", "Community organization",
                            "Community organization, Politics","Community organization, Population, Settlement"),]$category<-"Community organization"
    vars[vars$category%in%c("Community organization, Housing", "Community organization, Housing, Ecology"),]$category<-"Housing ecology"
    vars[vars$category%in%c("Kinship", "Marriage, Kinship","Marriage, Kinship, Community organization",
                            "Marriage, Kinship, Wealth Transactions, Economy"),]$category<-"Marriage, Kinship"
    vars[vars$category%in%c("Subsistence, Economy","Subsistence, Ecology"),]$category<-"Subsistence"
    
    vars[vars$category%in%c("Property, Wealth Transactions, Economy"),]$category<-"Property"
    vars[vars$category%in%c("Labour, Gender, Economy"),]$category<-"Labour"
    vars[vars$category%in%c("Ritual", "Ritual, Gender", "Ritual, Gender, Marriage"),]$category<-"Ritual"
    vars[vars$category%in%c("Politics, Leadership","Politics, Class, Economy","Politics, Leadership, Kinship",
                            "Politics, Leadership, Economy"),]$category<-"Politics, Class"
    # games? is this included EA035 not in dfm1 or dfm2
          
    workingdf$TraitCategory<-999
    for(i in 1:length(workingdf[,1]))
      {workingdf$TraitCategory[i]<-vars[vars$id%in%workingdf$Trait[i],]$category}
  
    return(workingdf)
}

############### ----------------------------------------------------------------------------------------------------------------------------------

# shared fam id


calcSElines_sharefam<-function(dtrial, name_folder, coldown="royalblue1", colup="indianred1")
{ dtrial$SEGeoD_LCPsl_MCOSTe<--999 ; dtrial$SEXericD_MCOSTx<- -999 ; dtrial$SETempD_MCOSTt<--999 ; 
  dtrial$SEShareFam<- -999  ; dtrial$SENbhood<- -999

for(i in 1: length(dtrial[,1]))
{ tvar<-dtrial$Trait[i]
load(paste0("data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_FamId/models/",name_folder,"/model_",tvar,"_","100",".rds"))
summary(model)->sumM
sumM$coefficients-> sumMc

# which RC is what?
load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmFamIdRPCA/",name_folder,"/","RCtraits_","_", tvar,".rds")) # RCtraits
RCs<-character()

RCs[1]<-RCtraits[rownames(RCtraits)%in%"nbhood",]$whichRC[1]
RCs[2]<-RCtraits[rownames(RCtraits)%in%"X.quali",]$whichRC[1]
RCs[3]<-RCtraits[rownames(RCtraits)%in%"sqrtgeod",]$whichRC[1]
RCs[4]<-RCtraits[rownames(RCtraits)%in%"sqrtEDtemp",]$whichRC[1]
RCs[5]<-RCtraits[rownames(RCtraits)%in%"sqrtEDxeric",]$whichRC[1]
names(RCs)<-c("Nbhood", "ShareFam", "GeoD_LCPsl_MCOSTe", "TempD_MCOSTt", "XericD_MCOSTx")
RCs[RCs%in%"dim1.rot"]<-"RC1" ; RCs[RCs%in%"dim2.rot"]<-"RC2"
RCs[RCs%in%"dim3.rot"]<-"RC3" ; RCs[RCs%in%"dim4.rot"]<-"RC4"
RCs[RCs%in%"dim5.rot"]<-"RC5"

dtrial$SEGeoD_LCPsl_MCOSTe[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"GeoD_LCPsl_MCOSTe"],][2]
dtrial$SEXericD_MCOSTx[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"XericD_MCOSTx"],][2]
dtrial$SETempD_MCOSTt[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"TempD_MCOSTt"],][2]
dtrial$SEShareFam[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"ShareFam"],][2]
dtrial$SENbhood[i]<-sumM$coefficients[rownames(sumM$coefficients)%in% RCs[names(RCs)%in%"Nbhood"],][2]

}
return(dtrial) 
}


df_moreadds_sharefam<-function(workingdf, name_folder,method_adj,vars, coldown="royalblue1", colup="indianred1") 
{# adjust pvalues
  workingdf$GeoD_LCPsl_MCOSTe_pvalue<-p.adjust (workingdf$GeoD_LCPsl_MCOSTe_pval, method = method_adj)
  workingdf$XericD_MCOSTx_pvalue<-p.adjust (workingdf$XericD_MCOSTx_pval, method = method_adj)
  workingdf$TempD_MCOSTt_pvalue<-p.adjust (workingdf$TempD_MCOSTt_pval, method = method_adj)
  workingdf$ShareFam_pvalue<-p.adjust (workingdf$ShareFam_pval, method = method_adj)
  workingdf$Nbhood_pvalue<-p.adjust (workingdf$Nbhood_pval, method = method_adj)
  
  # sign col
  workingdf$signGeoD_LCPsl_MCOSTe<- "grey72" ; workingdf$signXericD_MCOSTx<- "grey72" ;  workingdf$signTempD_MCOSTt<- "grey72" #ns
  workingdf$signNbhood<- "grey72" ; workingdf$signShareFam<- "grey72" ; 
  
  if(sum(workingdf$GeoD_LCPsl_MCOSTe_pvalue<0.05 & workingdf$GeoD_LCPsl_MCOSTe<0)>0) workingdf[workingdf$GeoD_LCPsl_MCOSTe_pvalue<0.05 & workingdf$GeoD_LCPsl_MCOSTe<0,]$signGeoD_LCPsl_MCOSTe<-coldown
  if(sum(workingdf$GeoD_LCPsl_MCOSTe_pvalue<0.05 & workingdf$GeoD_LCPsl_MCOSTe>0)>0) workingdf[workingdf$GeoD_LCPsl_MCOSTe_pvalue<0.05 & workingdf$GeoD_LCPsl_MCOSTe>0,]$signGeoD_LCPsl_MCOSTe<-colup
  
  if(sum(workingdf$XericD_MCOSTx_pvalue<0.05 & workingdf$XericD_MCOSTx<0)>0) workingdf[workingdf$XericD_MCOSTx_pvalue<0.05 & workingdf$XericD_MCOSTx<0,]$signXericD_MCOSTx<-coldown
  if(sum(workingdf$XericD_MCOSTx_pvalue<0.05 & workingdf$XericD_MCOSTx>0)>0) workingdf[workingdf$XericD_MCOSTx_pvalue<0.05 & workingdf$XericD_MCOSTx>0,]$signXericD_MCOSTx<-colup
  
  if(sum(workingdf$TempD_MCOSTt_pvalue<0.05 & workingdf$TempD_MCOSTt<0)>0) workingdf[workingdf$TempD_MCOSTt_pvalue<0.05 & workingdf$TempD_MCOSTt<0,]$signTempD_MCOSTt<-coldown
  if(sum(workingdf$TempD_MCOSTt_pvalue<0.05 & workingdf$TempD_MCOSTt>0)>0) workingdf[workingdf$TempD_MCOSTt_pvalue<0.05 & workingdf$TempD_MCOSTt>0,]$signTempD_MCOSTt<-colup
  
  if(sum(workingdf$Nbhood_pvalue<0.05 & workingdf$Nbhood<0)>0) workingdf[workingdf$Nbhood_pvalue<0.05 & workingdf$Nbhood<0,]$signNbhood<-coldown
  if(sum(workingdf$Nbhood_pvalue<0.05 & workingdf$Nbhood>0)>0) workingdf[workingdf$Nbhood_pvalue<0.05 & workingdf$Nbhood>0,]$signNbhood<-colup
  
  if(sum(workingdf$ShareFam_pvalue<0.05 & workingdf$ShareFam<0)>0) workingdf[workingdf$ShareFam_pvalue<0.05 & workingdf$ShareFam<0,]$signShareFam<-coldown
  if(sum(workingdf$ShareFam_pvalue<0.05 & workingdf$ShareFam>0)>0) workingdf[workingdf$ShareFam_pvalue<0.05 & workingdf$ShareFam>0,]$signShareFam<-colup
  
  # add trait category
  vars[vars$category%in%c("Community organization, Housing, Settlement", "Community organization",
                          "Community organization, Politics","Community organization, Population, Settlement"),]$category<-"Community organization"
  if (any(c("Community organization, Housing", "Community organization, Housing, Ecology")%in% vars$category)) vars[vars$category%in%c("Community organization, Housing", "Community organization, Housing, Ecology"),]$category<-"Housing ecology"
  vars[vars$category%in%c("Kinship", "Marriage, Kinship","Marriage, Kinship, Community organization",
                          "Marriage, Kinship, Wealth Transactions, Economy"),]$category<-"Marriage, Kinship"
  if (any(c("Subsistence, Economy","Subsistence, Ecology")%in% vars$category)) vars[vars$category%in%c("Subsistence, Economy","Subsistence, Ecology"),]$category<-"Subsistence"
  if (any(c("Property, Wealth Transactions, Economy")%in% vars$category)) vars[vars$category%in%c("Property, Wealth Transactions, Economy"),]$category<-"Property"
  if (any(c("Labour, Gender, Economy")%in% vars$category)) vars[vars$category%in%c("Labour, Gender, Economy"),]$category<-"Labour"
  vars[vars$category%in%c("Ritual", "Ritual, Gender", "Ritual, Gender, Marriage"),]$category<-"Ritual"
  if (any(c("Politics, Leadership","Politics, Class, Economy","Politics, Leadership, Kinship", "Politics, Leadership, Economy")%in% vars$category))
      vars[vars$category%in%c("Politics, Leadership","Politics, Class, Economy","Politics, Leadership, Kinship",
                              "Politics, Leadership, Economy"),]$category<-"Politics, Class"
  # games? is this included EA035 not in dfm1 or dfm2
  
  workingdf$TraitCategory<-999
  for(i in 1:length(workingdf[,1]))
  {workingdf$TraitCategory[i]<-vars[vars$id%in%workingdf$Trait[i],]$category}
  
  return(workingdf)
}


