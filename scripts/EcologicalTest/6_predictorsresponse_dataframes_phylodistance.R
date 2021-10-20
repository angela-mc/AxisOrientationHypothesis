rm(list=ls())

read.csv("data/latlong_soc_from_dplace-societies-2018-03-16.csv", stringsAsFactors = F)-> latlong_soc
read.csv("data/EA/EA/variables.csv", stringsAsFactors = F)-> vars
read.csv("data/EA/EA/codes_wallnewcodes.csv", stringsAsFactors = F)-> codes
read.csv("data/EA/EA/data_wallnewcodes.csv", stringsAsFactors = F)-> data
read.csv("data/EA/EA/societies.csv", stringsAsFactors = F)-> soc

library(pscl)
library(rcompanion)
library(DescTools)


read.csv("data/newcodesFINAL.csv", stringsAsFactors = F)->newcodes
unique(newcodes$Trait)->alltraits
traits_of_interest<-list() # which traits can we consider?

for(i in 1: length(alltraits))
  {ctrait<-alltraits[i]
  if(sum(!is.na(newcodes[newcodes$Trait%in%ctrait,]$New.code))>0) traits_of_interest<-c(traits_of_interest, ctrait)}
traits_of_interest<-unlist(traits_of_interest)

source("scripts/1getdata_fxn.R")
function_sort<-function(x) 
  {soc1<-strsplit(x, split="_")[[1]][1] ; soc2<-strsplit(x, split="_")[[1]][2]
  return(paste(sort(c(soc1, soc2)), collapse = "_")) }



#######################################################
#### build data for model for all traits of interest ##
#######################################################

i=1
for(i in 1: length(traits_of_interest))
  {trait_of_interest<-traits_of_interest[i]
  
  # datafor model
  load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJager/","socnames_pertrait_",trait_of_interest,".rda")) # socnames
  load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJager/","ctraits_pertrait_",trait_of_interest,".rda")) # ctraits_values
  load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJager/","pd_values_pertrait_",trait_of_interest,".rda")) # pd_values  
  load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJager/","edTemp_values_",trait_of_interest,".rda")) # 
  load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJager/","edXeric_values_",trait_of_interest,".rda")) # 
  load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJager/","gdist_valuesGeo_pertrait_",trait_of_interest,".rda")) # gdist_values
  
  # build sorted and unsorted pairsoc
  pairsoc_US<-paste(socnames[,1], socnames[,2], sep="_") 
  pairsoc_S<-unlist(lapply(pairsoc_US,function_sort)) # need them sorted ebcause AB=BA here
  
  # # which pairs?
  load(file=paste0("data/coordinates/dfnb_EA_", 100, ".rda")) # 100 closest nb pair_soc is SORTED as characters!
  to_keep1<-pairsoc_S%in%dfiEA$pair_soc
  
  # # + exclude islands
  landsoc<-latlong_soc[latlong_soc$Island_check==0,]$Society.id
  to_keep41<- socnames[,1]%in%landsoc
  to_keep42<- socnames[,2]%in%landsoc
  
  tokeepF<-to_keep1&to_keep41&to_keep42 # T in all
  dfm<-as.data.frame(cbind(pairsoc_US[tokeepF], pairsoc_S[tokeepF], socnames[tokeepF,1],socnames[tokeepF,2],ctraits_values[tokeepF],
                           pd_values[tokeepF],edTemp_values[tokeepF],edXeric_values[tokeepF],gdist_values[tokeepF]))
  colnames(dfm)<-c("pairsoc_US","pairsoc_S","soc1", "soc2","share","phyd","edTemp","edXeric", "geod") 
  
  # add GeoBarriers
  read.csv(file="data/Re_analysisGEbarriers/P1dfs/dfP1barriers.csv", stringsAsFactors = F)->dfP1barriers
  dfP1barriers<-dfP1barriers[dfP1barriers$pairsoc_sor%in%dfm$pairsoc_S,]
  dfm<-dfm[dfm$pairsoc_S%in%dfP1barriers$pairsoc_sor,]
  dfP1barriers<-dfP1barriers[match(dfm$pairsoc_S, dfP1barriers$pairsoc_sor),]
  colnames(dfP1barriers)
  dfm$MCOSTe<-dfP1barriers$MCOSTe ; dfm$LCPle<-dfP1barriers$LCPle
  dfm$MCOSTt<-dfP1barriers$MCOSTt ; dfm$LCPlt<-dfP1barriers$LCPlt
  dfm$MCOSTx<-dfP1barriers$MCOSTx ; dfm$LCPlx<-dfP1barriers$LCPlx
  
  # nb structure - these will have different NA depending on version of nb, add all and subset subset when doing RPCA or lm() model
  read.csv(paste0("data/predictors/preds3april/nbhoodCluster/dftotals/",trait_of_interest, ".csv"), stringsAsFactors = F)->nbdf 
  nbdf<-nbdf[!duplicated(nbdf$pairsoc),] # from buildinf dftotals, some overlap
  clist<-intersect(nbdf$pairsoc,dfm$pairsoc_S)
  nbdf<-nbdf[nbdf$pairsoc%in%clist,]
  
  dfm<-dfm[dfm$pairsoc_S%in%clist,]
  nbdf<-nbdf[match(dfm$pairsoc_S, nbdf$pairsoc),]
  dfm$nbhoodV1<-nbdf$nbhoodV1 ; dfm$nbhoodV25<-nbdf$nbhoodV25 ; dfm$nbhoodV210<-nbdf$nbhoodV210
  
  write.csv(dfm, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmJagerS1/dfmS1_", trait_of_interest, ".csv"))
  print(i)
}
