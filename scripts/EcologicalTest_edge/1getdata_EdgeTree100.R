
rm(list=ls())
library(ape)
library(tidyverse)

######################
###### DATA ##########
######################

read.csv("data/dplace-societies-2018-06-21.csv", stringsAsFactors = F)->dplace
read.csv("data/latlong_soc_from_dplace-societies-2018-03-16.csv", stringsAsFactors = F)->longlat
read.nexus("data/EdgeTree/mcc_cah_full.nex") -> mcctree
dftips<-read.csv(file="data/EdgeTree/edgetree_labels.csv") 

# longlat$edgelabel<-""
dplace$island_check<-0
dplace<-dplace[match(longlat$Society.id, dplace$Society.id),]
dplace$island_check<-longlat$Island_check

# match by gcode
sum(dplace$Glottolog.language.dialect.id%in%mcctree$tip.label)
dplace$edgelabel<-""
for(i in 1:length(dplace[,1])) 
  if(dplace$Glottolog.language.dialect.id[i]%in%mcctree$tip.label) dplace$edgelabel[i]<-dplace$Glottolog.language.dialect.id[i]

# successfull manual match: https://d-place.org/societysets#1/30/153
sum(dplace$edgelabel%in%"")
dplace[dplace$Preferred.society.name%in%"Chopi",]$edgelabel<-"chop1243"
dplace[dplace$Preferred.society.name%in%"Zigula",]$edgelabel<-"zigu1244"
dplace[dplace$Preferred.society.name%in%"Moroccans",]$edgelabel<-"moro1292" # parent
dplace[dplace$Preferred.society.name%in%"Sahel",]$edgelabel<-"tuni1259" # parent
dplace[dplace$Preferred.society.name%in%"Syrians",]$edgelabel<-"jude1266" # sister
dplace[dplace$Preferred.society.name%in%"Lebanese",]$edgelabel<-"cypr1248" # sister
dplace[dplace$Preferred.society.name%in%"Druze",]$edgelabel<-"nort3139" # sister
dplace[dplace$Preferred.society.name%in%"Yir Yoront",]$edgelabel<-"yiry1245"
dplace[dplace$Preferred.society.name%in%"Lummi",]$edgelabel<-"stra1244"
dplace[dplace$Preferred.society.name%in%"Klallam",]$edgelabel<-"stra1244" # some will be duplicated - will have to pick one at random
dplace[dplace$Preferred.society.name%in%"Twana",]$edgelabel<-"halk1245"
dplace[dplace$Preferred.society.name%in%"Heiltsuk",]$edgelabel<-"heil1246"
dplace[dplace$Preferred.society.name%in%"Makah",]$edgelabel<-"diti1235"
dplace[dplace$Preferred.society.name%in%"Quinault",]$edgelabel<-"bell1243"
dplace[dplace$Preferred.society.name%in%"Wappo",]$edgelabel<-"yuku1243"
dplace[dplace$Preferred.society.name%in%"Tongva",]$edgelabel<-"tuba1278"
dplace[dplace$Preferred.society.name%in%"Nama",]$edgelabel<-"nama1264"
dplace[dplace$Preferred.society.name%in%"/Xam",]$edgelabel<-"nuuu1241"
dplace[dplace$Preferred.society.name%in%"Ngala",]$edgelabel<-"bang1353"
dplace[dplace$Preferred.society.name%in%"Sherbro",]$edgelabel<-"bull1247"
dplace[dplace$Preferred.society.name%in%"Awuna",]$edgelabel<-"ewee1241"
dplace[dplace$Preferred.society.name%in%"Kuku",]$edgelabel<-"bari1284"
dplace[dplace$Preferred.society.name%in%"Jie",]$edgelabel<-"nyan1315"
dplace[dplace$Preferred.society.name%in%"Karamojong",]$edgelabel<-"nyan1315"
dplace[dplace$Preferred.society.name%in%"Livs",]$edgelabel<-"sout2679"
sum(dplace$edgelabel%in%"")

# ## parents of dialects
grfolder<-"/Users/angela_chira/Desktop/LingvDisparityShip/GIT/LinguisticDisparity/grambank-analysed/R_grambank/"
glottolog_fn <- paste0(grfolder,"output/non_GB_datasets/glottolog-cldf_wide_df.tsv") # assumes you've ran source("make_glottolog-cldf_table.R")
glottolog_df <- read_tsv(glottolog_fn,col_types = cols()) %>%
  dplyr::select(Language_ID, Language_level_ID, level, aes) %>%
  mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Language_ID, Language_level_ID)) #making language-level entities their own parent, so that we can use this column for aggregation easier.

glottolog_df<-as.data.frame(glottolog_df)
tom <- dplace[dplace$Glottolog.language.dialect.id%in%glottolog_df$Language_ID & dplace$edgelabel%in%"",]$Glottolog.language.dialect.id
for(i in 1:length(tom)){
  if(glottolog_df[glottolog_df$Language_ID%in%tom[i],]$Language_level_ID%in%mcctree$tip.label) dplace[dplace$Glottolog.language.dialect.id%in%tom[i],]$edgelabel<-glottolog_df[glottolog_df$Language_ID%in%tom[i],]$Language_level_ID
}
sum(dplace$edgelabel%in%"")
sum(!dplace$edgelabel%in%"" & !duplicated(dplace$edgelabel))

# dplace[dplace$island_check==1 & dplace$edgelabel%in%"",]$edgelabel<-"island" # for manual searches - no point matching, these points are not used in models

# unsuccessfull match manually: https://d-place.org/societysets#1/30/153
dplace[dplace$Preferred.society.name%in%"Ancient Romans",]$edgelabel<-"" 
dplace[dplace$Preferred.society.name%in%"Quileute",]$edgelabel<-""
dplace[dplace$Preferred.society.name%in%"Coos",]$edgelabel<-""
dplace[dplace$Preferred.society.name%in%"Alsea",]$edgelabel<-""
dplace[dplace$Preferred.society.name%in%"Siuslaw",]$edgelabel<-""
dplace[dplace$Preferred.society.name%in%"Tututni",]$edgelabel<-""
dplace[dplace$Preferred.society.name%in%"Yurok",]$edgelabel<-""
dplace[dplace$Preferred.society.name%in%"Eyak",]$edgelabel<-""
dplace[dplace$Preferred.society.name%in%"Tolowa",]$edgelabel<-""
dplace[dplace$Preferred.society.name%in%"Chumash",]$edgelabel<-""
dplace[dplace$Preferred.society.name%in%"Timucua",]$edgelabel<-""
dplace[dplace$Preferred.society.name%in%"Ancient Egyptians",]$edgelabel<-""
dplace[dplace$Preferred.society.name%in%"Boers",]$edgelabel<-""
dplace[dplace$Preferred.society.name%in%"Babylonians",]$edgelabel<-""

#   #keeping just one tip per language in the entire EDGE-tree
#   to_keep <- mcctree$tip.label %>% 
#     as.data.frame() %>% 
#     rename(tip.label = ".") %>% 
#     separate(col = tip.label , into = c("Language_ID", "Name_EDGE"), remove = F, sep = 8) %>% 
#     left_join(glottolog_df, by = "Language_ID") %>% 
#     group_by(Language_level_ID) %>% 
#     sample_n(1)
#   
#   #renaming tip labels to glottocodes
#   a <- mcctree$tip.label %>% 
#     as.data.frame() %>% 
#     rename(tip.label = ".") %>% 
#     separate(col = tip.label , into = c("Language_ID", "Name_EDGE"), remove = F, sep = 8) %>% 
#     left_join(glottolog_df, by = "Language_ID") %>% 
#     dplyr::select(Language_level_ID) %>% 
#     as.matrix() %>% 
#     as.vector()
#   lala<-data.frame("dialect"=a, "edget"=mcctree$tip.label)
# 
# sum(dplace[dplace$edgelabel%in%"",]$Glottolog.language.dialect.id%in%lala$dialect)  # 0..

sum(!dplace$edgelabel%in%"" & !duplicated(dplace$edgelabel))
list_tips<-dplace[!dplace$edgelabel%in%c(""),]
list_tips<-list_tips[,c("Preferred.society.name","Society.id","Cross.dataset.id","edgelabel")]
list_tips<-list_tips[!duplicated(list_tips$edgelabel),]
# sum(duplicated(list_tips$Preferred.society.name))
# sum(duplicated(list_tips$edgelabel))
chuck<-mcctree$tip.label[!mcctree$tip.label%in%list_tips$edgelabel]

library(ape)
library(phytools)
library(geiger)
library(phangorn)
tree<-read.nexus("/Users/angela_chira/Desktop/LingvDisparityShip/data/globaltrees_May/edge6636-March-2023-no-metadata.trees") # 1000 trees
# sampletree<-sample(x=c(1:1000), size=100)
# save(sampletree, file="data/EdgeTree100/sampletree.rds")
load(file="data/EdgeTree100/sampletree.rds")
tree<-tree[sampletree]
newtree<-list()
for(i in 1:length(tree)){
  newtree[[i]]<-drop.tip(tree[[i]], tip = chuck)
  tomatch<-list_tips[match(newtree[[i]]$tip.label, list_tips$edgelabel),]
  newtree[[i]]$tip.label<-list_tips$Society.id
}
# save(newtree, file="data/EdgeTree100/newtree.rds")

## get cophenetic_dist
library(ape)
library(geiger)
for(i in 1:length(newtree)){
  cophenetic_dist<-cophenetic.phylo(newtree[[i]])
  save(cophenetic_dist, file=paste0("data/EdgeTree100/newEDGEcoph100/newEDGEcopheneticD_",i,".rda"))
  print(i)
}

############ --- ########## --- ########## --- ##### --- ############ --- ##########

# consider doing this for 50 trees instead
sample50<-sample(x=1:100, size=50, replace=F)
for(i in 1:length(sample50)){
  load(paste0("data/EdgeTree100/newEDGEcoph100/newEDGEcopheneticD_",sample50[i],".rda")) 
  # save(cophenetic_dist, file=paste0("data/EdgeTree100/newEDGEcopheneticD_",i,".rda"))
  print(i)
}
# save(sample50, file="data/EdgeTree100/sample50.rda")

##########################
#### get data for model ##
##########################

rm(list=ls())
read.csv("data/latlong_soc_from_dplace-societies-2018-03-16.csv", stringsAsFactors = F)-> latlong_soc
read.csv("data/EA/EA/variables.csv", stringsAsFactors = F)-> vars
read.csv("data/EA/EA/codes_wallnewcodes.csv", stringsAsFactors = F)-> codes
read.csv("data/EA/EA/data_wallnewcodes.csv", stringsAsFactors = F)-> data
read.csv("data/EA/EA/societies.csv", stringsAsFactors = F)-> soc
read.csv("newcodesFINAL.csv", stringsAsFactors = F)->newcodes
unique(newcodes$Trait)->alltraits
traits_of_interest<-list() # which traits can we consider?
for(i in 1: length(alltraits))
  {ctrait<-alltraits[i]
   if(sum(!is.na(newcodes[newcodes$Trait%in%ctrait,]$New.code))>0) traits_of_interest<-c(traits_of_interest, ctrait)}
traits_of_interest<-unlist(traits_of_interest)
source("scripts/1getdata_fxn.R")

for(j in 1:50){

# i=1 # could have also read the common socs and subset EDGE pd values
for(i in 1: length(traits_of_interest))
  {trait_of_interest<-traits_of_interest[i]
  # common list of societies
  
  load(file=paste0("data/response/response3april/share_mat_",trait_of_interest,"_25IN",".rda")) # response
  rownames(share_ctraits_mat)->l1
  load(file=paste0("data/EdgeTree100/newEDGEcopheneticD_",j,".rda")) # EDGE
  rownames(cophenetic_dist) -> l2
  load(file="data/Re_analysisGEbarriers/P1EnvDis/edTemp_newEnvData.rds") # edTemp
  rownames(edTemp) -> l3
  load(file="data/Re_analysisGEbarriers/P1EnvDis/edXeric_newEnvData.rds") # edXeric
  load(file="data/predictors/preds3april/geodistances/mdist_matrix_distGeo.rda") # GD 
  rownames(mdist_matrix_distGeo)-> l4
  # which societies?
  soclist<-Reduce(intersect, list(l1,l2,l3,l4))
  
  # subset matrices to common list
  share_ctraits_mat<-share_ctraits_mat[soclist,soclist]
  cophenetic_dist<-cophenetic_dist[soclist,soclist]
  edTemp<-edTemp[soclist,soclist]
  edXeric<-edXeric[soclist,soclist]
  mdist_matrix_distGeo<-mdist_matrix_distGeo[soclist,soclist]
  # get vectors for model - given the common soclist - only pd_values, because the rest were subsetted always to the same societies, no need to re-generate
  pd_values<-extract_variable_model_vectors(cophenetic_dist, subset=T)
  # save(pd_values, file=paste0("data/EdgeTree100/dfmEDGE/","pd_values_pertrait_",trait_of_interest,"_",j,".rda"))
  print(paste(j,i)) #i
} # from i

}

# or do correlations here and that't that

### --------------------------------------------------------------------------------------------------------


#######################################################
#### build data for model for all traits of interest ## 
#######################################################


source("scripts/1getdata_fxn.R")
function_sort<-function(x)
{soc1<-strsplit(x, split="_")[[1]][1] ; soc2<-strsplit(x, split="_")[[1]][2]
return(paste(sort(c(soc1, soc2)), collapse = "_")) }

for(j in 1:50){


i=1
for(i in 1: length(traits_of_interest))
  {trait_of_interest<-traits_of_interest[i]

  # datafor model
  load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmEDGE/","socnames_pertrait_",trait_of_interest,".rda")) # socnames
  load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmEDGE/","ctraits_pertrait_",trait_of_interest,".rda")) # ctraits_values
  load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmEDGE/","edTemp_values_",trait_of_interest,".rda")) #
  load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmEDGE/","edXeric_values_",trait_of_interest,".rda")) #
  load(file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmEDGE/","gdist_valuesGeo_pertrait_",trait_of_interest,".rda")) # gdist_values

  load(file=paste0("data/EdgeTree100/dfmEDGE/","pd_values_pertrait_",trait_of_interest,"_",j,".rda")) # pd_values

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

  # write.csv(dfm, file=paste0("data/EdgeTree100/dfmEDGES1/dfmS1_", trait_of_interest,"_",j, ".csv"))
  print(paste(j,i))
}

}

############
#### rPCA ## 
############

name_folders<-c("nbhoodV1") # only first version
name_folder<-name_folders[1]
library(psych)

for(j in 1:50){
  
  listw<-list()
  
  for(i in 1: length(traits_of_interest))
    {trait_of_interest<-traits_of_interest[i]
    read.csv( paste0("data/EdgeTree100/dfmEDGES1/dfmS1_", trait_of_interest,"_",j, ".csv"), stringsAsFactors = F)->dfm # 
    
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
    
    # (R)PCA analysis
    
    per1 = sum( dfm$share%in%1) / length(dfm$share)  # don t consider models with data points and imbalance of >10%/90% and the ones with nbhood = 1 value
    if(length(dfm$share)>=50 & per1>=0.1 & per1<=0.9 & length(unique(dfm$nbhood))>1)
        { dataPCA<-dfm[,colnames(dfm)%in%c("logphyd", "sqrtgeod","nbhood","sqrtEDtemp", "sqrtEDxeric",
                                           "logMCOSTe", "logMCOSTt", "logMCOSTx", "logLCPle", "logLCPlt", "logLCPlx"),]
        myPCA<-principal(dataPCA, nfactors=5, rotate="varimax") # this function scales variables before doing PCA
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

        RCtraits$sign<-sign(RCtraits$loadingRC)
        dfm$RC1<-dfm$RC1 *unique(RCtraits[RCtraits$whichRC%in%"RC1",]$sign) # multiply scores in df_model with sign
        dfm$RC2<-dfm$RC2 *unique(RCtraits[RCtraits$whichRC%in%"RC2",]$sign)
        if("RC3"%in% RCtraits$whichRC) {dfm$RC3<-dfm$RC3 *unique(RCtraits[RCtraits$whichRC%in%"RC3",]$sign)
        } else {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC3"}
        if("RC4"%in% RCtraits$whichRC) {dfm$RC4<-dfm$RC4 *unique(RCtraits[RCtraits$whichRC%in%"RC4",]$sign)
        } else {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC4"}
        if("RC5"%in% RCtraits$whichRC) {dfm$RC5<-dfm$RC5 *unique(RCtraits[RCtraits$whichRC%in%"RC5",]$sign)
        } else {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"noRC5"}

        save(myPCA, file=paste0("data/EdgeTree100/dfmEDGERPCA/",name_folder,"/","pcaik_","_", trait_of_interest,"_",j,".rds"))
        write.csv(dfm, file =paste0("data/EdgeTree100/dfmEDGERPCA/",name_folder,"/","dfmS2_","_", trait_of_interest,"_",j,".csv") )
        save(RCtraits, file=paste0("data/EdgeTree100/dfmEDGERPCA/",name_folder,"/","RCtraits_","_", trait_of_interest,"_",j,".rds"))

        if(length(unique(RCtraits[rownames(RCtraits)%in%c("sqrtgeod","logMCOSTe","logLCPle","logLCPlt","logLCPlx"),]$whichRC))>1 |
           length(unique(RCtraits[rownames(RCtraits)%in%c("logphyd"),]$whichRC))>1 |
           length(unique(RCtraits[rownames(RCtraits)%in%c("nbhood"),]$whichRC))>1 |
           length(unique(RCtraits[rownames(RCtraits)%in%c("sqrtEDtemp","logMCOSTt"),]$whichRC))>1 |
           length(unique(RCtraits[rownames(RCtraits)%in%c("sqrtEDxeric","logMCOSTx"),]$whichRC))>1)
        {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"not_sameERCs"}
        
        } # if(length(dfm$share>=50) & per1>=0.1 & per1<=0.9 & length(unique(dfm$nbhood))>1)
    if(!(length(dfm$share>=50) & per1>=0.1 & per1<=0.9 & length(unique(dfm$nbhood))>1)) {listw<-c(listw,paste(trait_of_interest, name_folder)) ; names(listw)[length(listw)]<-"conditions"}
    
    print(paste(j,i))
    } # from i
  save(listw, file=paste0("data/EdgeTree100/dfmEDGERPCA/","listw_",j,".rds"))
}


###################
#### run GLMER ####
###################

rm(list=ls())
read.csv("data/latlong_soc_from_dplace-societies-2018-03-16.csv", stringsAsFactors = F)-> latlong_soc
read.csv("data/EA/EA/variables.csv", stringsAsFactors = F)-> vars
read.csv("data/EA/EA/codes_wallnewcodes.csv", stringsAsFactors = F)-> codes
read.csv("data/EA/EA/data_wallnewcodes.csv", stringsAsFactors = F)-> data
read.csv("data/EA/EA/societies.csv", stringsAsFactors = F)-> soc
read.csv("newcodesFINAL.csv", stringsAsFactors = F)->newcodes
unique(newcodes$Trait)->alltraits
traits_of_interest<-list() # which traits can we consider?
for(i in 1: length(alltraits))
{ctrait<-alltraits[i]
if(sum(!is.na(newcodes[newcodes$Trait%in%ctrait,]$New.code))>0) traits_of_interest<-c(traits_of_interest, ctrait)}
traits_of_interest<-unlist(traits_of_interest)
source("scripts/1getdata_fxn.R")

library(car)
library(pROC)
library(lme4)

name_folders<-c("nbhoodV1") # only first version
name_folder<-name_folders[1]

for(i in 1:50){
  
  load(file=paste0("data/EdgeTree100/dfmEDGERPCA/","listw_",i,".rds"))
  round_dec<-3
  diffRCorder<-list()

  for(j in 1: length(traits_of_interest))
    { trait_of_interest<-traits_of_interest[j]
    if(!paste(trait_of_interest, name_folder)%in%listw & paste0("dfmS2_", "_",trait_of_interest,"_",i,".csv") %in% list.files(paste0("data/EdgeTree100/dfmEDGERPCA/",name_folder)))  
      # for now the ones that 100% work
    { read.csv(file =paste0("data/EdgeTree100/dfmEDGERPCA/",name_folder,"/","dfmS2_", "_",trait_of_interest,"_",i,".csv"), stringsAsFactors = F)->dfm
      
      load(paste0("data/coordinates/dfnb_EA_",100,".rda")) # dfiEA
      dfm[dfm$pairsoc_S%in%dfiEA$pair_soc,]->dfmSC # this condition is already done, here because before we had different spatial scales
      
      model <- glmer(share ~ RC1 + RC2 + RC3 + RC4 + RC5 + 
                       (1 | soc1) + (1|soc2), data = dfmSC, family = binomial, 
                     control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
      
      save(model, file=paste0("data/EdgeTree100/glmerRCA_EDGE/models/",name_folder,"/model_",trait_of_interest,"_",100,"_",i,".rds"))
      
      vifm<-vif(model) # futile now since RPCAs
      summary(model)->sumM
      sumM$coefficients-> sumMc
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
      save(df_out, file=paste0("data/EdgeTree100/glmerRCA_EDGE/dfoutput/",name_folder,"/model_",trait_of_interest,"_",i,".rds"))
      
      # replace the names of RCs with real predictor name
      load(file=paste0("data/EdgeTree100/dfmEDGERPCA/",name_folder,"/","RCtraits_","_", trait_of_interest,"_",i,".rds")) # RCtraits
      RCs<-character()
      
      RCs[1]<-RCtraits[rownames(RCtraits)%in%"nbhood",]$whichRC[1]
      RCs[2]<-RCtraits[rownames(RCtraits)%in%"logphyd",]$whichRC[1]
      RCs[3]<-RCtraits[rownames(RCtraits)%in%"sqrtgeod",]$whichRC[1]
      RCs[4]<-RCtraits[rownames(RCtraits)%in%"sqrtEDtemp",]$whichRC[1]
      RCs[5]<-RCtraits[rownames(RCtraits)%in%"sqrtEDxeric",]$whichRC[1]
      names(RCs)<-c("Nbhood", "PhyD", "GeoD_LCPsl_MCOSTe", "TempD_MCOSTt", "XericD_MCOSTx")
      
      colnames(df_out)[colnames(df_out)%in%"RC1"]<-names(RCs)[RCs%in%"RC1"] ; colnames(df_out)[colnames(df_out)%in%"RC1_pval"]<-paste0(names(RCs)[RCs%in%"RC1"], "_pval")
      colnames(df_out)[colnames(df_out)%in%"RC2"]<-names(RCs)[RCs%in%"RC2"] ; colnames(df_out)[colnames(df_out)%in%"RC2_pval"]<-paste0(names(RCs)[RCs%in%"RC2"], "_pval")
      colnames(df_out)[colnames(df_out)%in%"RC3"]<-names(RCs)[RCs%in%"RC3"] ; colnames(df_out)[colnames(df_out)%in%"RC3_pval"]<-paste0(names(RCs)[RCs%in%"RC3"], "_pval")
      colnames(df_out)[colnames(df_out)%in%"RC4"]<-names(RCs)[RCs%in%"RC4"] ; colnames(df_out)[colnames(df_out)%in%"RC4_pval"]<-paste0(names(RCs)[RCs%in%"RC4"], "_pval")
      colnames(df_out)[colnames(df_out)%in%"RC5"]<-names(RCs)[RCs%in%"RC5"] ; colnames(df_out)[colnames(df_out)%in%"RC5_pval"]<-paste0(names(RCs)[RCs%in%"RC5"], "_pval")
      if(!(names(RCs)[RCs%in%"RC1"] %in% "GeoD_LCPsl_MCOSTe" & names(RCs)[RCs%in%"RC2"] %in%"XericD_MCOSTx" & names(RCs)[RCs%in%"RC3"] %in% "PhyD" & 
           names(RCs)[RCs%in%"RC4"]%in%"Nbhood" &  names(RCs)[RCs%in%"RC5"]%in%"TempD_MCOSTt")) diffRCorder<-c(diffRCorder, paste0(trait_of_interest, "_", name_folder))
      
      # save(df_out, file=paste0("data/EdgeTree100/glmerRCA_EDGE/dfoutput/",name_folder,"/dfoutNamesPred_",trait_of_interest,"_",i,".rds"))
      print(paste(i,j)) 
      
    } # from if(!paste(trait_of_interest, name_folder)%in%listw)

    } # from j = traits_of_interest

}

# ###########################
# #### big df ############### 
# ###########################

### put together results 
listm<-list.files("data/EdgeTree100/dfmEDGERPCA/")
listm<-listm[!listm%in%"nbhoodV1"]
fsub<-function(x) {return(gsub(" nbhoodV1", "",x))}
  
rm(dfm1)
rm(dfall)
for(j in 1:50){
  load(paste0("data/EdgeTree100/dfmEDGERPCA/listw_",j,".rds"))
  listw<-unlist(listw)
  tr<-unlist(lapply(listw, FUN=fsub))
  rm(dfm1)
  
  for(i in 1:length(traits_of_interest)){
    trait_of_interest<-traits_of_interest[i]
    if( paste0("dfoutNamesPred_",trait_of_interest,"_",j,".rds") %in% list.files(paste0("data/EdgeTree100/glmerRCA_EDGE/dfoutput/",name_folder,"/")))
      {load(file=paste0("data/EdgeTree100/glmerRCA_EDGE/dfoutput/",name_folder,"/dfoutNamesPred_",trait_of_interest,"_",j,".rds"))
       if(i==1) dfm1<-df_out
       if(i>1) dfm1<-rbind(dfm1, df_out)
      } # if
    } # i
  dfm1$GeoD_LCPsl_MCOSTe_pval<-p.adjust (dfm1$GeoD_LCPsl_MCOSTe_pval, method = "fdr") # p adjust
  dfm1$PhyD_pval<-p.adjust (dfm1$PhyD_pval, method = "fdr")
  dfm1$Nbhood_pval<-p.adjust (dfm1$Nbhood_pval, method = "fdr")
  dfm1$TempD_MCOSTt_pval<-p.adjust (dfm1$TempD_MCOSTt_pval, method = "fdr")
  dfm1$XericD_MCOSTx_pval<-p.adjust (dfm1$XericD_MCOSTx_pval, method = "fdr")
  dfm1<-dfm1[!dfm1$Trait%in%tr,] # double check
  dfm1$j<-j
  
  if(j==1) dfall<-dfm1
  if(j>1)  dfall<-rbind(dfall, dfm1)
  print(j)
} # j

### summarise: for each trait, if predictor is ok: % significant negative and % negative (pos for ndbood) 

dfsens<-data.frame("trait"=traits_of_interest, "GeoPeffect"=-1, "GeoPeffect_sign"=-1, "TempPeffect"=-1, "TempPeffect_sign"=-1,
                   "XerPeffect"=-1, "XerPeffect_sign"=-1, "PdPeffect"=-1, "PdPeffect_sign"=-1, "NbPeffect"=-1, "NbPeffect_sign"=-1)
for(i in 1:length(dfsens[,1]))
  if(dfsens$trait[i]%in%dfall$Trait)
  {
  a<-dfall[dfall$Trait%in%dfsens$trait[i],]
  n<-dim(a)[1]
  dfsens$GeoPeffect[i]<-sum(a$GeoD_LCPsl_MCOSTe<=0)/n
  dfsens$GeoPeffect_sign[i]<-sum(a$GeoD_LCPsl_MCOSTe<=0 & a$GeoD_LCPsl_MCOSTe_pval<0.05)/ sum(a$GeoD_LCPsl_MCOSTe<=0)
  dfsens$TempPeffect[i]<-sum(a$TempD_MCOSTt<=0)/n
  dfsens$TempPeffect_sign[i]<-sum(a$TempD_MCOSTt<=0 & a$TempD_MCOSTt_pval<0.05)/ sum(a$TempD_MCOSTt<=0)
  dfsens$XerPeffect[i]<-sum(a$XericD_MCOSTx<=0)/n
  dfsens$XerPeffect_sign[i]<-sum(a$XericD_MCOSTx<=0 & a$XericD_MCOSTx_pval<0.05)/ sum(a$XericD_MCOSTx<=0)
  
  dfsens$PdPeffect[i]<-sum(a$PhyD<=0)/n
  dfsens$PdPeffect_sign[i]<-sum(a$PhyD<=0 & a$PhyD_pval<0.05)/ sum(a$PhyD<=0)
  dfsens$NbPeffect[i]<-sum(a$Nbhood>=0)/n # pos for nbhood
  dfsens$NbPeffect_sign[i]<-sum(a$Nbhood>=0 & a$Nbhood_pval<0.05)/ sum(a$Nbhood>=0)
}
dfsens<-dfsens[!dfsens$GeoPeffect%in%-1,]

# if none are negative, then % of negative that are also significant is 0 (first NaN because division by 0)
dfsens[dfsens$GeoPeffect_sign%in%NaN,]$GeoPeffect_sign<-0
dfsens[dfsens$TempPeffect_sign%in%NaN,]$TempPeffect_sign<-0
dfsens[dfsens$XerPeffect_sign%in%NaN,]$XerPeffect_sign<-0
dfsens[dfsens$PdPeffect_sign%in%NaN,]$PdPeffect_sign<-0
dfsens[dfsens$NbPeffect_sign%in%NaN,]$NbPeffect_sign<-0

dfsens$name<-""
for(i in 1:length(dfsens[,1])) dfsens$name[i]<-vars[vars$id%in%dfsens$trait[i],]$title

## results main text
read.csv("newcodesFINAL.csv", stringsAsFactors = F)->newcodes
unique(newcodes$Trait)->alltraits
traits_of_interest<-list() # which traits can we consider?
for(i in 1: length(alltraits))
{ctrait<-alltraits[i]
if(sum(!is.na(newcodes[newcodes$Trait%in%ctrait,]$New.code))>0) traits_of_interest<-c(traits_of_interest, ctrait)}
traits_of_interest<-unlist(traits_of_interest)
source("scripts/10_rLCP_plotfiguresFXN.R")
coldown="dodgerblue2"
colup="firebrick2"
read.csv(file="data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_EDGE/dfm1.csv", stringsAsFactors = F)->dfm1
dtrial1<-calcSElines_EDGE(dfm1, name_folder="nbhoodV1",coldown = coldown, colup=colup) # here calc error bars
dtrial1<-df_moreadds(dtrial1, name_folder="nbhoodV1",method_adj="fdr",vars=vars,coldown = coldown, colup=colup) 
workingdf<-dtrial1 ; name_folder<-"nbhoodV1"
if( "Games" %in%workingdf$TraitCategory) workingdf[workingdf$TraitCategory%in%"Games",]$TraitCategory<-"Ritual"
sum(!workingdf$Trait%in%dfsens$trait)
dfsens<-dfsens[match(workingdf$Trait, dfsens$trait),]

# compare
dfsens$compareGeo<- -1
dfsens$compareTemp<- -1
dfsens$compareXeric<- -1
dfsens$comparePd<- -1
dfsens$compareNbhood<- -1

for(i in 1:length(dfsens[,1])){
  
 if(workingdf[workingdf$Trait%in%dfsens$trait[i],]$signGeoD_LCPsl_MCOSTe %in% "dodgerblue2")
   if(dfsens$GeoPeffect[i]>0.5 & dfsens$GeoPeffect_sign[i]>0.5) dfsens$compareGeo<-1
 if(workingdf[workingdf$Trait%in%dfsens$trait[i],]$signGeoD_LCPsl_MCOSTe %in% "grey72")
   if(dfsens$GeoPeffect_sign[i]<0.5) dfsens$compareGeo<-1
 if(workingdf[workingdf$Trait%in%dfsens$trait[i],]$signGeoD_LCPsl_MCOSTe %in% "firebrick2")
   if(dfsens$GeoPeffect[i]<0.5) dfsens$compareGeo<-1
 
 if(workingdf[workingdf$Trait%in%dfsens$trait[i],]$signTempD_MCOSTt %in% "dodgerblue2")
   if(dfsens$TempPeffect[i]>0.5 & dfsens$TempPeffect_sign[i]>0.5) dfsens$compareTemp<-1
 if(workingdf[workingdf$Trait%in%dfsens$trait[i],]$signTempD_MCOSTt %in% "grey72")
   if(dfsens$TempPeffect_sign[i]<0.5) dfsens$compareTemp<-1
 if(workingdf[workingdf$Trait%in%dfsens$trait[i],]$signTempD_MCOSTt %in% "firebrick2")
   if(dfsens$TempPeffect[i]<0.5) dfsens$compareTemp<-1
 
 if(workingdf[workingdf$Trait%in%dfsens$trait[i],]$signXericD_MCOSTx %in% "dodgerblue2")
   if(dfsens$XerPeffect[i]>0.5 & dfsens$XerPeffect_sign[i]>0.5) dfsens$compareXeric<-1
 if(workingdf[workingdf$Trait%in%dfsens$trait[i],]$signXericD_MCOSTx %in% "grey72")
   if(dfsens$XerPeffect_sign[i]<0.5) dfsens$compareXeric<-1
 if(workingdf[workingdf$Trait%in%dfsens$trait[i],]$signXericD_MCOSTx %in% "firebrick2")
   if(dfsens$XerPeffect[i]<0.5) dfsens$compareXeric<-1
 
}

dfsens[dfsens$TempPeffect_sign==0,]$trait
workingdf[workingdf$Trait%in%dfsens[dfsens$TempPeffect_sign==0,]$trait,]$signTempD_MCOSTt #only one is dodgerblue2
  # EA076 appears non-sign in sensitivity with 0.06 pvalues and negative trait values
  a<-dfall[dfall$Trait%in%"EA076",]

dfsens[dfsens$XerPeffect_sign==0,]$trait
workingdf[workingdf$Trait%in%dfsens[dfsens$XerPeffect_sign==0,]$trait,]$signXericD_MCOSTx # all grey

dfsens[dfsens$GeoPeffect_sign==0,]$trait
workingdf[workingdf$Trait%in%dfsens[dfsens$GeoPeffect_sign==0,]$trait,]$signGeoD_LCPsl_MCOSTe # only one is firebrick
  # EA077 which is also firebrick in sensitivity - correct
  a<-dfall[dfall$Trait%in%"EA077",]

dfsens[dfsens$trait%in% workingdf[workingdf$signGeoD_LCPsl_MCOSTe%in%"grey72",]$Trait  ,]
  # EA028 is significant in sensitivity at 0.05 actually
  a<-dfall[dfall$Trait%in%"EA028",]

dfsens[dfsens$trait%in% workingdf[workingdf$signTempD_MCOSTt%in%"grey72",]$Trait  ,]
  a<-dfall[dfall$Trait%in%"EA027",] # negative sign at 0.03

  dfsens[dfsens$trait%in% workingdf[workingdf$signXericD_MCOSTx%in%"grey72",]$Trait  ,]
  # matches

  write.csv(dfsens, file="data/EdgeTree100/dfsens_final.csv")
