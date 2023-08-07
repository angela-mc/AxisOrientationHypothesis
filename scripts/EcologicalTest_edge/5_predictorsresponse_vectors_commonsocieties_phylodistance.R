
rm(list=ls())

read.csv("data/latlong_soc_from_dplace-societies-2018-03-16.csv", stringsAsFactors = F)-> latlong_soc

read.csv("data/EA/EA/variables.csv", stringsAsFactors = F)-> vars
read.csv("data/EA/EA/codes_wallnewcodes.csv", stringsAsFactors = F)-> codes
read.csv("data/EA/EA/data_wallnewcodes.csv", stringsAsFactors = F)-> data
read.csv("data/EA/EA/societies.csv", stringsAsFactors = F)-> soc


##########################
########## get data ######
##########################

read.csv("data/newcodesFINAL.csv", stringsAsFactors = F)->newcodes
unique(newcodes$Trait)->alltraits
traits_of_interest<-list() # which traits can we consider?

for(i in 1: length(alltraits))
  {ctrait<-alltraits[i]
  if(sum(!is.na(newcodes[newcodes$Trait%in%ctrait,]$New.code))>0) traits_of_interest<-c(traits_of_interest, ctrait)}
traits_of_interest<-unlist(traits_of_interest)

source("scripts/1getdata_fxn.R")


i=1
for(i in 1: length(traits_of_interest))
  {trait_of_interest<-traits_of_interest[i]
  # common list of societies
  
  load(file=paste0("data/response/response3april/share_mat_",trait_of_interest,"_25IN",".rda")) # response
  rownames(share_ctraits_mat)->l1
  
  load(file="data/predictors/preds3april/phylogeny/EDGEcopheneticD.rda") 
  rownames(cophenetic_dist) -> l2
  
  load(file="data/Re_analysisGEbarriers/P1EnvDis/edTemp_newEnvData.rds") # edTemp
  rownames(edTemp) -> l3 # identic with edXeric
  
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
  
  # get vectors for model - given the common soclist
  socnames<-extract_socid_model_vectors(share_ctraits_mat, subset=T) ; save(socnames, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmEDGE/","socnames_pertrait_",trait_of_interest,".rda"))
  ctraits_values<-extract_variable_model_vectors(share_ctraits_mat, subset=T); save(ctraits_values, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmEDGE/","ctraits_pertrait_",trait_of_interest,".rda"))
  
  pd_values<-extract_variable_model_vectors(cophenetic_dist, subset=T); save(pd_values, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmEDGE/","pd_values_pertrait_",trait_of_interest,".rda"))
  edTemp_values<-extract_variable_model_vectors(edTemp, subset=T); save(edTemp_values, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmEDGE/","edTemp_values_",trait_of_interest,".rda"))
  edXeric_values<-extract_variable_model_vectors(edXeric, subset=T); save(edXeric_values, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmEDGE/","edXeric_values_",trait_of_interest,".rda"))
  
  gdist_values<-extract_variable_model_vectors(mdist_matrix_distGeo, subset=T); save(gdist_values, file=paste0("data/Re_analysisGEbarriers/P1_datamodel/dfmEDGE/","gdist_valuesGeo_pertrait_",trait_of_interest,".rda"))

  print(i) #i
  
} 
