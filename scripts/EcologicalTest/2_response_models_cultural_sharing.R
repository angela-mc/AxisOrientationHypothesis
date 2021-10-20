rm(list=ls())


#######################
#### cultural traits ##
#######################

# data folders Dplace - EA
read.csv("data/EA/EA/codes_wallnewcodes.csv", stringsAsFactors = F)-> codes
read.csv("data/EA/EA/data_wallnewcodes.csv", stringsAsFactors = F)-> data
read.csv("data/EA/EA/societies_mapping.csv", stringsAsFactors = F)-> socmap
read.csv("data/EA/EA/societies.csv", stringsAsFactors = F)-> soc
read.csv("data/EA/EA/variables.csv", stringsAsFactors = F)-> vars
read.csv("data/newcodesFINAL.csv", stringsAsFactors = F)-> newcodes

traits<-vars$id
cat_trats<-table(vars$category)

# fxn that extracts dataframes from the files above
source("scripts/1getdata_fxn.R")
trait_of_interest<-"EA042"
df<-extract_cultural_data(trait_of_interest=trait_of_interest,codes, data, soc, vars)



##########################################
#### cultural traits: shared/not shared ## 25 IN!
##########################################

read.csv("data/newcodesFINAL.csv", stringsAsFactors = F)-> newcodes
unique(newcodes$Trait)->alltraits
# which traits can we consider?
traits_of_interest<-list()

for(i in 1: length(alltraits))
  {ctrait<-alltraits[i]
   if(sum(!is.na(newcodes[newcodes$Trait%in%ctrait,]$New.code))>0) traits_of_interest<-c(traits_of_interest, ctrait)
   print(i)}
traits_of_interest<-unlist(traits_of_interest)


for(i in 1: length(traits_of_interest))
  {trait_of_interest<-traits_of_interest[i]

   df<-extract_cultural_data(trait_of_interest=trait_of_interest,codes, data, soc, vars)
   nc<-newcodes[newcodes$Trait%in%trait_of_interest,]
  
   df$Code2<-999 # replace original code with new code
   for(j in 1: length(df[,1])) {df$Code2[j]<-nc[nc$Original.code%in%df$Code[j],]$New.code}
   df<-df[complete.cases(df),] # only consider the ones which have no NA
   table(df$Code_name) 
  
   df$Code<-df$Code2 #fxn based on $code
   table(df$Code)
   share_ctraits_mat<-share_cul_matrix_fxn2(df)
   share_ctraits_mat[1:10, 1:10]
   save(share_ctraits_mat,file=paste0("data/response/response3april/share_mat_",trait_of_interest,"_25IN",".rda"))
  
  }
