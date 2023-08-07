
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
pruned<-drop.tip(mcctree, tip = chuck)
library(ape)
plot.phylo(ladderize(pruned),show.tip.label=F, type="fan")

list_tips<-list_tips[match(pruned$tip.label, list_tips$edgelabel),]
pruned$tip.label<-list_tips$Society.id

# write.nexus(pruned, file = "data/EdgeTree/prunedET.nex")


tree<-read.nexus(file = "data/EdgeTree/prunedET.nex")
pdf("manuscript/Figures_EDGE/treeFigS5.pdf", width = 10, height=10)
plot(ladderize(tree),type="fan", cex=0.2)
dev.off()
