
rm(list=ls())


################################
######## re-prune the tree ##### prune the jager tree for the correct tips (part1)
################################

library(geiger)
read.csv("data/dplace-societies-2018-06-21.csv", stringsAsFactors = F)->dplace
phyagain2 <- read.tree("data/worldtree_ml_glottologConstrained.tre")
phyagain2$tip.label<-tolower(phyagain2$tip.label)

load(file="data/newd1.rds")
d1<-d1[!d1$new_jtip%in%"",] # 868
sum(duplicated(d1$new_jtip)) # one duplicated
d1<-d1[!duplicated(d1$new_jtip),] # 867 - remove random duplicate
sum(duplicated(d1$glottocode)) # there will be duplicates - fine, as diff tips in dplace
keeptips<-as.character(d1$new_jtip) 
pruned_wtree<-drop.tip(phyagain2, tip=phyagain2$tip.label[!phyagain2$tip.label%in%keeptips])
#write.nexus(pruned_wtree, file="Jagerphy/phymatching/30sept_newtree/pruned_wtreeAC.nex") #after corrections 
### ----------### ----------### ----------### ----------### ----------### ----------### ----------### ----------### ----------


################################
######## TRIAL CALIBRATIONS #### root only - ie prune the jager tree for the correct tips (part2)
################################

rm(list=ls())
library(ape)

read.nexus(file="Jagerphy/phymatching/30sept_newtree/pruned_wtreeAC.nex")->pruned_wtree
load(file="newd1.rds")
d1[d1$new_jtip%in%pruned_wtree$tip.label,]->d1
d1<-d1[!duplicated(d1$new_jtip),] # 867 - remove random duplicate
d1<-d1[match(pruned_wtree$tip.label, d1$new_jtip),]
pruned_wtreeCI<-pruned_wtree ; pruned_wtreeCI$tip.label<-d1$CrossID
pruned_wtreeGC<-pruned_wtree ; pruned_wtreeGC$tip.label<-d1$glottocode

# names
which(colnames(d1)%in%"Phynames")->c3
which(colnames(d1)%in%"CrossID")->c1
which(colnames(d1)%in%"glottocode")->c2

fxntl<-function(x, output,c1,c2,c3){return(paste(x[c1],x[c2],x[c3], sep=" "))} 
d1$new_alltips<-apply(d1,1,fxntl,c1=c1,c2=c2,c3=c3)
d1<-d1[match(pruned_wtree$tip.label, d1$new_jtip),]
pruned_wtreeALL<-pruned_wtree ; pruned_wtreeALL$tip.label<-d1$new_alltips

# make pruned_wtreeALL UT for plotting
mycalibration <- makeChronosCalib(pruned_wtreeALL, node="root", age.min = 0.001, age.max=0.3) 
UTall<-chronos(pruned_wtreeALL, lambda = 0, model = "correlated", calibration = mycalibration, control = chronos.control() ) # fails optimization

# get rid of following tips - an.oceanic - prevent from converging
tipstorem<-d1$new_alltips[grep("an.oceanic",d1$new_alltips)]
trialt<-drop.tip(pruned_wtreeALL, tip =tipstorem) 

mycalibration <- makeChronosCalib(trialt, node="root", age.min = 0.001, age.max=0.3) 
trialtut<-chronos(trialt, lambda = 1, model = "correlated", calibration = mycalibration, control = chronos.control() ) # works

trialtut$tip.label
d1<-d1[match(trialtut$tip.label, d1$new_alltips),]
trialtut2<-trialtut ; trialtut2$tip.label<-d1$glottocode

#save(trialtut, file="Jagerphy/phymatching/30sept_newtree/trialtut.rds") 
#save(trialtut2, file="Jagerphy/phymatching/30sept_newtree/trialtut2.rds") 
#save(d1, file="Jagerphy/phymatching/30sept_newtree/newd1_anremov.rds") 
### ----------### ----------### ----------### ----------### ----------### ----------### ----------### ----------### ----------




##########################################
######## CALIBRATIONS  super families #### ie prune the jager tree for the correct tips (part3)
##########################################

# first identify issue tips - ie part3 is just exploratory

rm(list=ls())
library(ape)
library(phangorn)

#read.csv("Jagerphy/calibrations/GlottoV33/languages_and_dialects_geo.csv", stringsAsFactors = F)->gcodes
read.csv("data/languages_and_dialects_geo.csv", stringsAsFactors = F)->gcodes
load(file="Jagerphy/phymatching/30sept_newtree/trialtut.rds") 
load(file="Jagerphy/phymatching/30sept_newtree/trialtut2.rds") 
load(file="Jagerphy/phymatching/30sept_newtree/newd1_anremov.rds") 
read.csv("data/dplace-societies-2018-06-21.csv", stringsAsFactors = F)->dplace

# if ma in ['Australia', 'Papunesia'] and family != AUSTRONESIAN --> Sahul
# if ma in ['North America', 'South America'] or family == ESKIMO_ALEUT --> Americas
# if ma != 'Africa' or family == AFRO_ASIATIC or gc in AUSTRONESIAN_LANGS_IN_AFRICA --> ROW
# if ma == 'Eurasia' and family != ESKIMO_ALEUT: --> Eurasia

# these are all to exclude, anci1244 is not in dplace - so do not mention this one in methods paper text
exclude_lang<-c("afri1274","braz1246","hait1244","hebr1245","suri1272","sara1340","egyp1246","akka1240","lati1261","sans1269","anci1244")
AUSTRONESIAN = 'aust1307'
AFRO_ASIATIC = 'afro1255'
ESKIMO_ALEUT = 'eski1264'
AUSTRONESIAN_LANGS_IN_AFRICA = c("plat1254", "masi1268")

# add macroarea (from gcodes) and family (from dplace) in d1
d1[!d1$glottocode%in%dplace$Glottolog.language.dialect.id,] # 0 all there
d1[!d1$CrossID%in%dplace$Cross.dataset.id,] # 0 all there, by cross ID because this one does not repeat (except one, will choose one of the two, same fam)
d1[!d1$glottocode%in%gcodes$glottocode,] # 4  - will have macroarea -999 but family will be eskimo
d1$family<- -999 ; d1$macroarea<- -999
for(i in 1: length(d1[,1]))
  {d1$family[i]<-dplace[dplace$Cross.dataset.id%in%d1$CrossID[i],]$Language.family
  if(sum(gcodes$glottocode%in%d1$glottocode[i])>0) d1$macroarea[i]<-gcodes[gcodes$glottocode%in%d1$glottocode[i],]$macroarea
  print(i)}

d1[d1$macroarea%in%c("Australia","Papunesia") & !d1$family%in%"Austronesian",]$glottocode-> sahul
sahul<-sahul[!sahul%in%exclude_lang]
d1[d1$macroarea%in%c("North America","South America") | d1$family%in%"Eskimo-Aleut",]$glottocode-> americas
americas<-americas[!americas%in%exclude_lang]
d1[!d1$macroarea%in%c("Africa") | d1$family%in%"Afro-Asiatic" | d1$glottocode%in%AUSTRONESIAN_LANGS_IN_AFRICA,]$glottocode-> row
row<-row[!row%in%exclude_lang]

#### distribution of tips in super families
# trialtut2$tip.label[2]; trialtut$tip.label[2] # - check tip labels in the two trees are in the same order
dropmp<-which(trialtut2$tip.label%in%exclude_lang) # exclude these ones
todro2<-trialtut2$tip.label[dropmp]
todro<-trialtut$tip.label[dropmp]
trialtut2mp<-drop.tip(trialtut2, tip=todro2)
trialtutmp<-drop.tip(trialtut, tip=todro)
#trialtut2$tip.label[2]; trialtut$tip.label[2] # tip labels in the two trees are in the same order

  # here worth plotting ladderized instead

intree<-trialtut2mp$tip.label[trialtut2mp$tip.label%in%sahul]
nodeclade<-getMRCA(phy=trialtut2mp, tip=intree)
is.monophyletic(phy=trialtut2mp, tips=intree, plot=T, cex=0.5) # F
colsT<-rep("black", length(trialtut2mp$tip.label)) ;  colsT[which(trialtut2mp$tip.label%in%intree)]<-"red"
#pdf(paste0("Jagerphy/phymatching/30sept_newtree/UTsfams/UTtree_sahul",".pdf"), height = 10, width = 10)
plot(trialtutmp, type="fan",tip.color=colsT, cex=0.2) ; dev.off()

intree<-trialtut2mp$tip.label[trialtut2mp$tip.label%in%americas]
nodeclade<-getMRCA(phy=trialtut2mp, tip=intree)
is.monophyletic(phy=trialtut2mp, tips=intree, plot=T, cex=0.5) # F, root
colsT<-rep("black", length(trialtut2mp$tip.label)) ;  colsT[which(trialtut2mp$tip.label%in%intree)]<-"red"
#pdf(paste0("Jagerphy/phymatching/30sept_newtree/UTsfams/UTtree_americas",".pdf"), height = 10, width = 10)
par(xpd=T); plot(trialtutmp, type="fan",tip.color=colsT, cex=0.2,label.offset = 0.001) ; dev.off()

intree<-trialtut2mp$tip.label[trialtut2mp$tip.label%in%row]
nodeclade<-getMRCA(phy=trialtut2mp, tip=intree)
is.monophyletic(phy=trialtut2mp, tips=intree, plot=T, cex=0.5) # F, root
colsT<-rep("black", length(trialtut2mp$tip.label)) ;  colsT[which(trialtut2mp$tip.label%in%intree)]<-"red"
#pdf(paste0("Jagerphy/phymatching/30sept_newtree/UTsfams/UTtree_row",".pdf"), height = 10, width = 10)
par(xpd=T); plot(trialtutmp, type="fan",tip.color=colsT, cex=0.2,label.offset = 0.001) ; dev.off()

# HAVING DONE THIS, IDENTIFY the problem tips:
am_issue<-c("xd686 akab1249 ga.great_andamanese.aka_bea",
            "xd802 fuyu1242 tng.goilalan.mafulu",
            "xd798 mail1248 tng.mailuan.mailu",
            "xd819 namb1293 mum.morehead_and_upper_maro_rivers.nambo",
            "xd790 meri1244 wf.western_fly.meriam",
            "xd377 komo1258 kom.koman.koma",
            "xd813 iatm1242 sep.middle_sepik.nyaura",
            "xd791 ambu1247 sep.middle_sepik.maprik",
            "xd788 kwom1262 sep.middle_sepik.kwoma",
            "xd583 chec1245 nda.nakh.akkin_chechen", "xd1397 boro1282 mge.bororo.bororo",
            "xd7 naro1249 kk.khoe_kwadi.naro", "xd3 nama1265 kk.khoe_kwadi.nama",
            "xd8 xamm1241 tu.tu.kam_ka_ke","xd6 sand1273 sad.sandawe.sandawe")
sahul_issue<-c("xd802 fuyu1242 tng.goilalan.mafulu",
              "xd804 bana1292 lsr.grass.banaro", "xd854 tang1355 lsr.annaberg.tanggu",
              "xd807 bumb1241 tor.kombio_arapesh.arapesh", "xd764 abui1241 tap.greater_alor.abui",
              "xd792 foii1241 tng.kutubuan.foe","xd841 siwa1245 eb.east_bougainville.motuna",
              "xd817 orok1267 ele.eleman_proper.orokolo",
              "xd1356 yano1262 yan.yanomam.yanomami","xd1361 yano1261 yan.yanomam.yanomame",
              "xd1358 nina1238 yan.yanomam.yanam","xd1360 sanu1240 yan.yanomam.sanima")
row_issue1<-c("xd792 foii1241 tng.kutubuan.foe","xd841 siwa1245 eb.east_bougainville.motuna",
              "xd635 ainu1240 ain.ainu.ainu_nukkibetsu","xd531 labo1236 bas.basque.basque",
              "xd468 mang1399 sah.western_saharan.manga_kanuri",
              "xd498 teda1241 sah.western_saharan.tedaga",
              "xd492 daza1242 sah.western_saharan.dazaga",
              "xd693 tand1256 an.barito.malagasy_tandroy_mahafaly_ampanihy",
              "xd694 saka1291 an.barito.malagasy_sakalava_1",
              "xd695 tesa1236 an.barito.malagasy_antaisaka","xd440 nara1262 esu.nara.nara",
              "xd7 naro1249 kk.khoe_kwadi.naro","xd3 nama1265 kk.khoe_kwadi.nama",
              "xd8 xamm1241 tu.tu.kam_ka_ke","xd6 sand1273 sad.sandawe.sandawe",
              "xd377 komo1258 kom.koman.koma","xd584 imer1248 krt.kartvelian.georgian",
              "xd585 svan1243 krt.kartvelian.svan","xd1397 boro1282 mge.bororo.bororo")


###########################################
######## CALIBRATIONS 2 super families #### ie prune the jager tree for the correct tips (part4)
###########################################

# now prune issue tips - ie part4 is the action of part 3

rm(list=ls())
library(ape)
library(phangorn)

# the problem tips:
am_issue<-c("xd686 akab1249 ga.great_andamanese.aka_bea",
            "xd802 fuyu1242 tng.goilalan.mafulu",
            "xd798 mail1248 tng.mailuan.mailu",
            "xd819 namb1293 mum.morehead_and_upper_maro_rivers.nambo",
            "xd790 meri1244 wf.western_fly.meriam",
            "xd377 komo1258 kom.koman.koma",
            "xd813 iatm1242 sep.middle_sepik.nyaura",
            "xd791 ambu1247 sep.middle_sepik.maprik",
            "xd788 kwom1262 sep.middle_sepik.kwoma",
            "xd583 chec1245 nda.nakh.akkin_chechen", "xd1397 boro1282 mge.bororo.bororo",
            "xd7 naro1249 kk.khoe_kwadi.naro", "xd3 nama1265 kk.khoe_kwadi.nama",
            "xd8 xamm1241 tu.tu.kam_ka_ke","xd6 sand1273 sad.sandawe.sandawe")
sahul_issue<-c("xd802 fuyu1242 tng.goilalan.mafulu",
               "xd804 bana1292 lsr.grass.banaro", "xd854 tang1355 lsr.annaberg.tanggu",
               "xd807 bumb1241 tor.kombio_arapesh.arapesh", "xd764 abui1241 tap.greater_alor.abui",
               "xd792 foii1241 tng.kutubuan.foe","xd841 siwa1245 eb.east_bougainville.motuna",
               "xd817 orok1267 ele.eleman_proper.orokolo",
               "xd1356 yano1262 yan.yanomam.yanomami","xd1361 yano1261 yan.yanomam.yanomame",
               "xd1358 nina1238 yan.yanomam.yanam","xd1360 sanu1240 yan.yanomam.sanima")
row_issue1<-c("xd792 foii1241 tng.kutubuan.foe","xd841 siwa1245 eb.east_bougainville.motuna",
              "xd635 ainu1240 ain.ainu.ainu_nukkibetsu","xd531 labo1236 bas.basque.basque",
              "xd468 mang1399 sah.western_saharan.manga_kanuri",
              "xd498 teda1241 sah.western_saharan.tedaga",
              "xd492 daza1242 sah.western_saharan.dazaga",
              "xd693 tand1256 an.barito.malagasy_tandroy_mahafaly_ampanihy",
              "xd694 saka1291 an.barito.malagasy_sakalava_1",
              "xd695 tesa1236 an.barito.malagasy_antaisaka","xd440 nara1262 esu.nara.nara",
              "xd7 naro1249 kk.khoe_kwadi.naro","xd3 nama1265 kk.khoe_kwadi.nama",
              "xd8 xamm1241 tu.tu.kam_ka_ke","xd6 sand1273 sad.sandawe.sandawe",
              "xd377 komo1258 kom.koman.koma","xd584 imer1248 krt.kartvelian.georgian",
              "xd585 svan1243 krt.kartvelian.svan","xd1397 boro1282 mge.bororo.bororo")


read.csv("data/languages_and_dialects_geo.csv", stringsAsFactors = F)->gcodes
load(file="Jagerphy/phymatching/30sept_newtree/trialtut.rds") 
load(file="Jagerphy/phymatching/30sept_newtree/trialtut2.rds") 
load(file="Jagerphy/phymatching/30sept_newtree/newd1_anremov.rds") 
read.csv("data/dplace-societies-2018-06-21.csv", stringsAsFactors = F)->dplace

exclude_lang<-c("afri1274","braz1246","hait1244","hebr1245","suri1272","sara1340","egyp1246","akka1240","lati1261","sans1269","anci1244") # suri1272 is not in d-place anyway (ie no mathc)
# hebrew and ancent hebrew are the same in Dplace = cj3
AUSTRONESIAN_LANGS_IN_AFRICA = c("plat1254", "masi1268")

# add macroarea (from gcodes) and family (from dplace) in d1
d1$family<- -999 ; d1$macroarea<- -999
for(i in 1: length(d1[,1]))
  {d1$family[i]<-dplace[dplace$Cross.dataset.id%in%d1$CrossID[i],]$Language.family
  if(sum(gcodes$glottocode%in%d1$glottocode[i])>0) d1$macroarea[i]<-gcodes[gcodes$glottocode%in%d1$glottocode[i],]$macroarea
  print(i)}

d1[d1$macroarea%in%c("Australia","Papunesia") & !d1$family%in%"Austronesian",]$glottocode-> sahul
sahul<-sahul[!sahul%in%exclude_lang]
d1[d1$macroarea%in%c("North America","South America") | d1$family%in%"Eskimo-Aleut",]$glottocode-> americas
americas<-americas[!americas%in%exclude_lang]
d1[!d1$macroarea%in%c("Africa") | d1$family%in%"Afro-Asiatic" | d1$glottocode%in%AUSTRONESIAN_LANGS_IN_AFRICA,]$glottocode-> row
row<-row[!row%in%exclude_lang]

# final counts
droptips<-c(am_issue,sahul_issue, row_issue1) # there have duplicated
sum(!duplicated(droptips)) # 37
all(droptips%in%d1$new_alltips)
length(droptips) # 46
length(droptips[!duplicated(droptips)]) # 37
droptips2<-d1[d1$alltips%in%droptips,]$glottocode # 37

trialtut2mp<-drop.tip(trialtut2, tip=droptips2)
trialtutmp<-drop.tip(trialtut, tip=droptips)
trialtut2mp$tip.label[100] ; trialtutmp$tip.label[100] # tips same order

# exclude colonial languages
dropcol<-which(trialtut2mp$tip.label%in%exclude_lang)
drop1<-trialtut2mp$tip.label[dropcol]
drop2<-trialtutmp$tip.label[dropcol]


trialtut2mp<-drop.tip(trialtut2mp, tip=drop1)
trialtutmp<-drop.tip(trialtutmp, tip=drop2)
trialtut2mp$tip.label[120] ; trialtutmp$tip.label[120] # tips same order

# this is tree with list of final tips to keep after check-ups

#save(trialtutmp, file="Jagerphy/phymatching/30sept_newtree/UTrootJagerC1final.rds") # ie "Jagerphy/calibrations/UTrootJagerC1final.rds"


# here we start fresh, use the final tips tree for target tips, and calibrate the oroginal tree

############################
###### FAM CALIBRATIONS #### # with the tree pruned for offending tips
############################

rm(list=ls())

read.csv("data/languages_and_dialects_geo.csv", stringsAsFactors = F)->gcodes
read.csv("data/dplace-societies-2018-06-21.csv", stringsAsFactors = F)->dplace

load(file="Jagerphy/phymatching/30sept_newtree/newd1_anremov.rds") # ie Jagerphy/calibrations/name_of_file
load(file="Jagerphy/phymatching/30sept_newtree/UTrootJagerC1final.rds") # trialtutmp # this only serves as a list of tips
d1<-d1[d1$alltips%in%trialtutmp$tip.label,] # 785
sum(duplicated(d1$glottocode)) # 2
d1[duplicated(d1$glottocode),] # they have different cross-IDs

# re-prune jager world to the 896 non-offending, matched and checked tips (also excludes some an.oceanic which cause troubles for convergence and calibrations!)
library(geiger)
phyagain2 <- read.tree("data/worldtree_ml_glottologConstrained.tre")
phyagain2$tip.label <- tolower(phyagain2$tip.label)
keeptips<-as.character(d1$new_jtip) # 
phyagain2$tip.label[!phyagain2$tip.label%in%keeptips]->removed_tips
pruned_wtree<-drop.tip(phyagain2, tip=phyagain2$tip.label[!phyagain2$tip.label%in%keeptips])

d1<-d1[match(pruned_wtree$tip.label, d1$new_jtip),]
pruned_wtreeALL<-pruned_wtree
trialtutmp$tip.label[140] ; pruned_wtreeALL$tip.label[140] # same order
pruned_wtreeALL$tip.label<-d1$new_alltips

# read.csv("Jagerphy/calibrations/GlottoV33/calibWB.csv", stringsAsFactors = F)-> calibWB
# read.csv("Jagerphy/calibrations/GlottoV33/glottolog_languoid.csv/languoid.csv", stringsAsFactors = F)-> langoloid # d place files
read.csv("data/calibWB.csv", stringsAsFactors = F)-> calibWB
read.csv("data/languoid.csv", stringsAsFactors = F)-> langoloid # d place files

library(ape)
library(phangorn)

dfcalib<-data.frame("node"=numeric(),"age.min"=numeric(),"age.max"=numeric(), soft.bounds=numeric(), "monophyly"=numeric(), "nodename"=numeric(), "famname"=numeric())
for(i in 1: length(calibWB[,1]))
  { if(!calibWB$Clade3ID[i]%in%"") cid<-calibWB$Clade3ID[i]
  if(calibWB$Clade3ID[i]%in%"" & !calibWB$Clade2ID[i]%in%"") cid<-calibWB$Clade2ID[i]
  if(calibWB$Clade3ID[i]%in%"" & calibWB$Clade2ID[i]%in%"" & !calibWB$CladeID[i]%in%"") cid<-calibWB$CladeID[i]
  if(calibWB$Clade3ID[i]%in%"" & calibWB$Clade2ID[i]%in%"" & !calibWB$CladeID[i]%in%"") cid<-calibWB$CladeID[i]
  if(calibWB$Clade3ID[i]%in%"" & calibWB$Clade2ID[i]%in%"" & calibWB$CladeID[i]%in%"") cid<-calibWB$FamID[i]
  
  tips<-langoloid[langoloid$id%in%cid | langoloid$family_id%in%cid | langoloid$parent_id%in%cid,]$id
  intree<-d1[d1$glottocode%in%tips,]$new_alltips
  
  # get the node of this
  if(length(intree)>0) 
    { if(length(intree)>1) nodeclade<-getMRCA(phy=pruned_wtreeALL, tip=intree)
    if(length(intree)==1) nodeclade<-Ancestors(x=pruned_wtreeALL, node= which(pruned_wtreeALL$tip.label%in%intree), type="parent")
    dfcalib[i,]<-c(nodeclade, calibWB$AgeMin_yBP[i], calibWB$Agemax_yBP[i],F, "no",cid,calibWB$Name[i])
    if(is.monophyletic(phy=pruned_wtreeALL, tips=intree, plot=F, cex=0.5)) 
      {dfcalib$monophyly[i]<-"yes"
      colsT<-rep("black", length(pruned_wtreeALL$tip.label))
      colsT[which(pruned_wtreeALL$tip.label%in%intree)]<-"red"
      pdf(paste0("Jagerphy/phymatching/30sept_newtree/UTsfams/UTtree_",cid,".pdf"), height = 10, width = 10)
      plot(pruned_wtreeALL, type="fan",tip.color=colsT, cex=0.2)
      # add node to make sure it is ok
      nodelabels(node=nodeclade, frame = "circle", cex=0.2)
      
      # use the monophyletic tree for greater visibility, but don't use this tree for the node numbers as it might be diff nodes
        # it actually IS same order but this way is more consistent
      colsT<-rep("black", length(trialtutmp$tip.label))
      colsT[which(trialtutmp$tip.label%in%intree)]<-"red"
      plot(trialtutmp, type="fan",tip.color=colsT, cex=0.2)
      if(length(intree)>1) nodeclade<-getMRCA(phy=trialtutmp, tip=intree)
      if(length(intree)==1) nodeclade<-Ancestors(x=trialtutmp, node= which(trialtutmp$tip.label%in%intree), type="parent")
      nodelabels(node=nodeclade, frame = "circle", cex=0.2)
      dev.off()
      }
    } else dfcalib[i,]<-c(NA,NA,NA,NA,NA,cid,calibWB$FamID[i])   
  
  print(i)
}

dfcalib<-dfcalib[complete.cases(dfcalib),]
dfcalib[dfcalib$monophyly%in%"no",] # none - checked
dfcalib<-dfcalib[dfcalib$monophyly%in%"yes",]
# root node: 786
makeChronosCalib(pruned_wtreeALL, node="root", age.min = 0.15, age.max=0.25)
dfcalib[ (length(dfcalib[,1])+1),]<-c(786,150000, 250000, FALSE, "yes", "root", "root")  # add root

str(dfcalib)
dfcalib$node<-as.integer(as.numeric(dfcalib$node))
dfcalib$age.max<-as.numeric(dfcalib$age.max)
dfcalib$age.min<-as.numeric(dfcalib$age.min)
dfcalib$soft.bounds<-as.logical(dfcalib$soft.bounds)

dfcalib[dfcalib$famname%in%"Uto-Aztecan",]$age.min<-3258
dfcalib[dfcalib$famname%in%"Uto-Aztecan",]$age.max<-5025 # updated UA - checked

dfcalib2<-dfcalib[,colnames(dfcalib)%in%c("node", "age.min", "age.max","soft.bounds")]
dfcalib2<-dfcalib2[order(dfcalib2$node),]
dfcalib2$age.min<-dfcalib2$age.min/1000000
dfcalib2$age.max<-dfcalib2$age.max/1000000
str(dfcalib2)


# calibration with chronos
library(ape)
pruned_wtreeALL

# mycalibration <- makeChronosCalib(pruned_wtreeALL, node="root", age.min = 0.15, age.max=0.25)
# UT0<-chronos(pruned_wtreeALL, lambda = 1, model = "correlated", calibration = mycalibration, control = chronos.control() )
# plot(UT0, type="fan", show.tip.label = F, main="root 0.15-0.25") #just root2

# add calibrations families
rowsin<-1:28
  # error when including all: no reasonable starting dates for rows 10,14: Otomanguean,Tupian
toex<-c(10,14)
rowsin<-rowsin[!rowsin%in%toex]

UTfam<-chronos(pruned_wtreeALL, lambda = 0, model = "correlated", calibration = dfcalib2[rowsin,], control = chronos.control() )
plot(UTfam, type="fan", show.tip.label = F, main="all") 
#save(UTfam, file="Jagerphy/phymatching/30sept_newtree/UTfamlistlambda0.rds")


# different lambdas, pick based on ML
lambdas<-seq(from=0, to=10, by=1)
rowsin<-1:28 ; toex<-c(10,14) ; rowsin<-rowsin[!rowsin%in%toex] # these prevent convergence
UT_famlist<-list()
for(i in 1: length(lambdas))
  {UTfam<-chronos(pruned_wtreeALL, lambda = lambdas[i], model = "correlated", calibration = dfcalib2[rowsin,], control = chronos.control() )
  UT_famlist[[i]]<-UTfam
  print(i)}
names(UT_famlist)<-lambdas
#save(UT_famlist, file="Jagerphy/phymatching/30sept_newtree/UTfamlist.rds")

# ML lambda
attributes(UT_famlist[[2]])$ploglik #the maximum penalized log-likelihood.
attributes(UT_famlist[[2]])$PHIIC$lambda
get_att<-function(x, which_att) 
{if(which_att%in%"lambda") to_ret<-attributes(x)$PHIIC$lambda
if(which_att%in%"ploglik") to_ret<-attributes(x)$ploglik
return(to_ret)}
plot(unlist(lapply(UT_famlist,get_att, which_att="ploglik" ))
     ~ unlist(lapply(UT_famlist,get_att, which_att="lambda" )), xlab="lambda", ylab="ploglik") # lambda 0 has higher LH
plot(UT_famlist[[1]], type="fan", show.tip.label = F)
plot(UT_famlist[[2]], type="fan", show.tip.label = F)
plot(UT_famlist[[11]], type="fan", show.tip.label = F)
plot(UT_famlist[[1]]$edge.length~ UT_famlist[[2]]$edge.length) # similar

UT_famlist[which(unlist(lapply(UT_famlist,get_att, which_att="ploglik" ))%in%max(unlist(lapply(UT_famlist,get_att, which_att="ploglik" ))))]
names(UT_famlist)[[1]]
UTlambda0<-UT_famlist[[1]]
#save(UTlambda0, file="Jagerphy/phymatching/30sept_newtree/UTlambda0.rds")
load("Jagerphy/phymatching/30sept_newtree/UTlambda0.rds")


# figure for supplement
pdf("Jagerphy/phymatching/30sept_newtree/figS17.pdf", height = 10, width=10)
par(xpd=T); plot(ladderize(UTlambda0), type="fan", cex=0.2,label.offset = 0.001)
dev.off()
