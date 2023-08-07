
rm(list=ls())

library(geiger)
library(ape)


# #############################
# #### phylogenetic distance ##
# #############################
# 
## (3.1) new jager tree
library(ape)
library(geiger)
path<-"E:/WashUDesktop/other/BOX_15march/GeogrAxes/Jagerphy/NewVersionAutumn2021/jtree mac-20210927T134830Z-001/jtree mac/JTREE-20211004T080553Z-001/JTREE/"
load(file =paste0(path,"Jagerphy/phymatching/30sept_newtree/UTlambda0.rds")) # this is file is attached as a supplementary file as well
jagertree<-UTlambda0 ; rm(UTlambda0)

fxn_labels<-function(x){ return( strsplit(x, split=" ")[[1]][1])} # modify here for this new tree
labelscrossid<-lapply(jagertree$tip.label, fxn_labels)
labelscrossid<-unlist(labelscrossid)
#labelscrossid[duplicated(labelscrossid)]
read.csv("data/dplace-societies-2018-06-21.csv", stringsAsFactors = F)->dplace ## get labels as society IDs
fxn_labelsSocId<-function(x, dplace) {return(dplace[dplace$Cross.dataset.id%in%x,]$Society.id)}
labelsSocId<-unlist(lapply(labelscrossid,fxn_labelsSocId, dplace=dplace))
#labelsSocId[duplicated(labelsSocId)]
jagertree$tip.label<-labelsSocId

cophenetic_dist<-cophenetic.phylo(jagertree)
#save(cophenetic_dist, file="data/predictors/preds3april/phylogeny/Jagercophenetic_distAfterCalib_newtree.rda")

## (4) Edge tree
library(ape)
library(geiger)
tree<-read.nexus(file = "data/EdgeTree/prunedET.nex")
cophenetic_dist<-cophenetic.phylo(tree)
#save(cophenetic_dist, file="data/predictors/preds3april/phylogeny/EDGEcopheneticD.rda")



#############################
#### share language family ##
#############################

read.csv("data/dplace-societies-2018-06-21.csv", stringsAsFactors = F)->dplace
lfammat<-matrix(nrow=length(dplace[,1]), ncol=length(dplace[,1]))
rownames(lfammat)<-dplace$Society.id
colnames(lfammat)<-dplace$Society.id
fxn_sharefam<-function(x,soc2, dplace)
  {famx<-dplace[dplace$Society.id%in%x,]$Language.family
  famsoc2<-dplace[dplace$Society.id%in%soc2,]$Language.family
  if(famx%in%famsoc2) to_ret<-1 ;if(!famx%in%famsoc2) to_ret<-0
  return(to_ret)}
for(j in 1: dim(lfammat)[1])
  {lfammat[j,]<-unlist(lapply(colnames(lfammat), fxn_sharefam, soc2=rownames(lfammat)[j], dplace=dplace))
  print(j)}
#save(lfammat, file="data/predictors/preds3april/phylogeny/lfammat.rda")
load(file="data/predictors/preds3april/phylogeny/lfammat.rda")
lfammat[1:10,1:10]
## ---------------------------------------------------------------------------------------------------------


#####################################
#### calculate geographic distance ##
#####################################

library("geosphere")
read.csv("data/latlong_soc_from_dplace-societies-2018-03-16.csv", stringsAsFactors = F)-> latlong_soc
source("scripts/1getdata_fxn.R")

# use distGeo - results in meters
mdist_matrix_distGeo<-mdist_matrix_fxn("distGeo",latlong_soc)
#save(mdist_matrix_distGeo, file="data/predictors/preds3april/mdist_matrix_distGeo.rda")
mdist_matrix_distGeo[1:10,1:10]
rm(mdist_matrix_distGeo)
## -----------------------------------------------------------------------------------------



################
#### env data ## dissimilarity
################

read.csv("data/latlong_soc_from_dplace-societies-2018-03-16.csv", stringsAsFactors = F)-> longlat
load(file="data/Re_analysisGEbarriers/TempHarshness_raster.rds") # values already with -1 etc
TempHarshness_rasterscaled<-TempHarshness_raster # # scale them: (xi-xmean) / SD (i.e function scale) 
values(TempHarshness_rasterscaled)<-scale(values(TempHarshness_raster))

load(file="data/Re_analysisGEbarriers/XericHarshness_raster.rds") # values already with -1 etc
XericHarshness_rasterscaled<-XericHarshness_raster # # scale them: (xi-xmean) / SD (i.e function scale) 
values(XericHarshness_rasterscaled)<-scale(values(XericHarshness_raster))

coords<-data.frame(lon=longlat$Revised.longitude, lat=longlat$Revised.latitude)
coordinates(coords)<-c("lon","lat")

# verify coordinates
# plot(TempHarshness_rasterscaled) ; points(coords, pch=16, cex=0.5)
# abline(v=-12.27) ; abline (h=-37.11)
# plot(XericHarshness_rasterscaled) ; points(coords, pch=16, cex=0.5)
# abline(v=-12.27) ; abline (h=-37.11)
# load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/Elev_raster.rds")
# plot(Elev_raster) ; points(coords, pch=16, cex=0.5)
# abline(v=-12.27) ; abline (h=-37.11)

longlat$EnvPC1_relcp<-extract(x=TempHarshness_rasterscaled, y=coords)
longlat$EnvPC2_relcp<-extract(x=XericHarshness_rasterscaled, y=coords)

longlat_cc<- longlat[complete.cases(longlat$EnvPC1_relcp), ] # remove NAs from env no data

edTemp<-matrix(data=NA, nrow = length(longlat_cc$Society.id), ncol = length(longlat_cc$Society.id ))
edXeric<-matrix(data=NA, nrow = length(longlat_cc$Society.id), ncol = length(longlat_cc$Society.id ))
rownames(edTemp)<-longlat_cc$Society.id ; colnames(edTemp)<-longlat_cc$Society.id
rownames(edXeric)<-longlat_cc$Society.id ; colnames(edXeric)<-longlat_cc$Society.id

for(i in 1:length(longlat_cc$Society.id)) # order of longlat
  { soci<-longlat_cc$Society.id[i]
    edTemp[i,]<-abs(longlat_cc$EnvPC1_relcp - longlat_cc$EnvPC1_relcp[i])
    edXeric[i,]<-abs(longlat_cc$EnvPC2_relcp - longlat_cc$EnvPC2_relcp[i])
    print(i)
  }

edTemp[1:10,1:10]
edXeric[1:10,1:10]

save(longlat, file="data/Re_analysisGEbarriers/P1EnvDis/longlat_newEnvData.rds")
save(edTemp, file="data/Re_analysisGEbarriers/P1EnvDis/edTemp_newEnvData.rds")
save(edXeric, file="data/Re_analysisGEbarriers/P1EnvDis/edXeric_newEnvData.rds")
