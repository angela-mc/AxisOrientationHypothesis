rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)
source("scripts/10_rLCP_MCostFXN.R")


######################
########### part 1 ### elevation
######################

rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)

# get pairs
read.csv("data/latlong_soc_from_dplace-societies-2018-03-16.csv", stringsAsFactors = F)-> longlat
load("data/predictors/preds3april/dfiEA_noislands.rds") # list of pairs - input data
load("data/Re_analysisGEbarriers/Elev_raster.rds")
soci1<-unique(dfiEA$soci)
rm(dfE)
for(i in 1:length(soci1))
  {socO<-soci1[i]
   load(file=paste0("data/Re_analysisGEbarriers/P1_elevation_soci/elevation_", socO,".rds"))

   dfi<-data.frame("Soci"=rep(socO,length(result$namesDs)), "SocNB"=result$namesDs, "MCOSTe"=result$mcost, "LCPe"=result$plen)
   dfi$MCOSTe<-as.character(dfi$MCOSTe)
   dfi$LCPe<-as.character(dfi$LCPe)
   
   if(i==1) dfE<-dfi
   if(i>1) dfE<-rbind(dfE,dfi)
   print(i)
   
}

# reverse
soci1<-unique(dfiEA$nb)
rm(dfErev)
for(i in 1:length(soci1))
  {socO<-soci1[i]
   load(file=paste0("data/Re_analysisGEbarriers/P1_elevation_nb/elevation_", socO,".rds"))
  
   dfi<-data.frame("Soci"=rep(socO,length(result$namesDs)), "SocNB"=result$namesDs, "MCOSTe"=result$mcost, "LCPe"=result$plen)
   dfi$MCOSTe<-as.character(dfi$MCOSTe)
   dfi$LCPe<-as.character(dfi$LCPe)
  
  if(i==1) dfErev<-dfi
  if(i>1) dfErev<-rbind(dfErev,dfi)
  print(i)
  
}

# put together
dfE$Soci<-as.character(dfE$Soci) ; dfE$SocNB<-as.character(dfE$SocNB)
dfErev$Soci<-as.character(dfErev$Soci) ; dfErev$SocNB<-as.character(dfErev$SocNB)

fxn_cat_sort<-function(x, output, col1, col2)
  { s1<-x[col1]
    s2<-x[col2]
    to_ret<-sort(c(s1,s2))
    to_ret<-paste(to_ret,collapse ="_")
    return(to_ret)
}

dfE$pairsoc_unsor<-paste(as.character(dfE$Soci), as.character(dfE$SocNB),sep="_")
dfErev$pairsoc_unsor<-paste(as.character(dfErev$Soci), as.character(dfErev$SocNB),sep="_")
dfE$pairsoc_sor<- unlist(apply(dfE,1,fxn_cat_sort, col1=which(colnames(dfE)%in%"Soci"),col2=which(colnames(dfE)%in%"SocNB")))
dfErev$pairsoc_sor<- unlist(apply(dfErev,1,fxn_cat_sort, col1=which(colnames(dfErev)%in%"Soci"),col2=which(colnames(dfErev)%in%"SocNB")))

dfErev<-dfErev[match(dfE$pairsoc_sor, dfErev$pairsoc_sor),]
sum(!complete.cases(dfErev)) # 0
sum(!complete.cases(dfE)) # 0

dfEbarriers<-cbind(dfE, dfErev$MCOSTe, dfErev$LCPe)
colnames(dfEbarriers)[7]<-"MCOSTe_rev"
colnames(dfEbarriers)[8]<-"LCPe_rev"

write.csv(dfEbarriers, file="data/Re_analysisGEbarriers/P1dfs/dfEbariers.csv")


######################
########### part 1 ### temperature
######################

rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)

# get pairs
read.csv("data/latlong_soc_from_dplace-societies-2018-03-16.csv", stringsAsFactors = F)-> longlat
load("data/predictors/preds3april/dfiEA_noislands.rds") # list of pairs - input data

soci1<-unique(dfiEA$soci)
rm(dfE)
noTempdata<-list()
for(i in 1:length(soci1))
  {socO<-soci1[i]
  
  if(paste0("temp_", socO,".rds")%in%list.files("data/Re_analysisGEbarriers/P1_temp_soci/")) # some have extract NA! ie no env data
    {load(file=paste0("data/Re_analysisGEbarriers/P1_temp_soci/temp_", socO,".rds"))
    
    dfi<-data.frame("Soci"=rep(socO,length(result$namesDs)), "SocNB"=result$namesDs, "MCOSTt"=result$mcost[1,], "LCPt"=result$plen)
    dfi$MCOSTt<-as.character(dfi$MCOSTt)
    dfi$LCPt<-as.character(dfi$LCPt)
    
    if(i==1) dfE<-dfi
    if(i>1) dfE<-rbind(dfE,dfi)
   } else noTempdata<-c(noTempdata,socO)
  print(i)
  
}
noTempdata<-unlist(noTempdata)
noTempdata


# reverse
soci1<-unique(dfiEA$nb)
rm(dfErev)
for(i in 1:length(soci1))
  {socO<-soci1[i]
  
  if(paste0("temp_", socO,".rds")%in%list.files("data/Re_analysisGEbarriers/P1_temp_nb/")) # some have extract NA! ie no env data
    {load(file=paste0("data/Re_analysisGEbarriers/P1_temp_nb/temp_", socO,".rds"))
    
     dfi<-data.frame("Soci"=rep(socO,length(result$namesDs)), "SocNB"=result$namesDs, "MCOSTt"=result$mcost[1,], "LCPt"=result$plen)
     dfi$MCOSTt<-as.character(dfi$MCOSTt)
     dfi$LCPt<-as.character(dfi$LCPt)
    
     if(i==1) dfErev<-dfi
     if(i>1) dfErev<-rbind(dfErev,dfi)
     }
  print(i)
}

# put together
dfE$Soci<-as.character(dfE$Soci) ; dfE$SocNB<-as.character(dfE$SocNB)
dfErev$Soci<-as.character(dfErev$Soci) ; dfErev$SocNB<-as.character(dfErev$SocNB)

fxn_cat_sort<-function(x, output, col1, col2)
  { s1<-x[col1]
  s2<-x[col2]
  to_ret<-sort(c(s1,s2))
  to_ret<-paste(to_ret,collapse ="_")
  return(to_ret)
}

dfE$pairsoc_unsor<-paste(as.character(dfE$Soci), as.character(dfE$SocNB),sep="_")
dfErev$pairsoc_unsor<-paste(as.character(dfErev$Soci), as.character(dfErev$SocNB),sep="_")
dfE$pairsoc_sor<- unlist(apply(dfE,1,fxn_cat_sort, col1=which(colnames(dfE)%in%"Soci"),col2=which(colnames(dfE)%in%"SocNB")))
dfErev$pairsoc_sor<- unlist(apply(dfErev,1,fxn_cat_sort, col1=which(colnames(dfErev)%in%"Soci"),col2=which(colnames(dfErev)%in%"SocNB")))

dfErev[dfErev$SocNB%in%noTempdata,]$MCOSTt # all spath - so if dest is NA - all spath
dfErev<-dfErev[dfErev$pairsoc_sor%in%dfE$pairsoc_sor,] # prune to common pairsocs
dfE<-dfE[dfE$pairsoc_sor%in%dfErev$pairsoc_sor,]

dfErev<-dfErev[match(dfE$pairsoc_sor, dfErev$pairsoc_sor),]
sum(!complete.cases(dfErev)) # 0
sum(!complete.cases(dfE)) # 0

dfEbarriers<-cbind(dfE, dfErev$MCOSTt, dfErev$LCPt)
colnames(dfEbarriers)[7]<-"MCOSTt_rev"
colnames(dfEbarriers)[8]<-"LCPt_rev"

write.csv(dfEbarriers, file="data/Re_analysisGEbarriers/P1dfs/dfTbariers.csv")



######################
########### part 1 ### xeric
######################

rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)

# get pairs
read.csv("data/latlong_soc_from_dplace-societies-2018-03-16.csv", stringsAsFactors = F)-> longlat
load("data/predictors/preds3april/dfiEA_noislands.rds") # list of pairs - input data

soci1<-unique(dfiEA$soci)
rm(dfE)
noTempdata<-list()
for(i in 1:length(soci1))
  {socO<-soci1[i]
  
  if(paste0("temp_", socO,".rds")%in%list.files("data/Re_analysisGEbarriers/P1_xeric_soci/")) # some have extract NA! ie no env data
  {load(file=paste0("data/Re_analysisGEbarriers/P1_xeric_soci/temp_", socO,".rds"))
    
    dfi<-data.frame("Soci"=rep(socO,length(result$namesDs)), "SocNB"=result$namesDs, "MCOSTx"=result$mcost[1,], "LCPx"=result$plen)
    dfi$MCOSTx<-as.character(dfi$MCOSTx)
    dfi$LCPx<-as.character(dfi$LCPx)
    
    if(i==1) dfE<-dfi
    if(i>1) dfE<-rbind(dfE,dfi)
  } else noTempdata<-c(noTempdata,socO)
  print(i)
  
}
noTempdata<-unlist(noTempdata)
noTempdata


# reverse
soci1<-unique(dfiEA$nb)
rm(dfErev)
for(i in 1:length(soci1))
  {socO<-soci1[i]
  
  if(paste0("temp_", socO,".rds")%in%list.files("data/Re_analysisGEbarriers/P1_xeric_nb/")) # some have extract NA! ie no env data
  {load(file=paste0("data/Re_analysisGEbarriers/P1_xeric_nb/temp_", socO,".rds"))
    
    dfi<-data.frame("Soci"=rep(socO,length(result$namesDs)), "SocNB"=result$namesDs, "MCOSTx"=result$mcost[1,], "LCPx"=result$plen)
    dfi$MCOSTx<-as.character(dfi$MCOSTx)
    dfi$LCPx<-as.character(dfi$LCPx)
    
    if(i==1) dfErev<-dfi
    if(i>1) dfErev<-rbind(dfErev,dfi)
  }
  print(i)
}

# put together
dfE$Soci<-as.character(dfE$Soci) ; dfE$SocNB<-as.character(dfE$SocNB)
dfErev$Soci<-as.character(dfErev$Soci) ; dfErev$SocNB<-as.character(dfErev$SocNB)

fxn_cat_sort<-function(x, output, col1, col2)
  { s1<-x[col1]
  s2<-x[col2]
  to_ret<-sort(c(s1,s2))
  to_ret<-paste(to_ret,collapse ="_")
  return(to_ret)
}

dfE$pairsoc_unsor<-paste(as.character(dfE$Soci), as.character(dfE$SocNB),sep="_")
dfErev$pairsoc_unsor<-paste(as.character(dfErev$Soci), as.character(dfErev$SocNB),sep="_")
dfE$pairsoc_sor<- unlist(apply(dfE,1,fxn_cat_sort, col1=which(colnames(dfE)%in%"Soci"),col2=which(colnames(dfE)%in%"SocNB")))
dfErev$pairsoc_sor<- unlist(apply(dfErev,1,fxn_cat_sort, col1=which(colnames(dfErev)%in%"Soci"),col2=which(colnames(dfErev)%in%"SocNB")))

dfErev[dfErev$SocNB%in%noTempdata,]$MCOSTx # all spath - so if dest is NA - all spath
dfErev<-dfErev[dfErev$pairsoc_sor%in%dfE$pairsoc_sor,] # prune to common pairsocs
dfE<-dfE[dfE$pairsoc_sor%in%dfErev$pairsoc_sor,]

dfErev<-dfErev[match(dfE$pairsoc_sor, dfErev$pairsoc_sor),]
sum(!complete.cases(dfErev)) # 0
sum(!complete.cases(dfE)) # 0

dfEbarriers<-cbind(dfE, dfErev$MCOSTx, dfErev$LCPx)
colnames(dfEbarriers)[7]<-"MCOSTx_rev"
colnames(dfEbarriers)[8]<-"LCPx_rev"

write.csv(dfEbarriers, file="data/Re_analysisGEbarriers/P1dfs/dfXbariers.csv")


######################
########### part 1 ### final df - ll 3 one way & reverse
######################

rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)


read.csv(file="data/Re_analysisGEbarriers/P1dfs/dfEbariers.csv", stringsAsFactors = F)->ebarrier
read.csv(file="data/Re_analysisGEbarriers/P1dfs/dfTbariers.csv", stringsAsFactors = F)->tbarrier
read.csv(file="data/Re_analysisGEbarriers/P1dfs/dfXbariers.csv", stringsAsFactors = F)->xbarrier

# add 0 to sPath_extentpoint if geod == 0 
load("data/predictors/preds3april/dfiEA_noislands.rds")
dfiEA<-dfiEA[dfiEA$dist==0,]
ebarrier[ebarrier$pairsoc_sor%in%dfiEA$pair_soc,]$MCOSTe<-0 ; ebarrier[ebarrier$pairsoc_sor%in%dfiEA$pair_soc,]$LCPe<-0
ebarrier[ebarrier$pairsoc_sor%in%dfiEA$pair_soc,]$MCOSTe_rev<-0 ; ebarrier[ebarrier$pairsoc_sor%in%dfiEA$pair_soc,]$LCPe_rev<-0

tbarrier[tbarrier$pairsoc_sor%in%dfiEA$pair_soc,]$MCOSTt<-0 ; tbarrier[tbarrier$pairsoc_sor%in%dfiEA$pair_soc,]$LCPt<-0
tbarrier[tbarrier$pairsoc_sor%in%dfiEA$pair_soc,]$MCOSTt_rev<-0 ; tbarrier[tbarrier$pairsoc_sor%in%dfiEA$pair_soc,]$LCPt_rev<-0

xbarrier[xbarrier$pairsoc_sor%in%dfiEA$pair_soc,]$MCOSTx<-0 ; xbarrier[xbarrier$pairsoc_sor%in%dfiEA$pair_soc,]$LCPx<-0
xbarrier[xbarrier$pairsoc_sor%in%dfiEA$pair_soc,]$MCOSTx_rev<-0 ; xbarrier[xbarrier$pairsoc_sor%in%dfiEA$pair_soc,]$LCPx_rev<-0

# remove rest sPath_extentpoint
ebarrier<-ebarrier[!ebarrier$MCOSTe%in%"sPath_extentpoint",]
tbarrier<-tbarrier[!tbarrier$MCOSTt%in%"sPath_extentpoint",] 
xbarrier<-xbarrier[!xbarrier$MCOSTx%in%"sPath_extentpoint",] 

# prune all to c_socs for now
csocs<-intersect(ebarrier$pairsoc_sor,tbarrier$pairsoc_sor) # sor so that soc1_soc2 = soc2_soc1
csocs<-intersect(csocs, xbarrier$pairsoc_sor)
ebarrier<-ebarrier[ebarrier$pairsoc_sor%in%csocs,] ; ebarrier<-ebarrier[match(csocs, ebarrier$pairsoc_sor),]
tbarrier<-tbarrier[tbarrier$pairsoc_sor%in%csocs,] ; tbarrier<-tbarrier[match(csocs, tbarrier$pairsoc_sor),]
xbarrier<-xbarrier[xbarrier$pairsoc_sor%in%csocs,] ; xbarrier<-xbarrier[match(csocs, xbarrier$pairsoc_sor),]

plot(as.numeric(ebarrier$MCOSTe) ~ as.numeric(ebarrier$MCOSTe_rev)) # identical, same cost surface
plot(as.numeric(tbarrier$MCOSTt) ~ as.numeric(tbarrier$MCOSTt_rev)) # different
plot(as.numeric(xbarrier$MCOSTx) ~ as.numeric(xbarrier$MCOSTx_rev)) # different

# mean AB - BA
ebarrier$MCOST<-(as.numeric(ebarrier$MCOSTe) + as.numeric(ebarrier$MCOSTe_rev))/2
ebarrier$LCPl<-(as.numeric(ebarrier$LCPe) + as.numeric(ebarrier$LCPe_rev))/2
tbarrier$MCOST<-(as.numeric(tbarrier$MCOSTt) + as.numeric(tbarrier$MCOSTt_rev))/2
tbarrier$LCPl<-(as.numeric(tbarrier$LCPt) + as.numeric(tbarrier$LCPt_rev))/2
xbarrier$MCOST<-(as.numeric(xbarrier$MCOSTx) + as.numeric(xbarrier$MCOSTx_rev))/2
xbarrier$LCPl<-(as.numeric(xbarrier$LCPx) + as.numeric(xbarrier$LCPx_rev))/2

par(mfrow=c(1,3)) # log transformation produces ND
hist(ebarrier$MCOST) ; hist(log(ebarrier$MCOST+1)) ; hist(sqrt(ebarrier$MCOST))
hist(tbarrier$MCOST) ; hist(log(tbarrier$MCOST+1)) ; hist(sqrt(tbarrier$MCOST))
hist(xbarrier$MCOST) ; hist(log(xbarrier$MCOST+1)) ; hist(sqrt(xbarrier$MCOST))
# same with plen (see below)

# things are somewhat correlated
par(mfrow=c(2,3)) 
plot(log(tbarrier$MCOST+1)~log(xbarrier$MCOST+1))
plot(log(tbarrier$LCPl+1)~log(xbarrier$LCPl+1))
plot(log(tbarrier$MCOST+1)~log(ebarrier$MCOST+1))
plot(log(xbarrier$MCOST+1)~log(ebarrier$MCOST+1))
plot(log(ebarrier$MCOST+1)~log(xbarrier$MCOST+1))


# final df
dfP1barriers<-as.data.frame(cbind(ebarrier$Soci,ebarrier$SocNB,ebarrier$pairsoc_sor,ebarrier$pairsoc_unsor,
                        ebarrier$MCOST,ebarrier$LCPl,
                        tbarrier$MCOST,tbarrier$LCPl,
                        xbarrier$MCOST,xbarrier$LCPl))
colnames(dfP1barriers)<-c("Soci","SocNB","pairsoc_sor","pairsoc_unsor","MCOSTe","LCPle","MCOSTt","LCPlt","MCOSTx","LCPlx")
write.csv(dfP1barriers, file="data/Re_analysisGEbarriers/P1dfs/dfP1barriers.csv")    



# log plen
read.csv(file="data/Re_analysisGEbarriers/P1dfs/dfP1barriers.csv", stringsAsFactors = )->ebar
par(mfrow=c(1,3)) # log transformation produces ND
hist(ebar$LCPle) ; hist(log(ebar$LCPle+1)) ; hist(sqrt(ebar$LCPle))
hist(ebar$LCPlt) ; hist(log(ebar$LCPlt+1)) ; hist(sqrt(ebar$LCPlt))
hist(ebar$LCPlx) ; hist(log(ebar$LCPlx+1)) ; hist(sqrt(ebar$LCPlx))
