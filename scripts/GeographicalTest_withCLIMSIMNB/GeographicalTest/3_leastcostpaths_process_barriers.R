
rm(list=ls())


######################
########### part 2 ### elevation
######################

rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)

# get pairs dfi0
read.csv("data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
load("data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0
load("data/Re_analysisGEbarriers/Elev_raster.rds")

soci1<-unique(dfi0$soci)
rm(dfE)
for(i in 1:length(soci1))
  {socO<-soci1[i]
  load(file=paste0("data/Re_analysisGEbarriers/P2_elevation0/elevation_", socO,".rds"))
  
  dfi<-data.frame("Soci"=rep(socO,length(result$namesDs)), "SocNB"=result$namesDs, "MCOSTe"=result$mcost, "LCPe"=result$plen)
  dfi$MCOSTe<-as.character(dfi$MCOSTe)
  dfi$LCPe<-as.character(dfi$LCPe)
  
  if(i==1) dfE<-dfi
  if(i>1) dfE<-rbind(dfE,dfi)
  print(i)
  
}


# get pairs dfi2500
load("data/originsagric/dfnb_100_threshold_2500_noislands.rda") # dfi2500
soci1<-unique(dfi2500$soci)

rm(dfErev)
for(i in 1:length(soci1))
  {socO<-soci1[i]
  load(file=paste0("data/Re_analysisGEbarriers/P2_elevation2500/elevation_", socO,".rds"))
  
  dfi<-data.frame("Soci"=rep(socO,length(result$namesDs)), "SocNB"=result$namesDs, "MCOSTe"=result$mcost, "LCPe"=result$plen)
  dfi$MCOSTe<-as.character(dfi$MCOSTe)
  dfi$LCPe<-as.character(dfi$LCPe)
  
  if(i==1) dfErev<-dfi
  if(i>1) dfErev<-rbind(dfErev,dfi)
  print(i)
  
}

sum(!complete.cases(dfErev)) # 0
sum(!complete.cases(dfE)) # 0

write.csv(dfE, file="data/Re_analysisGEbarriers/P2dfs/dfEbariers0.csv") # spath expoint not dealt with
write.csv(dfErev, file="data/Re_analysisGEbarriers/P2dfs/dfEbariers2500.csv")


######################
########### part 2 ### temperature
######################

rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)

# get pairs dfi0
read.csv("data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
load("data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0
load("data/Re_analysisGEbarriers/TempHarshness_raster.rds")

soci1<-as.character(unique(dfi0$soci))
rm(dfE)
for(i in 1:length(soci1))
  {socO<-soci1[i]
  if(paste0("temp_",socO,".rds")%in%list.files("data/Re_analysisGEbarriers/P2_temp0/"))
    {load(file=paste0("data/Re_analysisGEbarriers/P2_temp0/temp_", socO,".rds"))
    
     dfi<-data.frame("Soci"=rep(socO,length(result$namesDs)), "SocNB"=result$namesDs, "MCOSTt"=result$mcost[1,], "LCPt"=result$plen)
     dfi$MCOSTt<-as.character(dfi$MCOSTt)
     dfi$LCPt<-as.character(dfi$LCPt)
    
     if(i==1) dfE<-dfi # i=1 has temperature data
     if(i>1) dfE<-rbind(dfE,dfi)}
    print(i)
  
}


# get pairs dfi2500
load("data/originsagric/dfnb_100_threshold_2500_noislands.rda") # dfi2500

soci1<-as.character(unique(dfi2500$soci))

rm(dfErev)
for(i in 1:length(soci1))
  {socO<-soci1[i]
    if(paste0("temp_",socO,".rds")%in%list.files("data/Re_analysisGEbarriers/P2_temp2500/"))
    {load(file=paste0("data/Re_analysisGEbarriers/P2_temp2500/temp_", socO,".rds"))

      dfi<-data.frame("Soci"=rep(socO,length(result$namesDs)), "SocNB"=result$namesDs, "MCOSTt"=result$mcost[1,], "LCPt"=result$plen)
      dfi$MCOSTt<-as.character(dfi$MCOSTt)
      dfi$LCPt<-as.character(dfi$LCPt)
      
      if(i==1) dfErev<-dfi # i=1 has temperature data
      if(i>1) dfErev<-rbind(dfErev,dfi)}
  print(i)
  
}


sum(!complete.cases(dfErev)) # 0
sum(!complete.cases(dfE)) # 0

write.csv(dfE, file="data/Re_analysisGEbarriers/P2dfs/dfTempbariers0.csv") # spath expoint not dealt with
write.csv(dfErev, file="data/Re_analysisGEbarriers/P2dfs/dfTempbariers2500.csv")




######################
########### part 2 ### xeric
######################

rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)

# get pairs dfi0
read.csv("data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
load("data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0
load("data/Re_analysisGEbarriers/XericHarshness_raster.rds")

soci1<-as.character(unique(dfi0$soci))
rm(dfE)
for(i in 1:length(soci1))
  {socO<-soci1[i]
  if(paste0("xeric_",socO,".rds")%in%list.files("data/Re_analysisGEbarriers/P2_xeric0/"))
  {load(file=paste0("data/Re_analysisGEbarriers/P2_xeric0/xeric_", socO,".rds"))
    
    dfi<-data.frame("Soci"=rep(socO,length(result$namesDs)), "SocNB"=result$namesDs, "MCOSTx"=result$mcost[1,], "LCPx"=result$plen)
    dfi$MCOSTx<-as.character(dfi$MCOSTx)
    dfi$LCPx<-as.character(dfi$LCPx)
    
    if(i==1) dfE<-dfi # i=1 has xeric data
    if(i>1) dfE<-rbind(dfE,dfi)}
  print(i)
  
}


# get pairs dfi2500
load("data/originsagric/dfnb_100_threshold_2500_noislands.rda") # dfi2500
soci1<-as.character(unique(dfi2500$soci))

rm(dfErev)
for(i in 1:length(soci1))
  {socO<-soci1[i]
  if(paste0("xeric_",socO,".rds")%in%list.files("data/Re_analysisGEbarriers/P2_xeric2500/"))
    {load(file=paste0("data/Re_analysisGEbarriers/P2_xeric2500/xeric_", socO,".rds"))
      
      dfi<-data.frame("Soci"=rep(socO,length(result$namesDs)), "SocNB"=result$namesDs, "MCOSTx"=result$mcost[1,], "LCPx"=result$plen)
      dfi$MCOSTx<-as.character(dfi$MCOSTx)
      dfi$LCPx<-as.character(dfi$LCPx)
      
      if(i==1) dfErev<-dfi # i=1 has xeric data
      if(i>1) dfErev<-rbind(dfErev,dfi)}
  print(i)
  
}

sum(!complete.cases(dfErev)) # 0
sum(!complete.cases(dfE)) # 0

write.csv(dfE, file="data/Re_analysisGEbarriers/P2dfs/dfXericbariers0.csv") # spath expoint not dealt with
write.csv(dfErev, file="data/Re_analysisGEbarriers/P2dfs/dfXericbariers2500.csv")



######################
########### part 2 ### final df
######################

rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)

fxn_cat_sort<-function(x, output, col1, col2)
    { s1<-x[col1]
    s2<-x[col2]
    to_ret<-sort(c(s1,s2))
    to_ret<-paste(to_ret,collapse ="_")
    return(to_ret)
}


# dfi0
read.csv("data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
load("/data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0

read.csv(file="data/Re_analysisGEbarriers/P2dfs/dfEbariers0.csv", stringsAsFactors = F)->ebarrier
read.csv(file="data/Re_analysisGEbarriers/P2dfs/dfTempbariers0.csv", stringsAsFactors = F)->tbarrier
read.csv(file="data/Re_analysisGEbarriers/P2dfs/dfXericbariers0.csv", stringsAsFactors = F)->xbarrier

# add parisoc name
ebarrier$pairsoc_unsor<-paste(as.character(ebarrier$Soci), as.character(ebarrier$SocNB),sep="_")
tbarrier$pairsoc_unsor<-paste(as.character(tbarrier$Soci), as.character(tbarrier$SocNB),sep="_")
xbarrier$pairsoc_unsor<-paste(as.character(xbarrier$Soci), as.character(xbarrier$SocNB),sep="_")

ebarrier$pairsoc_sor<- unlist(apply(ebarrier,1,fxn_cat_sort, col1=which(colnames(ebarrier)%in%"Soci"),col2=which(colnames(ebarrier)%in%"SocNB")))
tbarrier$pairsoc_sor<- unlist(apply(tbarrier,1,fxn_cat_sort, col1=which(colnames(tbarrier)%in%"Soci"),col2=which(colnames(tbarrier)%in%"SocNB")))
xbarrier$pairsoc_sor<- unlist(apply(xbarrier,1,fxn_cat_sort, col1=which(colnames(xbarrier)%in%"Soci"),col2=which(colnames(xbarrier)%in%"SocNB")))

# add 0 to sPath_extentpoint if geod == 0 , use unsorted AB!=BA
dfi0<-dfi0[dfi0$dist==0,]
ebarrier[ebarrier$pairsoc_unsor%in%dfi0$pair_soc_unsorted,]$MCOSTe<-0 ; ebarrier[ebarrier$pairsoc_unsor%in%dfi0$pair_soc_unsorted,]$LCPe<-0
tbarrier[tbarrier$pairsoc_unsor%in%dfi0$pair_soc_unsorted,]$MCOSTt<-0 ; tbarrier[tbarrier$pairsoc_unsor%in%dfi0$pair_soc_unsorted,]$LCPt<-0
xbarrier[xbarrier$pairsoc_unsor%in%dfi0$pair_soc_unsorted,]$MCOSTx<-0 ; xbarrier[xbarrier$pairsoc_unsor%in%dfi0$pair_soc_unsorted,]$LCPx<-0

# remove rest sPath_extentpoint
ebarrier<-ebarrier[!ebarrier$MCOSTe%in%"sPath_extentpoint",]
tbarrier<-tbarrier[!tbarrier$MCOSTt%in%"sPath_extentpoint",] 
xbarrier<-xbarrier[!xbarrier$MCOSTx%in%"sPath_extentpoint",] 

# prune all to c_socs for now
csocs<-intersect(ebarrier$pairsoc_unsor,tbarrier$pairsoc_unsor) # unsor so that soc1_soc2 != soc2_soc1
csocs<-intersect(csocs, xbarrier$pairsoc_unsor)
ebarrier<-ebarrier[ebarrier$pairsoc_unsor%in%csocs,] ; ebarrier<-ebarrier[match(csocs, ebarrier$pairsoc_unsor),]
tbarrier<-tbarrier[tbarrier$pairsoc_unsor%in%csocs,] ; tbarrier<-tbarrier[match(csocs, tbarrier$pairsoc_unsor),]
xbarrier<-xbarrier[xbarrier$pairsoc_unsor%in%csocs,] ; xbarrier<-xbarrier[match(csocs, xbarrier$pairsoc_unsor),]

ebarrier$MCOSTe<-as.numeric(ebarrier$MCOSTe) ; ebarrier$LCPe<-as.numeric(ebarrier$LCPe)
tbarrier$MCOSTt<-as.numeric(tbarrier$MCOSTt) ; tbarrier$LCPt<-as.numeric(tbarrier$LCPt)
xbarrier$MCOSTx<-as.numeric(xbarrier$MCOSTx) ; xbarrier$LCPx<-as.numeric(xbarrier$LCPx)

par(mfrow=c(1,3)) # log transformation produces ND
hist(ebarrier$MCOSTe) ; hist(log(ebarrier$MCOSTe+1)) ; hist(sqrt(ebarrier$MCOSTe))
hist(tbarrier$MCOSTt) ; hist(log(tbarrier$MCOSTt+1)) ; hist(sqrt(tbarrier$MCOSTt))
hist(xbarrier$MCOSTx) ; hist(log(xbarrier$MCOSTx+1)) ; hist(sqrt(xbarrier$MCOSTx))

hist(ebarrier$LCPe) ; hist(log(ebarrier$LCPe+1)) ; hist(sqrt(ebarrier$LCPe)) # log plen
hist(tbarrier$LCPt) ; hist(log(tbarrier$LCPt+1)) ; hist(sqrt(tbarrier$LCPt))
hist(xbarrier$LCPx) ; hist(log(xbarrier$LCPx+1)) ; hist(sqrt(xbarrier$LCPx))


# final df
dfP2barriers<-as.data.frame(cbind(ebarrier$Soci,ebarrier$SocNB,ebarrier$pairsoc_sor,ebarrier$pairsoc_unsor,
                                  ebarrier$MCOSTe,ebarrier$LCPe,
                                  tbarrier$MCOSTt,tbarrier$LCPt,
                                  xbarrier$MCOSTx,xbarrier$LCPx))
colnames(dfP2barriers)<-c("Soci","SocNB","pairsoc_sor","pairsoc_unsor","MCOSTe","LCPe","MCOSTt","LCPt","MCOSTx","LCPx")
write.csv(dfP2barriers, file="data/Re_analysisGEbarriers/P2dfs/dfP2barriers0.csv")    


# dfi2500
read.csv("data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
load("data/originsagric/dfnb_100_threshold_2500_noislands.rda")
dfi0<-dfi2500 # easier for ex code
read.csv(file="data/Re_analysisGEbarriers/P2dfs/dfEbariers2500.csv", stringsAsFactors = F)->ebarrier
read.csv(file="data/Re_analysisGEbarriers/P2dfs/dfTempbariers2500.csv", stringsAsFactors = F)->tbarrier
read.csv(file="data/Re_analysisGEbarriers/P2dfs/dfXericbariers2500.csv", stringsAsFactors = F)->xbarrier

# add parisoc name
ebarrier$pairsoc_unsor<-paste(as.character(ebarrier$Soci), as.character(ebarrier$SocNB),sep="_")
tbarrier$pairsoc_unsor<-paste(as.character(tbarrier$Soci), as.character(tbarrier$SocNB),sep="_")
xbarrier$pairsoc_unsor<-paste(as.character(xbarrier$Soci), as.character(xbarrier$SocNB),sep="_")

ebarrier$pairsoc_sor<- unlist(apply(ebarrier,1,fxn_cat_sort, col1=which(colnames(ebarrier)%in%"Soci"),col2=which(colnames(ebarrier)%in%"SocNB")))
tbarrier$pairsoc_sor<- unlist(apply(tbarrier,1,fxn_cat_sort, col1=which(colnames(tbarrier)%in%"Soci"),col2=which(colnames(tbarrier)%in%"SocNB")))
xbarrier$pairsoc_sor<- unlist(apply(xbarrier,1,fxn_cat_sort, col1=which(colnames(xbarrier)%in%"Soci"),col2=which(colnames(xbarrier)%in%"SocNB")))

# add 0 to sPath_extentpoint if geod == 0 , use unsorted AB!=BA
  # none since this is df2500

# remove rest sPath_extentpoint
ebarrier<-ebarrier[!ebarrier$MCOSTe%in%"sPath_extentpoint",]
tbarrier<-tbarrier[!tbarrier$MCOSTt%in%"sPath_extentpoint",] 
xbarrier<-xbarrier[!xbarrier$MCOSTx%in%"sPath_extentpoint",] 

# prune all to c_socs for now
csocs<-intersect(ebarrier$pairsoc_unsor,tbarrier$pairsoc_unsor) # unsor so that soc1_soc2 != soc2_soc1
csocs<-intersect(csocs, xbarrier$pairsoc_unsor)
ebarrier<-ebarrier[ebarrier$pairsoc_unsor%in%csocs,] ; ebarrier<-ebarrier[match(csocs, ebarrier$pairsoc_unsor),]
tbarrier<-tbarrier[tbarrier$pairsoc_unsor%in%csocs,] ; tbarrier<-tbarrier[match(csocs, tbarrier$pairsoc_unsor),]
xbarrier<-xbarrier[xbarrier$pairsoc_unsor%in%csocs,] ; xbarrier<-xbarrier[match(csocs, xbarrier$pairsoc_unsor),]

ebarrier$MCOSTe<-as.numeric(ebarrier$MCOSTe) ; ebarrier$LCPe<-as.numeric(ebarrier$LCPe)
tbarrier$MCOSTt<-as.numeric(tbarrier$MCOSTt) ; tbarrier$LCPt<-as.numeric(tbarrier$LCPt)
xbarrier$MCOSTx<-as.numeric(xbarrier$MCOSTx) ; xbarrier$LCPx<-as.numeric(xbarrier$LCPx)

par(mfrow=c(1,3)) # log transformation produces ND
hist(ebarrier$MCOSTe) ; hist(log(ebarrier$MCOSTe+1)) ; hist(sqrt(ebarrier$MCOSTe))
hist(tbarrier$MCOSTt) ; hist(log(tbarrier$MCOSTt+1)) ; hist(sqrt(tbarrier$MCOSTt))
hist(xbarrier$MCOSTx) ; hist(log(xbarrier$MCOSTx+1)) ; hist(sqrt(xbarrier$MCOSTx))

hist(ebarrier$LCPe) ; hist(log(ebarrier$LCPe+1)) ; hist(sqrt(ebarrier$LCPe)) # log plen
hist(tbarrier$LCPt) ; hist(log(tbarrier$LCPt+1)) ; hist(sqrt(tbarrier$LCPt))
hist(xbarrier$LCPx) ; hist(log(xbarrier$LCPx+1)) ; hist(sqrt(xbarrier$LCPx))


# final df
dfP2barriers<-as.data.frame(cbind(ebarrier$Soci,ebarrier$SocNB,ebarrier$pairsoc_sor,ebarrier$pairsoc_unsor,
                                  ebarrier$MCOSTe,ebarrier$LCPe,
                                  tbarrier$MCOSTt,tbarrier$LCPt,
                                  xbarrier$MCOSTx,xbarrier$LCPx))
colnames(dfP2barriers)<-c("Soci","SocNB","pairsoc_sor","pairsoc_unsor","MCOSTe","LCPe","MCOSTt","LCPt","MCOSTx","LCPx")
write.csv(dfP2barriers, file="data/Re_analysisGEbarriers/P2dfs/dfP2barriers2500.csv")    

