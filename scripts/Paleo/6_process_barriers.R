rm(list=ls())

require(movecost)
require(sp)
require(raster)
require(gdistance)
source("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/scripts/10_rLCP_MCostFXN.R")

whichk<-"8k"
whichk<-"12k"
whichk<-"4k"

######################
########### part 2 ### elevation - this is the same
######################

read.csv(file="data/Re_analysisGEbarriers/P2dfs/dfEbariers0.csv", stringsAsFactors = F)->dfE # spath expoint not dealt with
read.csv(file="data/Re_analysisGEbarriers/P2dfs/dfEbariers2500.csv", stringsAsFactors = F)->dfErev
write.csv(dfE, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfEbariers0.csv")) 
write.csv(dfErev, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfEbariers2500.csv"))



######################
########### part 2 ### temperature
######################


# get pairs dfi0
read.csv("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0
load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/TempHarshness_raster.rds")

soci1<-as.character(unique(dfi0$soci))
rm(dfE)
for(i in 1:length(soci1))
  {socO<-soci1[i]
  if(paste0("temp_",socO,".rds")%in%list.files(paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/P2_temp0_", whichk,"/")))
    {load(file=paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/P2_temp0_", whichk,"/temp_", socO,".rds"))
      
      dfi<-data.frame("Soci"=rep(socO,length(result$namesDs)), "SocNB"=result$namesDs, "MCOSTt"=result$mcost[1,], "LCPt"=result$plen)
      dfi$MCOSTt<-as.character(dfi$MCOSTt)
      dfi$LCPt<-as.character(dfi$LCPt)
      
      if(i==1) dfE<-dfi # i=1 has temperature data
      if(i>1) dfE<-rbind(dfE,dfi)}
  print(i)
  
}


# get pairs dfi2500
load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_2500_noislands.rda") # dfi2500
soci1<-as.character(unique(dfi2500$soci))

rm(dfErev)
for(i in 1:length(soci1))
  {socO<-soci1[i]
  if(paste0("temp_",socO,".rds")%in%list.files(paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/P2_temp2500_", whichk,"/")))
    {load(file=paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/P2_temp2500_", whichk,"/temp_", socO,".rds"))
      
      dfi<-data.frame("Soci"=rep(socO,length(result$namesDs)), "SocNB"=result$namesDs, "MCOSTt"=result$mcost[1,], "LCPt"=result$plen)
      dfi$MCOSTt<-as.character(dfi$MCOSTt)
      dfi$LCPt<-as.character(dfi$LCPt)
      
      if(i==1) dfErev<-dfi # i=1 has temperature data
      if(i>1) dfErev<-rbind(dfErev,dfi)}
  print(i)
  
}


sum(!complete.cases(dfErev)) # 0
sum(!complete.cases(dfE)) # 0

write.csv(dfE, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfTempbariers0.csv")) # spath expoint not dealt with
write.csv(dfErev, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfTempbariers2500.csv"))




######################
########### part 2 ### xeric
######################


# get pairs dfi0
read.csv("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0
load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/XericHarshness_raster.rds")

soci1<-as.character(unique(dfi0$soci))
rm(dfE)
for(i in 1:length(soci1))
  {socO<-soci1[i] ## HERE THEY HAVE BEEN SAVED AS TEMP
  if(paste0("temp_",socO,".rds")%in%list.files(paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/P2_xeric0_", whichk,"/")))
    {load(file=paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/P2_xeric0_", whichk,"/temp_", socO,".rds"))
      
      dfi<-data.frame("Soci"=rep(socO,length(result$namesDs)), "SocNB"=result$namesDs, "MCOSTx"=result$mcost[1,], "LCPx"=result$plen)
      dfi$MCOSTx<-as.character(dfi$MCOSTx)
      dfi$LCPx<-as.character(dfi$LCPx)
      
      if(i==1) dfE<-dfi # i=1 has xeric data
      if(i>1) dfE<-rbind(dfE,dfi)}
  print(i)
  
}


# get pairs dfi2500
load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_2500_noislands.rda") # dfi2500
soci1<-as.character(unique(dfi2500$soci))

rm(dfErev)
for(i in 1:length(soci1))
  {socO<-soci1[i] ## HERE THEY HAVE BEEN SAVED AS TEMP
  if(paste0("temp_",socO,".rds")%in%list.files(paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/P2_xeric2500_", whichk,"/")))
    {load(file=paste0("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/Re_analysisGEbarriers/paleoclim2021/P2_xeric2500_", whichk,"/temp_", socO,".rds"))
      
      dfi<-data.frame("Soci"=rep(socO,length(result$namesDs)), "SocNB"=result$namesDs, "MCOSTx"=result$mcost[1,], "LCPx"=result$plen)
      dfi$MCOSTx<-as.character(dfi$MCOSTx)
      dfi$LCPx<-as.character(dfi$LCPx)
      
      if(i==1) dfErev<-dfi # i=1 has xeric data
      if(i>1) dfErev<-rbind(dfErev,dfi)}
  print(i)
  
}

sum(!complete.cases(dfErev)) # 0
sum(!complete.cases(dfE)) # 0

write.csv(dfE, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfXericbariers0.csv")) # spath expoint not dealt with
write.csv(dfErev, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfXericbariers2500.csv"))


######################
########### part 2 ### final df
######################


fxn_cat_sort<-function(x, output, col1, col2)
  { s1<-x[col1]
  s2<-x[col2]
  to_ret<-sort(c(s1,s2))
  to_ret<-paste(to_ret,collapse ="_")
  return(to_ret)
}


# dfi0
read.csv("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_0_noislands.rda") # dfi0

read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfEbariers0.csv"), stringsAsFactors = F)->ebarrier
read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfTempbariers0.csv"), stringsAsFactors = F)->tbarrier
read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfXericbariers0.csv"), stringsAsFactors = F)->xbarrier

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


# things are somewhat correlated - probably given correlation with geodistance
par(mfrow=c(2,3)) 
plot(log(tbarrier$MCOSTt+1)~log(xbarrier$MCOSTx+1))
plot(log(tbarrier$LCPt+1)~log(xbarrier$LCPx+1))
plot(log(tbarrier$MCOSTt+1)~log(ebarrier$MCOSTe+1))
plot(log(xbarrier$MCOSTx+1)~log(ebarrier$MCOSTe+1))
plot(log(ebarrier$MCOSTe+1)~log(xbarrier$MCOSTx+1)) 


# final df
dfP2barriers<-as.data.frame(cbind(ebarrier$Soci,ebarrier$SocNB,ebarrier$pairsoc_sor,ebarrier$pairsoc_unsor,
                                  ebarrier$MCOSTe,ebarrier$LCPe,
                                  tbarrier$MCOSTt,tbarrier$LCPt,
                                  xbarrier$MCOSTx,xbarrier$LCPx))
colnames(dfP2barriers)<-c("Soci","SocNB","pairsoc_sor","pairsoc_unsor","MCOSTe","LCPe","MCOSTt","LCPt","MCOSTx","LCPx")
write.csv(dfP2barriers, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfP2barriers0.csv") )   


# dfi2500
read.csv("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/glotto_coordLL_socs.csv", stringsAsFactors = F)-> glotto
load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_2500_noislands.rda")
dfi0<-dfi2500 # easier for ex code
read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfEbariers2500.csv"), stringsAsFactors = F)->ebarrier
read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfTempbariers2500.csv"), stringsAsFactors = F)->tbarrier
read.csv(file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfXericbariers2500.csv"), stringsAsFactors = F)->xbarrier

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


# things are somewhat correlated - probably given correlation with geodistance
par(mfrow=c(2,3)) 
plot(log(tbarrier$MCOSTt+1)~log(xbarrier$MCOSTx+1))
plot(log(tbarrier$LCPt+1)~log(xbarrier$LCPx+1))
plot(log(tbarrier$MCOSTt+1)~log(ebarrier$MCOSTe+1))
plot(log(xbarrier$MCOSTx+1)~log(ebarrier$MCOSTe+1))
plot(log(ebarrier$MCOSTe+1)~log(xbarrier$MCOSTx+1)) 


# final df
dfP2barriers<-as.data.frame(cbind(ebarrier$Soci,ebarrier$SocNB,ebarrier$pairsoc_sor,ebarrier$pairsoc_unsor,
                                  ebarrier$MCOSTe,ebarrier$LCPe,
                                  tbarrier$MCOSTt,tbarrier$LCPt,
                                  xbarrier$MCOSTx,xbarrier$LCPx))
colnames(dfP2barriers)<-c("Soci","SocNB","pairsoc_sor","pairsoc_unsor","MCOSTe","LCPe","MCOSTt","LCPt","MCOSTx","LCPx")
write.csv(dfP2barriers, file=paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfP2barriers2500.csv"))    


# ### costs/lat and lon
# dev.off()
# load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/predictors/preds3april/dfiEA_noislands.rds")
# dfiEA<-dfiEA[dfiEA$pair_soc%in%dfP1barriers$pairsoc_sor,]
# dfiEA<-dfiEA[match(dfP1barriers$pairsoc_sor, dfiEA$pair_soc),]
# plot(log(as.numeric(as.character(dfP1barriers$MCOSTe))+1)~dfiEA$mlat, ylab="log MCOSTe")
# plot(log(as.numeric(as.character(dfP1barriers$MCOSTe))+1)~dfiEA$mlong, ylab="log MCOSTe")
# plot(log(as.numeric(as.character(dfP1barriers$MCOSTt))+1)~dfiEA$mlat, ylab="log MCOSTt")
# plot(log(as.numeric(as.character(dfP1barriers$MCOSTt))+1)~dfiEA$mlong, ylab="log MCOSTt")
# plot(log(as.numeric(as.character(dfP1barriers$MCOSTx))+1)~dfiEA$mlat, ylab="log MCOSTx")
# plot(log(as.numeric(as.character(dfP1barriers$MCOSTx))+1)~dfiEA$mlong, ylab="log MCOSTx")



######################
########### part 2 ### average /origin
######################


read.csv(file="data/Re_analysisGEbarriers/P2dfs/dfP2barriers0.csv", stringsAsFactors = F)->dfB0
load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_0_noislands.rda") 

soci1<-unique(dfB0$Soci)
dfB0$oragr<--999
for(i in 1:length(soci1)) 
  {n<-length(dfB0[dfB0$Soci%in%soci1[i],]$oragr)
  dfB0[dfB0$Soci%in%soci1[i],]$oragr<-rep(dfi0[dfi0$soci%in%soci1[i],]$oragr[1], n)}
-999%in%dfB0$oragr # F : ok

allOR<-unique(dfB0$oragr) # 18
pdf(paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfP2barriers0_perO.pdf"), height = 10, width = 10)
par(mfrow=c(2,3))
for( i in 1:length(allOR))
  {hist(dfB0[dfB0$oragr%in%allOR[i],]$MCOSTe,main=allOR[i], xlab="MCOSTe")
    hist(dfB0[dfB0$oragr%in%allOR[i],]$MCOSTt,main=allOR[i], xlab="MCOSTt")
    hist(dfB0[dfB0$oragr%in%allOR[i],]$MCOSTx,main=allOR[i], xlab="MCOSTx")
    
    hist(dfB0[dfB0$oragr%in%allOR[i],]$LCPe,main=allOR[i], xlab="LCPe")
    hist(dfB0[dfB0$oragr%in%allOR[i],]$LCPt,main=allOR[i], xlab="LCPt")
    hist(dfB0[dfB0$oragr%in%allOR[i],]$LCPx,main=allOR[i], xlab="LCPx")
    
}
dev.off()

# yes but the question is: per society origin

pdf(paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfP2barriers0_perSO.pdf"), height = 20, width = 10)
par(mfrow=c(6,3))

soci1<-unique(dfB0$Soci)
for(i in 1:length(soci1))
{hist(dfB0[dfB0$Soci%in%soci1[i],]$MCOSTe,main=paste0(soci1[i]), xlab="MCOSTe")
  hist(dfB0[dfB0$Soci%in%soci1[i],]$MCOSTt,main=paste0(soci1[i]), xlab="MCOSTt")
  hist(dfB0[dfB0$Soci%in%soci1[i],]$MCOSTx,main=paste0(soci1[i]), xlab="MCOSTx")
  
  hist(dfB0[dfB0$Soci%in%soci1[i],]$LCPe,main=paste0(soci1[i]), xlab="LCPe")
  hist(dfB0[dfB0$Soci%in%soci1[i],]$LCPt,main=paste0(soci1[i]), xlab="LCPt")
  hist(dfB0[dfB0$Soci%in%soci1[i],]$LCPx,main=paste0(soci1[i]), xlab="LCPx")
  print(i)
}
dev.off()



# # average stuff - DID NOT RUN ON PALEOCLIM
# pdf("data/Re_analysisGEbarriers/P2dfs/average_barriers_example.pdf", width=15, height=7)
# par(mfrow=c(1,2))
# read.csv(file="data/Re_analysisGEbarriers/P2dfs/dfP2barriers0.csv", stringsAsFactors = F)->dfB0
# hist(dfB0[dfB0$Soci%in%"lafo1243",]$MCOSTx, main="society of origin lafo1243", xlab="MCOST_xeric close range")
# mean(dfB0[dfB0$Soci%in%"lafo1243",]$MCOSTx)
# median(dfB0[dfB0$Soci%in%"lafo1243",]$MCOSTx)
# abline(v=median(dfB0[dfB0$Soci%in%"lafo1243",]$MCOSTx), col="blue", lwd=2)
# abline(v=mean(dfB0[dfB0$Soci%in%"lafo1243",]$MCOSTx), col="red", lwd=2)
# legend(fill=c("blue", "red"), y=60, x=1.5e+05,legend=c("median = 26196", "mean = 47647"), bty = "n")
# 
# read.csv(file="data/Re_analysisGEbarriers/P2_datamodel/dfi0_PCA_nf4/dfB_PCA.csv", stringsAsFactors = F)->dfB0
# hist(dfB0[dfB0$Soci%in%"lafo1243",]$RC3, main="society of origin lafo1243", xlab="RC3: MCOSTx_XericD close range")
# mean(dfB0[dfB0$Soci%in%"lafo1243",]$RC3)
# median(dfB0[dfB0$Soci%in%"lafo1243",]$RC3)
# abline(v=median(dfB0[dfB0$Soci%in%"lafo1243",]$RC3), col="blue", lwd=2)
# abline(v=mean(dfB0[dfB0$Soci%in%"lafo1243",]$RC3), col="red", lwd=2)
# legend(fill=c("blue", "red"), y=25, x=0.5,legend=c("median = 0.31", "mean = 0.28"), bty = "n")
# dev.off()


# df2500
read.csv(file="data/Re_analysisGEbarriers/P2dfs/dfP2barriers2500.csv", stringsAsFactors = F)->dfB2500
load("/Users/Angela/Desktop/other/BOX_15march/GeogrAxes/data/originsagric/dfnb_100_threshold_2500_noislands.rda") 
dfi0<-dfi2500 # easier with code
dfB0<-dfB2500

soci1<-unique(dfB0$Soci)
dfB0$oragr<--999
for(i in 1:length(soci1)) 
  {n<-length(dfB0[dfB0$Soci%in%soci1[i],]$oragr)
  dfB0[dfB0$Soci%in%soci1[i],]$oragr<-rep(dfi0[dfi0$soci%in%soci1[i],]$oragr[1], n)}
-999%in%dfB0$oragr # F : ok

allOR<-unique(dfB0$oragr) # 18
pdf(paste0("data/Re_analysisGEbarriers/paleoclim2021/P2dfs_", whichk,"/dfP2barriers2500_perO.pdf"), height = 10, width = 10)
par(mfrow=c(2,3))
for( i in 1:length(allOR))
{hist(dfB0[dfB0$oragr%in%allOR[i],]$MCOSTe,main=allOR[i], xlab="MCOSTe")
  hist(dfB0[dfB0$oragr%in%allOR[i],]$MCOSTt,main=allOR[i], xlab="MCOSTt")
  hist(dfB0[dfB0$oragr%in%allOR[i],]$MCOSTx,main=allOR[i], xlab="MCOSTx")
  
  hist(dfB0[dfB0$oragr%in%allOR[i],]$LCPe,main=allOR[i], xlab="LCPe")
  hist(dfB0[dfB0$oragr%in%allOR[i],]$LCPt,main=allOR[i], xlab="LCPt")
  hist(dfB0[dfB0$oragr%in%allOR[i],]$LCPx,main=allOR[i], xlab="LCPx")
  
}
dev.off()


