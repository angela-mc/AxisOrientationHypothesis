
rm(list=ls())

read.csv("data/latlong_soc_from_dplace-societies-2018-03-16.csv", stringsAsFactors = F)-> latlong_soc
read.csv("data/EA/EA/variables.csv", stringsAsFactors = F)-> vars
read.csv("data/EA/EA/codes_wallnewcodes.csv", stringsAsFactors = F)-> codes
read.csv("data/EA/EA/data_wallnewcodes.csv", stringsAsFactors = F)-> data
read.csv("data/EA/EA/societies.csv", stringsAsFactors = F)-> soc


library(pscl)
library(rcompanion)
library(DescTools)
library(corrplot)
library(rcompanion)
library(factoextra)
library(stats)

read.csv("data/newcodesFINAL.csv", stringsAsFactors = F)->newcodes
unique(newcodes$Trait)->alltraits
traits_of_interest<-list() # which traits can we consider?

for(i in 1: length(alltraits))
  {ctrait<-alltraits[i]
  if(sum(!is.na(newcodes[newcodes$Trait%in%ctrait,]$New.code))>0) traits_of_interest<-c(traits_of_interest, ctrait)}
traits_of_interest<-unlist(traits_of_interest)

library(ggplot2)
library(dplyr) # easier data wrangling
library(viridis) # colour blind friendly palette, works in B&W also
library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr)

source("scripts/10_rLCP_plotfiguresFXN.R")
coldown="dodgerblue2"
colup="firebrick2"

# read tables - ALL THESE HAVE _newtree extension - as they are built on the newest tree version 
  # internal functions - adjsut pvalues
read.csv(file="data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_Jager_newtree/dfm1.csv", stringsAsFactors = F)->dfm1
read.csv(file="data/Re_analysisGEbarriers/P1_datamodel/glmerRCA_Jager_newtree/dfm2.csv", stringsAsFactors = F)->dfm2

dtrial1<-calcSElines_newtree(dfm1, name_folder="nbhoodV1",coldown = coldown, colup=colup) # here calc error bars
dtrial2<-calcSElines_newtree(dfm2, name_folder="nbhoodV210",coldown = coldown, colup=colup) # here calc error bars

# adjust pvalues, add sign colour, add trait category - THESE ARE JUST CATEGORIES OF THE TRAITS I HAVE, re-labbeled for grouping
dtrial1<-df_moreadds(dtrial1, name_folder="nbhoodV1",method_adj="fdr",vars=vars,coldown = coldown, colup=colup) 
dtrial2<-df_moreadds(dtrial2, name_folder="nbhoodV210",method_adj="fdr",vars=vars,coldown = coldown, colup=colup) 

# working df: dftrial1 or dtrial2
workingdf<-dtrial1 ; name_folder<-"nbhoodV1"
workingdf<-dtrial2 ; name_folder<-"nbhoodV210"

### ------------------------------------------------------------------------------------------------------------------------------------------

# coefficient ranges
range(workingdf$GeoD_LCPsl_MCOSTe)
range(workingdf$XericD_MCOSTx)
range(workingdf$TempD_MCOSTt)
range(workingdf$PhyD)
range(workingdf$Nbhood) # much more positive

min<-min(c(workingdf$GeoD_LCPsl_MCOSTe,workingdf$XericD_MCOSTx,workingdf$TempD_MCOSTt,
           workingdf$PhyD,workingdf$Nbhood))
max<-max(c(workingdf$GeoD_LCPsl_MCOSTe,workingdf$XericD_MCOSTx,workingdf$TempD_MCOSTt,
           workingdf$PhyD,workingdf$Nbhood))

hist(workingdf$Nbhood, xlim=c( (min-0.5),(max+0.5)), ylim=c(0,20), col=scales::alpha("magenta",0.5))
hist(workingdf$PhyD, add=T, col=scales::alpha("greenyellow",0.5))
hist(workingdf$GeoD_LCPsl_MCOSTe, add=T, col=scales::alpha("blueviolet",0.5))
hist(workingdf$XericD_MCOSTx, add=T, col=scales::alpha("gold1",0.5))
hist(workingdf$TempD_MCOSTt, add=T, col=scales::alpha("red4",0.5))

### ------------------------------------------------------------------------------------------------------------------------------------------


#################
### PLOTTING ####
#################

# merge the one trait in games into ritual - in the figure, category is labelled Ritual, Games
if( "Games" %in%workingdf$TraitCategory) workingdf[workingdf$TraitCategory%in%"Games",]$TraitCategory<-"Ritual"

# order df by category,  leave 1 empty for category breakup
df <- workingdf[order(workingdf$TraitCategory),]
df[1,]->emptydf ; emptydf$signGeoD_LCPsl_MCOSTe<-"white" ; emptydf$signXericD_MCOSTx<-"white" ; emptydf$signTempD_MCOSTt<-"white"
emptydf$signNbhood<-"white" ; emptydf$signPhyD<-"white" ;

# oder of categories
table(df$TraitCategory)
df<-rbind(df[df$TraitCategory%in%"Subsistence",],emptydf,
          df[df$TraitCategory%in%"Housing ecology",],emptydf,
          df[df$TraitCategory%in%"Property",],emptydf,
          df[df$TraitCategory%in%"Marriage, Kinship",],emptydf,
          df[df$TraitCategory%in%"Community organization",],emptydf,
          df[df$TraitCategory%in%"Politics, Class",],emptydf,
          df[df$TraitCategory%in%"Labour",],emptydf,
          df[df$TraitCategory%in%"Ritual",])
workingdf<-df
workingdf$xpred<-seq(from=1, to=length(workingdf[,1]), by=1)

workingdf$Name_tr # useful for looking at which trait do what
table(workingdf$TraitCategory)

# empty plot: GeoD_LCPsl_MCOSTe
pdf(paste0("manuscript/2PNASSub/newtree/Fig1_",name_folder,"_GeoD_LCPsl_MCOSTe.pdf"), height = 4, width = 7)

par(mar=c(1,7,2,2))
# coeff ranges for the geobarriers PCs'
minGB<-min(c(workingdf$GeoD_LCPsl_MCOSTe, workingdf$XericD_MCOSTx, workingdf$TempD_MCOSTt))-0.4
maxGB<-max(c(workingdf$GeoD_LCPsl_MCOSTe, workingdf$XericD_MCOSTx, workingdf$TempD_MCOSTt))+0.2

plot(workingdf$GeoD_LCPsl_MCOSTe~workingdf$xpred, col="white", ylab="Topographic travel costs\n & climate-related path lengths",yaxt="n", xlab="", xaxt="n", 
     ylim=c( minGB, maxGB), cex.lab=1.5, cex.axis=1) # empty plot
points (workingdf$GeoD_LCPsl_MCOSTe ~ workingdf$xpred, pch=18, col=workingdf$signGeoD_LCPsl_MCOSTe, cex=1.5)

workingdf[workingdf$signGeoD_LCPsl_MCOSTe%in%"white",]$xpred
abline(v=c(workingdf[workingdf$signGeoD_LCPsl_MCOSTe%in%"white",]$xpred), lty=3, col="gray")
abline(h=0, lty=2, col="black")

axis(side=2, at=c(-1,-0.5,0,0.5), labels = c("-1","-0.5","0","0.5"), tick=F,cex.axis=1.3)
mtext("A", side=3, adj=0, font=2,cex=1.5, line=+0.5)#ps=9,

# add error bars
for( j in 1: length(workingdf$GeoD_LCPsl_MCOSTe))
  if(!workingdf$signGeoD_LCPsl_MCOSTe[j]%in%"white")
  {arrows(x0= workingdf$xpred[j], x1 = workingdf$xpred[j], 
          y0 = workingdf$GeoD_LCPsl_MCOSTe[j] - workingdf$SEGeoD_LCPsl_MCOSTe[j],
          y1=, workingdf$GeoD_LCPsl_MCOSTe[j] + workingdf$SEGeoD_LCPsl_MCOSTe[j],code=3, angle=90, length=0, col=workingdf$signGeoD_LCPsl_MCOSTe[j])}
if(name_folder%in%"nbhoodV1"){
  text(y=rep(-1.4, 8),x=c(2,  8,  13.5, 26,  37,  41.5,  49,  57), 
       #labels=c("a","b","c",   "d", "e",  "f",    "g","h"),cex=1)
       labels=c("1","2","3",   "4", "5",  "6",    "7","8"),cex=1)
  
}
if(name_folder%in%"nbhoodV210"){
  
text(y=rep(-1.4, 8),x=c(2,  8,  13.5, 26,  37,  40.5,  47,  53.5), 
     #labels=c("a","b","c",   "d", "e",  "f",    "g","h"),cex=1)
     labels=c("1","2","3",   "4", "5",  "6",    "7","8"),cex=1)
}

dev.off()

# empty plot: TempD_MCOSTt
pdf(paste0("manuscript/2PNASSub/newtree/Fig1_",name_folder,"_TempD_MCOSTt.pdf"), height = 4, width = 7)
par(mar=c(1,7,2,2))
# coeff ranges for the geobarriers PCs'
minGB<-min(c(workingdf$GeoD_LCPsl_MCOSTe, workingdf$XericD_MCOSTx, workingdf$TempD_MCOSTt))-0.4
maxGB<-max(c(workingdf$GeoD_LCPsl_MCOSTe, workingdf$XericD_MCOSTx, workingdf$TempD_MCOSTt))+0.2

plot(workingdf$TempD_MCOSTt~workingdf$xpred, col="white", ylab="Temperature turnover",yaxt="n", xlab="", xaxt="n", 
     ylim=c( minGB, maxGB), cex.lab=1.5, cex.axis=1) # empty plot
points (workingdf$TempD_MCOSTt ~ workingdf$xpred, pch=18, col=workingdf$signTempD_MCOSTt, cex=1.5)

workingdf[workingdf$signTempD_MCOSTt%in%"white",]$xpred
abline(v=c(workingdf[workingdf$signTempD_MCOSTt%in%"white",]$xpred), lty=3, col="gray")
abline(h=0, lty=2, col="black")

axis(side=2, at=c(-1,-0.5,0,0.5), labels = c("-1","-0.5","0","0.5"), tick=F,cex.axis=1.3)
mtext("B", side=3, adj=0, font=2,cex=1.5, line=+0.5)#ps=9,

# add error bars
for( j in 1: length(workingdf$TempD_MCOSTt))
  if(!workingdf$signTempD_MCOSTt[j]%in%"white")
  {arrows(x0= workingdf$xpred[j], x1 = workingdf$xpred[j], 
          y0 = workingdf$TempD_MCOSTt[j] - workingdf$SETempD_MCOSTt[j],
          y1=, workingdf$TempD_MCOSTt[j] + workingdf$SETempD_MCOSTt[j],code=3, angle=90, length=0, col=workingdf$signTempD_MCOSTt[j])}
if(name_folder%in%"nbhoodV1"){
  text(y=rep(-1.4, 8),x=c(2,  8,  13.5, 26,  37,  41.5,  49,  57), 
       #labels=c("a","b","c",   "d", "e",  "f",    "g","h"),cex=1)
       labels=c("1","2","3",   "4", "5",  "6",    "7","8"),cex=1)
  
}
if(name_folder%in%"nbhoodV210"){
  
  text(y=rep(-1.4, 8),x=c(2,  8,  13.5, 26,  37,  40.5,  47,  53.5), 
       #labels=c("a","b","c",   "d", "e",  "f",    "g","h"),cex=1)
       labels=c("1","2","3",   "4", "5",  "6",    "7","8"),cex=1)
}
dev.off()


# empty plot: XericD_MCOSTx
pdf(paste0("manuscript/2PNASSub/newtree/Fig1_",name_folder,"XericD_MCOSTx.pdf"), height = 4, width = 7)
par(mar=c(1,7,2,2))
# coeff ranges for the geobarriers PCs'
minGB<-min(c(workingdf$GeoD_LCPsl_MCOSTe, workingdf$XericD_MCOSTx, workingdf$TempD_MCOSTt))-0.4
maxGB<-max(c(workingdf$GeoD_LCPsl_MCOSTe, workingdf$XericD_MCOSTx, workingdf$TempD_MCOSTt))+0.2

plot(workingdf$XericD_MCOSTx~workingdf$xpred, col="white", ylab="Aridity turnover",yaxt="n", xlab="", xaxt="n", 
     ylim=c( minGB, maxGB), cex.lab=1.5, cex.axis=1) # empty plot
points (workingdf$XericD_MCOSTx ~ workingdf$xpred, pch=18, col=workingdf$signXericD_MCOSTx, cex=1.5)

workingdf[workingdf$signXericD_MCOSTx%in%"white",]$xpred
abline(v=c(workingdf[workingdf$signXericD_MCOSTx%in%"white",]$xpred), lty=3, col="gray")
abline(h=0, lty=2, col="black")

# axis(side=1,at=c(2,8,15,27.5,39.5,46.5,54), labels=c("Subsistence","Housing,\nEcology","Community\norganization","Marriage,\nKinship",
#                                 "Property,\nEconomy","Labour,\nGender","Ritual"),tick=F)
axis(side=2, at=c(-1,-0.5,0,0.5), labels = c("-1","-0.5","0","0.5"), tick=F,cex.axis=1.3)
mtext("C", side=3, adj=0, font=2,cex=1.5, line=+0.5)#ps=9,

# add error bars
for( j in 1: length(workingdf$XericD_MCOSTx))
  if(!workingdf$signXericD_MCOSTx[j]%in%"white")
  {arrows(x0= workingdf$xpred[j], x1 = workingdf$xpred[j], 
          y0 = workingdf$XericD_MCOSTx[j] - workingdf$SEXericD_MCOSTx[j],
          y1=, workingdf$XericD_MCOSTx[j] + workingdf$SEXericD_MCOSTx[j],code=3, angle=90, length=0, col=workingdf$signXericD_MCOSTx[j])}

# add letters
if(name_folder%in%"nbhoodV1"){
  text(y=rep(-1.4, 8),x=c(2,  8,  13.5, 26,  37,  41.5,  49,  57), 
       #labels=c("a","b","c",   "d", "e",  "f",    "g","h"),cex=1)
       labels=c("1","2","3",   "4", "5",  "6",    "7","8"),cex=1)
  
}
if(name_folder%in%"nbhoodV210"){
  
  text(y=rep(-1.4, 8),x=c(2,  8,  13.5, 26,  37,  40.5,  47,  53.5), 
       #labels=c("a","b","c",   "d", "e",  "f",    "g","h"),cex=1)
       labels=c("1","2","3",   "4", "5",  "6",    "7","8"),cex=1)
}
dev.off()




# empty plot: PhyD
tit="D" #A or D
pdf(paste0("manuscript/2PNASSub/newtree/Fig1_",name_folder,"_phyd.pdf"), height = 4, width = 7)

par(mar=c(1,7,2,2))
# coeff ranges for the geobarriers PCs'
minGB<-min(c(workingdf$GeoD_LCPsl_MCOSTe, workingdf$XericD_MCOSTx, workingdf$TempD_MCOSTt))-0.4
maxGB<-max(c(workingdf$GeoD_LCPsl_MCOSTe, workingdf$XericD_MCOSTx, workingdf$TempD_MCOSTt))+0.2
range(workingdf$PhyD)
if(name_folder%in%"nbhoodV210"){
  minGB<-min(c(workingdf$GeoD_LCPsl_MCOSTe, workingdf$XericD_MCOSTx, workingdf$TempD_MCOSTt))-0.6
  maxGB<-max(c(workingdf$GeoD_LCPsl_MCOSTe, workingdf$XericD_MCOSTx, workingdf$TempD_MCOSTt))+0.5
  
}

plot(workingdf$PhyD~workingdf$xpred, col="white", ylab="Phylogenetic distance",yaxt="n", xlab="", xaxt="n", 
     ylim=c( minGB, maxGB), cex.lab=1.5, cex.axis=1) # empty plot
points (workingdf$PhyD ~ workingdf$xpred, pch=18, col=workingdf$signPhyD, cex=1.5)

workingdf[workingdf$signPhyD%in%"white",]$xpred
abline(v=c(workingdf[workingdf$signPhyD%in%"white",]$xpred), lty=3, col="gray")
abline(h=0, lty=2, col="black")

axis(side=2, at=c(-1,-0.5,0,0.5), labels = c("-1","-0.5","0","0.5"), tick=F,cex.axis=1.3)
mtext(tit, side=3, adj=0, font=2,cex=1.5, line=+0.5)#ps=9,

# add error bars
for( j in 1: length(workingdf$PhyD))
  if(!workingdf$signPhyD[j]%in%"white")
  {arrows(x0= workingdf$xpred[j], x1 = workingdf$xpred[j], 
          y0 = workingdf$PhyD[j] - workingdf$SEPhyD[j],
          y1=, workingdf$PhyD[j] + workingdf$SEPhyD[j],code=3, angle=90, length=0, col=workingdf$signPhyD[j])}
if(name_folder%in%"nbhoodV1"){
  text(y=rep(-1.4, 8),x=c(2,  8,  13.5, 26,  37,  41.5,  49,  57), 
       #labels=c("a","b","c",   "d", "e",  "f",    "g","h"),cex=1)
       labels=c("1","2","3",   "4", "5",  "6",    "7","8"),cex=1)
  
}
if(name_folder%in%"nbhoodV210"){
  
  text(y=rep(-1.4, 8),x=c(2,  8,  13.5, 26,  37,  40.5,  47,  53.5), 
       #labels=c("a","b","c",   "d", "e",  "f",    "g","h"),cex=1)
       labels=c("1","2","3",   "4", "5",  "6",    "7","8"),cex=1)
}
dev.off()



# empty plot: Nbhood
tit="E" # E or B
pdf(paste0("manuscript/2PNASSub/newtree/Fig1_",name_folder,"_Nbhood.pdf"), height = 4, width = 7)

par(mar=c(1,7,2,2))

plot(workingdf$Nbhood~workingdf$xpred, col="white", ylab="Strength of cultural diffusion",yaxt="n", xlab="", xaxt="n", 
     ylim=c(-1, max(workingdf$Nbhood)+2), cex.lab=1.5, cex.axis=1) # empty plot
points (workingdf$Nbhood ~ workingdf$xpred, pch=18, col=workingdf$signNbhood, cex=1.5)

workingdf[workingdf$signNbhood%in%"white",]$xpred
abline(v=c(workingdf[workingdf$signNbhood%in%"white",]$xpred), lty=3, col="gray")
abline(h=0, lty=2, col="black")

# axis(side=1,at=c(2,8,15,27.5,39.5,46.5,54), labels=c("Subsistence","Housing,\nEcology","Community\norganization","Marriage,\nKinship",
#                                 "Property,\nEconomy","Labour,\nGender","Ritual"),tick=F)
axis(side=2, at=c(0,1,2,3,4), labels = c("0","1","2","3","4"), tick=F,cex.axis=1.3)
mtext(tit, side=3, adj=0, font=2,cex=1.5, line=+0.5)#ps=9,

# add error bars
for( j in 1: length(workingdf$Nbhood))
  if(!workingdf$signNbhood[j]%in%"white")
  {arrows(x0= workingdf$xpred[j], x1 = workingdf$xpred[j], 
          y0 = workingdf$Nbhood[j] - workingdf$SENbhood[j],
          y1=, workingdf$Nbhood[j] + workingdf$SENbhood[j],code=3, angle=90, length=0, col=workingdf$signNbhood[j])}
if(name_folder%in%"nbhoodV1"){
  text(y=rep(-1.4, 8),x=c(2,  8,  13.5, 26,  37,  41.5,  49,  57), 
       #labels=c("a","b","c",   "d", "e",  "f",    "g","h"),cex=1)
       labels=c("1","2","3",   "4", "5",  "6",    "7","8"),cex=1)
  
}
if(name_folder%in%"nbhoodV210"){
  
  text(y=rep(-1, 8),x=c(2,  8,  13.5, 26,  37,  40.5,  47,  53.5), 
       #labels=c("a","b","c",   "d", "e",  "f",    "g","h"),cex=1)
       labels=c("1","2","3",   "4", "5",  "6",    "7","8"),cex=1)
}
dev.off()

workingdf$Name_tr # useful for looking at which trait do what
workingdf[workingdf$TempD_MCOSTt_pval<0.05 & workingdf$XericD_MCOSTx_pvalue<0.05,]$Name_tr

