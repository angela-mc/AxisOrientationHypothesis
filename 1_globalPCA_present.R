# Global geographic PCA of environmental conditions
#
# Carlos A. Botero
# Washington University

rm(list=ls())

require(EnvStats)
library(raster)

###########################################
# load geographic data

load(file="data/Re_analysisGEbarriers/MeanT_raster.rds")
load(file="data/Re_analysisGEbarriers/VarT_raster.rds")
load(file="data/Re_analysisGEbarriers/Pt_raster.rds")
load(file="data/Re_analysisGEbarriers/MeanP_raster.rds")
load(file="data/Re_analysisGEbarriers/CvP_raster.rds")
load(file="data/Re_analysisGEbarriers/Pp_raster.rds")


# note we are using variance for temperature and CV for precipitation
mydata <- as.data.frame(cbind(values(MeanT_raster), values(VarT_raster), values(Pt_raster),
                              values(MeanP_raster), values(CvP_raster), values(Pp_raster)))
names(mydata) <- c('MeanT', 'VarT', 'Pt', 'MeanP', 'CvP', 'Pp')
mydata <- na.omit(mydata)

# intialize rasters for storage
XericHarshness_raster <- TempHarshness_raster <- Pt_raster

rm(list = ls()[-which(ls() %in% c('mydata', 'XericHarshness_raster', 'TempHarshness_raster'))])

# transform variables to approximate normality (when necessary), 
# center and scale (use same labels for convenience)

###########################################
# Helper function to simplify Box-Cox transformations
myBCtransform <- function(myvector) {
  # shift scale to positive numbers and identify optimal lambda for box-cox transformation
  mylambda <- boxcox(as.numeric(myvector)-min(as.numeric(myvector))+1, optimize = T)$lambda
  
  # transform
  myvector <- scale(boxcoxTransform(as.numeric(myvector)-min(as.numeric(myvector))+1, mylambda))
  return (scale(myvector))
}

# normalize and scale prior to PCA
mydata$MeanP <- myBCtransform(mydata$MeanP)
mydata$CvP <- myBCtransform(mydata$CvP)
mydata$Pp <- myBCtransform(mydata$Pp)
mydata$MeanT <- myBCtransform(mydata$MeanT)
mydata$VarT <- myBCtransform(mydata$VarT)
mydata$Pt <- myBCtransform(mydata$Pt)

# Estimate  Principal Components Analysis
require(psych)
myPCA <- principal(mydata[,c('MeanT','VarT','Pt', 'MeanP','CvP','Pp')], 
                   nfactors=2, rotate='varimax' )
myPCA

# put values into rasters so we can used them later
values(TempHarshness_raster)[as.numeric(row.names(mydata))] <- -1*myPCA$scores[,'RC1']
values(XericHarshness_raster)[as.numeric(row.names(mydata))] <- -1*myPCA$scores[,'RC2']

save(TempHarshness_raster,file="data/Re_analysisGEbarriers/TempHarshness_raster.rds") # values already with -1 etc
save(XericHarshness_raster,file="data/Re_analysisGEbarriers/XericHarshness_raster.rds") # values already with -1 etc
