#######################
#### cultural traits ##
#######################


#trait_of_interest<-"EA042"

extract_cultural_data<- function(trait_of_interest,codes, data, soc, vars)
    {trait_df<- data.frame("SocID"=soc$id, "SocNameID"=soc$pref_name_for_society,
                           "VarID"=rep(trait_of_interest, length(soc$id)),
                           "VarTitle"=vars[vars$id%in%trait_of_interest,]$title,
                           "VarCategory"=vars[vars$id%in%trait_of_interest,]$category,
                           "Code"=-999,"Code_name"=-999)
    # add code
    data[data$var_id%in%trait_of_interest,]-> dataT
    dataT<-dataT[match(trait_df$SocID, dataT$soc_id),]
    trait_df$Code<-dataT$code

    # add code name
    extract_code_name<-function(x, df=codes[codes$var_id%in%trait_of_interest,]) {return(df[df$code%in%x,]$name)}
    trait_df$Code_name<-unlist(lapply(trait_df$Code, extract_code_name))

    return(trait_df)
    }
# 
# 
# # share culture
# share_cul_matrix_fxn<-function(df)
#     {ndim<-length(df[,1])
#      share_matrix<-matrix(ncol=ndim, nrow = ndim)
#      rownames(share_matrix)<-df$SocID
#      colnames(share_matrix)<-df$SocID
#     
#      socs<-df$SocID
#      fxn_belong<-function(soc1,soc2, df) {rez<-ifelse(df[df$SocID%in%soc1,]$Code%in% df[df$SocID%in%soc2,]$Code,1,0)
#      return(rez)}
#      
#      for(i in 1: ndim)
#       {share_matrix[i,]<-unlist(lapply(socs,fxn_belong,soc1=rownames(share_matrix)[i], df=df))
#        print(i)}
#      
#      return(share_matrix)
#     }
# 
# 
## rownames are codes - faster
share_cul_matrix_fxn2<-function(df)
  {ndim<-length(df[,1])
  share_matrix<-matrix(ncol=ndim, nrow = ndim)
  rownames(share_matrix)<-df$Code
  colnames(share_matrix)<-df$Code

  fxn_belong<-function(x, code1) {rez<-ifelse(code1%in%x,1,0); return(rez)}
  codes2<-df$Code

  for(i in 1: ndim)
  {share_matrix[i,]<-unlist(lapply(codes2, fxn_belong,code1=rownames(share_matrix)[i]))
  print(i)}

  rownames(share_matrix)<-df$SocID
  colnames(share_matrix)<-df$SocID

  return(share_matrix)
  }

# potentially even faster: if rownames = 1 -copy cols, if not , reverse 0 with 1


#####################################
#### calculate geographic distance ##
#####################################


# distance between all pairs of societies - matrix
mdist_matrix_fxn<- function(method_dist,latlong_soc){
  mdist_matrix<-matrix(ncol=1291, nrow = 1291)
  rownames(mdist_matrix)<-latlong_soc$Society.id
  colnames(mdist_matrix)<-latlong_soc$Society.id

  # col names coords
  extract_coord_cols<-function(x) {return(c(latlong_soc[latlong_soc$Society.id%in%x,]$Revised.longitude, latlong_soc[latlong_soc$Society.id%in%x,]$Revised.latitude))}
  list_coords<-lapply(colnames(mdist_matrix), FUN=extract_coord_cols)

  for(i in 1: dim(mdist_matrix)[1])
  {
    soci<-rownames(mdist_matrix)[i]
    coordi<-list_coords[[i]]

    if(method_dist%in%"distCosine") mdist_matrix[i,]<-unlist(lapply(list_coords,distCosine,p1=coordi) )
    if(method_dist%in%"distHaversine") mdist_matrix[i,]<-unlist(lapply(list_coords,distHaversine,p1=coordi) )
    if(method_dist%in%"distVincentySphere") mdist_matrix[i,]<-unlist(lapply(list_coords,distVincentySphere,p1=coordi) )
    if(method_dist%in%"distVincentyEllipsoid") mdist_matrix[i,]<-unlist(lapply(list_coords,distVincentyEllipsoid,p1=coordi) )
    if(method_dist%in%"distGeo") mdist_matrix[i,]<-unlist(lapply(list_coords,distGeo,p1=coordi) )

    print(i)
  }
  return(mdist_matrix)}



##########################
#### get data for model ##
##########################

# get names (all names or a subset - ie each pair once)
extract_socid_model_vectors<- function (mat, subset=T)
  {soc2<-rep(rownames(mat), length(rownames(mat))) #columns
   fxn_soc1<- function(x, n) {rez<-rep(x,n); return(rez)}
   soc1<-unlist(lapply(rownames(mat), fxn_soc1, n=length(rownames(mat)))) # rows

   if(subset==T)
     {mat[lower.tri(mat, diag = T)]<-NA
      v1<-as.vector(t(mat)) # gets all values row by row
      soc1<-soc1[complete.cases(v1)] # gets rid of NA
      soc2<-soc2[complete.cases(v1)] # gets rid of NA
     }
     to_ret<-as.matrix(cbind(soc1, soc2)) # each row element with its column pair
   return(to_ret)

  }


# get values from matrices for pairwise societies
extract_variable_model_vectors<-function(mat, subset=T)
  {mat[lower.tri(mat, diag = T)]<-NA
   v2<-as.vector(t(mat)) # gets all values row by row
   v2<-v2[complete.cases(v2)] # gets rid of NA

   return(v2)
}
 
# raster_from_polig<-function(polygon, plot=F)
#   {r <- raster(ncol=180, nrow=180)
#   extent(r) <- extent(polygon)
#   rp <- rasterize(polygon, r)
#   if(plot==T) plot(rp,useRaster=T)
#   return(rp)
# }
# 
# coords_nas<-function(latlong_soc, rWorld, coords_all)
#   { #where are NAs given df & its coords + raster
#     latlong_soc[is.na(extract(x=rWorld, y=coords_all)),]-> nac
#     cy<-data.frame(lon=nac$Revised.longitude, lat=nac$Revised.latitude)
#     coordinates(cy)<-c("lon","lat")
#     return(cy)
# } 
# 
# 
# 
# library(geosphere)
# # distance matrix brute
# mdist_matrix_fxn_NB<- function(method_dist="distGeo",glotto_coordLL){
#   mdist_matrix<-matrix(ncol=length(glotto_coordLL[,1]), nrow = length(glotto_coordLL[!glotto_coordLL$EAsoc%in%"-999",] [,1]))
#   rownames(mdist_matrix)<-glotto_coordLL[!glotto_coordLL$EAsoc%in%"-999",]$EAsoc2
#   colnames(mdist_matrix)<-glotto_coordLL$EAsoc2
#   
#   # col names coords
#   extract_coord_cols<-function(x) {return(c(glotto_coordLL[glotto_coordLL$EAsoc2%in%x,]$EAlong, glotto_coordLL[glotto_coordLL$EAsoc2%in%x,]$EAlat))}
#   list_coords<-lapply(colnames(mdist_matrix), FUN=extract_coord_cols)
#   names(list_coords)<-colnames(mdist_matrix)
#   
#   for(i in 1: dim(mdist_matrix)[1])
#   {
#     soci<-rownames(mdist_matrix)[i]
#     coordi<-list_coords[names(list_coords)%in%soci][[1]]
#     
#     if(method_dist%in%"distCosine") mdist_matrix[i,]<-unlist(lapply(list_coords,distCosine,p1=coordi) )
#     if(method_dist%in%"distHaversine") mdist_matrix[i,]<-unlist(lapply(list_coords,distHaversine,p1=coordi) )
#     if(method_dist%in%"distVincentySphere") mdist_matrix[i,]<-unlist(lapply(list_coords,distVincentySphere,p1=coordi) )
#     if(method_dist%in%"distVincentyEllipsoid") mdist_matrix[i,]<-unlist(lapply(list_coords,distVincentyEllipsoid,p1=coordi) )
#     if(method_dist%in%"distGeo") mdist_matrix[i,]<-unlist(lapply(list_coords,distGeo,p1=coordi) )
#     
#     print(i)
#   }
#   return(mdist_matrix)}
# 
# 
# 
# # library(geosphere)
# # # distance matrix brute
# # mdist_matrix_fxn_NB<- function(method_dist="distGeo",glotto_coordLL){
# #   mdist_matrix<-matrix(ncol=length(glotto_coordLL[,1]), nrow = length(glotto_coordLL[!glotto_coordLL$EAsoc%in%"-999",] [,1]))
# #   rownames(mdist_matrix)<-glotto_coordLL[!glotto_coordLL$EAsoc%in%"-999",]$EAsoc2
# #   colnames(mdist_matrix)<-glotto_coordLL$EAsoc2
# #   
# #   # col names coords
# #   extract_coord_cols<-function(x) {return(c(glotto_coordLL[glotto_coordLL$EAsoc2%in%x,]$EAlong, glotto_coordLL[glotto_coordLL$EAsoc2%in%x,]$EAlat))}
# #   list_coords<-lapply(colnames(mdist_matrix), FUN=extract_coord_cols)
# #   
# #   
# #   df<-data.frame("Soc"=rownames(mdist_matrix), "Number"=(1: length( rownames(mdist_matrix) ) ) )
# #   df$Soc<-as.character(df$Soc)
# #   
# #   f = function(x, output,list_coords) {
# #     # x is the row of type Character
# #     index<-x[2]  # access element in second column
# #     coordi<-list_coords[[index]]
# #     soci<-x[1] #acces element in first column
# #     to_ret<-unlist(lapply(list_coords,distGeo,p1=coordi) )
# #     
# #     return(to_ret)
# #   }
# #   
# #   mdist_matrix<-apply(df[1:4,], 1, f, list_coords=list_coords)
# #   return(mdist_matrix)}
# # 
# # 
# 
# 
# # #####################################
# # #### calculate geographic distance ## between pairsocs
# # #####################################
# # 
# # 
# # # distance between all pairs of societies - matrix
# # mdist_matrix_fxn_PNB<- function(method_dist,dfnb){
# #   mdist_matrix<-matrix(ncol=length(rownames(dfnb)), nrow = length(rownames(dfnb)))
# #   rownames(mdist_matrix)<-dfnb$pair_soc
# #   colnames(mdist_matrix)<-dfnb$pair_soc
# #   
# #   # col names coords
# #   extract_coord_cols<-function(x) {return(c(dfnb[dfnb$pair_soc%in%x,]$mlong, dfnb[dfnb$pair_soc%in%x,]$mlat))}
# #   list_coords<-lapply(colnames(mdist_matrix), FUN=extract_coord_cols)
# #   
# #   for(i in 1: dim(mdist_matrix)[1])
# #   {
# #     psoci<-rownames(mdist_matrix)[i]
# #     coordi<-list_coords[[i]]
# #     
# #     if(method_dist%in%"distCosine") mdist_matrix[i,]<-unlist(lapply(list_coords,distCosine,p1=coordi) )
# #     if(method_dist%in%"distHaversine") mdist_matrix[i,]<-unlist(lapply(list_coords,distHaversine,p1=coordi) )
# #     if(method_dist%in%"distVincentySphere") mdist_matrix[i,]<-unlist(lapply(list_coords,distVincentySphere,p1=coordi) )
# #     if(method_dist%in%"distVincentyEllipsoid") mdist_matrix[i,]<-unlist(lapply(list_coords,distVincentyEllipsoid,p1=coordi) )
# #     if(method_dist%in%"distGeo") mdist_matrix[i,]<-unlist(lapply(list_coords,distGeo,p1=coordi) )
# #     
# #     print(i)
# #   }
# #   return(mdist_matrix)}
# 
# 
# #####################################
# #### calculate geographic distance ## between pairsocs MCLAPPLy
# #####################################
# 
# 
# # distance between all pairs of societies - matrix
# mdist_matrix_fxn_PNB<- function(method_dist,dfnb){
#  
#   # col names coords
#   extract_coord_cols<-function(x) {return(c(dfnb[dfnb$pair_soc%in%x,]$mlong, dfnb[dfnb$pair_soc%in%x,]$mlat))}
#   list_coords<-lapply(dfnb$pair_soc, FUN=extract_coord_cols)
#   
#   mclapply(1:length(dfnb$pair_soc), function(i)
#   {
#     psoci<-dfnb$pair_soc[i]
#     coordi<-list_coords[[i]]
# 
#     if(method_dist%in%"distGeo") to_save<-unlist(lapply(list_coords,distGeo,p1=coordi) )
#     save(to_save, file=paste0("data/predictors/preds3april/mpointsPNB/PNBRow_",psoci, ".rds"))
#     print(i)}, mc.cores = 20, mc.preschedule=F)
#    }
# 