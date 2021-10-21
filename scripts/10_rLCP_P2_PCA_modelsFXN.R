# 
# ##################################
# ######## ordered areas figure  ###
# ##################################
# 
# 
# find_order_barriers<-function(names_lvls,lvls, dfb0, whichRC, title, add_to_lim, add_to_axis, cexp="equal") # cexp = equal/SR 
# { 
#   means1<-numeric() ; sds1<-numeric() ; sr<-numeric()# mean & sd
#   for(j in 1:length(lvls))
#   {means1[j]<-mean(dfb0[dfb0$oragr%in%lvls[j],colnames(dfb0)%in%whichRC])
#   sds1[j]<-sd(dfb0[dfb0$oragr%in%lvls[j],colnames(dfb0)%in%whichRC]) 
#   sr[j]<-length(dfb0[dfb0$oragr%in%lvls[j],colnames(dfb0)%in%whichRC])}
#   
#   names(means1)<-lvls ; names(sds1)<-lvls ; names(sr)<-lvls
#   ordm<-names(means1)[order(means1)]
#   
#   noarea<-seq(from=1, to=length(lvls),by=1) ; names(noarea)<-lvls
#   noarea<-noarea[order(factor(names(noarea), levels =ordm))]
#   
#   
#   # matpval<-mats$matpval
#   # groups<-list()
#   # for(j in 1:length(ordm))
#   #   {groups[[j]]<-c(ordm[j], colnames(matpval)[ matpval[ which(rownames(matpval)%in%ordm[j]),]>0.05])}
#   # names(groups)<-ordm
#   
#   # plot
#   x<-seq(from=1, to=length(lvls),by=1)
#   y<- means1[order(means1)] # this is for ordered means
# 
#   cols_vplots<-c("#00A08A","#EBCC2A","deeppink","darkmagenta","blue3")
#   cols_vplots<-c("#5B1A18","darkgreen","goldenrod1","violetred","dodgerblue1")
#   cols<- c(rep(cols_vplots[1],7),rep(cols_vplots[2],4), rep(cols_vplots[3],3) , cols_vplots[4],cols_vplots[5])
#   names(cols)<-lvls
#   cols<-cols[order(factor(names(cols), levels =ordm))] # this is for ordered means
#   
#   sr<-sr[order(factor(names(sr), levels =ordm))]
#   scaledsr<-log(sr)+0.5
#   #scaledsr[!names(sr)%in%"West Africa T"]<- scaledsr[!names(sr)%in%"West Africa T"]+1
#   
#   rylim<-range(dfb0[,colnames(dfb0)%in%whichRC])
#   if(cexp=="equal") plot( y~x, col=cols, xlab="", ylab="", xaxt='n',yaxt='n', pch=18, cex=4.5,ylim=c(rylim[1]-add_to_lim, rylim[2]+add_to_lim))
#   if(cexp=="sr") plot( y~x, col=cols, xlab="", ylab="", xaxt='n',yaxt='n', pch=18, cex=scaledsr, ylim=c(rylim[1]-add_to_lim, rylim[2]+add_to_lim))
#   
#   mtext(side=2, text = title, line=+2.5, font=2,adj=0.5,cex=1.2) # x axis label
#   axis(side=2, at =  c(rylim[1]+add_to_axis,0, rylim[2]-add_to_axis), labels=c( round(rylim[1]+add_to_axis, digits=1),0, round(rylim[2]-add_to_axis, digits=1)), tick = F, cex.axis=1)
#   #abline(h=0, lty=2, col="gray")
#   
#   # add SD
#   sds1<-sds1[order(factor(names(sds1), levels =ordm))]
#   means1<-means1[order(factor(names(means1), levels =ordm))]
#   
#   
#   for(j in 1:length(sds1))
#   { arrows(x0= x[j], y0=means1[j], x1=x[j], y1= (means1[j]+ sds1[j]), angle = 180,col=cols[j])
#     arrows(x0= x[j], y0=means1[j], x1=x[j], y1= (means1[j]- sds1[j]), angle = 180,col=cols[j])}
#   
#   text(x=x, y=means1, labels=noarea, font=2,col="white")  
#   #return(groups)
# }
# 
# 
##################################
######## ordered areas figure  ### version non-ordered means
##################################

#names_lvls,lvls, dfb0
# whichRC="RC1"
# title="Topographic travel costs\n&climate-related path lengths"
# add_to_lim=0.7
# add_to_axis=0.5
# cexp = "equal"


find_order_barriers_venom<-function(names_lvls,lvls, dfb0, whichRC, title, add_to_lim1, add_to_lim2=add_to_lim1, add_to_axis, cexp="equal") # cexp = equal/SR
{
  means1<-numeric() ; sds1<-numeric() ; sr<-numeric()# mean & sd
  for(j in 1:length(lvls))
  {means1[j]<-mean(dfb0[dfb0$oragr%in%lvls[j],colnames(dfb0)%in%whichRC])
  sds1[j]<-sd(dfb0[dfb0$oragr%in%lvls[j],colnames(dfb0)%in%whichRC])
  sr[j]<-length(dfb0[dfb0$oragr%in%lvls[j],colnames(dfb0)%in%whichRC])}

  names(means1)<-lvls ; names(sds1)<-lvls ; names(sr)<-lvls
  ordm<-names(means1)[order(means1)]

  noarea<-seq(from=1, to=length(lvls),by=1) ; names(noarea)<-lvls
  #noarea<-noarea[order(factor(names(noarea), levels =ordm))] # for ordering


  # matpval<-mats$matpval
  # groups<-list()
  # for(j in 1:length(ordm))
  #   {groups[[j]]<-c(ordm[j], colnames(matpval)[ matpval[ which(rownames(matpval)%in%ordm[j]),]>0.05])}
  # names(groups)<-ordm

  # plot
  x<-seq(from=1, to=length(lvls),by=1)
  #y<- means1[order(means1)] # this is for ordered means
  y<- means1 # order of conts

  cols_vplots<-c("#00A08A","#EBCC2A","deeppink","darkmagenta","blue3")
  cols_vplots<-c("#5B1A18","darkgreen","goldenrod1","violetred","dodgerblue1")
  cols<- c(rep(cols_vplots[1],7),rep(cols_vplots[2],4), rep(cols_vplots[3],3) , cols_vplots[4],cols_vplots[5])
  names(cols)<-lvls
  #cols<-cols[order(factor(names(cols), levels =ordm))] # this is for ordered means

  sr<-sr[order(factor(names(sr), levels =ordm))]
  scaledsr<-log(sr)+0.5
  #scaledsr[!names(sr)%in%"West Africa T"]<- scaledsr[!names(sr)%in%"West Africa T"]+1

  rylim<-range(dfb0[,colnames(dfb0)%in%whichRC])
  if(cexp=="equal") plot( y~x, col=cols, xlab="", ylab="", xaxt='n',yaxt='n', pch=18, cex=4.5,ylim=c(rylim[1]-add_to_lim1, rylim[2]+add_to_lim2))
  if(cexp=="sr") plot( y~x, col=cols, xlab="", ylab="", xaxt='n',yaxt='n', pch=18, cex=scaledsr, ylim=c(rylim[1]-add_to_lim1, rylim[2]+add_to_lim2))

  mtext(side=2, text = title, line=+2.5, font=2,adj=0.5,cex=1.2) # x axis label
  axis(side=2, at =  c(rylim[1]+add_to_axis,0, rylim[2]-add_to_axis), labels=c( round(rylim[1]+add_to_axis, digits=1),0, round(rylim[2]-add_to_axis, digits=1)), tick = F, cex.axis=1)
  #abline(h=0, lty=2, col="gray")

  # add SD
  #sds1<-sds1[order(factor(names(sds1), levels =ordm))]
  #means1<-means1[order(factor(names(means1), levels =ordm))]


  for(j in 1:length(sds1))
  { arrows(x0= x[j], y0=means1[j], x1=x[j], y1= (means1[j]+ sds1[j]), angle = 180,col=cols[j])
    arrows(x0= x[j], y0=means1[j], x1=x[j], y1= (means1[j]- sds1[j]), angle = 180,col=cols[j])}

  text(x=x, y=means1, labels=noarea, font=2,col="white")
  #return(groups)
}

# 
# 
# ##################################
# ######## coef plots continents  ##
# ##################################
# 
# 
# cont_means_plots<-function(dfb0, whichRC,title, ordergr,ylim,add_to_lim,add_to_axis, mod1)
# { # get means & sd
#   means<-numeric() ; sds<-numeric()
#   for(i in 1:length(ordergr))
#   {means[i]<-mean(dfb0[dfb0$cont%in%ordergr[i],colnames(dfb0)%in%whichRC])
#   sds[i]<-sd(dfb0[dfb0$cont%in%ordergr[i],colnames(dfb0)%in%whichRC])
#   }
#   
#   # # plot
#   # x<-seq(from=1, to=length(ordergr),by=1)
#   # y<-means
#   # plot( y~x, col=cols_vplots, xlab="", ylab="", xaxt='n',yaxt='n', pch=18, cex=4,ylim=c(ylim[1]-add_to_lim, ylim[2]+add_to_lim))
#   # mtext(side=2, text = title, line=+2.5, font=2,adj=0.5,cex=1.5) # x axis label
#   # axis(side=2, at =  c(ylim[1]+add_to_axis,0, ylim[2]-add_to_axis), labels=c( round(ylim[1]+add_to_axis),0, round(ylim[2]-add_to_axis)), tick = F, cex.axis=1)
#   # abline(h=means[1], lty=2, col="gray")
#   # 
#   # # add SD
#   # for(j in 1:length(sds))
#   # { arrows(x0= x[j], y0=means[j], x1=x[j], y1= (means[j]+ sds[j]), angle = 180,col="gray75")
#   #   arrows(x0= x[j], y0=means[j], x1=x[j], y1= (means[j]- sds[j]), angle = 180,col="gray75")}
#   
#   
#   # # vertical plot
#   # y<-seq(from=1, to=length(ordergr),by=1)
#   # x<-means
#   # summary(mod1)->sum
#   # sumc<-sum$coefficients
#   # selr<-which(rownames(sumc)%in%c("contFAfrica","contFSouthAmerica","contFMesoamerica","contFNorthAmerica"))
#   # estimates<-sumc[selr,1]
#   # 
#   # plot(y~x, xlab="", ylab="", xaxt='n',yaxt='n', pch=18, cex=2, xlim=c(ylim[1]-add_to_lim, ylim[2]+add_to_lim), ylim=c(0.5,5.5),col=cols_vplots)
#   # abline(v=means[1], lty=2, col="gray")
#   # # add SD
#   # for(j in 1:length(sds))
#   # { arrows(x0= x[j], y0=y[j], x1=x[j]+sds[j], y1= y[j], angle = 180,col="gray75")
#   #   arrows(x0= x[j], y0=y[j], x1=x[j]-sds[j], y1= y[j], angle = 180,col="gray75")}
#   
#   
#   # plot of effects
#   summary(mod1)->sum
#   sumc<-sum$coefficients
#   selr<-which(rownames(sumc)%in%c("contFAfrica","contFSouthAmerica","contFMesoamerica","contFNorthAmerica")) # don t need Asia = level of comparisons
#   estimates<-sumc[selr,1]
#   estimatesER<-sumc[selr,2]
#   colsplot<-cols_vplots[2:length(cols_vplots)] # not Asia
#   colsplot [sumc[selr,4]>0.05]<-"gray82"
#   y<-seq(from=1, to=length(estimates),by=1)
#   x<-estimates
#   
#   xrange<-c(range(estimates)[1]-add_to_lim, range(estimates)[2]+add_to_lim)
#   if(xrange[1]>= (-0.5)) xrange[1]<-(-0.5)
#   if(xrange[2]<=0.5) xrange[2]<-0.5
#   
#   plot(y~x, xlab="", ylab="", xaxt='n',yaxt='n', pch=18, cex=3, xlim=xrange,  ylim=c(0.5,4.5),col=colsplot) 
#   abline(v=0, lty=2, col="gray")
#   # add SD
#   for(j in 1:length(estimates))
#   { arrows(x0= estimates[j], y0=y[j], x1=estimates[j]+estimatesER[j], y1= y[j], angle = 180,col=colsplot[j])
#     arrows(x0=estimates[j], y0=y[j], x1=estimates[j]-estimatesER[j], y1= y[j], angle = 180,col=colsplot[j])}
#   # mtext(side=2, text = title, line=+2.5, font=2,adj=0.5,cex=1) # x axis label
#   mtext(side=3, text = title, line=+1, font=2,adj=0.5,cex=1) # x axis label
#   
#   if( abs(xrange[1]) < abs(xrange[2]) ) xrange[2]=abs(xrange[1])
#   if( abs(xrange[2]) < abs(xrange[1]) ) xrange[1]=(-1)* abs(xrange[2])
#   
#   if(xrange[2]>=1.5) xlimup<-1.5
#   if(xrange[2]>=1 & xrange[2]<1.5) xlimup<-1
#   if(xrange[2]>=0.5 & xrange[2]<1) xlimup<-0.5
#   
#   if(xrange[1]<=(-1.5)) xlimdown<-(-1.5)
#   if(xrange[1]>(-1.5) & xrange[1]<=(-1) ) xlimdown<- (-1)
#   if(xrange[1]>(-1) & xrange[1]<=(-0.5) ) xlimdown<- (-0.5)
#   
#   #axis(side=1, at =  c(range(estimates)[1]+add_to_axis, 0,range(estimates)[2]-add_to_axis), labels=c(round(range(estimates)[1]+add_to_axis, digits=1),0, round(range(estimates)[2]-add_to_axis, digits=1)), tick = F, cex.axis=2)
#   axis(side=1, at =  c(xlimdown, 0,xlimup), labels=c(xlimdown, 0,xlimup), tick = F, cex.axis=1.5)
#   
# }
# 
# 
# cont_means_plots2<-function(dfb0, whichRC,title, ordergr1,ordergr2,ylim,add_to_lim,add_to_axis, mod12, mod122)
# { # get means & sd
#   means<-numeric() ; sds<-numeric()
#   for(i in 1:length(ordergr1))
#   {means[i]<-mean(dfb0[dfb0$cont2%in%ordergr1[i],colnames(dfb0)%in%whichRC])
#   sds[i]<-sd(dfb0[dfb0$cont2%in%ordergr1[i],colnames(dfb0)%in%whichRC])
#   }
#   
#   # plot of effects mod12
#   summary(mod12)->sum12
#   sum12<-sum12$coefficients
#   selr<-which(rownames(sum12)%in%c("cont2FAfrica","cont2FSouthAmerica","cont2FMesoamerica","cont2FNorthAmerica")) # don t need Asia = level of comparisons
#   estimates1<-sum12[selr,1]
#   estimatesER1<-sum12[selr,2]
#   colsplot<-cols_vplots[2:length(cols_vplots)] # not Asia
#   colsplot [sum12[selr,4]>0.05]<-"gray82"
#   y1<-seq(from=1, to=length(estimates1),by=1)
#   y1<-y1-0.1
#   x1<-estimates1
#   
#   # plot of effects mod122
#   summary(mod122)->sum122
#   sum122<-sum122$coefficients
#   selr<-which(rownames(sum122)%in%c("cont2F2Africa","cont2F2SouthAmerica","cont2F2Mesoamerica","cont2F2NorthAmerica")) # don t need Asia = level of comparisons
#   estimates2<-sum122[selr,1]
#   estimatesER2<-sum122[selr,2]
#   colsplot<-cols_vplots[2:length(cols_vplots)] # not Asia
#   colsplot [sum122[selr,4]>0.05]<-"gray82"
#   y2<-seq(from=1, to=length(estimates2),by=1)
#   y2<-y2+0.1
#   x2<-estimates2
#   
#   xlimtotal<-c(min(range(estimates1)[1],range(estimates2)[1])-add_to_lim, max(range(estimates1)[2],range(estimates2)[2])+add_to_lim)
#   
#   plot(y1~x1, xlab="", ylab="", xaxt='n',yaxt='n', pch=18, cex=2.5, xlim=xlimtotal,  ylim=c(0.5,4.5),col=colsplot) 
#   abline(v=0, lty=2, col="gray")
#   # add SD
#   for(j in 1:length(estimates1))
#   { arrows(x0= estimates1[j], y0=y1[j], x1=estimates1[j]+estimatesER1[j], y1= y1[j], angle = 180,col="gray75")
#     arrows(x0=estimates1[j], y0=y1[j], x1=estimates1[j]-estimatesER1[j], y1= y1[j], angle = 180,col="gray75")}
#   #mtext(side=2, text = title, line=+2.5, font=2,adj=0.5,cex=1) # x axis label
#   mtext(side=3, text = title, line=+1, font=2,adj=0.5,cex=1) # x axis label
#   
#   axis(side=1, at =  c(xlimtotal[1]+add_to_axis, 0,xlimtotal[2]-add_to_axis), labels=c(round(xlimtotal[1]+add_to_axis, digits=1),0, round(xlimtotal[2]-add_to_axis, digits=1)), tick = F, cex.axis=1)
#   
#   
#   points(y2~x2, pch=18, cex=2.5,col=colsplot) 
#   abline(v=0, lty=2, col="gray")
#   # add SD
#   for(j in 1:length(estimates2))
#   { arrows(x0= estimates2[j], y0=y2[j], x1=estimates2[j]+estimatesER2[j], y1= y2[j], angle = 180,col="gray75")
#     arrows(x0=estimates2[j], y0=y2[j], x1=estimates2[j]-estimatesER2[j], y1= y2[j], angle = 180,col="gray75")}
#   
#   
# }
# 
# 
# ##########################
# ####### matrix per area ##
# ##########################
# 
# library(BAMMtools)
# library(viridis)
# 
# matFigure_diffTukey_allcomp_halfMordered<-function(names_lvls,lvls, dfb0,whichRC,dfres,title)
# {# matrixes figure differences
#   
#   # means for order
#   means1<-numeric() ; sr<-numeric()# mean & sd
#   for(j in 1:length(lvls))
#   {means1[j]<-mean(dfb0[dfb0$oragr%in%lvls[j],colnames(dfb0)%in%whichRC])
#    sr[j]<-length(dfb0[dfb0$oragr%in%lvls[j],colnames(dfb0)%in%whichRC])}
#   
#   names(means1)<-lvls ; names(sr)<-lvls
#   ordm<-names(means1)[order(means1)]
#   
#   noarea<-seq(from=1, to=length(lvls),by=1) ; names(noarea)<-lvls
#   noarea<-noarea[order(factor(names(noarea), levels =ordm))]
#   
#   matC<-matrix(data=0, ncol=length(lvls),nrow=length(lvls))  
#   colnames(matC)<-lvls
#   rownames(matC)<-lvls
#   
#   matpval<-matC
#   
#   for(i in 1:length(lvls))
#   { areai<-lvls[i]
#    for(j in 1:length(lvls))
#     #if(rownames(matC)[j]%in%lvls) 
#       { c1<- paste(areai,lvls[j], sep="-") 
#         c2<- paste(lvls[j],areai, sep="-") # column - row
#         if(c1%in%c2)  {matC[j,i]<- 0
#         matpval[j,i]<-0} # same soc
#       
#         if(c1%in%rownames(dfres))
#           {whichr<-which(rownames(dfres)%in%c1)
#           matC[j,i]<- dfres[whichr,1] 
#           matpval[j,i]<-dfres[whichr,4] }
#       
#         if(c2%in%rownames(dfres))
#           {whichr<-which(rownames(dfres)%in%c2)
#           matC[j,i]<- (-1)* dfres[whichr,1] 
#           matpval[j,i]<-dfres[whichr,4] }
#         }
#     #print(i)
#     }
#   
#   matC[matpval>0.05]<-0 # only significant differences to be considered
#   
#   vmin<-range(matC)[1]-0.5
#   vmax<-range(matC)[2]+0.5
#   vminall<- (-1)* max(abs(vmin), abs(vmax))
#   vmaxall<- (1)* max(abs(vmin), abs(vmax))
#   
#   vector<-seq(from = vminall, to=vmaxall, length=100)
#   #dpal<-get("palettes", envir=BAMMtools:::.colorEnv)
#   #palfun<-colorRampPalette(dpal$RdBu, space="Lab")
#   #palfun <- colorRampPalette(c("royalblue","white","indianred" ))
#   palfun <- colorRampPalette(c("royalblue","white","royalblue" ))
#   order=findInterval(vector,sort(vector))
#   cols_v<-palfun(length(vector))[order]
#   #plot(vector~vector, col=cols_v) ; abline(h=0) #ok
#   
#   valsC<-as.numeric(matC)
#   names(valsC)<-rep("grey92", length(valsC))
#   for(i in 1: length(valsC))
#     {which_poz1<--999 
#     for(jj in 1: (length(vector)-1)) if(valsC[i] >=vector[jj] & valsC[i] <vector[jj+1]) which_poz1<-jj
#     names(valsC)[i]<-cols_v[which_poz1]
#     #print(i)
#     }
#   matC_col<-matrix(data=names(valsC), ncol=dim(matC)[2], nrow=dim(matC)[1])
#   rownames(matC_col)<-rownames(matC) ; colnames(matC_col)<-colnames(matC) ; 
#   
#   matC_col[matpval>0.05]<-"grey92" # "grey92" # white
#   
#   # reorder matrices
#   matC<-matC[ordm, ordm]
#   matpval<-matpval[ordm, ordm]
#   matC_col<-matC_col[ordm,ordm]
#   
#   matC_col[upper.tri(matC_col)]<-"white" # make upper triangle white ie consider only lower tri
#   
#   # colours
#   cols_vplots<-c("#00A08A","#EBCC2A","deeppink","darkmagenta","blue3")
#   cols_vplots<-c("#5B1A18","darkgreen","goldenrod1","violetred","dodgerblue1")
#   
#   cols<- c(rep(cols_vplots[1],7),rep(cols_vplots[2],4), rep(cols_vplots[3],3) , cols_vplots[4],cols_vplots[5])
#   names(cols)<-lvls
#   cols<-cols[order(factor(names(cols), levels =ordm))]
#   
#   # PLOT
#   xymax<-max(dim(matC))+1
#   x<- (0:xymax) ; y<- (0:xymax)
#   plot( x~y, col="white", xlab="", ylab="", xaxt='n',yaxt='n',frame.plot = FALSE) # scaffold
#   mtext(side=1, text = title, line=+1, font=2,adj=0,cex=1)
#   
#   for(i in 1:dim(matC)[2])
#     { x_col<- rep (i, dim(matC)[1])
#       y_row<-1:dim(matC)[1]
#       points(y_row~x_col, col=rev(matC_col[,i]), pch=15,cex=2.5)
#     }
#   
#   # labels
#   labels<-1:dim(matC)[2] #labels<-rownames(matC)
#   labels<-noarea
#   
#   # column text
#   y<-rep(0, dim(matC)[2]) ; x= (dim(matC)[1]:1)
#   points( y~x, col=rev(cols), pch=18, cex=3)
#   #text( x= (1:dim(matC)[1]), y= rep(dim(matC)[2]+1, dim(matC)[2]),  labels = labels, xpd = TRUE,cex=0.8, adj=0.5, font=2)
#   
#   #rows text 
#   x<-rep(0,dim(matC)[2]) ; y= (dim(matC)[1]:1)
#   points( y~x, col=cols, pch=18, cex=3)
#   #text( x=rep(0,dim(matC)[2]) , y= (dim(matC)[1]:1), labels =labels, xpd = TRUE,cex=0.8, adj=0.5, font=2)
#   
#   # # upper mat flipped
#   # for(i in 1:dim(matC)[2])
#   # { #x_col<- rep (i, dim(matC)[1])
#   #   x_col<- dim(matC)[1]:1
#   #   y_row<-rep(dim(matC)[1]-i+1, dim(matC)[1])
#   #   #y_row<-dim(matC)[1]:1
#   #   points(y_row~x_col, col=matC_col[i,], pch=15,cex=2.5) # matC_col[i,] plot by row matC_col[,i] #plot by column
#   # }
#   # 
#   # #axis(side=3, at=(1:dim(matC)[2]), labels = colnames(matC),tick=F, las=2)
#   # # labels
#   # labels<-1:dim(matC)[2] #labels<-rownames(matC)
#   # if(ordered==T) labels=noarea
#   # # column text
#   # y<-rep(dim(matC)[2]+1, dim(matC)[2]) ; x= 1:dim(matC)[1]
#   # points( y~x, col=cols, pch=18, cex=3)
#   # #text( x= x, y= y, labels = labels, xpd = TRUE,cex=0.8, adj=0.5, font=2) # srt=45
#   # #rows text
#   # x<-rep(0,dim(matC)[2]) ; y= (dim(matC)[1]:1)
#   # points( y~x, col=rev(cols), pch=18, cex=3)
#   # #text( x=x , y= y,labels =rev(labels), xpd = TRUE,cex=0.8, adj=0.5, font=2)
#   
#   to_ret<-list(matC,matpval,matC_col)
#   names(to_ret)<-c("matC","matpval","matC_col")
#   return(to_ret)
#   
# }
# 
# 
# 
#  ## NON-ORDERED
# 
matFigure_diffTukey_allcomp_halfMordered_venom<-function(names_lvls,lvls, dfb0,whichRC,dfres,title)
{# matrixes figure differences

  # means for order
  means1<-numeric() ; sr<-numeric()# mean & sd
  for(j in 1:length(lvls))
  {means1[j]<-mean(dfb0[dfb0$oragr%in%lvls[j],colnames(dfb0)%in%whichRC])
  sr[j]<-length(dfb0[dfb0$oragr%in%lvls[j],colnames(dfb0)%in%whichRC])}

  names(means1)<-lvls ; names(sr)<-lvls
  # ordm<-names(means1)[order(means1)]
  #
  # noarea<-seq(from=1, to=length(lvls),by=1) ; names(noarea)<-lvls
  # noarea<-noarea[order(factor(names(noarea), levels =ordm))]

  matC<-matrix(data=0, ncol=length(lvls),nrow=length(lvls))
  colnames(matC)<-lvls
  rownames(matC)<-lvls

  matpval<-matC

  for(i in 1:length(lvls))
  { areai<-lvls[i]
  for(j in 1:length(lvls))
    #if(rownames(matC)[j]%in%lvls)
  { c1<- paste(areai,lvls[j], sep="-")
  c2<- paste(lvls[j],areai, sep="-") # column - row
  if(c1%in%c2)  {matC[j,i]<- 0
  matpval[j,i]<-0} # same soc

  if(c1%in%rownames(dfres))
  {whichr<-which(rownames(dfres)%in%c1)
  matC[j,i]<- dfres[whichr,1]
  matpval[j,i]<-dfres[whichr,4] }

  if(c2%in%rownames(dfres))
  {whichr<-which(rownames(dfres)%in%c2)
  matC[j,i]<- (-1)* dfres[whichr,1]
  matpval[j,i]<-dfres[whichr,4] }
  }
  #print(i)
  }

  matC[matpval>0.05]<-0 # only significant differences to be considered

  vmin<-range(matC)[1]-0.5
  vmax<-range(matC)[2]+0.5
  vminall<- (-1)* max(abs(vmin), abs(vmax))
  vmaxall<- (1)* max(abs(vmin), abs(vmax))

  vector<-seq(from = vminall, to=vmaxall, length=100)
  palfun <- colorRampPalette(c("royalblue","white","royalblue" ))
  order=findInterval(vector,sort(vector))
  cols_v<-palfun(length(vector))[order]
  #plot(vector~vector, col=cols_v) ; abline(h=0) #ok

  valsC<-as.numeric(matC)
  names(valsC)<-rep("grey92", length(valsC))
  for(i in 1: length(valsC))
  {which_poz1<--999
  for(jj in 1: (length(vector)-1)) if(valsC[i] >=vector[jj] & valsC[i] <vector[jj+1]) which_poz1<-jj
  names(valsC)[i]<-cols_v[which_poz1]
  #print(i)
  }
  matC_col<-matrix(data=names(valsC), ncol=dim(matC)[2], nrow=dim(matC)[1])
  rownames(matC_col)<-rownames(matC) ; colnames(matC_col)<-colnames(matC) ;

  matC_col[matpval>0.05]<-"grey92" # "grey92" # white

  # # reorder matrices
  # matC<-matC[ordm, ordm]
  # matpval<-matpval[ordm, ordm]
  # matC_col<-matC_col[ordm,ordm]

  matC_col[upper.tri(matC_col)]<-"white" # make upper triangle white ie consider only lower tri

  # colours
  cols_vplots<-c("#00A08A","#EBCC2A","deeppink","darkmagenta","blue3")
  cols_vplots<-c("#5B1A18","darkgreen","goldenrod1","violetred","dodgerblue1")

  cols<- c(rep(cols_vplots[1],7),rep(cols_vplots[2],4), rep(cols_vplots[3],3) , cols_vplots[4],cols_vplots[5])
  names(cols)<-lvls
  #cols<-cols[order(factor(names(cols), levels =ordm))]

  # PLOT
  xymax<-max(dim(matC))+1
  x<- (0:xymax) ; y<- (0:xymax)
  plot( x~y, col="white", xlab="", ylab="", xaxt='n',yaxt='n',frame.plot = FALSE) # scaffold
  mtext(side=1, text = title, line=+1, font=2,adj=0,cex=1)

  for(i in 1:dim(matC)[2])
  { x_col<- rep (i, dim(matC)[1])
  y_row<-1:dim(matC)[1]
  points(y_row~x_col, col=rev(matC_col[,i]), pch=15,cex=2.5)
  }

  # labels
  labels<-1:dim(matC)[2] #labels<-rownames(matC)
  #labels<-noarea

  # column text
  y<-rep(0, dim(matC)[2]) ; x= (dim(matC)[1]:1)
  points( y~x, col=rev(cols), pch=18, cex=3)
  #text( x= (1:dim(matC)[1]), y= rep(dim(matC)[2]+1, dim(matC)[2]),  labels = labels, xpd = TRUE,cex=0.8, adj=0.5, font=2)

  #rows text
  x<-rep(0,dim(matC)[2]) ; y= (dim(matC)[1]:1)
  points( y~x, col=cols, pch=18, cex=3)
  #text( x=rep(0,dim(matC)[2]) , y= (dim(matC)[1]:1), labels =labels, xpd = TRUE,cex=0.8, adj=0.5, font=2)

  # # upper mat flipped
  # for(i in 1:dim(matC)[2])
  # { #x_col<- rep (i, dim(matC)[1])
  #   x_col<- dim(matC)[1]:1
  #   y_row<-rep(dim(matC)[1]-i+1, dim(matC)[1])
  #   #y_row<-dim(matC)[1]:1
  #   points(y_row~x_col, col=matC_col[i,], pch=15,cex=2.5) # matC_col[i,] plot by row matC_col[,i] #plot by column
  # }
  #
  # #axis(side=3, at=(1:dim(matC)[2]), labels = colnames(matC),tick=F, las=2)
  # # labels
  # labels<-1:dim(matC)[2] #labels<-rownames(matC)
  # if(ordered==T) labels=noarea
  # # column text
  # y<-rep(dim(matC)[2]+1, dim(matC)[2]) ; x= 1:dim(matC)[1]
  # points( y~x, col=cols, pch=18, cex=3)
  # #text( x= x, y= y, labels = labels, xpd = TRUE,cex=0.8, adj=0.5, font=2) # srt=45
  # #rows text
  # x<-rep(0,dim(matC)[2]) ; y= (dim(matC)[1]:1)
  # points( y~x, col=rev(cols), pch=18, cex=3)
  # #text( x=x , y= y,labels =rev(labels), xpd = TRUE,cex=0.8, adj=0.5, font=2)

  to_ret<-list(matC,matpval,matC_col)
  names(to_ret)<-c("matC","matpval","matC_col")
  return(to_ret)

}
# 
# 
# matFigure_diffTukey<-function(EurasianAreas, RestAreas,dfres, title)
#   {# matrixes figure differences
#   matC<-matrix(data=0, ncol=length(EurasianAreas),nrow=length(RestAreas))
#   colnames(matC)<-EurasianAreas
#   rownames(matC)<-RestAreas
# 
#   matpval<-matC
# 
#   for(i in 1:length(EurasianAreas))
#   { areai<-EurasianAreas[i]
#   for(j in 1:length(RestAreas))
#     #if(rownames(matC)[j]%in%lvls)
#       { c1<- paste(areai,RestAreas[j], sep="-")
#       c2<- paste(RestAreas[j],areai, sep="-") # socrest-Euarea
# 
#       if(c1%in%rownames(dfres))
#         {whichr<-which(rownames(dfres)%in%c1)
#         matC[j,i]<- dfres[whichr,1]
#         matpval[j,i]<-dfres[whichr,4] }
# 
#       if(c2%in%rownames(dfres))
#         {whichr<-which(rownames(dfres)%in%c2)
#         matC[j,i]<- (-1)* dfres[whichr,1]
#         matpval[j,i]<-dfres[whichr,4] }
#       }
#   #print(i)
#   }
# 
#   matC[matpval>0.05]<-0 # only significant differences to be considered
#   vmin<-range(matC)[1]-0.5
#   vmax<-range(matC)[2]+0.5
#   vminall<- (-1)* max(abs(vmin), abs(vmax))
#   vmaxall<- (1)* max(abs(vmin), abs(vmax))
# 
#   vector<-seq(from = vminall, to=vmaxall, length=100)
#   #dpal<-get("palettes", envir=BAMMtools:::.colorEnv)
#   #palfun<-colorRampPalette(dpal$RdBu, space="Lab")
#   palfun <- colorRampPalette(c("royalblue","white","indianred" ))
# 
#   order=findInterval(vector,sort(vector))
#   cols_v<-palfun(length(vector))[order]
#   #plot(vector~vector, col=cols_v) #ok
#   #abline(h=0)
# 
#   valsC<-as.numeric(matC)
#   names(valsC)<-rep("white", length(valsC))
#   for(i in 1: length(valsC))
#     {which_poz1<--999
#     for(jj in 1: (length(vector)-1)) if(valsC[i] >=vector[jj] & valsC[i] <vector[jj+1]) which_poz1<-jj
#     names(valsC)[i]<-cols_v[which_poz1]
#     #print(i)
#     }
#     matC_col<-matrix(data=names(valsC), ncol=dim(matC)[2], nrow=dim(matC)[1])
#     rownames(matC_col)<-rownames(matC) ; colnames(matC_col)<-colnames(matC) ;
# 
#     whiterows<-which(rownames(matC_col)%in%"Erow")
#     matC_col[whiterows,]<-"white"
#     matC_col[matpval>0.05]<-"grey92"
# 
#     # vmatC<-as.numeric(matC) # by column
#     # library(viridis)
#     # ii <- cut(vmatC, breaks = seq(min(vmatC), max(vmatC), len =length(vmatC)), include.lowest = TRUE)
#     # colors<-magma(length(vmatC)-1)[ii]
#     # matC_col<-matrix(data=colors, ncol=dim(matC)[2], nrow=dim(matC)[1])
# 
#     # scaffold plot
#     xymax<-max(dim(matC))+1
#     x<- (1:xymax) ; y<- (1:xymax)
#     par(mar=c(c(3, 8, 4.1, 0)))
#     plot( x~y, col="white", xlab="", ylab="", xaxt='n',yaxt='n',frame.plot = FALSE) # scaffold
#     mtext(side=1, text = title, line=+1, font=2,adj=0,cex=1)
# 
#     for(i in 1:dim(matC)[2])
#     { x_col<- rep (i, dim(matC)[1])
#     y_row<-1:dim(matC)[1]
#     points(y_row~x_col, col=rev(matC_col[,i]), pch=15,cex=2.5)
#     }
# 
#     #axis(side=3, at=(1:dim(matC)[2]), labels = colnames(matC),tick=F, las=2)
# 
#    # column text
#     text( x = (1:dim(matC)[2]) , y= rep(dim(matC)[1]+1,dim(matC)[2]), srt = 45,
#           labels = colnames(matC), xpd = TRUE,cex=0.8, adj=0)
#     #rows text
#     if(title%in%"GeoD_LCPsl_MCOSTe" | title%in%"4D barrier")
#       {labels<-RestAreas ; labels[labels%in%"Erow"]<-""
#        text( x=rep(0,dim(matC)[2]) , y= (dim(matC)[1]:1),
#             labels =labels, xpd = TRUE,cex=0.8, adj=1)}
# 
#     # # add legend
#     # vector<-seq(from = vmin, to=vmax, length=100)
#     # palfun <- colorRampPalette(c("royalblue","white","indianred" ))
#     # order=findInterval(vector,sort(vector))
#     # cols_v<-palfun(length(vector))[order]
#     # x<-rep(0,100)
#     # points(x ~ vector, col=cols_v,pch=15,xlab="", ylab="", xaxt='n',yaxt='n',frame.plot = FALSE,cex=2) #ok
#     # axis(side=1, at=c(round(vmin-0.5),0,round(vmax+0.5)), labels=c(round(vmin-0.5),0,round(vmax+0.5)), tick=F,pos=0)
#     # mtext(text = "Barrier strength in Eurasian areas",
#     #       side = 1, line = -3,cex=1.2)
# 
# 
#     to_ret<-list(matC,matpval,matC_col)
#     names(to_ret)<-c("matC","matpval","matC_col")
#     return(to_ret)
# 
# }
# 
matFigure_diffTukey_key<-function(dfres)
  { values<-dfres[,1]
    values[dfres[,4]>0.05]<-0

    vmin<-range(values)[1]-0.5
    vmax<-range(values)[2]+0.5
    vminall<- (-1)* max(abs(vmin), abs(vmax))
    vmaxall<- (1)* max(abs(vmin), abs(vmax))

    vector<-seq(from = vminall, to=vmaxall, length=100)
    #dpal<-get("palettes", envir=BAMMtools:::.colorEnv)
    #palfun<-colorRampPalette(dpal$RdBu, space="Lab")
    palfun <- colorRampPalette(c("royalblue","white","indianred" ))

    order=findInterval(vector,sort(vector))
    cols_v<-palfun(length(vector))[order]
    x<-rep(0,100)
    plot(x ~ vector, col=cols_v,pch=15,xlab="", ylab="", xaxt='n',yaxt='n',frame.plot = FALSE,cex=2) #ok

    axis(side=1, at=c( round(vminall)+1,0,round(vmaxall)-1 ), labels=c(round(vminall)+1,0,round(vmaxall)-1), tick=F,pos=0)
    mtext(text = "Barrier strength\nin column areas",
          side = 1, line = -3,cex=1)

}

# 
# 
# matFigure_diffTukey_allcomp<-function(EurasianAreas, RestAreas,dfres,title, whichRC, dfb0)
# {# matrixes figure differences
#   allareas<-c(EurasianAreas,RestAreas) ; allareas<-allareas[!allareas%in%"Erow"]
#   areasuse<-allareas
#   matC<-matrix(data=0, ncol=length(allareas),nrow=length(allareas))  
#   colnames(matC)<-allareas
#   rownames(matC)<-allareas
#   
#   matpval<-matC
#   
#   for(i in 1:length(allareas))
#     { areai<-allareas[i]
#     for(j in 1:length(allareas))
#       #if(rownames(matC)[j]%in%lvls) 
#         { c1<- paste(areai,allareas[j], sep="-") 
#           c2<- paste(allareas[j],areai, sep="-") # column - row
#           if(c1%in%c2)  {matC[j,i]<- 0
#                         matpval[j,i]<-0} # same soc
#           
#         if(c1%in%rownames(dfres))
#           {whichr<-which(rownames(dfres)%in%c1)
#           matC[j,i]<- dfres[whichr,1] 
#           matpval[j,i]<-dfres[whichr,4] }
#         
#         if(c2%in%rownames(dfres))
#           {whichr<-which(rownames(dfres)%in%c2)
#            matC[j,i]<- (-1)* dfres[whichr,1] 
#            matpval[j,i]<-dfres[whichr,4] }
#         }
#     #print(i)
#     }
#   
#   matC[matpval>0.05]<-0 # only significant differences to be considered
#   vmin<-range(matC)[1]-0.5
#   vmax<-range(matC)[2]+0.5
#   vminall<- (-1)* max(abs(vmin), abs(vmax))
#   vmaxall<- (1)* max(abs(vmin), abs(vmax))
#   
#   vector<-seq(from = vminall, to=vmaxall, length=100)
#   #dpal<-get("palettes", envir=BAMMtools:::.colorEnv)
#   #palfun<-colorRampPalette(dpal$RdBu, space="Lab")
#   palfun <- colorRampPalette(c("royalblue","white","indianred" ))
#   
#   order=findInterval(vector,sort(vector))
#   cols_v<-palfun(length(vector))[order]
#   #plot(vector~vector, col=cols_v) #ok
#   #abline(h=0)
#   
#   valsC<-as.numeric(matC)
#   names(valsC)<-rep("white", length(valsC))
#   for(i in 1: length(valsC))
#     {which_poz1<--999 
#     for(jj in 1: (length(vector)-1)) if(valsC[i] >=vector[jj] & valsC[i] <vector[jj+1]) which_poz1<-jj
#     names(valsC)[i]<-cols_v[which_poz1]
#     #print(i)
#     }
#   matC_col<-matrix(data=names(valsC), ncol=dim(matC)[2], nrow=dim(matC)[1])
#   rownames(matC_col)<-rownames(matC) ; colnames(matC_col)<-colnames(matC) ; 
#   
#   #whiterows<-which(rownames(matC_col)%in%"Erow")
#   #matC_col[whiterows,]<-"white"
#   matC_col[matpval>0.05]<-"grey92"
# 
#   # ORDERING PART
#   noarea<-seq(from=1,to=length(allareas),by=1) ; names(noarea)<-areasuse
#   # order matrix
#   areasuse<-allareas 
#   means1<-numeric()
#   for(j in 1:length(areasuse)) means1[j]<-mean(dfb0[dfb0$oragr%in%areasuse[j],colnames(dfb0)%in%whichRC])
#   names(means1)<-areasuse
#   ordm<-names(means1)[order(means1)]
#   
#   lvls<- c("South trop ch", "Lower_MiddleY" ,"Chinese_loess", "W_Yuman_E_Tib","Fertile_Cresc", "Sava_W_India","Ganges_E_Indi"  ,"S_India", 
#            "West Africa T", "W_African_Sav","Sudanic_Savan", "Ethipian plat", 
#            "NW_Lowland_SA", "N_Lowland_SA","C/S_Andes","Southwes amaz" ,
#            "Mesoamerica","E_North_Ameri")
#   cols_vplots<-c("#00A08A","#EBCC2A","deeppink","darkmagenta","blue3")
#   cols<- c(cols_vplots[1],rep(cols_vplots[1],7),rep(cols_vplots[2],4), rep(cols_vplots[3],4) , cols_vplots[4],cols_vplots[5])
#   names(cols)<-lvls
#   cols<-cols[names(cols)%in%areasuse]
#   cols<-cols[order(factor(names(cols), levels =ordm))]
#   noarea<-noarea[order(factor(names(noarea), levels =ordm))]
#   
#   # reorder matrices
#   matC<-matC[ordm, ordm]
#   matpval<-matpval[ordm, ordm]
#   matC_col<-matC_col[ordm, ordm]
#   
# 
#   # scaffold plot
#   xymax<-max(dim(matC))+1
#   x<- (0:xymax) ; y<- (0:xymax)
#   
#   plot( x~y, col="white", xlab="", ylab="", xaxt='n',yaxt='n',frame.plot = FALSE) # scaffold
#   #mtext(side=1, text = title, line=+1, font=2,adj=0,cex=1)
#   
#   for(i in 1:dim(matC)[2])
#     { x_col<- rep (i, dim(matC)[1])
#       y_row<-1:dim(matC)[1]
#       points(y_row~x_col, col=rev(matC_col[,i]), pch=15,cex=2.5)
#     }
#   
#   #axis(side=3, at=(1:dim(matC)[2]), labels = colnames(matC),tick=F, las=2)
#   
#   # labels
#   labels<-1:dim(matC)[2] #labels<-rownames(matC)
#   labels<-noarea
#   # column text
#   y<-rep(dim(matC)[2]+1, dim(matC)[2]) ; x= (dim(matC)[1]:1)
#   points( y~x, col=rev(cols), pch=18, cex=3)
#   #text( x= (1:dim(matC)[1]), y= rep(dim(matC)[2]+1, dim(matC)[2]),  labels = labels, xpd = TRUE,cex=0.8, adj=0.5, font=2)
#   
#   #rows text 
#   x<-rep(0,dim(matC)[2]) ; y= (dim(matC)[1]:1)
#   points( y~x, col=cols, pch=18, cex=3)
#   #text( x=rep(0,dim(matC)[2]) , y= (dim(matC)[1]:1), labels =labels, xpd = TRUE,cex=0.8, adj=0.5, font=2)
#   
# 
#   # delimite continents
#   #abline(v=c(8.5,12.5, 16.5,17.5,18.5))
#   #abline(h=c(0.5,1.5,2.5,6.5,10.5, 18.5))
#   # cols_vplots<-c("#00A08A","#EBCC2A","deeppink","darkmagenta","blue3")
#   # 
#   # rect(xleft = 0.5, ybottom=10.6,ytop=18.5,xright=18.5,border="#00A08A",lwd=2)
#   # rect(xleft = 0.5, ybottom=6.6,ytop=10.5,xright=18.5,border="#EBCC2A",lwd=2)
#   # rect(xleft = 0.5, ybottom=2.6,ytop=6.5,xright=18.5,border="deeppink",lwd=2)
#   # rect(xleft = 0.5, ybottom=1.5,ytop=2.5,xright=18.5,border="darkmagenta",lwd=2)
#   # rect(xleft = 0.5, ybottom=0.45,ytop=1.45,xright=18.5,border="blue3",lwd=2)
#   
#   # # add legend
#   # vector<-seq(from = vmin, to=vmax, length=100)
#   # palfun <- colorRampPalette(c("royalblue","white","indianred" ))
#   # order=findInterval(vector,sort(vector))
#   # cols_v<-palfun(length(vector))[order]
#   # x<-rep(0,100)
#   # points(x ~ vector, col=cols_v,pch=15,xlab="", ylab="", xaxt='n',yaxt='n',frame.plot = FALSE,cex=2) #ok
#   # axis(side=1, at=c(round(vmin-0.5),0,round(vmax+0.5)), labels=c(round(vmin-0.5),0,round(vmax+0.5)), tick=F,pos=0)
#   # mtext(text = "Barrier strength in Eurasian areas",
#   #       side = 1, line = -3,cex=1.2)
#   
#   
#   to_ret<-list(matC,matpval,matC_col)
#   names(to_ret)<-c("matC","matpval","matC_col")
#   return(to_ret)
#   
# }
# 
# 
##################################
######## variance and weights  ###
##################################

calc_var<-function(dfplot, column, opc=F)
{minvar1=10000 ; maxvar1=-1
minvar2=10000 ; maxvar2=-1
minvar3=10000 ; maxvar3=-1
minvar4=10000 ; maxvar4=-1
if(opc==T) {minvaropc=10000 ; maxvaropc=-1}

for(j in 1: length(levels(dfplot[,colnames(dfplot)%in%column])))
{ if( var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC1) <minvar1) minvar1<- var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC1)
if( var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC2) <minvar2) minvar2<- var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC2)
if( var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC3) <minvar3) minvar3<- var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC3)
if( var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC4) <minvar4) minvar4<- var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC4)
if(opc==T) if( var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$allBarier) <minvaropc) minvaropc<- var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$allBarier)

if( var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC1) >maxvar1) maxvar1<- var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC1)
if( var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC2) >maxvar2) maxvar2<- var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC2)
if( var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC3) >maxvar3) maxvar3<- var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC3)
if( var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC4) >maxvar4) maxvar4<- var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC4)
if(opc==T) if( var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$allBarier) >maxvaropc) maxvaropc<- var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$allBarier)

}
if(opc==T) to_ret<-c(maxvar1/minvar1,maxvar2/minvar2,maxvar3/minvar3,maxvar4/minvar4, maxvaropc/minvaropc)
if(opc==F) to_ret<-c(maxvar1/minvar1,maxvar2/minvar2,maxvar3/minvar3,maxvar4/minvar4)
return(to_ret)
}


calc_var_per_cont<-function(dfplot, column, opc=F)
{varRC1<-numeric() ; varRC2<-numeric() ; varRC3<-numeric() ; varRC4<-numeric()
if(opc==T) varopc<-numeric()
for(j in 1: length(levels(dfplot[,colnames(dfplot)%in%column])))
  { varRC1[j]<-var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC1)
  varRC2[j]<-var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC2)
  varRC3[j]<-var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC3)
  varRC4[j]<-var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$RC4)
  if(opc==T)  varopc[j]<-var(dfplot[dfplot[,colnames(dfplot)%in%column]%in%levels(dfplot[,colnames(dfplot)%in%column])[j],]$allBarier)

}
names(varRC1)<-levels(dfplot[,colnames(dfplot)%in%column])
names(varRC2)<-levels(dfplot[,colnames(dfplot)%in%column])
names(varRC3)<-levels(dfplot[,colnames(dfplot)%in%column])
names(varRC4)<-levels(dfplot[,colnames(dfplot)%in%column])
if(opc==T) names(varopc)<-levels(dfplot[,colnames(dfplot)%in%column])

to_ret<-list(varRC1,varRC2,varRC3,varRC4)
if(opc==T) to_ret<-list(varRC1,varRC2,varRC3,varRC4,varopc)
return(to_ret)
}

weightcol<-function(dfplot, column, varlist,opc=F)
{w1<-rep(-1,length(dfplot[,1])) ; w2<-rep(-1,length(dfplot[,1])); w3<-rep(-1,length(dfplot[,1])) ; w4<-rep(-1,length(dfplot[,1]))
if(opc==T) wopc<-rep(-1,length(dfplot[,1]))
for(j in 1:length(varlist[[1]]))
{w1[dfplot[,colnames(dfplot)%in%column]%in%names(varlist[[1]])[j]]<-1/varlist[[1]][j]
 w2[dfplot[,colnames(dfplot)%in%column]%in%names(varlist[[2]])[j]]<-1/varlist[[2]][j]
 w3[dfplot[,colnames(dfplot)%in%column]%in%names(varlist[[3]])[j]]<-1/varlist[[3]][j]
 w4[dfplot[,colnames(dfplot)%in%column]%in%names(varlist[[4]])[j]]<-1/varlist[[4]][j]
 if(opc==T)  wopc[dfplot[,colnames(dfplot)%in%column]%in%names(varlist[[5]])[j]]<-1/varlist[[5]][j]

}
to_ret<-list(w1,w2,w3,w4)
if(opc==T) to_ret<-list(w1,w2,w3,w4,wopc)
return(to_ret)
}
 
 
#############################
######## moran s correl   ###
#############################


MI_plot<-function(model,dfb0,distm){
    matx<-as.matrix(model$residuals)
    rownames(matx)<-dfb0$Soci
    c<-lets.correl(x = matx, y=distm, z=12,equidistant = T, plot = F) # for P2 the data are clumper per regions so I do not want #socs equal in bins, I want per distance
    toplot<-nrow(c)/2
    plot(c[1:toplot,1] ~ c[1:toplot,5], ylim=c(-1,1), pch=19, ylab="Moran's I", xlab="Distance",cex.lab=1.5)
    lines(c[1:toplot,1] ~ c[1:toplot,5], ylim=c(-1,1), lty=2, col="gray75")
    abline(h=0, lty=2, col="gray50")
    for(j in 1:toplot) # add SE
    {arrows(x0= c[j,5], y0=c[j,1], x1=c[j,5], y1= (c[j,1]+ c[j,2]), angle = 180)
      arrows(x0= c[j,5], y0=c[j,1], x1=c[j,5], y1= (c[j,1]- c[j,2]), angle = 180)}
}

# ########################################
# #### map of all points & barriers ######
# ########################################
# 
# library(BAMMtools)
# map_points_barrier_i<-function(dfplot,whichRC, lvls, names_lvls, dfplot_all )
#   {
#    # add cols to dfplot
#    dpal <- get("palettes", envir = BAMMtools:::.colorEnv)
#    palfun <- colorRampPalette(dpal$RdBu, space="Lab")
#    n.cols <- 64 # Find universal colour breaks:
#   
#    # add the min and the max - this way the distribution is sim to 0
#    which_col<-dfplot[,colnames(dfplot)%in%whichRC]
#    which_col<-c(which_col, (-1)*max(abs(max(which_col)), abs(min(which_col))), max(abs(max(which_col)), abs(min(which_col))))
#   
#    all.breaks <- assignColorBreaks(which_col, NCOLORS=n.cols, logcolor=F, method="linear") # method = jenks /quantile
#    all.pal <- palfun(length(all.breaks))
#    
#    all.pal <- rev(heat.colors(length(all.breaks)))
#    all.pal <- viridis(length(all.breaks))
#    
#    palfun <- colorRampPalette(c("cyan","red" ))
#    all.pal <- palfun(length(all.breaks))
#    
#    dpal <- get("palettes", envir = BAMMtools:::.colorEnv)
#    palfun <- colorRampPalette(dpal$RdBu, space="Lab")
#    all.pal <- palfun(length(all.breaks))
#    
#    cuts <- as.numeric(cut(which_col, breaks = all.breaks, include.lowest=T))
#    cols <- all.pal[cuts]
#    cols[is.na(cols)] <- all.pal[length(all.pal)]
#    dfplot$colsegm<-cols[1: (length(cols)-2)] # exclude the last 2 --> these are the added min and max
#   
#    dfplot_all$colsegm<- -999
#    for(jj in 1:length(dfplot[,1]))
#    {dfplot_all[dfplot_all$soci%in%dfplot$Soci[jj],]$colsegm<- rep( dfplot[dfplot$Soci%in%dfplot$Soci[jj],]$colsegm,length(dfplot_all[dfplot_all$soci%in%dfplot$Soci[jj],]$colsegm) ) 
#     #print(jj)
#     }
#    dfplot_all<-dfplot_all[!dfplot_all$colsegm%in%-999,]
#    
#    for(i in 1: length(lvls))
#     { #plot(raster_cost_longlat, col="light gray", legend=F, axes=F, box=F)
#       #apply(dfplot[dfplot$oragr_which%in%regs[i],], 1,fxn_segment,clat=clat,clon=clon,cnblat=cnblat,cnblon=cnblon,ccol=ccol )
#       
#       if(i%in%c(8,9,10,11))
#       {a<-maps::map(database = "world",xlim=c(-25,60), ylim = c(-50,50), fill=T, col=scales::alpha("gray90", 1), border="gray90")}
#       #par(xpd=F) ; map.axes(side=1,cex.axis=1,at=-131,labels="") ; par(xpd=F)}
#       
#       if(i%in%c(15,16))
#       {a<-maps::map(database = "world",xlim=c(-130,-45), ylim = c(-25,75), fill=T, col=scales::alpha("gray90", 1), border="gray90")}
#       
#       if(i%in%c(12,13,14))
#       {a<-maps::map(database = "world",xlim=c(-100,-15), ylim = c(-60,40), fill=T, col=scales::alpha("gray90", 1), border="gray90")}
#       
#       if(i%in%c(5))
#       {a<-maps::map(database = "world",xlim=c(0,85), ylim = c(-25,75), fill=T, col=scales::alpha("gray90", 1), border="gray90")}
#       
#       if(i%in%c(4,3,1,2))
#       {a<-maps::map(database = "world",xlim=c(65,150), ylim = c(-25,75), fill=T, col=scales::alpha("gray90", 1), border="gray90")}
#       
#       if(i%in%c(7,6))
#       {a<-maps::map(database = "world",xlim=c(40,125), ylim = c(-25,75), fill=T, col=scales::alpha("gray90", 1), border="gray90")}
#       
#       # if(i==7) #add south india 
#       #   {a<-maps::map(database = "world",xlim=c(40,125), ylim = c(-25,75), fill=T, col=scales::alpha("dark gray", 1), border="dark gray")
#       #    points(dfplot[dfplot$oragr%in%"S_India",]$nblat~dfplot[dfplot$oragr_which%in%regs[i],]$nblong,col=dfplot[dfplot$oragr_which%in%regs[i],]$colsegm, cex=0.3, pch=19)
#       #    points(dfplot[dfplot$oragr_which%in%regs[i],]$lat~dfplot[dfplot$oragr_which%in%regs[i],]$long, col="black", cex=0.3, pch=19)
#       #   
#       #   }
#       points(dfplot_all[dfplot_all$oragr%in%lvls[i],]$nblat~dfplot_all[dfplot_all$oragr%in%lvls[i],]$nblong,col=scales::alpha(dfplot_all[dfplot_all$oragr%in%lvls[i],]$colsegm,0.5), cex=1, pch=20)
#       #points(dfplot[dfplot$oragr%in%lvls[i],]$lat~dfplot[dfplot$oragr%in%lvls[i],]$long, col="black", cex=0.1, pch=19)
#       mlat_area<-median(dfplot[dfplot$oragr%in%lvls[i],]$lat)
#       mlong_area<-median(dfplot[dfplot$oragr%in%lvls[i],]$long)
#       points(mlat_area~mlong_area,col="black", pch=8,cex=1)
#       
#       title(main=paste0(names_lvls[i]), cex.main=1, line=+1)
#      }
# 
# }
# 
# 
# library(BAMMtools)
# library(viridis)
# map_points_barrier<-function(dfplot,whichRC, lvls, names_lvls,title )
# {
#   # add cols to dfplot
#   n.cols <- 64 # Find universal colour breaks:
#   
#   # add the min and the max - this way the distribution is sim to 0
#   which_col<-dfplot[,colnames(dfplot)%in%whichRC]
#   which_col<-c(which_col, (-1)*max(abs(max(which_col)), abs(min(which_col))), max(abs(max(which_col)), abs(min(which_col))))
#   
#   all.breaks <- assignColorBreaks(which_col, NCOLORS=n.cols, logcolor=F, method="linear")
#     # If method = "quantile" macroevolutionary rates are binned into NCOLORS+1 percentiles and rates
#     # in each bin are mapped to a color determined by the pal argument in plot.bammdata. Alternatively,
#     # if method = "linear" macroevolutionary rates are binned into NCOLORS+1 equal length intervals
#     # between the minimum and maximum.
#   
#   all.pal <- rev(heat.colors(length(all.breaks)))
#   all.pal <- viridis(length(all.breaks))
#  
#   palfun <- colorRampPalette(c("cyan","red" ))
#   all.pal <- palfun(length(all.breaks))
#   # can try also colorRamps::blue2red or colorspace::diverge_hsv
#   dpal <- get("palettes", envir = BAMMtools:::.colorEnv)
#   palfun <- colorRampPalette(dpal$RdBu, space="Lab")
#   all.pal <- palfun(length(all.breaks))
#   
#   cuts <- as.numeric(cut(which_col, breaks = all.breaks, include.lowest=T))
#   cols <- all.pal[cuts]
#   cols[is.na(cols)] <- all.pal[length(all.pal)]
#   dfplot$colsegm<-cols[1: (length(cols)-2)] # exclude the last 2 --> these are the added min and max
#   
#   a<-maps::map(database = "world",xlim=c(-170,180), ylim = c(-60,90), fill=T, col=scales::alpha("gray90", 1), border="gray90",mar=c(0,0,0,0))
#   
#   for(i in 1: length(lvls))
#   {
#     points(dfplot[dfplot$oragr%in%lvls[i],]$lat~dfplot[dfplot$oragr%in%lvls[i],]$long,col=scales::alpha(dfplot[dfplot$oragr%in%lvls[i],]$colsegm,0.5), cex=0.7, pch=20)
#     #points(dfplot[dfplot$oragr%in%lvls[i],]$lat~dfplot[dfplot$oragr%in%lvls[i],]$long, col="black", cex=0.1, pch=19)
#     #mlat_area<-median(dfplot[dfplot$oragr%in%lvls[i],]$lat)
#     #mlong_area<-median(dfplot[dfplot$oragr%in%lvls[i],]$long)
#     #points(mlat_area~mlong_area,col="white", pch=8,cex=0.5)
#     
#     #if(i==1) title(main=paste0(names_lvls[i]), cex.main=1, line=+1)
#     #if(i>1) title(main=paste0(names_lvls[i]), cex.main=1, line=+1)
#   }
#   
#   title(main=title, cex.main=1, line=+1)
#   
# }