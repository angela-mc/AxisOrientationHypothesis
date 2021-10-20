#dtm=raster_costO; origin = coordsO; destin=coordsD

movecost_elev<-function (dtm, slope = NULL, origin, destin = NULL, funct = "t", 
                         time = "h", outp = "r", sl.crit = 10, W = 70, L = 0, N = 1, 
                         V = 1.2, moves = 16, breaks = NULL, cont.lab = TRUE, destin.lab = TRUE, 
                         cex.breaks = 0.6, cex.lcp.lab = 0.6, oneplot = TRUE, export = FALSE) 
{
  if (is.null(slope) == TRUE) {
    slope <- raster::terrain(dtm, opt = "slope", unit = "degrees", 
                             neighbors = 8)
  }
  else {
    slope <- slope
  }
  if (funct == "t") {
    cost_function <- function(x) {
      6 * exp(-3.5 * abs(tan(x * pi/180) + 0.05))
    }
    main.title <- paste0("Walking-time isochrones (in ", 
                         time, ") around origin")
    sub.title <- "Walking-time based on the Tobler's on-path hiking function"
    legend.cost <- paste0("walking-time (", time, ")")
    sub.title.lcp.plot <- paste0("LCP(s) and walking-time distance(s) based on the Tobler's on-path hiking function (time in ", 
                                 time, ") \nblack dot=start location\n red dot(s)=destination location(s)")
  }
  
  
  # # -------- other fxn ------------------------
  # if (funct == "tofp") {
  #   cost_function <- function(x) {
  #     (6 * exp(-3.5 * abs(tan(x * pi/180) + 0.05))) * 0.6
  #   }
  #   main.title <- paste0("Walking-time isochrones (in ", 
  #                        time, ") around origin")
  #   sub.title <- "Walking-time based on the Tobler's off-path hiking function"
  #   legend.cost <- paste0("walking-time (", time, ")")
  #   sub.title.lcp.plot <- paste0("LCP(s) and walking-time distance(s) based on the Tobler's off-path hiking function (time in ", 
  #                                time, ") \nblack dot=start location\n red dot(s)=destination location(s)")
  # }
  # if (funct == "mt") {
  #   cost_function <- function(x) {
  #     4.8 * exp(-5.3 * abs((tan(x * pi/180) * 0.7) + 0.03))
  #   }
  #   main.title <- paste0("Walking-time isochrones (in ", 
  #                        time, ") around origin")
  #   sub.title <- "Walking-time based on the Marquez-Perez et al.'s modified Tobler hiking function"
  #   legend.cost <- paste0("walking-time (", time, ")")
  #   sub.title.lcp.plot <- paste0("LCP(s) and walking-time distance(s) based on the Marquez-Perez et al.'s modified Tobler hiking function (time in ", 
  #                                time, ") \nblack dot=start location\n red dot(s)=destination location(s)")
  # }
  # if (funct == "ic") {
  #   cost_function <- function(x) {
  #     (0.11 + exp(-(tan(x * pi/180) * 100 + 5)^2/(2 * 30)^2)) * 
  #       3.6
  #   }
  #   main.title <- paste0("Walking-time isochrones (in ", 
  #                        time, ") around origin")
  #   sub.title <- "Walking-time based on the (on-path) Irmischer-Clarke's modified Tobler hiking function"
  #   legend.cost <- paste0("walking-time (", time, ")")
  #   sub.title.lcp.plot <- paste0("LCP(s) and walking-time distance(s) based on the (on-path) Irmischer-Clarke's modified Tobler hiking function (time in ", 
  #                                time, ") \nblack dot=start location\n red dot(s)=destination location(s)")
  # }
  # if (funct == "icofp") {
  #   cost_function <- function(x) {
  #     (0.11 + 0.67 * exp(-(tan(x * pi/180) * 100 + 2)^2/(2 * 
  #                                                          30)^2)) * 3.6
  #   }
  #   main.title <- paste0("Walking-time isochrones (in ", 
  #                        time, ") around origin")
  #   sub.title <- "Walking-time based on the (off-path) Irmischer-Clarke's modified Tobler hiking function"
  #   legend.cost <- paste0("walking-time (", time, ")")
  #   sub.title.lcp.plot <- paste0("LCP(s) and walking-time distance(s) based on the (off-path) Irmischer-Clarke's modified Tobler hiking function (time in ", 
  #                                time, ") \nblack dot=start location\n red dot(s)=destination location(s)")
  # }
  # if (funct == "ug") {
  #   cost_function <- function(x) {
  #     1/(0.0277 * (tan(x * pi/180) * 100) + 0.6115)
  #   }
  #   main.title <- paste0("Walking-time isochrones (in ", 
  #                        time, ") around origin")
  #   sub.title <- "Walking-time based on the Uriarte Gonzalez's cost function"
  #   legend.cost <- paste0("walking-time (", time, ")")
  #   sub.title.lcp.plot <- paste0("LCP(s) and walking-time distance(s) based on the Uriarte Gonzalez's cost function (time in ", 
  #                                time, ") \nblack dot=start location\n red dot(s)=destination location(s)")
  # }
  # if (funct == "ree") {
  #   cost_function <- function(x) {
  #     1/(tan(x * pi/180)/tan(1 * pi/180))
  #   }
  #   main.title <- "Accumulated cost isolines around origin"
  #   sub.title <- "Cost based on the slope-based relative energetic expenditure cost function"
  #   legend.cost <- "cost"
  #   sub.title.lcp.plot <- paste0("LCP(s) and cost distance(s) based on the slope-based relative energetic expenditure cost function \nblack dot=start location\n red dot(s)=destination location(s)")
  # }
  # if (funct == "hrz") {
  #   cost_function <- function(x) {
  #     1/((1337.8 * tan(x * pi/180)^6 + 278.19 * tan(x * 
  #                                                     pi/180)^5 - 517.39 * tan(x * pi/180)^4 - 78.199 * 
  #           tan(x * pi/180)^3 + 93.419 * tan(x * pi/180)^2 + 
  #           19.825 * tan(x * pi/180) + 1.64))
  #   }
  #   main.title <- "Accumulated cost isolines around origin"
  #   sub.title <- "Cost based on the Herzog's metabolic cost function \n cost in J / (Kg*m)"
  #   legend.cost <- "metabolic cost J / (Kg*m)"
  #   sub.title.lcp.plot <- paste0("LCP(s) and cost distance(s) based on the Herzog's metabolic cost function \ncost in J / (Kg*m) \nblack dot=start location\n red dot(s)=destination location(s)")
  # }
  # if (funct == "wcs") {
  #   cost_function <- function(x) {
  #     1/(1 + ((tan(x * pi/180) * 100)/sl.crit)^2)
  #   }
  #   main.title <- "Accumulated cost isolines around origin"
  #   sub.title <- paste0("Cost based on the wheeled-vehicle critical slope cost function \ncritical slope set to ", 
  #                       sl.crit, " percent")
  #   legend.cost <- "cost"
  #   sub.title.lcp.plot <- paste0("LCP(s) and cost distance(s) based on the wheeled-vehicle critical slope cost function \ncritical slope set to ", 
  #                                sl.crit, " percent \nblack dot=start location\n red dot(s)=destination location(s)")
  # }
  # if (funct == "vl") {
  #   cost_function <- function(x) {
  #     1/(1.5 * W + 2 * (W + L) * (L/W)^2 + N * (W + L) * 
  #          (1.5 * V^2 + 0.35 * V * (tan(x * pi/180) * 100) + 
  #             10))
  #   }
  #   main.title <- "Accumulated cost isolines around origin"
  #   sub.title <- paste0("Cost based on the Van Leusen's metabolic energy expenditure cost function \nparameters: W: ", 
  #                       W, "; L: ", L, "; N: ", N, "; V: ", V)
  #   legend.cost <- "energy expenditure cost (Megawatts)"
  #   sub.title.lcp.plot <- paste0("LCP(s) and cost distance(s) based on the Van Leusen's metabolic energy expenditure cost function \n cost in Megawatts; parameters: W: ", 
  #                                W, "; L: ", L, "; N: ", N, "; V: ", V, "\nblack dot=start location\n red dot(s)=destination location(s)")
  # }
  # if (funct == "p") {
  #   cost_function <- function(x) {
  #     1/(1.5 * W + 2 * (W + L) * (L/W)^2 + N * (W + L) * 
  #          (1.5 * V^2 + 0.35 * V * (tan(x * pi/180) * 100)))
  #   }
  #   main.title <- "Accumulated cost isolines around origin"
  #   sub.title <- paste0("Cost based on the Pandolf et al.'s metabolic energy expenditure cost function \nparameters: W: ", 
  #                       W, "; L: ", L, "; N: ", N, "; V: ", V)
  #   legend.cost <- "energy expenditure cost (Megawatts)"
  #   sub.title.lcp.plot <- paste0("LCP(s) and cost distance(s) based on the Pandolf et al.'s metabolic energy expenditure cost function \n cost in Megawatts; parameters: W: ", 
  #                                W, "; L: ", L, "; N: ", N, "; V: ", V, "\nblack dot=start location\n red dot(s)=destination location(s)")
  # }
  # 
  # # -------- ------------------------------------------------------------ ------------------------
  
  
  if (funct == "t" | funct == "tofp" | funct == "mt" | funct == 
      "ic" | funct == "icofp") {
    speed.kmh <- raster::calc(slope, cost_function)
    speed.ms <- speed.kmh * 0.278
    cost.TRANS <- gdistance::transition(speed.ms, transitionFunction = mean, 
                                        directions = moves)
  }
  
  
  # # -------- other fxn ------------------------
  # if (funct == "ree" | funct == "hrz" | funct == "wcs" | funct == 
  #     "vl" | funct == "p" | funct == "ug") {
  #   cost <- raster::calc(slope, cost_function)
  #   cost.TRANS <- gdistance::transition(cost, transitionFunction = mean, 
  #                                       directions = moves)
  # }
  # # -------- ------------------------------------------------------------ ------------------------
  
  
  ## calcualte mcost
  Conductance <- gdistance::geoCorrection(cost.TRANS)
  accum_final <- gdistance::accCost(Conductance, sp::coordinates(origin))
  
  if (funct == "t" | funct == "tofp" | funct == "mt" | funct == 
      "ic" | funct == "ug") {
    if (time == "h") {
      accum_final <- accum_final/3600
    }
    else {
      accum_final <- accum_final/60
    }
  }
  
  # # -------- other fxn ------------------------
  # if (funct == "vl" | funct == "p") {
  #   accum_final <- accum_final/1e+06
  # }
  # # -------- ------------------------------------------------------------ ------------------------
  
  
  if (is.null(breaks) == TRUE) {
    breaks <- round((max(accum_final[][is.finite(accum_final[])]) - 
                       min(accum_final[][is.finite(accum_final[])]))/10, 
                    2)
  }
  levels <- seq(min(accum_final[][is.finite(accum_final[])]), 
                max(accum_final[][is.finite(accum_final[])]), breaks)
  
  
  # plotting: remove
  
  # if (is.null(destin) == FALSE & oneplot == TRUE) {
  #   m <- rbind(c(1, 2))
  #   layout(m)
  # }
  
  # if (outp == "r") {
  #   raster::plot(accum_final, main = main.title, sub = sub.title, 
  #                cex.main = 0.95, cex.sub = 0.75, legend.lab = legend.cost, 
  #                col = terrain.colors(255))
  #   raster::contour(accum_final, add = TRUE, levels = levels, 
  #                   labcex = cex.breaks, drawlabels = cont.lab)
  #   raster::plot(origin, pch = 20, add = TRUE)
  # }
  # else {
  #   raster::contour(accum_final, levels = levels, main = main.title, 
  #                   sub = sub.title, cex.main = 0.95, cex.sub = 0.75, 
  #                   labcex = cex.breaks, drawlabels = cont.lab)
  #   raster::plot(origin, pch = 20, add = TRUE)
  # }
  
  # error for NA - this should never happen, because even if isolines don t work, i don t actully use them
  if(class(tryCatch (isolines <- raster::rasterToContour(accum_final, levels = levels), error=function(e) 1))%in%"numeric")
  {results<-"Infvals"}
  if(!class(tryCatch (isolines <- raster::rasterToContour(accum_final, levels = levels), error=function(e) 1))%in%"numeric")
  {isolines <- raster::rasterToContour(accum_final, levels = levels)}
  
  #isolines <- raster::rasterToContour(accum_final, levels = levels)
  
  if (is.null(destin) == FALSE) {
    sPath <- gdistance::shortestPath(Conductance, sp::coordinates(origin), 
                                     sp::coordinates(destin), output = "SpatialLines")
    
    # raster::plot(dtm, main = "Digital Terrain Model with Least-cost Path(s)",
    #              sub = sub.title.lcp.plot, cex.main = 0.9, cex.sub = 0.7,
    #              legend.lab = "Elevation (masl)")
    # raster::plot(origin, add = TRUE, pch = 20)
    # raster::plot(destin, add = TRUE, pch = 20, col = "red")
    # graphics::lines(sPath)
    
    # sPath_extentpoint error
    spe<-numeric()
    plen<-numeric()
    for(j in 1: length(sPath))
    {if (identical(sPath[j]@bbox[,1],sPath[j]@bbox[,2])) 
    {spe<-c(spe,j)
    plen[j]<-"sPath_extentpoint"
    }
      if (!identical(sPath[j]@bbox[,1],sPath[j]@bbox[,2])) 
      {plen[j]<-rgeos::gLength(sPath[j], byid = TRUE)}
    } # from j
    
    
    #sPath$length <- rgeos::gLength(sPath, byid = TRUE)
    #sPath$length<-plen
    #destin$cost <- raster::extract(accum_final, destin)
    mcost<-raster::extract(accum_final, destin) # for spath 0 - cost is 0
    spe<-unlist(spe) ; mcost[spe]<-"sPath_extentpoint"
    
    
    # plotting and exporting: remove
    # if (destin.lab == TRUE) {
    #   raster::text(sp::coordinates(destin), labels = round(destin$cost, 2), pos = 4, cex = cex.lcp.lab)
    #   if (export == TRUE) {
    #     rgdal::writeOGR(sPath, ".", paste0("LCPs_", funct), 
    #                     driver = "ESRI Shapefile")
    #   }
    # }
  }
  else {
    sPath = NULL
    dest.loc.w.cost = NULL
  }
  
  # exporting: remove
  # if (export == TRUE) {
  #   raster::writeRaster(accum_final, paste0("accum_cost_surf_", 
  #                                           funct), format = "GTiff")
  #   rgdal::writeOGR(isolines, ".", paste0("isolines_", funct), 
  #                   driver = "ESRI Shapefile")
  # }
  
  # plotting remove
  # if (is.null(destin) == FALSE & oneplot == TRUE) {
  #   par(mfrow = c(1, 1))
  # }
  # results <- list(accumulated.cost.raster = accum_final, isolines = isolines, 
  #                 LCPs = sPath, dest.loc.w.cost = destin)
  
  results<-list(sPath=sPath,mcost=mcost, plen=plen)
  return(results)
}


# temp and prec
movecostAngela_vOCTDontAdapt_multdestin<-function(dtm,origin, destin, geoC="yes")
{# v2
  r<-dtm
  values(r)[values(r)%in%0]<-1e-12 # account for 0
  fxn1D <- function(x){1/x[2]} # don t adapt
  
  T <- transition(r, fxn1D, 8, symm=FALSE)
  if(geoC%in%"yes") T <- geoCorrection(T)
  sPath2 <- shortestPath(T, origin, destin, output="SpatialLines") 
  
  spe<-numeric()
  plen<-numeric()
  for(j in 1: length(sPath2))
  {if (identical(sPath2[j]@bbox[,1],sPath2[j]@bbox[,2])) 
  {spe<-c(spe,j)
  plen[j]<-"sPath_extentpoint"}
    if (!identical(sPath2[j]@bbox[,1],sPath2[j]@bbox[,2])) 
    {plen[j]<-rgeos::gLength(sPath2[j], byid = TRUE)}
  } # from j
  
  mcost<-costDistance(T, origin, destin) # for spath 0 - cost is 0
  spe<-unlist(spe) ; mcost[spe]<-"sPath_extentpoint"
  
  results<-list(sPath=sPath2,mcost=mcost, plen=plen)
  return(results)  
  
}