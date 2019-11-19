#' Soil Profiles
#'
#' Real soils have discontinuities, but for APSIM it might be beneficial to be able to create a soil profile with an arbitrary number of layers and have flexibility in the distribution of soil physical and chemical properties. What are the steps needed to create these soil profiles?
#'  
#'  1. A function which can create a soil matrix with many layers (max 50)
#'  2. This function should contain the Physical/Water, Chemical, InitialWater, Analysis, InitialN, Organic/SoilOrganicMatter 
#'  3. Create Physical/Water with columns: Depth, Thickness, BD, AirDry, LL15, DUL, SAT, KS
#'  
#'  @title Create APSIM-X Soil Profiles
#'  @name apsimx_soil_profile
#'  @description Generates a soil profile that can then replace the existing one in an .apsimx simulation file
#'  @param nlayers Number of soil layers (default = 10)
#'  @param Depth specific depths for each soil layer (cm)
#'  @param Thickness thickness for each soil layer (mm)
#'  @param BD bulk density for each soil layer (g/cc) -- 'cc' is cubic cm
#'  @param AirDry air dry for each soil layer (mm/mm)
#'  @param LL15 lower limit (15 bar) for each soil layer (mm/mm)
#'  @param DUL drainage upper limit (0.33 bar) for each soil layer (mm/mm)
#'  @param SAT saturation (0 bar) for each soil layer (mm/mm)
#'  @param KS saturated hydraulic conductivity (mm/day)
#'  @param crops name of crops being grown
#'  @export
#'  @examples 
#'  \dontrun{
#'  sp <- apsimx_soil_profile()
#'  }
#'  
#'  

apsimx_soil_profile <-  function(nlayers = 10, 
                                  Depth = NULL, 
                                  Thickness = NULL, 
                                  BD = NULL, 
                                  AirDry = NULL,
                                  LL15 = NULL,
                                  DUL = NULL,
                                  SAT = NULL,
                                  KS = NULL,
                                  crops = c("Maize","Wheat"),
                                  OC = NULL,  OC.CN = NULL,
                                  FOM = NULL,  FOM.CN=NULL,
                                  F.mbio = NULL, F.hum = NULL,  
                                  soil.bottom = 150,
                                  water.table = 200, 
                                  soil.type = 0,
                                  dist.parms = list(a = 0, b = 0.5)){
  
  ## 1. and 2. Depth and Thickness
  if(missing(Depth) & missing(Thickness)){
    depth.0 <- round(seq(from = 0, to = soil.bottom, length.out = nlayers + 1))
      
    Depth <- character(nlayers)
    Thickness <- numeric(nlayers)
      
    for(i in 1:nlayers){
      Depth[i] <- paste0(depth.0[i],"-",depth.0[i+1])  
      Thickness[i] <- depth.0[i + 1] - depth.0[i]
    }
  }else{
    if(!missing(Depth) & !missing(Thickness)){
      stop("Only specify Depth OR Thickness")
    }
  }

  ## 3. Bulk density
  ## 1.1 is a default value of Bulk Density
  if(missing(BD)) BD <- 1.1 * soil_variable_profile(nlayers, 
                                                    a = dist.parms$a,
                                                    b = dist.parms$b)
  if(is.list(BD)){
    if(length(BD) != 3) stop("BD list should be of length 3")
    ## First element will be the max value of BD 
    BD.max <- BD[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    BD <- BD.max * soil_variable_profile(nlayers, a = BD[[2]], b = BD[[3]])
  }
  
  ## 4. AirDry with default value 0.10
  if(missing(AirDry)) AirDry <- 0.10 * soil_variable_profile(nlayers, 
                                                             a = dist.parms$a,
                                                             b = dist.parms$b)
  if(is.list(AirDry)){
    if(length(AirDry) != 3) stop("AirDry list should be of length 3")
    ## First element will be the max value of AirDry 
    AirDry.max <- AirDry[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    AirDry <- AirDry.max * soil_variable_profile(nlayers, a = AirDry[[2]], b = AirDry[[3]])
  }
   
  ## 4. LL15 with default value of 0.15
  if(missing(LL15)) LL15 <- 0.15 * soil_variable_profile(nlayers, 
                                                             a = dist.parms$a,
                                                             b = dist.parms$b)
  if(is.list(LL15)){
    if(length(LL15) != 3) stop("LL15 list should be of length 3")
    ## First element will be the max value of LL15
    LL15.max <- LL15[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    LL15 <- LL15.max * soil_variable_profile(nlayers, a = LL15[[2]], b = LL15[[3]])
  }
  
  ## 5. DUL with default value of 0.25
  if(missing(DUL)) DUL <- 0.25 * soil_variable_profile(nlayers, 
                                                       a = dist.parms$a,
                                                       b = dist.parms$b)
  if(is.list(DUL)){
    if(length(DUL) != 3) stop("DUL list should be of length 3")
      ## First element will be the top value of DUL 
      DUL.max <- DUL[[1]]
      ## second element will be the a parameter 
      ## third element will be the b parameter 
      DUL <- DUL.max * soil_variable_profile(nlayers, a = DUL[[2]], b = DUL[[3]])
  }
    
  ## 6. SAT with default 0.45
  if(missing(SAT)) SAT <- 0.45 * soil_variable_profile(nlayers, 
                                                       a = dist.parms$a,
                                                       b = dist.parms$b)
  if(is.list(SAT)){
    if(length(SAT) != 3) stop("SAT list should be of length 3")
     ## First element will be the max value of SAT 
     SAT.max <- SAT[[1]]
     ## second element will be the a parameter 
     ## third element will be the b parameter 
     SAT <- SAT.max * soil_variable_profile(nlayers, a = SAT[[2]], b = SAT[[3]])
  }
    
  ## 7. KS with default value 100
  if(missing(KS)) KS <- 100 * soil_variable_profile(nlayers, 
                                                    a = dist.parms$a,
                                                    b = dist.parms$b)
  if(is.list(KS)){
    if(length(KS) != 3) stop("KS list should be of length 3")
    ## First element will be the top value of KS
    KS.max <- KS[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    KS <- KS.max * soil_variable_profile(nlayers, a = KS[[2]], b = KS[[3]])
  }
    
    ## 5. Fifth variable OC
    if(missing(OC)) OC <- soil_variable_profile(nlayers, 
                                                a = dist.parms$a,
                                                b = dist.parms$b)
    if(is.list(OC)){
      if(length(OC) != 3) stop("OC list should be of length 3")
      ## First element will be the top value of OC
      OC.max <- OC[[1]]
      ## second element will be the a parameter 
      ## third element will be the b parameter 
      OC <- OC.max * soil_variable_profile(nlayers, a = OC[[2]], b = OC[[3]])
    }
    
    ## 6. Sixth variable OC.CN
    if(missing(OC.CN)) OC.CN <- soil_variable_profile(nlayers, 
                                                      a = dist.parms$a,
                                                      b = dist.parms$b)
    if(is.list(OC.CN)){ 
      if(length(OC.CN) != 3) stop("OC.CN  list should be of length 3")
      ## First element will be the top value of OC.CN 
      OC.CN.max <- OC.CN[[1]]
      ## second element will be the a parameter 
      ## third element will be the b parameter 
      OC.CN <- OC.CN.max * soil_variable_profile(nlayers, a = OC.CN[[2]], b = OC.CN[[3]])
    }
    
    ## 7. Seventh variable FOM
    if(missing(FOM)) FOM <- soil_variable_profile(nlayers, 
                                                  a = dist.parms$a,
                                                  b = dist.parms$b)
    if(is.list(FOM)){ 
      if(length(FOM) != 3) stop("FOM list should be of length 3")
      ## First element will be the top value of FOM
      FOM.max <- FOM[[1]]
      ## second element will be the a parameter 
      ## third element will be the b parameter 
      FOM <- FOM.max * soil_variable_profile(nlayers, a = FOM[[2]], b = FOM[[3]])
    }
    
    ## 8. Eight variable FOM.CN
    if(missing(FOM.CN)) FOM.CN <- soil_variable_profile(nlayers, 
                                                        a = dist.parms$a,
                                                        b = dist.parms$b)
    if(is.list(FOM.CN)){ 
      if(length(FOM.CN) != 3) stop("FOM.CN list should be of length 3")
      ## First element will be the top value of FOM.CN
      FOM.CN.max <- FOM.CN[[1]]
      ## second element will be the a parameter 
      ## third element will be the b parameter 
      FOM.CN <- FOM.CN.max * soil_variable_profile(nlayers, a = FOM.CN[[2]], b = FOM.CN[[3]])
    }
    
    ## 9. Ninth variable F.mbio
    if(missing(F.mbio)) F.mbio <- soil_variable_profile(nlayers, 
                                                        a = dist.parms$a,
                                                        b = dist.parms$b)
    if(is.list(F.mbio)){ 
      if(length(F.mbio) != 3) stop("F.mbio list should be of length 3")
      ## First element will be the top value of F.mbio
      F.mbio.max <- F.mbio[[1]]
      ## second element will be the a parameter 
      ## third element will be the b parameter 
      F.mbio <- F.mbio.max * soil_variable_profile(nlayers, a = F.mbio[[2]], b = F.mbio[[3]])
    }
    
    ## 10. Tenth variable F.hum
    if(missing(F.hum)) F.hum <- soil_variable_profile(nlayers, 
                                                      a = dist.parms$a,
                                                      b = dist.parms$b)
    if(is.list(F.hum)){ 
      if(length(F.hum) != 3) stop("F.hum list should be of length 3")
      ## First element will be the top value of F.hum
      F.hum.max <- F.hum[[1]]
      ## second element will be the a parameter 
      ## third element will be the b parameter 
      F.hum <- F.hum.max * soil_variable_profile(nlayers, a = F.hum[[2]], b = F.hum[[3]])
    }
    
    soil <- data.frame(Depth=Depth, Thickness=Thickness, BD=BD, 
                       AirDry=AirDry, LL15=LL15, DUL=DUL, SAT=SAT, 
                       KS=KS, OC=OC, OC.CN=OC.CN, FOM=FOM, FOM.CN=FOM.CN, 
                       F.mbio=F.mbio, F.hum=F.hum)
    names(soil) <- c("Depth","Thickness", "BD", "AirDry","LL15",
                        "DUL","SAT","KS","OC","OC.CN",
                        "FOM","FOM.CN","F.mbio","F.hum")
    class(soil) <- c("apsimx_soil_profile","data.frame")
    #rownames(soil) <- paste0("layer_",1:nlayers)
    return(soil)
  }


soil_variable_profile <- function(nlayers, a = 0.5, b = 0.5){
  
  ## The shape will be based on the
  ## Ricker function
  ## Y = a * X * exp(-b * X)
  ## https://en.wikipedia.org/wiki/Ricker_model
  ## We'll see if it is flexible enough
  ## If not the specific proportions should be passed
  ## If a = 0, then I will use the exponential decay
  if(a > 0 & b != 0){
    tmp <- a * 1:nlayers * exp(-b * 1:nlayers)
    ans <- tmp/max(tmp)
  }
  
  if(a == 0 & b != 0){
    ans <- exp(-b * 1:nlayers) / exp(-b)
  }
  
  if(a == 0 & b == 0){
    ans <- rep(1, nlayers)
  }
  ans
}

plot.apsimx_soil_profile <- function(x, property = c("all", "water",
                                              "LL15","DUL","SAT","KS",
                                              "OC", "OC.CN", 
                                              "FOM","FOM.CN",
                                              "F.mbio","F.hum")){
  
  ## Add soil bottom depth
  x$soil.depth.bottom <- sapply(as.character(x$Depth), FUN = function(x) as.numeric(strsplit(x,"-")[[1]][2]))
  property <- match.arg(property)
  if(property != "all" & property != "water"){
    
    tmp <- as.data.frame(unclass(x))[,c(property,"Depth","soil.depth.bottom")]
    qp <- qplot(x = tmp[,1], y = -tmp[,3], 
                xlab = property, ylab = "Soil Depth (cm)", 
                geom = "line")
    print(qp)
  }
  
  if(property == "all"){
    
    tmp <- as.data.frame(unclass(x))
    ## This looks dumb, but I'd rather not need a new package for such a simple task
    ll <- data.frame(var = "LL", dist = tmp[,"LL15"])
    dul <- data.frame(var = "DUL", dist = tmp[,"DUL"])
    sat <- data.frame(var = "SAT", dist = tmp[,"SAT"])
    ks <- data.frame(var = "KS", dist = tmp[,"KS"])
    oc <- data.frame(var = "OC", dist = tmp[,"OC"])
    oc.cn <- data.frame(var = "OC.CN", dist = tmp[,"OC.CN"])
    fom <- data.frame(var = "FOM", dist = tmp[,"FOM"])
    fom.cn <- data.frame(var = "FOM.CN", dist = tmp[,"FOM.CN"])
    f.mbio <- data.frame(var = "F.mbio", dist = tmp[,"F.mbio"])
    f.hum <- data.frame(var = "F.hum", dist = tmp[,"F.hum"])
    
    soil.depth.bottoms <- rep(x$soil.depth.bottom, 10)
    
    dat0 <- rbind(ll,dul,sat,ks,oc,oc.cn,fom,fom.cn,f.mbio,f.hum)
    dat <- data.frame(dat0, soil.depths = soil.depth.bottoms)
    
    gp <- ggplot(data = dat, aes(x = dist, y = -soil.depths)) +
      ylab("Soil Depth (cm)") + xlab("") + 
      facet_wrap(~var, scales = "free") +
      geom_path()
    print(gp)
  }
  
  if(property == "water"){
    
    tmp <- as.data.frame(unclass(x))
    gp <- ggplot(data = tmp, aes(x = -soil.depths, y = SAT)) +
      xlab("Soil Depth (cm)") + ylab("proportion") + 
      geom_line() + ggtitle("Soil water") +
      geom_ribbon(aes(ymin = LL15, ymax = DUL), color = "blue",
                  fill = "deepskyblue1") + 
      coord_flip() ##+ ylim(c(0,0.75)) 
    print(gp)
  }
}
