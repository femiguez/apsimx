#' Soil Profiles
#'
#' Real soils have discontinuities, but for APSIM it might be beneficial to be able to create 
#' a soil profile with an arbitrary number of layers and have flexibility in the 
#' distribution of soil physical and chemical properties. What are the steps needed 
#' to create these soil profiles?
#'  
#'  1. A function which can create a soil matrix with many layers
#'  2. This function should contain the Physical or Water, Chemical, InitialWater, Analysis, InitialN, Organic or SoilOrganicMatter 
#'  3. Create Physical or Water with columns: Depth, Thickness, BD, AirDry, LL15, DUL, SAT, KS
#'  
#' @title Create APSIM-X Soil Profiles
#' @name apsimx_soil_profile
#' @description Generates a soil profile that can then replace the existing one in an .apsimx simulation file
#' @param nlayers Number of soil layers (default = 10)
#' @param Depth specific depths for each soil layer (cm)
#' @param Thickness thickness for each soil layer (mm)
#' @param BD bulk density for each soil layer (g/cc) -- 'cc' is cubic cm
#' @param AirDry air dry for each soil layer (mm/mm)
#' @param LL15 lower limit (15 bar) for each soil layer (mm/mm)
#' @param DUL drainage upper limit (0.33 bar) for each soil layer (mm/mm)
#' @param SAT saturation (0 bar) for each soil layer (mm/mm)
#' @param KS saturated hydraulic conductivity (mm/day)
#' @param crop.LL lower limit for a specific crop
#' @param crop.KL root ability to extract water for a specific crop
#' @param crop.XF soil root exploration for a specific crop
#' @param OC organic carbon (percent)
#' @param OC.CN organci carbon C:N ratio
#' @param FOM fresh organic matter (kg/ha)
#' @param FOM.CN fresh organic matter C:N ratio
#' @param FBiom Fraction of microbial biomass
#' @param FInert Fraction of inert carbon
#' @param soil.bottom bottom of the soil profile
#' @param water.table water table level
#' @param soil.type might use it in the future for auto filling missing information
#' @param crops name of crops being grown
#' @param dist.parms parameter values for creating a profile
#' @export
#' @examples 
#' \dontrun{
#'  sp <- apsimx_soil_profile()
#'  }
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
                                 crop.LL = NULL,
                                 crop.KL = NULL,
                                 crop.XF = NULL,
                                 OC = NULL,  OC.CN = NULL,
                                 FOM = NULL,  FOM.CN=NULL,
                                 FBiom = NULL, FInert = NULL,  
                                 soil.bottom = 150,
                                 water.table = 200, 
                                 soil.type = 0,
                                 crops = c("Maize","Soybean","Wheat"),
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
   
  ## 8. crop.LL with default value 0.15
  if(missing(crop.LL)) crop.LL <- 0.15 * soil_variable_profile(nlayers, 
                                                      a = dist.parms$a,
                                                      b = dist.parms$b)
  if(is.list(crop.LL)){
    if(length(crop.LL) != 3) stop("crop.LL list should be of length 3")
    ## First element will be the top value of crop.LL
    crop.LL.max <- crop.LL[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    crop.LL <- crop.LL.max * soil_variable_profile(nlayers, a = crop.LL[[2]], b = crop.LL[[3]])
  }
  
  ## 9. crop.KL with default value 0.06
  if(missing(crop.KL)) crop.KL <- 0.06 * soil_variable_profile(nlayers, 
                                                               a = dist.parms$a,
                                                               b = dist.parms$b)
  if(is.list(crop.KL)){
    if(length(crop.KL) != 3) stop("crop.KL list should be of length 3")
    ## First element will be the top value of crop.KL
    crop.KL.max <- crop.KL[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    crop.KL <- crop.KL.max * soil_variable_profile(nlayers, a = crop.KL[[2]], b = crop.KL[[3]])
  }
  
  ## 10. crop.XF with default value 1
  if(missing(crop.XF)) crop.XF <- 1 * soil_variable_profile(nlayers, 
                                                               a = dist.parms$a,
                                                               b = dist.parms$b)
  if(is.list(crop.XF)){
    if(length(crop.XF) != 3) stop("crop.XF list should be of length 3")
    ## First element will be the top value of crop.XF
    crop.XF.max <- crop.XF[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    crop.XF <- crop.XF.max * soil_variable_profile(nlayers, a = crop.XF[[2]], b = crop.XF[[3]])
  }
  
  ## 11. Organic Carbon
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
    
  ## 12. Organic Carbon C:N ratio
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
    
  ## 13. Fresh Organic Matter (kg/ha)
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
    
  ## 14. Fresh Organic Matter C:N ratio
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
    
  ## 15. Fraction of microbial biomass with default 0.04
  if(missing(FBiom)) FBimo <- 0.04 * soil_variable_profile(nlayers, 
                                                        a = dist.parms$a,
                                                        b = dist.parms$b)
  if(is.list(FBiom)){ 
    if(length(FBiom) != 3) stop("FBiom list should be of length 3")
    ## First element will be the top value of F.mbio
    FBiom.max <- FBiom[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    FBiom <- FBiom.max * soil_variable_profile(nlayers, a = FBiom[[2]], b = FBiom[[3]])
  }
    
  ## 16. Fraction of inert soil organic carbon
  if(missing(FInert)) FInert <- soil_variable_profile(nlayers, 
                                                      a = dist.parms$a,
                                                      b = dist.parms$b)
  if(is.list(FInert)){ 
    if(length(FInert) != 3) stop("FInert list should be of length 3")
    ## First element will be the top value of FInert
    FInert.max <- FInert[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    FInert <- FInert.max * soil_variable_profile(nlayers, a = FInert[[2]], b = FInert[[3]])
  }
    
  soil <- data.frame(Depth=Depth, Thickness=Thickness, BD=BD, 
                     AirDry=AirDry, LL15=LL15, DUL=DUL, SAT=SAT, 
                     KS=KS, 
                     OC=OC, OC.CN=OC.CN, FOM=FOM, FOM.CN=FOM.CN, 
                      FBiom=FBiom, FInert=FInert)
  
    names(soil) <- c("Depth","Thickness", "BD", "AirDry","LL15",
                        "DUL","SAT","KS","OC","OC.CN",
                        "FOM","FOM.CN","FBiom","FInert")
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
                                              "FBiom","FInert")){
  ## Test for existence of ggplot2
  if(!requireNamespace("ggplot2",quietly = TRUE)){
    warning("ggplot2 is required for this plotting function")
    return(NULL)
  }
  ## Really dumb... but for now...
  dist <- NA; soil.depths <- NA; soil.depth.bottom <- NA; SAT <- NA
  LL15 <- NA; DUL <- NA
  ## Add soil bottom depth
  x$soil.depth.bottom <- sapply(as.character(x$Depth), FUN = function(x) as.numeric(strsplit(x,"-")[[1]][2]))
  property <- match.arg(property)
  if(property != "all" & property != "water"){
    
    tmp <- as.data.frame(unclass(x))[,c(property,"Depth","soil.depth.bottom")]
    qp <- ggplot2::qplot(x = tmp[,1], y = -tmp[,3], 
                         xlab = property, ylab = "Soil Depth (cm)", 
                         geom = "line")
    print(qp)
  }
  
  if(property == "all"){
    
    tmp <- as.data.frame(unclass(x))
    ## This looks dumb, but I'd rather not need a new package for such a simple task
    bd <- data.frame(var = "BD", dist = tmp[,"BD"])
    ad <- data.frame(var = "AirDry", dist = tmp[,"AirDry"])
    ll <- data.frame(var = "LL15", dist = tmp[,"LL15"])
    dul <- data.frame(var = "DUL", dist = tmp[,"DUL"])
    sat <- data.frame(var = "SAT", dist = tmp[,"SAT"])
    ks <- data.frame(var = "KS", dist = tmp[,"KS"])
    oc <- data.frame(var = "OC", dist = tmp[,"OC"])
    oc.cn <- data.frame(var = "OC.CN", dist = tmp[,"OC.CN"])
    fom <- data.frame(var = "FOM", dist = tmp[,"FOM"])
    fom.cn <- data.frame(var = "FOM.CN", dist = tmp[,"FOM.CN"])
    fbiom <- data.frame(var = "FBiom", dist = tmp[,"FBiom"])
    finert <- data.frame(var = "FInert", dist = tmp[,"FInert"])
    
    soil.depth.bottoms <- rep(x$soil.depth.bottom, 12)
    
    dat0 <- rbind(bd,ad,ll,dul,sat,ks,oc,oc.cn,fom,fom.cn,fbiom,finert)
    dat <- data.frame(dat0, soil.depths = soil.depth.bottoms)
    
    gp <- ggplot2::ggplot(data = dat, ggplot2::aes(x = dist, y = -soil.depths)) +
      ggplot2::ylab("Soil Depth (cm)") + ggplot2::xlab("") + 
      ggplot2::facet_wrap(~var, scales = "free") +
      ggplot2::geom_path()
    print(gp)
  }
  
  if(property == "water"){
    
    tmp <- as.data.frame(unclass(x))
    gp <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = -soil.depth.bottom, y = SAT)) +
              ggplot2::xlab("Soil Depth (cm)") + ggplot2::ylab("proportion") + 
              ggplot2::geom_line() + ggplot2::ggtitle("Soil water") +
              ggplot2::geom_ribbon(ggplot2::aes(ymin = LL15, ymax = DUL), color = "blue",
                      fill = "deepskyblue1") + 
              ggplot2::coord_flip() ##+ ylim(c(0,0.75)) 
    print(gp)
  }
}
