#' Soil Profiles
#'
#' Real soils might have discontinuities, but for APSIM it might be beneficial to be able to create 
#' a soil profile with an arbitrary number of layers and have flexibility in the 
#' distribution of soil physical and chemical properties. Steps:
#'  
#'  1. \code{\link{apsimx_soil_profile}} is a function which can create a soil matrix with many layers \cr
#'  2. It allows for creating a smooth distribution for Physical (or Water), Chemical, InitialWater, Analysis, InitialN, Organic or SoilOrganicMatter \cr
#'  3. The distribution can be specified with the \sQuote{a} and \sQuote{c} parameter of an exponential decay function, using a list. E.g. DUL = list(0.35, 0, -0.1).
#'  This means that the top value for DUL will be 0.35 and it will decay with a rate of -0.1. \cr
#'  4. If an increase and then a decay is needed the Ricker function can be used. See \sQuote{SSricker} in the \sQuote{nlraa} package. \cr
#'  
#' @title Create APSIM-X Soil Profiles
#' @name apsimx_soil_profile
#' @rdname apsimx_soil_profile
#' @description Generates a soil profile that can then replace the existing one in an \sQuote{.apsimx} or \sQuote{.apsim} simulation file
#' @param nlayers Number of soil layers (default = 10)
#' @param Depth specific depths for each soil layer (cm)
#' @param Thickness thickness for each soil layer (mm)
#' @param BD bulk density for each soil layer (g/cc) -- \sQuote{cc} is cubic cm
#' @param AirDry air dry for each soil layer (mm/mm)
#' @param LL15 lower limit (15 bar) for each soil layer (mm/mm)
#' @param DUL drainage upper limit (0.33 bar) for each soil layer (mm/mm)
#' @param SAT saturation (0 bar) for each soil layer (mm/mm)
#' @param KS saturated hydraulic conductivity (mm/day)
#' @param crop.LL lower limit for a specific crop
#' @param crop.KL root ability to extract water for a specific crop
#' @param crop.XF soil root exploration for a specific crop
#' @param Carbon organic carbon (percent)
#' @param SoilCNRatio organic carbon C:N ratio
#' @param FOM fresh organic matter (kg/ha)
#' @param FOM.CN fresh organic matter C:N ratio
#' @param FBiom Fraction of microbial biomass (0-1)
#' @param FInert Fraction of inert carbon (0-1)
#' @param NO3N nitrate nitrogen (Chemical) (ppm)
#' @param NH4N ammonium nitrogen (Chemical) (ppm)
#' @param PH soil pH
#' @param soil.bottom bottom of the soil profile (cm)
#' @param water.table water table level (not used at the moment) (cm)
#' @param soil.type might use it in the future for auto filling missing information
#' @param crops name of crops being grown
#' @param metadata list with soil metadata. For possible parameters and values see an example of \code{\link{inspect_apsimx}} with soil.child = \dQuote{Metadata}.
#' @param soilwat optional \sQuote{list} of class \sQuote{soilwat_parms}
#' @param swim optional \sQuote{list} of class \sQuote{swim_parms}
#' @param dist.parms parameter values for creating a profile. If a == 0 and b == 0 then \cr
#' a constant value of 1 is used. If a == 0 and b != 0, then an exponential decay is used. \cr
#' If a != 0 and b != 0 then the equation is \code{a*soil.layer*exp(-b*soil.layer)}.  
#' @return a soil profile with class \sQuote{soil_profile} with elements \sQuote{soil}, \sQuote{crops}, \sQuote{metadata},
#' \sQuote{soilwat} and \sQuote{swim}.
#' @export
#' @examples 
#' \donttest{
#'  sp <- apsimx_soil_profile()
#'  require(ggplot2)
#'  plot(sp)
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
                                 Carbon = NULL,  
                                 SoilCNRatio = NULL,
                                 FOM = NULL,  
                                 FOM.CN=NULL,
                                 FBiom = NULL, 
                                 FInert = NULL,  
                                 NO3N = NULL,
                                 NH4N = NULL,
                                 PH = NULL,
                                 soil.bottom = 150,
                                 water.table = 200, 
                                 soil.type = 0,
                                 crops = c("Maize","Soybean","Wheat"),
                                 metadata = NULL,
                                 soilwat = NA,
                                 swim = NA,
                                 dist.parms = list(a = 0, b = 0.2)){

  if(!is.null(Depth) & !is.null(Thickness)){
    stop("Only specify Depth OR Thickness")
  }
  
  ## 1. and 2. Depth and Thickness
  if(missing(Depth) && missing(Thickness)){
    depth.0 <- round(seq(from = 0, to = soil.bottom, length.out = nlayers + 1))
      
    Depth <- character(nlayers)
    Thickness <- numeric(nlayers)
      
    for(i in 1:nlayers){
      Depth[i] <- paste0(depth.0[i],"-",depth.0[i+1])  
      Thickness[i] <- (depth.0[i + 1] - depth.0[i]) * 10
    }
  }
  ## If only Thickness is missing
  if(missing(Thickness) && !missing(Depth)){
    Thickness <- numeric(nlayers)
    for(i in 1:length(Depth)){
      tmp <- strsplit(Depth[i],"-")[[1]]
      Thickness[i] <- (tmp[2] - tmp[1]) * 10
    }
  }
  
  ## If only Depth is missing
  if(!missing(Thickness) && missing(Depth)){
    Depth <- numeric(nlayers)
    thcks <- cumsum(c(0,Thickness))/10
    for(i in 1:length(Thickness)){
      Depth[i] <- paste0(thcks[i],"-",thcks[i + 1])
    }
  }
  
  ## 3. Bulk density
  ## 1.1 is a default value of Bulk Density
  if(missing(BD)) BD <- 1.1 * soil_variable_profile(nlayers, 
                                                    a = dist.parms$a,
                                                    b = -0.05)
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
                                                               b = 0)
  if(is.list(crop.XF)){
    if(length(crop.XF) != 3) stop("crop.XF list should be of length 3")
    ## First element will be the top value of crop.XF
    crop.XF.max <- crop.XF[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    crop.XF <- crop.XF.max * soil_variable_profile(nlayers, a = crop.XF[[2]], b = crop.XF[[3]])
  }
  
  ## 11. Organic Carbon
  if(missing(Carbon)) Carbon <- 1.2 * soil_variable_profile(nlayers, 
                                                a = dist.parms$a,
                                                b = dist.parms$b)
  if(is.list(Carbon)){
    if(length(Carbon) != 3) stop("Carbon list should be of length 3")
    ## First element will be the top value of OC
    Carbon.max <- Carbon[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    Carbon <- Carbon.max * soil_variable_profile(nlayers, a = Carbon[[2]], b = Carbon[[3]])
  }
    
  ## 12. Organic Carbon C:N ratio
  if(missing(SoilCNRatio)) SoilCNRatio <- 12 * soil_variable_profile(nlayers, 
                                                      a = dist.parms$a,
                                                      b = 0)
  if(is.list(SoilCNRatio)){ 
    if(length(SoilCNRatio) != 3) stop("SoilCNRatio  list should be of length 3")
    ## First element will be the top value of SoilCNRatio 
    SoilCNRatio.max <- SoilCNRatio[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    SoilCNRatio <- SoilCNRatio.max * soil_variable_profile(nlayers, a = SoilCNRatio[[2]], b = SoilCNRatio[[3]])
  }
    
  ## 13. Fresh Organic Matter (kg/ha)
  if(missing(FOM)) FOM <- 150 * soil_variable_profile(nlayers, 
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
  if(missing(FOM.CN)) FOM.CN <- 40 * soil_variable_profile(nlayers, 
                                                        a = dist.parms$a,
                                                        b = 0)
  if(is.list(FOM.CN)){ 
    if(length(FOM.CN) != 3) stop("FOM.CN list should be of length 3")
    ## First element will be the top value of FOM.CN
    FOM.CN.max <- FOM.CN[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    FOM.CN <- FOM.CN.max * soil_variable_profile(nlayers, a = FOM.CN[[2]], b = FOM.CN[[3]])
  }
    
  ## 15. Fraction of microbial biomass with default 0.04
  if(missing(FBiom)) FBiom <- 0.04 * soil_variable_profile(nlayers, 
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
  if(missing(FInert)) FInert <- 0.8 * soil_variable_profile(nlayers, 
                                                      a = dist.parms$a,
                                                      b = -0.01)
  if(is.list(FInert)){ 
    if(length(FInert) != 3) stop("FInert list should be of length 3")
    ## First element will be the top value of FInert
    FInert.max <- FInert[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    FInert <- FInert.max * soil_variable_profile(nlayers, a = FInert[[2]], b = FInert[[3]])
  }
  
  ## 17. Chemical soil nitrate-N
  if(missing(NO3N)) NO3N <- 0.5 * soil_variable_profile(nlayers, 
                                                            a = dist.parms$a,
                                                            b = 0.01)
  if(is.list(NO3N)){ 
    if(length(NO3N) != 3) stop("NO3N list should be of length 3")
    ## First element will be the top value of NO3N
    NO3N.max <- NO3N[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    NO3N <- NO3N.max * soil_variable_profile(nlayers, a = NO3N[[2]], b = NO3N[[3]])
  }
  
  ## 18. Chemical soil ammonium-N
  if(missing(NH4N)) NH4N <- 0.05 * soil_variable_profile(nlayers, 
                                                        a = dist.parms$a,
                                                        b = 0.01)
  if(is.list(NH4N)){ 
    if(length(NH4N) != 3) stop("NH4N list should be of length 3")
    ## First element will be the top value of NH4N
    NH4N.max <- NH4N[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    NH4N <- NH4N.max * soil_variable_profile(nlayers, a = NH4N[[2]], b = NH4N[[3]])
  }
   
  ## 19. Chemical soil PH
  if(missing(PH)) PH <- 6.5 * soil_variable_profile(nlayers, 
                                                         a = dist.parms$a,
                                                         b = 0)
  if(is.list(PH)){ 
    if(length(PH) != 3) stop("PH list should be of length 3")
    ## First element will be the top value of PH
    PH.max <- PH[[1]]
    ## second element will be the a parameter 
    ## third element will be the b parameter 
    PH <- PH.max * soil_variable_profile(nlayers, a = PH[[2]], b = PH[[3]])
  }
  
  soil <- data.frame(Depth=Depth, Thickness=Thickness, BD=BD, 
                     AirDry=AirDry, LL15=LL15, DUL=DUL, SAT=SAT, 
                     KS=KS, crop.XF=crop.XF, crop.KL = crop.KL,
                     crop.LL = crop.LL, Carbon=Carbon, 
                     SoilCNRatio=SoilCNRatio, FOM=FOM, 
                     FOM.CN=FOM.CN, FBiom=FBiom, FInert=FInert,
                     NO3N=NO3N, NH4N=NH4N, PH=PH)
  
    names(soil) <- c("Depth","Thickness", "BD", "AirDry","LL15",
                     "DUL","SAT","KS","crop.XF","crop.KL","crop.LL",
                     "Carbon","SoilCNRatio", "FOM","FOM.CN","FBiom","FInert",
                     "NO3N","NH4N","PH")
    
    ## Check for reasonable values
    check_apsimx_soil_profile(soil)
    
    ans <- list(soil=soil, crops = crops, metadata = metadata, soilwat = soilwat, swim = swim)
    class(ans) <- "soil_profile"
    return(ans)
  }

#' This function creates a profile for a given soil variable
#' with flexibility about the given shape: \cr
#' 1. constant \cr
#' 2. exponential decay \cr
#' 3. ricker \cr
#' @name soil_variable_profile
#' @description create a profile given a number of layers
#' @param nlayers number of soil layers
#' @param a parameter in a function
#' @param b parameter in a function
#' @return It returns a numeric vector
#' @noRd
#' 
soil_variable_profile <- function(nlayers, a = 0.5, b = 0.5){
  
  if(a < 0) stop("a parameter cannot be negative")
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

#' @rdname apsimx_soil_profile
#' @description plotting function for a soil profile, it requires \sQuote{ggplot2}
#' @param x object of class \sQuote{soil_profile}.
#' @param ... additional plotting arguments (none use at the moment).
#' @param property \dQuote{all} for plotting all soil properties, \dQuote{water} for just SAT, DUL and LL15
#' @return it produces a plot
#' @export 
plot.soil_profile <- function(x,..., property = c("all", "water","BD",
                                              "AirDry","LL15","DUL","SAT",
                                              "KS","crop.XF","crop.KL",
                                              "crop.LL", "Carbon", "SoilCNRatio", 
                                              "FOM","FOM.CN","FBiom",
                                              "FInert","NO3N","NH4N","PH")){
  ## Test for existence of ggplot2
  if(!requireNamespace("ggplot2", quietly = TRUE)){
    warning("ggplot2 is required for this plotting function")
    return(NULL)
  }
  ## Really dumb... but for now...
  dist <- NA; soil.depths <- NA; soil.depth.bottom <- NA; SAT <- NA
  LL15 <- NA; DUL <- NA; AirDry <- NA
  xsoil <- x$soil
  ## Add soil bottom depth
  xsoil$soil.depth.bottom <- sapply(as.character(xsoil$Depth), FUN = function(x) as.numeric(strsplit(x,"-")[[1]][2]))
  property <- match.arg(property)
  if(property != "all" & property != "water"){
    
    tmp <- xsoil[,c(property,"Depth","soil.depth.bottom")]
    qp <- ggplot2::ggplot() + 
                   ggplot2::geom_point(ggplot2::aes(x = tmp[,1], y = -tmp[,3])) + 
                   ggplot2::geom_path(ggplot2::aes(x = tmp[,1], y = -tmp[,3])) +
                   ggplot2::xlab(property) + 
                   ggplot2::ylab("Soil Depth (cm)") 
    print(qp)
  }
  
  if(property == "all"){
    
    tmp <- xsoil
    ## This looks dumb, but I'd rather not need a new package for such a simple task
    bd <- data.frame(var = "BD", dist = tmp[,"BD"]) # 1
    ad <- data.frame(var = "AirDry", dist = tmp[,"AirDry"]) # 2
    ll <- data.frame(var = "LL15", dist = tmp[,"LL15"]) # 3
    dul <- data.frame(var = "DUL", dist = tmp[,"DUL"]) # 4
    sat <- data.frame(var = "SAT", dist = tmp[,"SAT"]) # 5
    ks <- data.frame(var = "KS", dist = tmp[,"KS"]) # 6
    c.xf <- data.frame(var = "crop.XF", dist = tmp[,"crop.XF"]) # 7
    c.kl <- data.frame(var = "crop.KL", dist = tmp[,"crop.KL"]) # 8
    c.ll <- data.frame(var = "crop.LL", dist = tmp[,"crop.LL"]) # 9
    carbon <- data.frame(var = "Carbon", dist = tmp[,"Carbon"]) # 10
    soilcn <- data.frame(var = "SoilCNRatio", dist = tmp[,"SoilCNRatio"]) # 11
    fom <- data.frame(var = "FOM", dist = tmp[,"FOM"]) # 12
    fom.cn <- data.frame(var = "FOM.CN", dist = tmp[,"FOM.CN"]) # 13
    fbiom <- data.frame(var = "FBiom", dist = tmp[,"FBiom"]) # 14
    finert <- data.frame(var = "FInert", dist = tmp[,"FInert"]) # 15
    no3n <- data.frame(var = "NO3N", dist = tmp[,"NO3N"]) # 16
    nh4n <- data.frame(var = "NH4N", dist = tmp[,"NH4N"]) # 17
    ph <- data.frame(var = "PH", dist = tmp[,"PH"]) # 18
    
    soil.depth.bottoms <- rep(xsoil$soil.depth.bottom, 18)
    
    dat0 <- rbind(bd,ad,ll,dul,sat,ks,c.xf,c.kl,c.ll,carbon,
                  soilcn,fom,fom.cn,fbiom,finert,no3n,nh4n,ph)
    
    dat <- data.frame(dat0, soil.depths = soil.depth.bottoms)
    
    gp <- ggplot2::ggplot(data = dat, ggplot2::aes(x = dist, y = -soil.depths)) +
      ggplot2::ylab("Soil Depth (cm)") + ggplot2::xlab("") + 
      ggplot2::facet_wrap(~var, scales = "free") +
      ggplot2::geom_path()
    print(gp)
  }
  
  if(property == "water"){
    
    tmp <- xsoil
    gp <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = -soil.depth.bottom, y = SAT)) +
              ggplot2::xlab("Soil Depth (cm)") + 
              ggplot2::ylab("proportion") + 
              ggplot2::geom_line() +
              ggplot2::geom_line(ggplot2::aes(x = -soil.depth.bottom, y = AirDry), color = "red") + 
              ggplot2::ggtitle("Soil water (AirDry, LL15, DUL, SAT)") +
              ggplot2::geom_ribbon(ggplot2::aes(ymin = LL15, ymax = DUL), 
                                   color = "blue",
                                   fill = "deepskyblue1") + 
              ggplot2::coord_flip()  
    print(gp)
  }
}

#' Check an apsimx soil profile
#' 
#' @rdname apsimx_soil_profile
#' @description checking an apsimx soil profile for reasonable values
#' @param x object of class \sQuote{soil_profile} or the \sQuote{soil} 
#' component within an object of class \sQuote{soil_profile}.
#' @return It does not produce output unless potential issues are found. Only warnings
#' are produced and it returns an object of class \sQuote{soil_profile}.
#' @export 
#' 

check_apsimx_soil_profile <- function(x){
  
  if(inherits(x, "soil_profile")){
    soil <- x$soil
  }else{
    soil <- x
  } 
  
  vars <- c("Depth","Thickness", "BD", "AirDry","LL15",
            "DUL","SAT","KS","crop.XF","crop.KL","crop.LL",
            "Carbon","SoilCNRatio", "FOM","FOM.CN","FBiom","FInert",
            "NO3N","NH4N","PH")
  
  soil.names <- names(soil)
  
  if(length(soil.names %in% vars) < length(vars)){
    mtch.nms <- match(soil.names, vars)
    mssn.nms <- vars[-mtch.nms]
    warning(paste("One or more variables missing:", mssn.nms))
  }

  ## Depth cannot be easily checked, need to think harder about this one
  ## Thickness
  if(min(soil$Thickness) <= 0) warning("Thickness is zero or negative")
  ## Bulk Density (BD)
  if(min(soil$BD) <= 0) warning("BD (Bulk Density) cannot be zero or negative")
  if(max(soil$BD) >= 3) warning("BD (Bulk Density) value is too high")
  ## AirDry
  if(min(soil$AirDry) <= 0) warning("AirDry is zero or negative")
  if(max(soil$AirDry) > 1) warning("AirDry is too high")
  ## LL15
  if(min(soil$LL15) <= 0) warning("LL15 is zero or negative")
  if(max(soil$LL15) > 1) warning("LL15 is too high")
  ## DUL
  if(min(soil$DUL) <= 0) warning("DUL is zero or negative")
  if(max(soil$DUL) > 1) warning("DUL is too high")
  ## SAT
  if(min(soil$SAT) <= 0) warning("SAT is zero or negative")
  if(max(soil$SAT) > 1) warning("SAT is too high")
  ## KS 
  if(min(soil$KS) <= 0) warning("KS is zero or negative")
  ## crop.XF
  if(min(soil$crop.XF) <= 0) warning("crop.XF is zero or negative")
  if(max(soil$crop.XF) > 1) warning("crop.XF is too high")
  ## crop.KL
  if(min(soil$crop.KL) <= 0) warning("crop.KL is zero or negative")
  if(max(soil$crop.KL) > 1) warning("crop.KL is too high")
  ## crop.LL
  if(min(soil$crop.LL) <= 0) warning("crop.LL is zero or negative")
  if(max(soil$crop.LL) > 1) warning("crop.LL is too high")
  ## Carbon
  if(min(soil$Carbon) <= 0) warning("Carbon is zero or negative")
  ## SoilCNRatio
  if(min(soil$SoilCNRatio) <= 0) warning("SoilCNRatio is zero or negative")
  ## FOM
  if(min(soil$FOM) <= 0) warning("FOM is zero or negative")
  ## FOM.CN
  if(min(soil$FOM.CN) <= 0) warning("FOM.CN is zero or negative")
  ## FBiom
  if(min(soil$FBiom) <= 0) warning("FBiom is zero or negative")
  if(max(soil$FBiom) > 1) warning("FBiom is too high")
  ## FInert
  if(min(soil$FInert) <= 0) warning("FInert is zero or negative")
  if(max(soil$FInert) > 1) warning("FInert is too high")
  ## NO3N
  if(min(soil$NO3N) <= 0) warning("NO3N is zero or negative")
  ## NH4N
  if(min(soil$NH4N) <= 0) warning("NH4N is zero or negative")
  ## PH
  if(min(soil$PH) <= 0) warning("PH is zero or negative")
  if(max(soil$PH) > 14) warning("PH is too high")
  
  SATminusDUL <- soil$SAT - soil$DUL
  DULminusLL <- soil$DUL - soil$LL
  DULminuscrop.LL <- soil$DUL - soil$crop.LL
  SATminusLL <- soil$SAT - soil$LL
  
  if(any(SATminusDUL <= 0))
    warning("DUL cannot be greater than SAT")
  
  if(any(DULminusLL <= 0))
    warning("LL cannot be greater than DUL")
  
  if(any(DULminuscrop.LL <= 0))
    warning("crop.LL cannot be greater than DUL")

  if(any(SATminusLL <= 0))
    warning("LL cannot be greater than SAT")

  return(invisible(x))
}
