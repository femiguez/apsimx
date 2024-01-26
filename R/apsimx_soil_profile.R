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
#' @param ParticleSizeClay particle size clay (in percent)
#' @param ParticleSizeSilt particle size silt (in percent)
#' @param ParticleSizeSand particle size sand (in percent)
#' @param soil.bottom bottom of the soil profile (cm)
#' @param water.table water table level (not used at the moment) (cm)
#' @param soil.type might use it in the future for auto filling missing information
#' @param crops name of crops being grown
#' @param metadata list with soil metadata. For possible parameters and values see an example of \code{\link{inspect_apsimx}} with soil.child = \dQuote{Metadata}.
#' @param soilwat optional \sQuote{list} of class \sQuote{soilwat_parms}
#' @param swim optional \sQuote{list} of class \sQuote{swim_parms}
#' @param initialwater optional \sQuote{list} of class \sQuote{initialsoilwater_parms}
#' @param soilorganicmatter optional \sQuote{list} of class \sQuote{soilorganicmatter_parms}
#' @param dist.parms parameter values for creating a profile. If a == 0 and b == 0 then \cr
#' a constant value of 1 is used. If a == 0 and b != 0, then an exponential decay is used. \cr
#' If a != 0 and b != 0 then the equation is \code{a*soil.layer*exp(-b*soil.layer)}.  
#' @param check whether to check for reasonable values using \code{\link{check_apsimx_soil_profile}}
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
                                 ParticleSizeClay = NULL,
                                 ParticleSizeSilt = NULL,
                                 ParticleSizeSand = NULL,
                                 soil.bottom = 150,
                                 water.table = 200, 
                                 soil.type = 0,
                                 crops = c("Maize","Soybean","Wheat"),
                                 metadata = NULL,
                                 soilwat = NA,
                                 swim = NA,
                                 initialwater = NA,
                                 soilorganicmatter = NA,
                                 dist.parms = list(a = 0, b = 0.2),
                                 check = TRUE){

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
  
  ## These are some values for a sandy clay loam
  if(missing(ParticleSizeClay)){
    ParticleSizeClay <- rep(25, nlayers)
  }else{
    if(length(ParticleSizeClay) != nlayers)
      stop("ParticleSizeClay length should be equal to the number of layers", call. = FALSE)
  }
  
  if(missing(ParticleSizeSilt)){
    ParticleSizeSilt <- rep(15, nlayers)
  }else{
    if(length(ParticleSizeSilt) != nlayers)
      stop("ParticleSizeSilt length should be equal to the number of layers", call. = FALSE)
  }
  
  if(missing(ParticleSizeSand)){
    ParticleSizeSand <- rep(60, nlayers)
  }else{
    if(length(ParticleSizeSand) != nlayers)
      stop("ParticleSizeSand length should be equal to the number of layers", call. = FALSE)
  }
  
  ## Build the crop soil
  crop.soil <- as.data.frame(replicate(length(crops), do.call(cbind, list(crop.KL, crop.LL, crop.XF))))
  names(crop.soil) <- as.vector(sapply(crops, function(x) paste0(x, c(".KL", ".LL", ".XF"))))
    
  soil <- data.frame(Depth=Depth, Thickness=Thickness, BD=BD, 
                     AirDry=AirDry, LL15=LL15, DUL=DUL, SAT=SAT, 
                     KS=KS, Carbon=Carbon, 
                     SoilCNRatio=SoilCNRatio, FOM=FOM, 
                     FOM.CN=FOM.CN, FBiom=FBiom, FInert=FInert,
                     NO3N=NO3N, NH4N=NH4N, PH=PH, 
                     ParticleSizeClay = ParticleSizeClay,
                     ParticleSizeSilt = ParticleSizeSilt,
                     ParticleSizeSand = ParticleSizeSand)
  
    names(soil) <- c("Depth","Thickness", "BD", "AirDry","LL15",
                     "DUL","SAT","KS", "Carbon","SoilCNRatio", "FOM",
                     "FOM.CN","FBiom","FInert",
                     "NO3N","NH4N","PH", "ParticleSizeClay", "ParticleSizeSilt",
                     "ParticleSizeSand")
    
    soil <- cbind(soil, crop.soil)
    
    ans <- list(soil=soil, crops = crops, metadata = metadata, soilwat = soilwat, swim = swim, initialwater = initialwater, soilorganicmatter = soilorganicmatter)
    class(ans) <- "soil_profile"
    
    ## Check for reasonable values
    if(check) check_apsimx_soil_profile(ans)
    
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
                                              "KS", "Carbon", "SoilCNRatio", 
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
  
  crops.property <- as.vector(sapply(x$crops, function(x) paste0(x, c(".XF", ".KL", ".LL"))))
  
  property.in.crops <- try(match.arg(property, choices = crops.property, several.ok = TRUE), silent = TRUE)
  
  properties <- c("all", "water","BD",
                  "AirDry","LL15","DUL","SAT",
                  "KS", "Carbon", "SoilCNRatio", 
                  "FOM","FOM.CN","FBiom",
                  "FInert","NO3N","NH4N","PH")
    
  if(inherits(property.in.crops, "try-error")){
    property <- try(match.arg(property), silent = TRUE) 
    if(inherits(property, "try-error")){
      cat("property should be one of:", c(properties, crops.property), "\n")
      stop("property does not match on of the possible properties", call. = FALSE)
    }
  }else{
    property <- try(match.arg(property, choices = crops.property, several.ok = TRUE), silent = TRUE) 
    if(inherits(property, "try-error")){
      cat("Available crop properties", crops.property, "\n")
      stop("Crop property is not in the possible list of crop properties", call. = FALSE)      
    }
  }

  if(property != "all" && property != "water"){
    
    tmp <- xsoil[,c(property,"Depth","soil.depth.bottom")]
    gp <- ggplot2::ggplot() + 
                   ggplot2::geom_point(ggplot2::aes(x = tmp[,1], y = -tmp[,3])) + 
                   ggplot2::geom_path(ggplot2::aes(x = tmp[,1], y = -tmp[,3])) +
                   ggplot2::xlab(property) + 
                   ggplot2::ylab("Soil Depth (cm)") 
    print(gp)
  }
  
  if(property == "all"){
    
    tmp <- xsoil
    ## Check if property is missing
    tmp.nms0 <- names(tmp)
    tmp.nms <- setdiff(tmp.nms0, c("Depth", "Thickness"))

    dat0 <- NULL
    for(i in tmp.nms){
      tmp1 <- data.frame(var = i, dist = tmp[,i])
      dat0 <- rbind(dat0, tmp1)
    }
    ## This looks dumb, but I'd rather not need a new package for such a simple task
    # bd <- data.frame(var = "BD", dist = tmp[,"BD"]) # 1
    # ad <- data.frame(var = "AirDry", dist = tmp[,"AirDry"]) # 2
    # ll <- data.frame(var = "LL15", dist = tmp[,"LL15"]) # 3
    # dul <- data.frame(var = "DUL", dist = tmp[,"DUL"]) # 4
    # sat <- data.frame(var = "SAT", dist = tmp[,"SAT"]) # 5
    # ks <- data.frame(var = "KS", dist = tmp[,"KS"]) # 6
    # carbon <- data.frame(var = "Carbon", dist = tmp[,"Carbon"]) # 7
    # soilcn <- data.frame(var = "SoilCNRatio", dist = tmp[,"SoilCNRatio"]) # 8
    # fom <- data.frame(var = "FOM", dist = tmp[,"FOM"]) # 9
    # fom.cn <- data.frame(var = "FOM.CN", dist = tmp[,"FOM.CN"]) # 10
    # fbiom <- data.frame(var = "FBiom", dist = tmp[,"FBiom"]) # 11
    # finert <- data.frame(var = "FInert", dist = tmp[,"FInert"]) # 12
    # no3n <- data.frame(var = "NO3N", dist = tmp[,"NO3N"]) # 13
    # nh4n <- data.frame(var = "NH4N", dist = tmp[,"NH4N"]) # 14
    # ph <- data.frame(var = "PH", dist = tmp[,"PH"]) # 15
    
    ## The code below is not needed
    crops.xf <- NULL
    crops.kl <- NULL
    crops.ll <- NULL
    num.crops <- length(x$crops)
    num.crops.dats <- num.crops * 3
    for(i in x$crops){
      crops.xf <- rbind(crops.xf, data.frame(var = paste0(i, ".XF"), dist = tmp[,paste0(i, ".XF")]))
      crops.kl <- rbind(crops.kl, data.frame(var = paste0(i, ".KL"), dist = tmp[,paste0(i, ".KL")])) 
      crops.ll <- rbind(crops.ll, data.frame(var = paste0(i, ".LL"), dist = tmp[,paste0(i, ".LL")])) 
    }

    soil.depth.bottoms <- rep(xsoil$soil.depth.bottom, length(tmp.nms))
    
    # dat0 <- rbind(bd, ad, ll, dul, sat, ks, carbon,
    #               soilcn, fom, fom.cn, fbiom, finert, no3n, nh4n, ph,
    #               crops.xf, crops.kl, crops.ll)
    
    dat <- data.frame(dat0, soil.depths = soil.depth.bottoms)
    dat$soil.depth.bottom <- NULL
    
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
  invisible(gp)
}

#' Check an apsimx soil profile
#' 
#' The value of soil particle density (2.65 g/cm3) is hard coded in APSIM. 
#' https://en.wikipedia.org/wiki/Bulk_density
#' 
#' @rdname apsimx_soil_profile
#' @description checking an apsimx soil profile for reasonable values
#' @param x object of class \sQuote{soil_profile} or the \sQuote{soil} 
#' component within an object of class \sQuote{soil_profile}.
#' @param particle.density default value for soil particle density (2.65 g/cm3)
#' @return It does not produce output unless potential issues are found. Only warnings
#' are produced and it returns an object of class \sQuote{soil_profile}.
#' @export 
#' 

check_apsimx_soil_profile <- function(x, particle.density = 2.65){
  
  if(!inherits(x, "soil_profile"))
    stop("object should be of class 'soil_profile'", call. = FALSE)
  
  if(inherits(x, "soil_profile")){
    soil <- x$soil
  }else{
    soil <- x
  } 
  
  crop.vars <- as.vector(sapply(x$crops, function(x) paste0(x, c(".KL", ".LL", ".XF"))))
  
  vars <- c("Depth","Thickness", "BD", "AirDry","LL15",
            "DUL","SAT","KS",
            "Carbon","SoilCNRatio", "FOM","FOM.CN","FBiom","FInert",
            "NO3N","NH4N","PH", crop.vars)
  
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
  ## crop.soil
  for(i in seq_along(crop.vars)){
    if(min(soil[[crop.vars[i]]]) < 0) warning(paste(crop.vars[i], "is negative"))
    if(max(soil[[crop.vars[i]]]) > 1) warning(paste(crop.vars[i], "is greater than 1"))
  }
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
  
  ## Checking texture if it exists
  if(!is.null(soil$Clay)){
    if(any(soil$Clay > 100)) warning("Clay cannot be greater than 100")
    if(any(soil$Clay < 0)) warning("Clay is negative")
  }
  
  if(!is.null(soil$Silt)){
    if(any(soil$Silt > 100)) warning("Silt cannot be greater than 100")
    if(any(soil$Silt < 0)) warning("Silt is negative")
  }
  
  if(!is.null(soil$Sand)){
    if(any(soil$Sand > 100)) warning("Sand cannot be greater than 100")
    if(any(soil$Sand < 0)) warning("Sand is negative")
  }
  
  SATminusDUL <- soil$SAT - soil$DUL
  DULminusLL <- soil$DUL - soil$LL
  DULminuscrop.LL <- soil$DUL - soil$crop.LL
  SATminusLL <- soil$SAT - soil$LL
  LLminuscrop.LL <- soil$LL - soil$crop.LL
  LLminusAirDry <- soil$LL - soil$AirDry
  AirDryminuscrop.LL <- soil$AirDry - soil$crop.LL
  
  if(any(SATminusDUL <= 0))
    warning("DUL cannot be greater than SAT")
  
  if(any(DULminusLL <= 0))
    warning("LL cannot be greater than DUL")
  
  if(any(DULminuscrop.LL <= 0))
    warning("crop.LL cannot be greater than DUL")

  if(any(SATminusLL <= 0))
    warning("LL cannot be greater than SAT")
  
  if(any(LLminuscrop.LL < 0))
    warning("crop.LL cannot be lower than soil LL")
  
  if(any(LLminusAirDry < 0))
    warning("AirDry cannot be greater than LL")
  
  if(any(AirDryminuscrop.LL < 0))
    warning("crop.LL cannot be lower than AirDry")
  
  ## This check is from APSIM Next Gen Models/Soils/Soil.cs line 250
  ## Compute max BD for each layer
  spd <- particle.density
  max_bd <- (1 - soil$SAT) * spd
  bd_diff <- max_bd - soil$BD

  for(j in seq_along(max_bd)){
    if(bd_diff[j] <= 0){
      warning(paste("Saturation of:", soil$SAT[j], "in layer:", j, "is above acceptable value of:", 1 - soil$BD[j] / spd, ".",
                    "You must adjust bulk density to below", (1 - soil$SAT[j]) * spd, "OR saturation to below", 1 - soil$BD[j] / 2.65))      
    }
  }
  
  ## Check for initial water
  if(!is.null(soil$initialwater)){
    ## Initial Water can't be greater than DUL?
    if(!is.na(soil$initialwater$InitialValues)){
      for(j in seq_along(soil$initialwater$InitialValues)){
        iwat <- soil$initialwater$InitialValues[j] - soil$DUL[j]
        if(iwat <= 0){
          warning(paste("Initial Water in layer:", j, "is greater than DUL"))
        }
      }      
    }
  }
  
  return(invisible(x))
}

#'
#' @title Fixing a soil profile
#' @name fix_apsimx_soil_profile
#' @param x object of class \sQuote{soil_profile}
#' @param soil.var whether to change SAT or BD. At the moment it only changes SAT.
#' @param particle.density soil particle density 
#' @param verbose whether to print the changes made to the soil profile
#' @noRd
#' @author Fernando Miguez with input from Julien Morel
#' @examples
#' \donttest{
#' sp <- get_isric_soil_profile(lonlat = c(1, 48))
#' ## This produces a warning
#' sp1 <- apsimx:::fix_apsimx_soil_profile(sp)
#' check_apsimx_soil_profile(sp1)
#' }
fix_apsimx_soil_profile <- function(x, soil.var = c("SAT", "BD"), particle.density = 2.65, verbose = TRUE){
  
  if(!inherits(x, "soil_profile"))
    stop("object should be of class 'soil_profile'", call. = FALSE)
  ## Heuristics for fixing soil profiles
  
  soil.var <- match.arg(soil.var)
  
  ## Bulk density and saturation issue
  max_bd <- (1 - x$soil$SAT) * 2.65
  bd_diff <- max_bd - x$soil$BD
  
  for(j in seq_along(max_bd)){
    if(bd_diff[j] <= 0){
      x$soil$SAT[j] <- 1 - x$soil$BD[j] / 2.65 - 0.001
      if(verbose && soil.var == "SAT"){
        cat("Saturation of:", x$soil$SAT[j], "in layer:", j, "was above acceptable value of:", 1 - x$soil$BD[j] / 2.65, ".",
            "It was adjusted to:", 1 - x$soil$BD[j] / 2.65 - 0.001, "\n")              
      }
      ## Fixing LL and air dry issue
      if(x$soil$LL15[j] < x$soil$AirDry[j]){
        if(verbose){
          cat("LL15 cannot be lower than AirDry in layer:", j,".\n",
              "It was adjusted to the value of AirDry.\n")
        }
        x$soil$LL15[j] <- x$soil$AirDry[j]
      }
    }
  }
  
  ## Trying to fix the initialwater issue
  if(!is.na(x$initialwater)){
    if(!is.na(x$initialwater$InitialValues)){
      for(j in seq_along(x$initialwater$InitialValues)){
        iwat <- x$initialwater$InitialValues[j] - x$soil$DUL[j]
        if(iwat < 0){
          x$initialwater$InitialValues[j] <- x$soil$DUL * 0.9  
          if(verbose){
            cat("InitialWater cannot be greater than DUL in layer:", j,".\n",
                "It was adjusted to the value of 0.9 * DUL.\n")
          }
        }
      }
    }
  }
  
  return(x)
}


#' 
#' @title Compare two or more soil profiles
#' @name compare_apsim_soil_profile
#' @rdname compare_apsim_soil_profile
#' @description Helper function which allows for a simple comparison among soil_profile objects
#' @param ... met file objects. Should be of class \sQuote{met}
#' @param soil.var meteorological variable to use in the comparison. Either \sQuote{all},
#' \sQuote{radn}, \sQuote{maxt}, \sQuote{mint}, \sQuote{rain}, \sQuote{rh}, 
#' \sQuote{wind_speed} or \sQuote{vp}. 
#' @param property meteorological variable to use in the comparison. Either \sQuote{all},
#' \sQuote{radn}, \sQuote{maxt}, \sQuote{mint}, \sQuote{rain}, \sQuote{rh}, 
#' \sQuote{wind_speed} or \sQuote{vp}. 
#' @param labels labels for plotting and identification of \sQuote{soil_profile} objects.
#' @param check whether to check \sQuote{soil_profile} objects using \sQuote{check_apsimx_soil_profile}.
#' @param verbose whether to print agreement values (default is FALSE).
#' @note I have only tested this for 2 or 3 objects. The code is set up to be able to 
#' compare more, but I'm not sure that would be all that useful.
#' @export
#' @return object of class \sQuote{soil_profile_mrg}, which can be used for further plotting
#' @examples 
#' \dontrun{
#' require(soilDB)
#' require(sp)
#' require(sf)
#' require(spData)
#' # Get two soil profiles
#' sp1 <- get_ssurgo_soil_profile(lonlat = c(-93, 42))
#' sp2 <- get_ssurgo_soil_profile(lonlat = c(-92, 41))
#' # Compare them
#' cmp <- compare_apsim_soil_profile(sp1[[1]], sp2[[1]], labels = c("sp1", "sp2"))
#' # Plot the variables
#' plot(cmp)
#' }
#' 

compare_apsim_soil_profile <- function(..., 
                                       soil.var = c("all", "Thickness", 
                                          "BD", "AirDry", "LL15", 
                                          "DUL", "SAT", "KS", "Carbon", "SoilCNRatio",
                                          "FOM", "FOM.CN", "FBiom", "FInert", "NO3N",
                                          "NH4N", "PH"),
                                      property,
                                      labels,
                                      check = FALSE,
                                      verbose = FALSE){
  
  soils <- list(...)
  
  n.soils <- length(soils)
  
  soil.var <- match.arg(soil.var)
  
  if(!missing(property)) soil.var <- property
  
  if(!missing(property) && soil.var == "all")
    warning("Either use property or soil.var but not both. soil.var will be ignored.")
  
  if(n.soils < 2) stop("you should provide at least two soil_profiles", call. = FALSE)
  
  soil1 <- soils[[1]]
  
  m.nms <- NULL
  if(!missing(labels)){
    m.nms <- labels
    if(length(labels) != n.soils)
      stop(" 'labels' length should be the same as the number of 'soil_profile' objects", call. = FALSE)
  } 
  
  if(!inherits(soil1, "soil_profile")) stop("object should be of class 'soil_profile' ", call. = FALSE)
  
  ## Check for any issues
  if(check) check_apsimx_soil_profile(soil1$soil)
  
  ## Should have the same number of layers
  soil.mrg <- soil1$soil
  names(soil.mrg) <- paste0(names(soil.mrg), ".1")  
  soil1 <- soil1$soil
  nms1 <- names(soil1)
  
  for(i in 2:n.soils){
    
    if(check) check_apsimx_soil_profile(soils[[i]])
    
    soil.i <- soils[[i]]$soil
    
    if(nrow(soil1) != nrow(soil.i)) stop("soil profiles should have the same number of rows", call. = FALSE)
    
    if(ncol(soil1) != ncol(soil.i) || length(setdiff(names(soil1), names(soil.i))) > 0){
      warning("Number of columns is not the same for the soil profiles. Selecting the ones in common.")
      common.names <- intersect(names(soil1), names(soil.i))
      soil1 <- subset(soil1, select = common.names)
      soil.i <- subset(soil.i, select = common.names)
    }
  
    names(soil1) <- paste0(names(soil1), ".1")  
    names(soil.i) <- paste0(names(soil.i), ".", i)
    ## drop the year.i and day.i names
    soil.mrg <- cbind(soil.mrg, soil.i)
  }
  
  if(soil.var == "all"){
    ans <- data.frame(variable = setdiff(names(soil1), c("Depth")),
                      vs = NA, labels = NA,
                      bias = NA, slope = NA, corr = NA)
    if(missing(labels)) ans$labels <- NULL
  }else{
    ans <- data.frame(variable = soil.var,
                      vs = NA, labels = NA,
                      bias = NA, slope = NA, corr = NA)
    if(missing(labels)) ans$labels <- NULL
  }
  
  ## Calculate bias for all variables
  if(soil.var == "all"){
    soil.var.sel <- nms1[!(nms1 %in% c("Depth"))]
    gvar.sel <- paste0(soil.var.sel, collapse = "|")
    idx.soil.mrg <- grep(gvar.sel, names(soil.mrg))
    soil.mrg.s <- soil.mrg[,idx.soil.mrg]
    
    k <- 1  
    ## Compute Bias matrix
    for(i in soil.var.sel){
      if(verbose) cat("Variable ", i, "\n")
      ans$variable[k] <- i
      tmp <- soil.mrg.s[, grep(i, names(soil.mrg.s))]
      if(ncol(tmp) > 2){
        if(i == "FOM"){
          tmp <- soil.mrg.s[, grep("FOM.[1-9]", names(soil.mrg.s))]    
        }else{
          tmp <- soil.mrg.s[, grep("FOM.CN", names(soil.mrg.s))]   
        }
      }
      if(ncol(tmp) < 2) stop("merged selected variables should be at least of length 2", call. = FALSE)
      
      for(j in 2:ncol(tmp)){
        if(verbose) cat(names(tmp)[j - 1], " vs. ", names(tmp)[j], "\n")
        ans$vs[k] <- paste(names(tmp)[j - 1], "vs.", names(tmp)[j])
        if(!missing(labels)){
          if(verbose) cat("labels", labels[j - 1], " vs. ", labels[j], "\n")
          ans$labels[k] <- paste(labels[j - 1], "vs.", labels[j])
        } 
        if(abs(sum(tmp[, j - 1] - tmp[, j])) < 0.0001){
          if(verbose) cat(paste("Variable", i, "appears identical \n"))
          ans$bias[k] <- NA
          ans$slope[k] <- NA
          ans$corr[k] <- NA
          ans$rss[k] <- NA
          ans$rmse[k] <- NA
          next
        }
          
        fm0 <- lm(tmp[, j - 1] ~ tmp[, j])
        if(verbose) cat(" \t Bias: ", coef(fm0)[1], "\n")
        ans$bias[k] <- coef(fm0)[1]
        if(verbose) cat(" \t Slope: ", coef(fm0)[2], "\n")
        ans$slope[k] <- coef(fm0)[2]
        if(verbose) cat(" \t Corr: ", cor(tmp[,j - 1], tmp[, j]), "\n")
        ans$corr[k] <- cor(tmp[,j - 1], tmp[, j])
        if(verbose) cat(" \t RSS: ", deviance(fm0), "\n")
        ans$rss[k] <- deviance(fm0)
        if(verbose) cat(" \t RMSE: ", sigma(fm0), "\n")
        ans$rmse[k] <- sigma(fm0)
      }
      k <- k + 1
    }
  }
  
  if(soil.var != "all"){
    ## Just select the appropriate variable
    idx.soil.mrg <- grep(soil.var, names(soil.mrg))
    soil.mrg.s <- soil.mrg[,idx.soil.mrg]
    
    if(verbose) cat("Variable ", soil.var, "\n")
    ans$variable[1] <- soil.var
    
    tmp <- soil.mrg.s
    for(j in 2:ncol(tmp)){
      if(verbose) cat(names(tmp)[j - 1], " vs. ", names(tmp)[j], "\n")
      ans$vs[1] <- paste(names(tmp)[j - 1], "vs.", names(tmp)[j])
      if(!missing(labels)){
        if(verbose) cat("labels", labels[j - 1], " vs. ", labels[j], "\n")
        ans$labels[1] <- paste(labels[j - 1], "vs.", labels[j])
      }
      fm0 <- lm(tmp[, j - 1] ~ tmp[, j])
      if(verbose) cat(" \t Bias: ", coef(fm0)[1], "\n")
      ans$bias[1] <- coef(fm0)[1]
      if(verbose) cat(" \t Slope: ", coef(fm0)[2], "\n")
      ans$slope[1] <- coef(fm0)[2]
      if(verbose) cat(" \t Corr: ", cor(tmp[,j - 1], tmp[, j]), "\n")
      ans$corr[1] <- suppressWarnings(cor(tmp[,j - 1], tmp[, j]))
      if(verbose) cat(" \t RSS: ", deviance(fm0), "\n")
      ans$rss[1] <- deviance(fm0)
      if(verbose) cat(" \t RMSE: ", sigma(fm0), "\n")
      ans$rmse <- sigma(fm0)
    }
  }
  
  attr(soil.mrg, "soil.names") <- m.nms
  attr(soil.mrg, "length.soils") <- n.soils  
  soil.mrg <- structure(list(soil.mrg = soil.mrg, index.table = ans),
                       class = "soil_profile_mrg")
  invisible(soil.mrg)
}

#' print method for soil_profile_mrg
#' Only variables which are not identical will be printed
#' @rdname compare_apsim_soil_profile
#' @description print method for \sQuote{soil_profile_mrg}
#' @param x object of class \sQuote{soil_profile_mrg}
#' @param ... additional arguments passed to print
#' @param digits number of digits to print (default is 2)
#' @return a table with indexes for the soil profiles
#' @export
print.soil_profile_mrg <- function(x, ..., digits = 2){
  print(x$index.table[!is.na(x$index.table$bias),], digits = digits)
}

#' Plotting function for comparing soil profiles
#' @rdname compare_apsim_soil_profile
#' @description plotting function for compare_apsim_soil_profile, it requires ggplot2
#' @param x object of class \sQuote{soil_profile_mrg}
#' @param ... \sQuote{soil_profile} objects. Should be of class \sQuote{soil_profile}
#' @param plot.type either \sQuote{depth}, \sQuote{vs}, \sQuote{diff} or \sQuote{density}
#' @param pairs pair of objects to compare, defaults to 1 and 2 but others are possible
#' @param soil.var soil variable to plot 
#' @param span argument to be passed to \sQuote{geom_smooth}
#' @return it produces a plot
#' @export
#' 
plot.soil_profile_mrg <- function(x, ..., plot.type = c("depth", "vs", "diff", "density"),
                         pairs = c(1, 2),
                         soil.var = c("all", "Thickness", 
                                      "BD", "AirDry", "LL15", 
                                      "DUL", "SAT", "KS", "Carbon", "SoilCNRatio",
                                      "FOM", "FOM.CN", "FBiom", "FInert", "NO3N",
                                      "NH4N", "PH"),
                         span = 0.75){
  
  if(!requireNamespace("ggplot2", quietly = TRUE)){
    warning("ggplot2 is required for this plotting function")
    return(NULL)
  }

  plot.type <- match.arg(plot.type)
  soil.var <- match.arg(soil.var)
    
  if(plot.type != "depth" && soil.var == "all")
    stop("Please select a soil variable for this type of plot", call. = FALSE)
  
  x <- x$soil.mrg
  
  value <- NULL; depth <- NULL; soil <- NULL
  
  m.nms <- attr(x, "soil.names")
  if(max(pairs) > attr(x, "length.soils")) stop("pairs index larger than length of soils")
  
  if(soil.var == "all"){
    num.vars <- length(grep(".1", names(x), fixed = TRUE))
    num.soils <- attr(x, "length.soils")
    soil.labels <- attr(x, "soil.names")
    tmp <- NULL
    for(i in seq_len(num.soils)){
      wch.col <- grep(paste0(".", i), names(x), fixed = TRUE)
      if(length(wch.col) != num.vars)
        stop("Could not merge soil profiles", call. = FALSE)
      tmp0 <- x[,wch.col]
      names(tmp0) <- gsub(paste0(".", i), "", names(tmp0), fixed = TRUE)
      tmp1 <- data.frame(soil = soil.labels[i], tmp0)
      ## Insert depth variable
      tmp1$depth[1] <- tmp1$Thickness[1] / 2
      tmp1$cum.thickness <- cumsum(tmp1$Thickness)
      for(i in 2:nrow(tmp1)){
        tmp1$depth[i] <- tmp1$cum.thickness[i - 1] + tmp1$Thickness[i] / 2
      }      
      tmp1$Depth <- NULL
      tmp2 <- NULL
      vars <- setdiff(names(tmp1), c("soil", "Thickness"))
      for(j in seq_along(vars)){
        tmp2 <- rbind(tmp2, data.frame(soil = tmp1[["soil"]], depth = tmp1$depth, 
                                       variable = vars[j], value = tmp1[[vars[j]]]))
      }
      tmp <- rbind(tmp, tmp2)
    }
    tmp.o <- tmp[order(tmp$soil, tmp$variable, tmp$depth),]
    tmp3 <- tmp.o[!tmp.o$variable %in% c("depth", "cum.thickness"),]

    gp1 <- ggplot2::ggplot(data = tmp3, ggplot2::aes(x = value, y = depth * 0.1, color = soil)) + 
      ggplot2::facet_wrap(~ variable, scales = "free") + 
      ggplot2::geom_point() + 
      ggplot2::geom_path() + 
      ggplot2::scale_y_reverse() + 
      ggplot2::ylab("Depth (cm)") 
    print(gp1)
  }
  
  if(plot.type == "vs" && soil.var != "all"){
    tmp <- x[, grep(soil.var, names(x))]
    prs <- paste0(soil.var, ".", pairs)
    gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = eval(parse(text = eval(prs[1]))), 
                                                    y = eval(parse(text = eval(prs[2]))))) +
      ggplot2::geom_point() + 
      ggplot2::xlab(paste(m.nms[pairs[1]], prs[1])) + 
      ggplot2::ylab(paste(m.nms[pairs[2]], prs[2])) + 
      ggplot2::geom_smooth(method = "lm") + 
      ggplot2::geom_abline(intercept = 0, slope = 1, color = "orange")
    
    print(gp1)
  }
  
  if(plot.type == "diff" && soil.var != "all"){
    
    prs0 <- paste0(soil.var, ".", pairs)
    prs <- paste0(prs0, collapse = "|")
    tmp <- x[, grep(prs, names(x))]
    
    ## x Variable is prs[1]
    ## y Variable is prs[2] - prs[1]
    dff <- tmp[,prs0[2]] - tmp[,prs0[1]]
    
    gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = eval(parse(text = eval(prs0[1]))), 
                                                    y = dff)) +
      ggplot2::geom_point() + 
      ggplot2::xlab(paste(m.nms[pairs[1]], prs0[1])) + 
      ggplot2::ylab(paste("Difference", prs0[2], "-", prs0[1])) + 
      ggplot2::geom_smooth(method = "lm", ...) + 
      ggplot2::geom_hline(yintercept = 0, color = "orange")
    
    print(gp1)   
  }
  
  if(plot.type == "depth" && soil.var != "all"){
    
    prs0 <- paste0(soil.var, ".", pairs)
    prs <- paste0(prs0, collapse = "|")
    x$depth[1] <- x$Thickness.1[1] / 2
    x$cum.thickness <- cumsum(x$Thickness.1)
    for(i in 2:nrow(x)){
      x$depth[i] <- x$cum.thickness[i - 1] + x$Thickness.1[i] / 2
    } 
    tmp <- x[, grep(prs, names(x))]
    tmp$Depth <- x$depth * 0.1

    gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(y = .data[["Depth"]], 
                                                    x = eval(parse(text = eval(prs0[1]))),
                                                    color = paste(m.nms[pairs[1]], prs0[1]))) +
      
      ggplot2::geom_point() + 
      ggplot2::geom_path() + 
      ggplot2::geom_point(ggplot2::aes(x = eval(parse(text = eval(prs0[2]))),
                                       color = paste(m.nms[pairs[2]], prs0[2]))) + 
      ggplot2::geom_path(ggplot2::aes(x = eval(parse(text = eval(prs0[2]))),
                                       color = paste(m.nms[pairs[2]], prs0[2]))) + 
      ggplot2::ylab("Depth (cm)") + 
      ggplot2::xlab(soil.var) + 
      ggplot2::scale_y_reverse() + 
      ggplot2::theme(legend.title = ggplot2::element_blank())
    
    print(gp1)   
  }
  
  if(plot.type == "density" && soil.var != "all"){
    
    prs0 <- paste0(soil.var, ".", pairs)
    prs <- paste0(prs0, collapse = "|")
    tmp <- x[, grep(prs, names(x))]
    
    gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = eval(parse(text = eval(prs0[1]))),
                                                    color = paste(m.nms[pairs[1]], prs0[1]))) + 
      ggplot2::geom_density() + 
      ggplot2::geom_density(ggplot2::aes(x = eval(parse(text = eval(prs0[2]))),
                                         color = paste(m.nms[pairs[2]], prs0[2]))) +
      ggplot2::xlab(soil.var) + 
      ggplot2::theme(legend.title = ggplot2::element_blank())
    
    print(gp1)
  }
  invisible(gp1)
}

#' Function to calculate carbon stocks. The output units depend on the choice of area.
#' If \sQuote{m2} is used, then the output units will be \sQuote{kg/m2}. If the \sQuote{area}
#' is \sQuote{ha}, then the output units will be \sQuote{Mg/ha}.
#' 
#' Note that the bulk density (which is needed in the calculation) is
#' available as part of the \sQuote{soil_profile} object.
#' 
#' @title Calculate soil carbon stocks
#' @description Calculation of carbon stocks based on an object of class \sQuote{soil_profile}
#' @name carbon_stocks
#' @param x object of class \sQuote{soil_profile}
#' @param depth soil depth (in meters). If missing then the whole soil profile is used.
#' @param area either \sQuote{m2} meter squared or \sQuote{ha}.
#' @param method interpolation method. Either \sQuote{linear} or \sQuote{constant}.
#' @param ... additional arguments passed to internal functions (none used at the moment).
#' @return returns a value with attribute \sQuote{units} and \sQuote{depth}
#' @export
#' @examples 
#' \dontrun{
#' sp <- apsimx_soil_profile()
#' carbon_stocks(sp)
#' carbon_stocks(sp, depth = 0.1)
#' carbon_stocks(sp, depth = 0.2)
#' carbon_stocks(sp, depth = 0.3)
#' carbon_stocks(sp, depth = 0.4)
#' }

carbon_stocks <- function(x, depth, area = c("m2", "ha"), method = c("linear", "constant"), ...){
  
  if(!inherits(x, "soil_profile")){
    stop("This function is intended to be used with an object of class 'soil_profile'", call. = FALSE)
  }
  
  bottom <- sum(x$soil$Thickness) * 1e-3 ## Thickness is in mm, so after conversion this is in meters
  
  if(!missing(depth)){
    if(depth <= 0) stop("'depth' should be a positive number", call. = FALSE)
    if(depth > bottom) stop("'depth' should be a lower number than the bottom of the soil profile ", call. = FALSE)
    if(depth > 10){
      warning("'depth' should be in meters and the value entered is larger than 10. Is this correct?")
    }
  }
  
  area <- match.arg(area)
  method <- match.arg(method)
  
  ## Compute carbon for the whole profile
  if(missing(depth)){
    weights <- x$soil$Thickness / sum(x$soil$Thickness)
    ### Total volume is equal to 'bottom' (m^3)
    ## Original BD (from APSIM) is reported in g/cc, which needs to be multipled by 1e3 to get kg/m^3
    ## Carbon is in percent so it needs to be divided by 100 to get it as a proportion.
    weighted.carbon <- sum(x$soil$Carbon * 1e-2 * weights * x$soil$BD * 1e3) 
    total.carbon <- weighted.carbon * bottom
    depth <- bottom
  }else{
    ## If depth only includes the first layer
    if(depth <= x$soil$Thickness[1] * 1e-3){
      first.layer.carbon <- x$soil$Carbon[1] * 1e-2 * x$soil$BD[1] * 1e3
      total.carbon <- first.layer.carbon * depth
    }else{
      total.carbon <- 0
      cum.thick <- cumsum(x$soil$Thickness) * 1e-3 ## Cumulative thickness in meters
      for(i in 1:nrow(x$soil)){
        ## If the desired depth is greater than the current depth
        ## then add the soil carbon as it is
        if(depth >= cum.thick[i]){ 
          layer.carbon <- x$soil$Carbon[i] * 1e-2 * (x$soil$BD[i] * 1e3) * (x$soil$Thickness[i] * 1e-3)
          total.carbon <- total.carbon + layer.carbon
        }else{
         ## In this case, we need to interpolate 
          crbn <- x$soil$Carbon
          bds <- x$soil$BD
          dat <- data.frame(depth = cum.thick, carbon = crbn, bd = bds)
          tmp.c <- stats::approx(dat$depth, y = dat$carbon, xout = depth, method = method)
          tmp.bd <- stats::approx(dat$depth, y = dat$bd, xout = depth, method = method)  
          layer.carbon <- tmp.c$y * 1e-2 * (tmp.bd$y * 1e3) * (depth - cum.thick[i - 1])
          total.carbon <- total.carbon + layer.carbon
          break
        }
      }
    }
  }

  if(area == "ha"){
    ans <- total.carbon * 1e4 * 1e-3 ## 1e4 converts from m2 to ha. 1e3 converts from kg to Mg
    attr(ans, "units") <- "Mg/ha"
  }else{
    ans <- total.carbon
    attr(ans, "units") <- "kg/m2"
  }
  attr(ans, "depth (m)") <- depth 
  return(ans)
}

#' Function to calculate available water content. The output units depend on the choice of area.
#' If \sQuote{m} is used, then the output units will be \sQuote{mm}. If the \sQuote{area} is \sQuote{m2},
#' then the output units will be in \sQuote{m3}. If the \sQuote{area} is \sQuote{ha}, then the output units will be \sQuote{kg/ha}.
#' 
#' @title Calculate available water content
#' @description Calculation of available water content based on an object of class \sQuote{soil_profile}
#' @name available_water_content
#' @param x object of class \sQuote{soil_profile}
#' @param depth soil depth (in meters). If missing then the whole soil profile is used.
#' @param area either \sQuote{m} meter, \sQuote{m2} meter squared or \sQuote{ha}.
#' @param method interpolation method. Either \sQuote{linear} or \sQuote{constant}.
#' @param weights optional weights
#' @param ... additional arguments passed to internal functions (none used at the moment).
#' @return returns a value with attribute \sQuote{units} and \sQuote{depth}
#' @export
#' @examples 
#' \dontrun{
#' sp <- apsimx_soil_profile()
#' available_water_content(sp)
#' }

available_water_content <- function(x, 
                                    depth, 
                                    area = c("m", "m2", "ha"), 
                                    method = c("linear", "constant"), 
                                    weights, ...){
  
  if(!inherits(x, "soil_profile")){
    stop("This function is intended to be used with an object of class 'soil_profile'", call. = FALSE)
  }
  
  bottom <- sum(x$soil$Thickness) * 1e-3 ## Thickness is in mm, so after conversion this is in meters
  
  if(!missing(depth)){
    if(depth <= 0) stop("'depth' should be a positive number", call. = FALSE)
    if(depth > bottom) stop("'depth' should be a lower number than the bottom of the soil profile ", call. = FALSE)
    if(depth > 10){
      warning("'depth' should be in meters and the value entered is larger than 10. Is this correct?")
    }
  }
  
  area <- match.arg(area)
  method <- match.arg(method)
  
  ## Compute carbon for the whole profile
  if(missing(depth)){
    layer.depth <- x$soil$Thickness ## Thickness in mm
    layer.awc <- x$soil$DUL - x$soil$LL15
    total.awc <- sum(layer.depth * layer.awc)
    depth <- bottom
  }else{
    ## If depth only includes the first layer
    if(depth <= x$soil$Thickness[1] * 1e-3){
      first.layer.awc <- x$soil$DUL[1] - x$soil$LL15[1]
      total.awc <- first.layer.awc * (depth * 1e3) ## Depth is in meters, this answer is in mm
    }else{
      total.awc <- 0
      cum.thick <- cumsum(x$soil$Thickness) * 1e-3 ## Cumulative thickness in meters
      for(i in 1:nrow(x$soil)){
        ## If the desired depth is greater than the current depth
        ## then add the available water content as it is
        if(depth >= cum.thick[i]){ 
          layer.awc <- (x$soil$DUL[i] - x$soil$LL15[i]) * x$soil$Thickness[i] ## in mm
          total.awc <- total.awc + layer.awc
        }else{
          ## In this case, we need to interpolate 
          awc <- x$soil$DUL - x$soil$LL15
          dat <- data.frame(depth = cum.thick, awc = awc)
          tmp.awc <- stats::approx(dat$depth, y = dat$awc, xout = depth, method = method)
          layer.awc <- tmp.awc$y * (depth - cum.thick[i - 1]) * 1e3 ## This is in mm
          total.awc <- total.awc + layer.awc
          break
        }
      }
    }
  }

  if(area == "m"){
    ans <- total.awc 
    attr(ans, "units") <- "mm"
  }
  
  if(area == "m2"){
    ans <- total.awc * 1e-3 ## 1e-3 converts from mm to m3
    attr(ans, "units") <- "m3"
  }
  
  if(area == "ha"){
    ans <- total.awc * 1e4 ## 1 mm is = 1 kg/m2, 1 ha = 10000m2
    attr(ans, "units") <- "kg/ha"
  }
  
  attr(ans, "depth (m)") <- depth 
  return(ans)
}