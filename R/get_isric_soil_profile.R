#' Source: https://www.isric.org/ \cr
#' Details: https://www.isric.org/explore/soilgrids/faq-soilgrids \cr
#' 
#' Pedotransfer functions: Saxton and Rawls, 2006. Soil Water Characteristic Estimates by Texture and Organic Matter for Hydrologic Solutions.
#' Soil Sci. Soc. Am. J. 70:1569–1578. \cr
#' 
#' TODO: need to look into how this is done in APSIM NG
#' https://github.com/APSIMInitiative/ApsimX/pull/3994/files \cr
#' 
#' NOTE: Eric Zurcher provided help by sending me an R file originally written by
#' Andrew Moore. It provides a bit of context for how some of the decisions
#' were made for constructing the synthetic soil profiles in APSIM. (email from june 3 2021).
#' 
#' @title Generate a synthetic APSIM soil profile from the ISRIC soil database
#' @description Retrieves soil data from the ISRIC global database and converts it to an APSIM soil_profile object
#' @name get_isric_soil_profile
#' @param lonlat Longitude and latitude vector (e.g. c(-93, 42)).
#' @param statistic default is the mean
#' @param soil.profile a soil profile to fill in in case the default one is not appropriate
#' @param find.location.name default is TRUE. Use either maps package or photon API to find Country/State.
#' If you are running this function many times it might be better to set this to FALSE.
#' @param fix whether to fix compatibility between saturation and bulk density (default is FALSE).
#' @param verbose argument passed to the fix function.
#' @param check whether to check the soil profile (default is TRUE)
#' @param physical whether soil physical properties are obtained from the data base or through \sQuote{SR}, Saxton and Rawls pedotransfer functions.
#' @param xargs additional arguments passed to \code{\link{apsimx_soil_profile}} or \sQuote{apsimx:::approx_soil_variable} function. At the moment these are:
#' \sQuote{soil.bottom}, \sQuote{crops}, and \sQuote{nlayers} for the first function and \sQuote{method} for the second function. 
#' @return it generates an object of class \sQuote{soil_profile}.
#' @details Variable which are directly retrieved and a simple unit conversion is performed: \cr
#' * Bulk density - bdod \cr
#' * Carbon - soc \cr
#' * Clay - clay \cr
#' * Sand - sand \cr
#' * PH - phh2o \cr
#' * Nitrogen - nitrogen \cr
#' Variables which are optionally estimated using pedotransfer functions: \cr
#' LL15, DUL, SAT, KS, AirDry \cr
#' TO-DO: \cr
#' What do I do with nitrogen? \cr
#' Can I use CEC? \cr
#' How can I have a guess at FBiom and Finert? \cr
#' FBiom does not depend on any soil property at the moment, should it? \cr
#' @seealso \code{\link{apsimx_soil_profile}}, \code{\link{edit_apsim_replace_soil_profile}}, \code{\link{edit_apsimx_replace_soil_profile}}.
#' @export
#' @author Fernando E. Miguez, Eric Zurcher (CSIRO) and Andrew Moore (CSIRO)
#' @examples 
#' \dontrun{
#' ## Only run this if rest.isric.org is working
#' rest.isric.on <- suppressWarnings(try(readLines("http://rest.isric.org", 
#' n = 1, warn = FALSE), silent = TRUE))
#'
#' ## Get soil profile properties for a single point
#' if(!inherits(rest.isric.on, "try-error")){
#'   sp1 <- get_isric_soil_profile(lonlat = c(-93, 42), fix = TRUE, verbose = FALSE)
#'   ## Visualize
#'   plot(sp1)
#'   plot(sp1, property = "water")
#'  }
#' }

get_isric_soil_profile <- function(lonlat, 
                                   statistic = c("mean", "Q0.5"),
                                   soil.profile,
                                   find.location.name = TRUE,
                                   fix = FALSE,
                                   verbose = TRUE,
                                   check = TRUE,
                                   physical = c("default", "SR"),
                                   xargs = NULL){

  statistic <- match.arg(statistic)
  physical <- match.arg(physical)
  
  #### Create extent step ####
  lon <- as.numeric(lonlat[1])
  lat <- as.numeric(lonlat[2])
  
  if(lon < -180 || lon > 180) stop("longitude should be between -180 and 180")
  if(lat < -90 || lat > 90) stop("latitude should be between -90 and 90")
  
##  rest0 <- "https://rest.soilgrids.org/soilgrids/v2.0/properties/query?lon="
  rest0 <- "https://rest.isric.org/soilgrids/v2.0/properties/query?lon="
  rest1 <- paste0(rest0, lon, "&lat=", lat)
  rest.properties <- paste("&property=bdod", 
                           "property=soc",
                           "property=phh2o",
                           "property=clay", 
                           "property=sand", 
                           "property=nitrogen",
                           "property=cec", 
                           "property=wv0010",
                           "property=wv0033",
                           "property=wv1500", sep = "&")
  rest.depths <- paste("&depth=0-5cm", "depth=0-30cm", "depth=5-15cm", 
                       "depth=15-30cm", "depth=30-60cm", "depth=60-100cm", "depth=100-200cm", sep = "&")
  rest.statistic <- paste("&value", statistic, sep = "=")
  rest.query <- paste0(rest1, rest.properties, rest.depths, rest.statistic)
  rest.data <- jsonlite::fromJSON(rest.query)
  
  #### Process query
  sp.nms <- rest.data$properties$layers[["name"]]
  
  if(!identical(sp.nms, c("bdod", "cec", "clay", "nitrogen", "phh2o", "sand", "soc", "wv0010", "wv0033", "wv1500"))){
    cat("Found these properties", sp.nms, "\n")
    cat("Expected these properties", c("bdod", "cec", "clay", "nitrogen", "phh2o", "sand", "soc", "wv0010", "wv0033", "wv1500"), "\n")
    stop("soil properties names do not match")
  }
    
  bdod <- rest.data$properties$layers[1,3][[1]][,3]
  cec <- rest.data$properties$layers[2,3][[1]][,3]
  clay <- rest.data$properties$layers[3,3][[1]][,3]
  nitrogen <- rest.data$properties$layers[4,3][[1]][,3]
  phh2o <- rest.data$properties$layers[5,3][[1]][,3]
  sand <- rest.data$properties$layers[6,3][[1]][,3]
  soc <- rest.data$properties$layers[7,3][[1]][,3]
  wv0010 <- rest.data$properties$layers[8,3][[1]][,3]
  wv0033 <- rest.data$properties$layers[9,3][[1]][,3]
  wv1500 <- rest.data$properties$layers[10,3][[1]][,3]
  
  if(any(is.na(soc))) stop("No soil data available for this location. Did you specify the coordinates correctly?")

  ## These are the default thicknesses in ISRIC
  o.thcknss <- c(50, 100, 150, 300, 400, 1000) ## in mm
  thcknss <- c(50, 100, 150, 300, 400, 1000) ## in mm
  
  ## Some variables can be passed to apsimx:::approx_soil_variable
  soil.bottom <- 200
  method <- "constant"
  nlayers <- 6
  crps <- c("Maize", "Soybean", "Wheat")
  if(!is.null(xargs)){
    ### Soil bottom
    if(!is.null(xargs$soil.bottom) && is.null(xargs$nlayers)){
      soil.bottom <- xargs$soil.bottom
      thcknss <- apsimx_soil_profile(nlayers = nlayers, soil.bottom = soil.bottom)$soil$Thickness
    }
    ### Method
    if(!is.null(xargs$method)){
      method <- xargs$method
    }
    ### Number of layers
    if(!is.null(xargs$nlayers) && is.null(xargs$soil.bottom)){
      nlayers <- xargs$nlayers
      ### Need to redefine 'Thickness'
      thcknss <- apsimx_soil_profile(nlayers = nlayers)$soil$Thickness
    }
    ### Do I need this?
    if(!is.null(xargs$nlayers) && !is.null(xargs$soil.bottom)){
      nlayers <- xargs$nlayers
      soil.bottom <- xargs$soil.bottom
      ### Need to redefine 'Thickness'
      thcknss <- apsimx_soil_profile(nlayers = nlayers, soil.bottom = soil.bottom)$soil$Thickness
    }
    ### Crops
    if(!is.null(xargs$crops)){
      crps <- xargs$crops
    }
  }

  ## Create the empty soil profile
  if(missing(soil.profile)){
    if(is.null(xargs)){
      new.soil <- FALSE
    }else{
      new.soil <- TRUE      
    }
    soil_profile <- apsimx_soil_profile(nlayers = nlayers, Thickness = thcknss, soil.bottom = soil.bottom, crops = crps) 
    soil_profile$soil$ParticleSizeClay <- NA
    soil_profile$soil$ParticleSizeSilt <- NA
    soil_profile$soil$ParticleSizeSand <- NA
    soil_profile$soil$CEC <- NA  
    soil_profile$soil$Nitrogen <- NA
    soil_profile$soil$DUL <- NA
    soil_profile$soil$LL15 <- NA
    soil_profile$soil$SAT <- NA
    
  }else{
    ## stop("This is not fully implemented yet. Submit a github issue if you need it.", call. = FALSE)
    soil_profile <- soil.profile
    new.soil <- TRUE
  }
  
  ### For some of the conversions see: https://www.isric.org/explore/soilgrids/faq-soilgrids
  if(new.soil){
    sp.xout <- cumsum(soil_profile$soil$Thickness)
    
    ### Will this take care of both number of layers and soil.bottom?
    if(length(cumsum(thcknss)) != length(bdod[[1]])){
      bdod.dat <- stats::approx(data.frame(cumsum(o.thcknss), bdod[[1]] * 1e-2), xout = sp.xout)
      soc.dat <- stats::approx(data.frame(cumsum(o.thcknss), soc[[1]] * 1e-2), xout = sp.xout)
      phh2o.dat <- stats::approx(data.frame(cumsum(o.thcknss), phh2o[[1]] * 1e-1), xout = sp.xout)
      clay.dat <- stats::approx(data.frame(cumsum(o.thcknss), clay[[1]] * 1e-1), xout = sp.xout)
      sand.dat <- stats::approx(data.frame(cumsum(o.thcknss), sand[[1]] * 1e-1), xout = sp.xout)
      nitrogen.dat <- stats::approx(data.frame(cumsum(o.thcknss), nitrogen[[1]]), xout = sp.xout)
      cec.dat <- stats::approx(data.frame(cumsum(o.thcknss), cec[[1]]), xout = sp.xout)
      wv0010.dat <- stats::approx(data.frame(cumsum(o.thcknss), wv0010[[1]]), xout = sp.xout)
      wv0033.dat <- stats::approx(data.frame(cumsum(o.thcknss), wv0033[[1]]), xout = sp.xout)
      wv1500.dat <- stats::approx(data.frame(cumsum(o.thcknss), wv1500[[1]]), xout = sp.xout)
    }else{
      bdod.dat <- data.frame(x = cumsum(thcknss), y = bdod[[1]] * 1e-2)
      soc.dat <- data.frame(x = cumsum(thcknss), y = soc[[1]] * 1e-2)
      phh2o.dat <- data.frame(x = cumsum(thcknss), y = phh2o[[1]] * 1e-1)
      clay.dat <- data.frame(x = cumsum(thcknss), y = clay[[1]] * 1e-1)
      sand.dat <- data.frame(x = cumsum(thcknss), y = sand[[1]] * 1e-1)
      nitrogen.dat <- data.frame(x = cumsum(thcknss), y = nitrogen[[1]])
      cec.dat <- data.frame(x = cumsum(thcknss), y = cec[[1]])
      wv0010.dat <- data.frame(x = cumsum(thcknss), y = wv0010[[1]])
      wv0033.dat <- data.frame(x = cumsum(thcknss), y = wv0033[[1]])
      wv1500.dat <- data.frame(x = cumsum(thcknss), y = wv1500[[1]])
    }

    soil_profile$soil$BD <- approx_soil_variable(bdod.dat, 
                                                 xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y
    soil_profile$soil$Carbon <- approx_soil_variable(soc.dat, 
                                                 xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y
    soil_profile$soil$PH <- approx_soil_variable(phh2o.dat, 
                                                     xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y 
    soil_profile$soil$ParticleSizeClay <- approx_soil_variable(clay.dat, 
                                                 xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y 
    soil_profile$soil$ParticleSizeSand <- approx_soil_variable(sand.dat, 
                                                   xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y 
    soil_profile$soil$Nitrogen <- approx_soil_variable(nitrogen.dat, 
                                                   xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y
    soil_profile$soil$CEC <- approx_soil_variable(cec.dat, 
                                                       xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y 
    soil_profile$soil$SAT <- approx_soil_variable(wv0010.dat, 
                                                     xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y * 1e-3
    soil_profile$soil$DUL <- approx_soil_variable(wv0033.dat, 
                                                     xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y * 1e-3  
    soil_profile$soil$LL15 <- approx_soil_variable(wv1500.dat, 
                                                  xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y * 1e-3   
  }else{
    soil_profile$soil$BD <- bdod[[1]] * 1e-2
    soil_profile$soil$Carbon <- soc[[1]] * 1e-2
    soil_profile$soil$PH <- phh2o[[1]] * 1e-1  
    soil_profile$soil$ParticleSizeClay <- clay[[1]] * 1e-1
    soil_profile$soil$ParticleSizeSand <- sand[[1]] * 1e-1
    soil_profile$soil$Nitrogen <- nitrogen[[1]]
    soil_profile$soil$CEC <- cec[[1]]
    soil_profile$soil$SAT <- wv0010[[1]] * 1e-3
    soil_profile$soil$DUL <- wv0033[[1]] * 1e-3
    soil_profile$soil$LL15 <- wv1500[[1]] * 1e-3
  }

  soil_profile$soil$ParticleSizeSilt <- 100 - (soil_profile$soil$ParticleSizeSand + soil_profile$soil$ParticleSizeClay)
  ## Populating DUL and LL. These are equations from Table 1 in Saxton and Rawls 2006
  if(physical == "SR"){
    soil_profile$soil$DUL <- sr_dul(soil_profile$soil$ParticleSizeClay, soil_profile$soil$ParticleSizeSand, soil_profile$soil$Carbon * 2)
    soil_profile$soil$LL15 <- sr_ll(soil_profile$soil$ParticleSizeClay, soil_profile$soil$ParticleSizeSand, soil_profile$soil$Carbon * 2)
    DUL_S <- sr_dul_s(soil_profile$soil$ParticleSizeClay, soil_profile$soil$ParticleSizeSand, soil_profile$soil$Carbon * 2)
    soil_profile$soil$SAT <- sr_sat(soil_profile$soil$ParticleSizeSand, soil_profile$soil$DUL, DUL_S)    
  }

  ## Comparing this to the previous calculation
  soil_profile$soil$KS <- sr_ks(soil_profile$soil$ParticleSizeClay, soil_profile$soil$ParticleSizeSand, soil_profile$soil$Carbon * 2)
  
  soil_profile$soil$AirDry <- soil_profile$soil$LL15
  soil_profile$soil$AirDry[1] <- soil_profile$soil$LL15[1] * 0.5 ## AirDry is half of LL for the first layer
  
  for(i in soil_profile$crops){
    soil_profile$soil[[paste0(i,".LL")]] <- soil_profile$soil$LL15 ## Without better information  
  }
  
  #### Passing parameters from soilwat
  ## The soil texture class will be based on the first layer only
  txt_clss <- texture_class(soil_profile$soil$ParticleSizeClay[1] * 1e-2, soil_profile$soil$ParticleSizeSilt[1] * 1e-2)
  t2sp <- texture2soilParms(txt_clss)
  
  if(missing(soil.profile)){
    soil_profile$soilwat <- soilwat_parms(Salb = t2sp$Albedo, CN2Bare = t2sp$CN2, 
                                          SWCON = rep(t2sp$SWCON, nrow(soil_profile$soil)),
                                          Thickness = thcknss)    
  }else{
    soil_profile$soilwat <- soilwat_parms(Salb = t2sp$Albedo, CN2Bare = t2sp$CN2, 
                                          SWCON = rep(t2sp$SWCON, length(soil_profile$soil$Thickness)),
                                          Thickness = soil_profile$soil$Thickness)    
  }
  
  ### Passing the initial soil water?
  if(new.soil){
    ini.wat <- (soil_profile$soil$DUL + soil_profile$soil$LL15) / 2
    isw <- initialwater_parms(Thickness = soil_profile$soil$Thickness, 
                              InitialValues = ini.wat)
  }else{
    ini.wat <- (soil_profile$soil$DUL + soil_profile$soil$LL15) / 2
    isw <- initialwater_parms(Thickness = thcknss, InitialValues = ini.wat)    
  }

  soil_profile$initialwater <- isw

  if(find.location.name){
    if(requireNamespace("maps", quietly = TRUE)){
      country <- maps::map.where(x = lon, y = lat)
      if(country == "USA"){
        state <- toupper(maps::map.where(database = "county", x = lon, y = lat)) 
      }else{
        url <- paste0("https://photon.komoot.io/reverse?lon=", lon, "&lat=", lat)
        fgeo <- jsonlite::fromJSON(url)
        state <- fgeo$feature$properties$state
      }
    }else{
      url <- paste0("https://photon.komoot.io/reverse?lon=", lon, "&lat=", lat)
      fgeo <- jsonlite::fromJSON(url)
      state <- fgeo$feature$properties$state
      country <- fgeo$features$properties$country
    }    
  }else{
    state <- ""
    country <- ""
  }

  #### Attributes ####
  alist <- list()
  alist$SoilType <- paste("SoilType = ", txt_clss)
  alist$State <- state
  alist$Country <- country
  alist$Longitude <- lon
  alist$Latitude <- lat
  alist$DataSource <- paste("Original source is www.isric.org. See: https://www.isric.org/explore/soilgrids/faq-soilgrids ",Sys.time())
  alist$Comments <- paste("resolution = 250m",
                          "- taxonomic classification name =", txt_clss,
                          "- drainage class =", NA, 
                          "- elevation =", NA,
                          "- slope =", NA,
                          "- geomdesc =", NA)
  
  soil_profile$metadata <- alist

  if(fix) soil_profile <- fix_apsimx_soil_profile(soil_profile, verbose = verbose)
  
  if(check) check_apsimx_soil_profile(soil_profile)
  
  return(soil_profile)  
}


#### Pedo Transfer equations (Saxton and Rawls) ####

## Field Capacity or DUL
## According to Saxton and Rawls the inputs are in percent
## However, to match the values in Table 3 I need to convert
## clay and sand to proportion, BUT OM stays in percent!
sr_dul <- function(clay, sand, om){
  p2p <- 1e-2
  clay <- clay * p2p
  sand <- sand * p2p
  om <- om 
  ans0 <- -0.251 * sand + 0.195 * clay + 0.011 * om +
    0.006 * (sand * om) - 0.027 * (clay * om) + 0.452 * (sand * clay) + 0.299
  ans <- ans0 + (1.283 * ans0^2 - 0.374 * ans0 - 0.015)
  ans
}

sr_dul_s <- function(clay, sand, om){
  p2p <- 1e-2
  clay <- clay * p2p
  sand <- sand * p2p
  om <- om 
  ans0 <- 0.278 * sand + clay * 0.034 + om * 0.022 +
    -0.018 * sand * om - 0.027 * clay * om + 
    -0.584 * sand * clay + 0.078
  ans <- ans0 + (0.636 * ans0 - 0.107)
  ans
}

sr_sat <- function(sand, sr_dul, sr_dul_s){
  sand <- sand * 1e-2
  ans <- sr_dul + sr_dul_s - 0.097 * sand + 0.043
  ans
}

sr_ll <- function(clay, sand, om){
  p2p <- 1e-2 ## percent to proportion
  clay <- clay * p2p
  sand <- sand * p2p
  om <- om
  ans0 <- -0.024 * sand + 0.487 * clay + 0.006 * om + 
    0.005 * (sand * om) - 0.013 * (clay * om) + 0.068 * (sand * clay) + 0.031
  ans <- ans0 + (0.14 * ans0 - 0.02)
  ans
} 

### Saxton and Rawls KS
### This version was developed to estimate KS in mm/h
### and this is for 'gravel-free' soil
### Multiplying by 24 converts hours to a full day
### but I'm not sure this will work. APSIM is not too
### sensitive to this soil property as long as it is not
### too low
sr_ks <- function(clay, sand, om, units = c("mm/day", "mm/h")){
  
  units <- match.arg(units)
  dul <- sr_dul(clay, sand, om)
  dul_s <- sr_dul_s(clay, sand, om)
  ll15 <- sr_ll(clay, sand, om)
  sat <- sr_sat(sand, dul, dul_s)
  B <- (log(1500) - log(33))/(log(dul) - log(ll15))
  Lambda <- 1/B
  conv <- ifelse(units == "mm/day", 24, 1)
  ans <- 1930 * (sat - dul)^(3 - Lambda) * conv
  
  if(any(is.na(ans))){
    stop("The function resulted in an NA")
  }
  ans
}

## Texture to other parameters
texture2soilParms <- function(texture.class = "NO DATA"){

  if(length(texture.class) > 1)
    stop("This function only takes arguments of length equal to 1", call. = FALSE)
  
  textureClasses <- c("clay", "silty clay", "sandy clay", "clay loam", "silty clay loam", "sandy clay loam", "loam", "silty loam", "sandy loam", "silt", "loamy sand", "sand", "NO DATA")  
  Albedo <- c(0.12, 0.12, 0.13, 0.13, 0.12, 0.13, 0.13, 0.14, 0.13, 0.13, 0.16, 0.19, 0.13)
  CN2 <- c(73.0, 73.0, 73.0, 73.0, 73.0, 73.0, 73.0, 73.0, 68.0, 73.0, 68.0, 68.0, 73.0)
  SWCON <- c(0.25, 0.3, 0.3, 0.4, 0.5, 0.5, 0.5, 0.5, 0.6, 0.5, 0.6, 0.75, 0.5)
  
  wtc <- which(textureClasses == texture.class)
  ans <- list(textureClasses = textureClasses[wtc], Albedo = Albedo[wtc], CN2 = CN2[wtc], SWCON = SWCON[wtc])
  ans
}

## Potentially useful function provided by Eric Zurcher
## written by Andrew Moore
## I think the values should be in the 0-1 range
#==========================================================================
# Texture class mapping function
#==========================================================================

# Re-express the PSD in terms of the International system, using an equation from Minasny et al. (2001)

intl_clay_propn <- function( usda_clay, usda_silt ) { 
  return( usda_clay) 
}

intl_silt_propn <- function( usda_clay, usda_silt ) { 
  return( max( 0.0, -0.0041 - 0.127*usda_clay + 0.553*usda_silt + 0.17*usda_clay^2 - 0.19*usda_silt^2 + 0.59*usda_clay*usda_silt ) ) 
}

intl_sand_propn <- function( usda_clay, usda_silt ) { 
  return( 1.0 - intl_clay_propn( usda_clay, usda_silt ) - intl_silt_propn( usda_clay, usda_silt ) )
}  

# Texture triangle as equations

texture_class <- function (usda_clay, usda_silt ) {
  
  if(length(usda_clay) > 1 || length(usda_silt) > 1)
    stop("This function only accepts numeric vectors of length equal to 1", call. = FALSE)
  
  if(usda_clay < 0 || usda_clay > 1) stop("usda_clay should be between 0 and 1")
  if(usda_silt < 0 || usda_silt > 1) stop("usda_silt should be between 0 and 1")
  
  intl_clay <- intl_clay_propn( usda_clay, usda_silt )
  intl_silt <- intl_silt_propn( usda_clay, usda_silt )
  intl_sand <- 1.0 - intl_clay - intl_silt
  
  if ((intl_sand < 0.75 - intl_clay) && (intl_clay >= 0.40)) {
    class <- "silty clay"
  } else if ((intl_sand < 0.75 - intl_clay) && (intl_clay >= 0.26)) {
    class <- "silty clay loam"
  } else if (intl_sand < 0.75 - intl_clay) {
    class <- "silty loam"
  } else if ((intl_clay >= 0.40 + (0.305-0.40)/(0.635-0.35) * (intl_sand-0.35)) && (intl_clay < 0.50 + (0.305-0.50)/(0.635-0.50) * (intl_sand - 0.50))) {
    class <- "clay"
  } else if (intl_clay >= 0.26 + (0.305-0.26)/(0.635-0.74) * (intl_sand-0.74)) {
    class <- "sandy clay"
  } else if ((intl_clay >= 0.26 + (0.17-0.26)/(0.83-0.49) * (intl_sand-0.49)) && (intl_clay < 0.10 + (0.305-0.10)/(0.635-0.775) * (intl_sand - 0.775))) {
    class <- "clay loam"
  } else if (intl_clay >= 0.26 + (0.17-0.26)/(0.83-0.49) * (intl_sand-0.49)) {
    class <- "sandy clay loam"
  } else if ((intl_clay >= 0.10 + (0.12-0.10)/(0.63-0.775) * (intl_sand-0.775)) && (intl_clay < 0.10 + (0.305-0.10)/(0.635-0.775) * (intl_sand - 0.775))) {
    class <- "loam"
  } else if (intl_clay >= 0.10 + (0.12-0.10)/(0.63-0.775) * (intl_sand-0.775)) {
    class <- "sandy loam"
  } else if (intl_clay < 0.00 + (0.08-0.00)/(0.88-0.93) * (intl_sand-0.93)) {
    class <- "loamy sand"
  } else {
    class <- "sand"
  }  
  
  return( class )
}

### Saxton and Rawls (Table 3 in the paper)
sr_table <- function(clay, sand, om){
  
  if(missing(clay)){
    texture.classes <- c("sand", "loamy sand", "sandy loam", "loam", "silty loam", "silt", "sandy clay loam", "clay loam", "silty clay loam", "silty clay", "sandy clay", "clay")
  }else{
    
    if(length(clay) != length(sand))
      stop("length of 'clay' should be the same as length of 'sand'")
    
    if(length(clay) != length(om))
      stop("length of 'clay' should be the same as length of 'om'")
    
    if(any(clay < 0)) stop("Clay should be greater than zero")
    if(any(clay > 100)) stop("Clay should be less than 100")
    if(any(sand < 0)) stop("Sand should be greater than zero")
    if(any(sand > 100)) stop("Sand should be less than 100")
    
  }
  
  ans <- data.frame(texture.class = NA, sand = NA, clay = NA,
                    om = NA, ll15 = NA, dul = NA, sat = NA, 
                    paw = NA, ks = NA)
  
  if(missing(clay)){
    clay <- c(5, 5, 10, 20, 15, 5, 25, 35, 35, 45, 40, 50)
    sand <- c(88, 80, 65, 40, 20, 10, 60, 30, 10, 10, 50, 25)
    om <- rep(2.5, length(clay))
    for(i in seq_along(texture.classes)){
      ans[i, "texture.class"] <- texture.classes[i]
      ans[i, "sand"] <- sand[i]
      ans[i, "clay"] <- clay[i]
      ans[i, "om"] <- om[i]
      ll15 <- sr_ll(clay[i], sand[i], om[i])
      ans[i, "ll15"] <- ll15
      dul <- sr_dul(clay[i], sand[i], om[i])
      ans[i, "dul"] <- dul
      dul_s <- sr_dul_s(clay[i], sand[i], om[i])
      ans[i, "sat"] <- sr_sat(sand[i], dul, dul_s)
      ans[i, "paw"] <- dul - ll15 
      ans[i, "ks"] <- sr_ks(clay[i], sand[i], om[i], units = "mm/h")
    }    
  }else{
    for(i in seq_along(clay)){
      clay.plus.sand <- clay[i] + sand[i]
      if(clay.plus.sand >= 100){
        warning("Clay plus sand cannot be greater or equal to 100")
      }
      silt <- 100 - (clay[i] + sand[i])
      ans[i, "texture.class"] <- texture_class(clay[i], silt)
      ans[i, "sand"] <- sand[i]
      ans[i, "clay"] <- clay[i]
      ans[i, "om"] <- om[i]
      ll15 <- sr_ll(clay[i], sand[i], om[i])
      ans[i, "ll15"] <- ll15
      dul <- sr_dul(clay[i], sand[i], om[i])
      ans[i, "dul"] <- dul
      dul_s <- sr_dul_s(clay[i], sand[i], om[i])
      ans[i, "sat"] <- sr_sat(sand[i], dul, dul_s)
      ans[i, "paw"] <- dul - ll15 
      ans[i, "ks"] <- sr_ks(clay[i], sand[i], om[i], units = "mm/h")
    }
  }
 return(ans) 
}