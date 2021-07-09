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
#' @details Variable which are directly retrieved and a simple unit conversion is performed: \cr
#' * Bulk density - bdod \cr
#' * Carbon - soc \cr
#' * Clay - clay \cr
#' * Sand - sand \cr
#' * PH - phh2o \cr
#' * Nitrogen - nitrogen \cr
#' Variables which are estimated using pedotransfer functions: \cr
#' LL15, DUL, SAT, KS, AirDry \cr
#' TO-DO: \cr
#' What do I do with nitrogen? \cr
#' Can I use CEC? \cr
#' How can I have a guess at FBiom and Finert? \cr
#' FBiom does not depend on any soil property at the moment, should it? \cr
#' @seealso \code{\link{apsimx_soil_profile}}, \code{\link{edit_apsim_replace_soil_profile}}, \code{\link{edit_apsimx_replace_soil_profile}},
#' @export
#' @author Fernando E. Miguez, Eric Zurcher (CSIRO) and Andrew Moore (CSIRO)
#' @examples 
#' \dontrun{
#' ## Get soil profile properties for a single point
#' sp1 <- get_isric_soil_profile(lonlat = c(-93, 42))
#' 
#' ## Visualize
#' plot(sp1)
#' plot(sp1, property = "water")
#' 
#' 
#' }

get_isric_soil_profile <- function(lonlat, 
                                   statistic = c("mean", "Q0.5"),
                                   soil.profile,
                                   find.location.name = TRUE){

  statistic <- match.arg(statistic)

  #### Create extent step ####
  lon <- as.numeric(lonlat[1])
  lat <- as.numeric(lonlat[2])
  
  if(lon < -180 || lon > 180) stop("longitude should be between -180 and 180")
  if(lat < -90 || lat > 90) stop("latitude should be between -90 and 90")
  
  rest0 <- "https://rest.soilgrids.org/soilgrids/v2.0/properties/query?lon="
  rest1 <- paste0(rest0, lon, "&lat=", lat)
  rest.properties <- paste("&property=bdod", 
                           "property=soc",
                           "property=phh2o",
                           "property=clay", 
                           "property=sand", 
                           "property=nitrogen",
                           "property=cec", sep = "&")
  rest.depths <- paste("&depth=0-5cm", "depth=0-30cm", "depth=5-15cm", 
                       "depth=15-30cm", "depth=30-60cm", "depth=60-100cm", "depth=100-200cm", sep = "&")
  rest.statistic <- paste("&value", statistic, sep = "=")
  rest.query <- paste0(rest1, rest.properties, rest.depths, rest.statistic)
  rest.data <- jsonlite::fromJSON(rest.query)
  
  #### Process query
  sp.nms <- rest.data$properties$layers[["name"]]
  
  if(!identical(sp.nms, c("bdod", "cec", "clay", "nitrogen", "phh2o", "sand", "soc"))){
    cat("Found these properties", sp.nms, "\n")
    cat("Expected these properties", c("bdod", "cec", "clay", "nitrogen", "phh2o", "sand", "soc"), "\n")
    stop("soil properties names do not match")
  }
    
  bdod <- rest.data$properties$layers[1,3][[1]][,3]
  cec <- rest.data$properties$layers[2,3][[1]][,3]
  clay <- rest.data$properties$layers[3,3][[1]][,3]
  nitrogen <- rest.data$properties$layers[4,3][[1]][,3]
  phh2o <- rest.data$properties$layers[5,3][[1]][,3]
  sand <- rest.data$properties$layers[6,3][[1]][,3]
  soc <- rest.data$properties$layers[7,3][[1]][,3]
  
  if(any(is.na(soc))) stop("No soil data available for this location. Did you specify the coordinates correctly?")

  ## Create the empty soil profile
  if(missing(soil.profile)){
    thcknss <- c(50, 100, 150, 300, 400, 1000) ## in mm
    soil_profile <- apsimx_soil_profile(nlayers = 6, Thickness = thcknss) 
    soil_profile$soil$Clay <- NA
    soil_profile$soil$Silt <- NA
    soil_profile$soil$Sand <- NA
    soil_profile$soil$CEC <- NA  
    soil_profile$soil$Nitrogen <- NA
    soil_profile$soil$DUL <- NA
    soil_profile$soil$LL15 <- NA
    soil_profile$soil$SAT <- NA
  }else{
    soil_profile <- soil.profile
  }

  ### For some of the conversions see: https://www.isric.org/explore/soilgrids/faq-soilgrids
  soil_profile$soil$BD <- bdod[[1]] * 1e-2
  soil_profile$soil$Carbon <- soc[[1]] * 1e-2
  soil_profile$soil$PH <- phh2o[[1]] * 1e-1  
  soil_profile$soil$Clay <- clay[[1]] * 1e-1
  soil_profile$soil$Sand <- sand[[1]] * 1e-1
  soil_profile$soil$Nitrogen <- nitrogen[[1]]
  soil_profile$soil$CEC <- cec[[1]]
  
  soil_profile$soil$Silt <- 100 - (soil_profile$soil$Clay + soil_profile$soil$Sand)
  
  ## Populating DUL and LL. These are equations from Table 1 in Saxton and Rawls 2006
  soil_profile$soil$DUL <- sr_dul(soil_profile$soil$Clay, soil_profile$soil$Sand, soil_profile$soil$Carbon * 2)
  soil_profile$soil$LL15 <- sr_ll(soil_profile$soil$Clay, soil_profile$soil$Sand, soil_profile$soil$Carbon * 2)
  DUL_S <- sr_dul_s(soil_profile$soil$Clay, soil_profile$soil$Sand, soil_profile$soil$Carbon * 2)
  soil_profile$soil$SAT <- sr_sat(soil_profile$soil$Sand, soil_profile$soil$DUL, DUL_S)
  
  B <- (log(1500) - log(33))/(log(soil_profile$soil$DUL) - log(soil_profile$soil$LL15))
  Lambda <- 1/B
  soil_profile$soil$KS <- (1930 * (soil_profile$soil$SAT - soil_profile$soil$DUL)^(3 - Lambda)) * 100
  
  soil_profile$soil$AirDry <- soil_profile$soil$LL15
  soil_profile$soil$AirDry[1] <- soil_profile$soil$LL15[1] * 0.5 ## AirDry is half of LL for the first layer
  
  #### Passing parameters from soilwat
  ## The soil texture class will be based on the first layer only
  txt_clss <- texture_class(soil_profile$soil$Clay[1] * 1e-2, soil_profile$soil$Silt[1] * 1e-2)
  t2sp <- texture2soilParms(txt_clss)
  soil_profile$soilwat <- soilwat_parms(Salb = t2sp$Albedo, CN2Bare = t2sp$CN2, SWCON = rep(t2sp$SWCON, 6))
  
  if(requireNamespace("maps")){
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
  
  return(soil_profile)  
}


#### Pedo Transfer equations (Saxton and Rawls) ####

## Field Capacity or DUL
sr_dul <- function(clay, sand, om){
  clay <- clay * 1e-2
  sand <- sand * 1e-2
  om <- om * 1e-2
  ans0 <- -0.251 * sand + 0.195 * clay + 0.011 * om +
    0.006 * (sand * om) - 0.027 * (clay * om) + 0.452 * (sand * clay) + 0.299
  ans <- ans0 + (1.283 * ans0^2 - 0.374 * ans0 - 0.015)
  ans
}

sr_dul_s <- function(clay, sand, om){
  clay <- clay * 1e-2
  sand <- sand * 1e-2
  om <- om * 1e-2
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
  clay <- clay * 1e-2
  sand <- sand * 1e-2
  om <- om * 1e-2
  ans0 <- -0.024 * sand + 0.487 * clay + 0.006 * om + 
    0.005 * sand * om + 0.013 *clay * om + 0.068 *sand * clay +  0.031
  ans <- ans0 + (0.14 * ans0 - 0.02)
  ans
} 

## Texture to other parameters
texture2soilParms <- function(texture.class = "NO DATA"){

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



