#' Source: https://esoil.io/TERNLandscapes/Public/Pages/SLGA/index.html
#' 
#' @title Generate a synthetic APSIM soil profile from the SLGA soil database
#' @description Retrieves soil data from the SLGA database (Australia) and converts it to an APSIM soil_profile object
#' @name get_slga_soil_profile
#' @param lonlat Longitude and latitude vector (e.g. c(151.8306, -27.4969)).
#' @param statistic default is the mean
#' @param soil.profile a soil profile to fill in in case the default one is not appropriate
#' @param find.location.name default is TRUE. Use either maps package or photon API to find Country/State.
#' If you are running this function many times it might be better to set this to FALSE.
#' @param fix whether to fix compatibility between saturation and bulk density (default is FALSE).
#' @param verbose argument passed to the fix function.
#' @param check whether to check the soil profile (default is TRUE)
#' @param physical whether soil physical properties are obtained from the data base or through \sQuote{SR}, Saxton and Rawls pedotransfer functions.
#' @param xargs additional arguments passed to \code{\link{apsimx_soil_profile}} or \sQuote{apsimx:::approx_soil_variable} function. At the moment these are:
#' \sQuote{soil.bottom}, \sQuote{method} and \sQuote{nlayers}.
#' @return it generates an object of class \sQuote{soil_profile}.
#' @seealso \code{\link{apsimx_soil_profile}}, \code{\link{edit_apsim_replace_soil_profile}}, \code{\link{edit_apsimx_replace_soil_profile}}.
#' @export
#' @author Fernando E. Miguez, Chloe (Yunru Lai), Eric Zurcher (CSIRO) and Andrew Moore (CSIRO)
#' @examples 
#' \dontrun{
#' ## Get soil profile properties for a single point
#'   sp1 <- get_slga_soil_profile(lonlat = c(151.8306, -27.4969), fix = TRUE, verbose = FALSE)
#'   ## Visualize
#'   plot(sp1)
#'   plot(sp1, property = "water")
#' }


# this is based on the get_isric_soil_profile function of Fernando Miguez, Eric Zurcher and Andrew Moore
get_slga_soil_profile <- function(lonlat, 
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
  
  ## Determine if the location is in Australia
  if(requireNamespace("maps", quietly = TRUE)){
    country <- maps::map.where(x = lon, y = lat)
    if(country != "Australia" || is.na(country))
      stop("These coordinates do not correspond to a location in Australia. \n Did you specify the coordinates correctly?")
  }
  
  ## This code is now unnecessay, I think 
  if (lon < 112 || lon > 154) stop("Longitude should be between 112 and 154 for the extent of Australia")
  if (lat < -44 || lat > -10) stop("Latitude should be between -44 and -10 for the extent of Australia")
  
  slga <- get_slga_soil(lonlat = lonlat)
  
  ## These are the default thicknesses in ISRIC
  ## They also match thickness values in SLGA
  ## thcknss <- c(50, 100, 150, 300, 400, 1000) ## in mm
  thcknss <- slga$thickness * 10 ## converts cm to mm
  o.thcknss <- thcknss
  
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
    soil_profile <- soil.profile
    new.soil <- TRUE
  }
  
  # Calculate SAT (saturation) for each depth layer based on BD and particle density (2.65 g/cm³)
  particle_density <- 2.65  # Particle density (g/cm³)
  
  if (new.soil) {
    sp.xout <- cumsum(soil_profile$soil$Thickness)
    
    ### Will this take care of both number of layers and soil.bottom?
    if(length(cumsum(thcknss)) != length(slga[["bdod"]])){
      bdod.dat <- stats::approx(data.frame(cumsum(o.thcknss), slga[["bdod"]]), xout = sp.xout)
      soc.dat <- stats::approx(data.frame(cumsum(o.thcknss), slga[["soc"]]), xout = sp.xout)
      phh2o.dat <- stats::approx(data.frame(cumsum(o.thcknss), slga[["phh2o"]]), xout = sp.xout)
      clay.dat <- stats::approx(data.frame(cumsum(o.thcknss), slga[["clay"]]), xout = sp.xout)
      sand.dat <- stats::approx(data.frame(cumsum(o.thcknss), slga[["sand"]]), xout = sp.xout)
      nitrogen.dat <- stats::approx(data.frame(cumsum(o.thcknss), slga[["nitrogen"]]), xout = sp.xout)
      cec.dat <- stats::approx(data.frame(cumsum(o.thcknss), slga[["cec"]]), xout = sp.xout)
      ## wv0010.dat <- approx(data.frame(cumsum(o.thcknss), wv0010[[1]]), xout = sp.xout)
      wv0033.dat <- stats::approx(data.frame(cumsum(o.thcknss), slga[["wv0033"]]), xout = sp.xout)
      wv1500.dat <- stats::approx(data.frame(cumsum(o.thcknss), slga[["wv1500"]]), xout = sp.xout)
      sat.tmp <- (1- (slga[["bdod"]]/particle_density))*0.99
      sat.dat <- stats::approx(data.frame(cumsum(o.thcknss), sat.tmp), xout = sp.xout)
    }else{
      bdod.dat <- data.frame(x = cumsum(thcknss), y = slga[["bdod"]])
      soc.dat <- data.frame(x = cumsum(thcknss), y = slga[["soc"]])
      phh2o.dat <- data.frame(x = cumsum(thcknss), y = slga[["phh2o"]])
      clay.dat <- data.frame(x = cumsum(thcknss), y = slga[["clay"]])
      sand.dat <- data.frame(x = cumsum(thcknss), y = slga[["sand"]])
      nitrogen.dat <- data.frame(x = cumsum(thcknss), y = slga[["nitrogen"]])
      cec.dat <- data.frame(x = cumsum(thcknss), y = slga[["cec"]])
      wv0033.dat <- data.frame(x = cumsum(thcknss), y = slga[["wv0033"]])
      wv1500.dat <- data.frame(x = cumsum(thcknss), y = slga[["wv1500"]])
      sat.dat <- data.frame(x = cumsum(thcknss), y = (1- (slga[["bdod"]]/particle_density))*0.99)
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
    soil_profile$soil$SAT <- approx_soil_variable(sat.dat, 
                                                  xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y * 1e-3
    soil_profile$soil$DUL <- approx_soil_variable(wv0033.dat, 
                                                  xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y * 1e-3  
    soil_profile$soil$LL15 <- approx_soil_variable(wv1500.dat, 
                                                   xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y * 1e-3
    
  } else {
    soil_profile$soil$BD <- slga[["bdod"]]  # Bulk density
    soil_profile$soil$Carbon <- slga[["soc"]]  # Soil organic carbon
    soil_profile$soil$PH <- slga[["phh2o"]]  # pH
    soil_profile$soil$ParticleSizeClay <- slga[["clay"]]  # Clay percentage
    soil_profile$soil$ParticleSizeSand <- slga[["sand"]]  # Sand percentage
    soil_profile$soil$ParticleSizeSilt <- slga[["silt"]]  # Silt percentage
    soil_profile$soil$Nitrogen <- slga[["nitrogen"]]  # Nitrogen content
    soil_profile$soil$CEC <- slga[["cec"]]  # Cation exchange capacity
    soil_profile$soil$DUL <- slga[["wv0033"]] * 1e-2  # Drained upper limit
    soil_profile$soil$LL15 <- slga[["wv1500"]] * 1e-2  # Lower limit (wilting point)
    soil_profile$soil$SAT <- (1 - (soil_profile$soil$BD / particle_density))*0.99 # This is Chloe's method
  }
  
  soil_profile$soil$ParticleSizeSilt <- 100 - (soil_profile$soil$ParticleSizeSand + soil_profile$soil$ParticleSizeClay)
  
  # Ensure SAT is greater than DUL; if not, set SAT to 1.1 times DUL
  soil_profile$soil$SAT <- ifelse(soil_profile$soil$SAT > soil_profile$soil$DUL, 
                                  soil_profile$soil$SAT, 
                                  1.1 * soil_profile$soil$DUL)
  
## Populating DUL and LL. These are equations from Table 1 in Saxton and Rawls 2006
  if(physical == "SR"){
    soil_profile$soil$DUL <- sr_dul(soil_profile$soil$ParticleSizeClay, soil_profile$soil$ParticleSizeSand, soil_profile$soil$Carbon * 2)
    soil_profile$soil$LL15 <- sr_ll(soil_profile$soil$ParticleSizeClay, soil_profile$soil$ParticleSizeSand, soil_profile$soil$Carbon * 2)
    DUL_S <- sr_dul_s(soil_profile$soil$ParticleSizeClay, soil_profile$soil$ParticleSizeSand, soil_profile$soil$Carbon * 2)
    soil_profile$soil$SAT <- sr_sat(soil_profile$soil$ParticleSizeSand, soil_profile$soil$DUL, DUL_S)    
  }

  ## This calculation of KS is a new addition (2025-01-22)
  soil_profile$soil$KS <- sr_ks(soil_profile$soil$ParticleSizeClay, soil_profile$soil$ParticleSizeSand, soil_profile$soil$Carbon * 2)
  
  soil_profile$soil$AirDry <- soil_profile$soil$LL15
  soil_profile$soil$AirDry[1] <- soil_profile$soil$LL15[1] * 0.5 ## AirDry is half of LL for the first layer
  
  for(i in soil_profile$crops){
    soil_profile$soil[[paste0(i,".LL")]] <- soil_profile$soil$LL15 ## Without better information  
  }
  
  #### Passing soil properties from soilwat
  ## The soil texture class in the metadata will be based on the first layer only
  # note that this is the USA texture triangle - will need to update to the Australian one later
  txt_clss <- texture_class_slga(soil_profile$soil$ParticleSizeClay * 1e-2, soil_profile$soil$ParticleSizeSilt * 1e-2)
  # Get soil parameters
  t2sp <- texture2soilParms_slga(txt_clss)
  
  if(missing(soil.profile)){
    soil_profile$soilwat <- soilwat_parms(Salb = t2sp$Albedo, CN2Bare = t2sp$CN2, 
                                          SWCON = t2sp$SWCON,
                                          Thickness = thcknss)    
  }else{
    soil_profile$soilwat <- soilwat_parms(Salb = t2sp$Albedo, CN2Bare = t2sp$CN2, 
                                          SWCON = t2sp$SWCON,
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

  ## Will this always be Australia?
  ## photon might be nice for location
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
  alist$DataSource <- paste("Original source is Soil and Landscape Grid of Australia. See: https://esoil.io/TERNLandscapes/Public/Pages/SLGA/index.html", Sys.time())
  alist$Comments <- paste("resolution = 90 m",
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

## Texture to other soil properties

## This function was vectorized by Chloe
## If more than one textured is supplied only the first one is used
## for CN2 and Albedo, but different SWCON values can be provided
## assuming that this is for deeper depths. This only makes sense
## if the first layer is the top layer and the other ones are the 
## deeper layers of the soil profile
texture2soilParms_slga <- function(texture.class = "NO DATA") { 
  # Define texture classes and associated parameters
  textureClasses <- c("clay", "silty clay", "sandy clay", "clay loam", "silty clay loam", 
                      "sandy clay loam", "loam", "silty loam", "sandy loam", "silt", 
                      "loamy sand", "sand", "NO DATA")  
  Albedo <- c(0.12, 0.12, 0.13, 0.13, 0.12, 0.13, 0.13, 0.14, 0.13, 0.13, 0.16, 0.19, 0.13)
  CN2 <- c(73.0, 73.0, 73.0, 73.0, 73.0, 73.0, 73.0, 73.0, 68.0, 73.0, 68.0, 68.0, 73.0)
  SWCON <- c(0.25, 0.3, 0.3, 0.4, 0.5, 0.5, 0.5, 0.5, 0.6, 0.5, 0.6, 0.75, 0.5)
  
  # Match the first layer of texture.class
  wtc <- match(texture.class[1], textureClasses, nomatch = length(textureClasses))
  
  # Match all layers of texture.class for SWCON
  swcon <- SWCON[match(texture.class, textureClasses, nomatch = length(textureClasses))]
  
  # Return results
  ans <- list(
    textureClasses = textureClasses[wtc],  # Only for the first layer
    Albedo = Albedo[wtc],                  # Only for the first layer
    CN2 = CN2[wtc],                        # Only for the first layer
    SWCON = swcon                          # Vector for all layers
  )
  return(ans)
}


## Potentially useful function provided by Eric Zurcher
## written by Andrew Moore
## I think the values should be in the 0-1 range
#==========================================================================
# Texture class mapping function
#==========================================================================

# Re-express the PSD in terms of the International system, using an equation from Minasny et al. (2001)

# intl_clay_propn <- function( usda_clay, usda_silt ) { 
#   return( usda_clay) 
# }
# 
# intl_silt_propn <- function( usda_clay, usda_silt ) { 
#   return( max( 0.0, -0.0041 - 0.127*usda_clay + 0.553*usda_silt + 0.17*usda_clay^2 - 0.19*usda_silt^2 + 0.59*usda_clay*usda_silt ) ) 
# }
# 
# intl_sand_propn <- function( usda_clay, usda_silt ) { 
#   return( 1.0 - intl_clay_propn( usda_clay, usda_silt ) - intl_silt_propn( usda_clay, usda_silt ) )
# }  

# Texture triangle as equations

## The function below was written by Andrew Moore and/or Eric Zurcher and
## modified by Fernando Miguez and Chloe
# redefine the texture_class function to accept vectors
texture_class_slga <- function(usda_clay, usda_silt) {

  if(length(usda_clay) != length(usda_silt))
    stop("length of clay should be equal to length of silt", call. = FALSE)
  if (any(usda_clay < 0 | usda_clay > 1)) stop("All values in usda_clay should be between 0 and 1")
  if (any(usda_silt < 0 | usda_silt > 1)) stop("All values in usda_silt should be between 0 and 1")
  
  if(any(usda_clay + usda_silt > 1))
    stop("clay plus silt should be less than one", call. = FALSE)
  
  intl_clay <- usda_clay
  intl_silt <- usda_silt
  intl_sand <- 1.0 - intl_clay - intl_silt
  
  # Initialize result vector
  classes <- rep(NA, length = length(usda_clay))
  
  # Apply texture triangle rules
  classes[(intl_sand < 0.75 - intl_clay) & (intl_clay >= 0.40)] <- "silty clay"
  classes[(intl_sand < 0.75 - intl_clay) & (intl_clay >= 0.26) & is.na(classes)] <- "silty clay loam"
  classes[(intl_sand < 0.75 - intl_clay) & is.na(classes)] <- "silty loam"
  classes[(intl_clay >= 0.40 + (0.305 - 0.40) / (0.635 - 0.35) * (intl_sand - 0.35)) & 
            (intl_clay < 0.50 + (0.305 - 0.50) / (0.635 - 0.50) * (intl_sand - 0.50)) & is.na(classes)] <- "clay"
  classes[(intl_clay >= 0.26 + (0.305 - 0.26) / (0.635 - 0.74) * (intl_sand - 0.74)) & is.na(classes)] <- "sandy clay"
  classes[(intl_clay >= 0.26 + (0.17 - 0.26) / (0.83 - 0.49) * (intl_sand - 0.49)) & 
            (intl_clay < 0.10 + (0.305 - 0.10) / (0.635 - 0.775) * (intl_sand - 0.775)) & is.na(classes)] <- "clay loam"
  classes[(intl_clay >= 0.26 + (0.17 - 0.26) / (0.83 - 0.49) * (intl_sand - 0.49)) & is.na(classes)] <- "sandy clay loam"
  classes[(intl_clay >= 0.10 + (0.12 - 0.10) / (0.63 - 0.775) * (intl_sand - 0.775)) & 
            (intl_clay < 0.10 + (0.305 - 0.10) / (0.635 - 0.775) * (intl_sand - 0.775)) & is.na(classes)] <- "loam"
  classes[(intl_clay >= 0.10 + (0.12 - 0.10) / (0.63 - 0.775) * (intl_sand - 0.775)) & is.na(classes)] <- "sandy loam"
  classes[(intl_clay < 0.00 + (0.08 - 0.00) / (0.88 - 0.93) * (intl_sand - 0.93)) & is.na(classes)] <- "loamy sand"
  classes[is.na(classes)] <- "sand"
  
  return(classes)
}

### The function below was created by Chloe (Yunru Lai)
### Modified by Fernando Miguez 2025-01-11

#' The data comes from https://esoil.io/TERNLandscapes/Public/Pages/SLGA/index.html
#' 
#' @title Retrieve soil profile data from SLGA (Soils for Australia)
#' @description This function gets a soil profile for the Australia extent
#' @name get_slga_soil
#' @param lonlat Longitude and latitude vector (e.g. c(151.8306, -27.4969))
#' @return a data.frame with elements: depth (midpoint in cm), depths (as character in cm), thickness (cm), clay, sand, silt, wv1500, wv0033, bdod, nitrogen, phh2o, cec, soc
#' @author Chloe (Yunru Lai) and Fernando E. Miguez
#' @export
#' @examples
#' \dontrun{
#' ## retrieve data from longitude and latitude 151.8305805675806 and -27.496873026858598
#' ## Note: This can take a couple of minutes
#' slga.soil <- get_slga_soil(lonlat = c(151.8306, -27.4969)) 
#' 
#' }

get_slga_soil <- function(lonlat) {
  
  if(missing(lonlat))
    stop("lonlat is missing without default", call. = FALSE)
  
  #### Create extent step ####
  longitude <- as.numeric(lonlat[1])
  latitude <- as.numeric(lonlat[2])
  
  if (longitude < 112 || longitude > 154) stop("Longitude should be between 112 and 154 for the extent of Australia")
  if (latitude < -44 || latitude > -10) stop("Latitude should be between -44 and -10 for the extent of Australia")
  
  ### API endpoint
  root.url <- "https://esoil.io/TERNLandscapes/Public/Products/TERN/SLGA/"
    
  # Define properties
  properties <- list(
    clay = paste0(root.url, "CLY/CLY_000_005_EV_N_P_AU_TRN_N_20210902.tif"),
    sand = paste0(root.url, "SND/SND_000_005_EV_N_P_AU_TRN_N_20210902.tif"),
    silt = paste0(root.url, "SLT/SLT_000_005_EV_N_P_AU_TRN_N_20210902.tif"),
    wv1500 = paste0(root.url, "L15/L15_000_005_EV_N_P_AU_TRN_N_20210614.tif"),
    wv0033 = paste0(root.url, "DUL/DUL_000_005_EV_N_P_AU_TRN_N_20210614.tif"),
    bdod = paste0(root.url, "BDW/BDW_000_005_EV_N_P_AU_TRN_N_20230607.tif"),
    nitrogen = paste0(root.url, "NTO/NTO_000_005_EV_N_P_AU_NAT_C_20231101.tif"),
    phh2o = paste0(root.url, "PHW/PHW_000_005_EV_N_P_AU_TRN_N_20220520.tif"),
    cec = paste0(root.url, "CEC/CEC_000_005_EV_N_P_AU_TRN_N_20220826.tif"),
    soc = paste0(root.url, "SOC/SOC_000_005_EV_N_P_AU_TRN_N_20220727.tif")
  )
  ## Previously used property
  # des = "https://esoil.io/TERNLandscapes/Public/Products/TERN/SLGA/DES/DES_000_200_EV_N_P_AU_TRN_C_20190901.tif",
  
  # Define depths
  depths <- c("000_005", "005_015", "015_030", "030_060", "060_100", "100_200")
  depth.midpoint <- c(2.5, 10, 22.5, 45, 80, 150) ## In cm 
  thickness <- c(5, 10, 15, 30, 40, 100) ## In cm
  # Initialize results list
  results <- list()
  
  # Loop through each property and depth
  for (property_name in names(properties)) {
    property_url <- properties[[property_name]]
    property_results <- numeric(length(depths))  # Initialize vector of length 6
    
    for (i in seq_along(depths)) {
      depth <- depths[i]
      
      # Replace "000_005" with the current depth
      cog_path <- gsub("000_005", depth, property_url)
      
      # Construct the API request URL
      api_url <- paste0(
        "https://esoil.io/TERNLandscapes/RasterProductsAPI/Drill?",
        "format=json&verbose=false&COGPath=", cog_path,
        "&latitude=", latitude, "&longitude=", longitude
      )
      
      # Make the API request
      response <- jsonlite::fromJSON(api_url)
      ## This returns a data.frame when it works
      
      if(!inherits(response, 'try-error')){
        property_results[i] <- response$Value  # Assign to vector
      }else{
        message("Could not retrieve data for", cog_path, latitude, longitude)
        property_results[i] <- NA  # Handle errors gracefully
      }
    }
    
    # Store the results for the property
    results[[property_name]] <- property_results
  }
  
  ans <- data.frame(depth = depth.midpoint, depths = depths, thickness = thickness,
                    as.data.frame(results))
  return(ans)
}

