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
  
  if (lon < 112 || lon > 154) stop("Longitude should be between 112 and 154 for the extent of Australia")
  if (lat < -44 || lat > -10) stop("Latitude should be between -44 and -10 for the extent of Australia")
  
  slga <- get_slga_soil(lat, lon)
  
  ## These are the default thicknesses in ISRIC
  thcknss <- c(50, 100, 150, 300, 400, 1000) ## in mm
  
  ## Some variables can be passed to apsimx:::approx_soil_variable
  soil.bottom <- 200
  method <- "constant"
  nlayers <- 10
  crps <- c("Maize", "Soybean", "Wheat")
  if(!is.null(xargs)){
    ### Soil bottom
    if(!is.null(xargs$soil.bottom)){
      soil.bottom <- xargs$soil.bottom
    }
    ### Method
    if(!is.null(xargs$method)){
      method <- xargs$method
    }
    ### Number of layers
    if(!is.null(xargs$nlayers)){
      nlayers <- xargs$nlayers
    }
    ### Crops
    if(!is.null(xargs$crops)){
      crps <- xargs$crops
    }
  }

  ## Create the empty soil profile
  if(missing(soil.profile)){
    new.soil <- FALSE
    soil_profile <- apsimx_soil_profile(nlayers = 6, Thickness = thcknss, crops = crps) 
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
  

  
  # Calculate SAT (saturation) for each depth layer based on BD and particle density (2.65 g/cm³)
  particle_density <- 2.65  # Particle density (g/cm³)
  
  if (new.soil) {
    sp.xout <- cumsum(soil_profile$soil$Thickness)
    
    soil_profile$soil$BD <- approx_soil_variable(data.frame(x = cumsum(thcknss), y = slga[["bdod"]]), 
                                                 xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y    
    soil_profile$soil$Carbon <- approx_soil_variable(data.frame(x = cumsum(thcknss), y = slga[["soc"]]), 
                                                     xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y    
    soil_profile$soil$PH <- approx_soil_variable(data.frame(x = cumsum(thcknss), y = slga[["phh2o"]]), 
                                                 xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y    
    soil_profile$soil$ParticleSizeClay <- approx_soil_variable(data.frame(x = cumsum(thcknss), y = slga[["clay"]]), 
                                                               xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y   
    soil_profile$soil$ParticleSizeSand <- approx_soil_variable(data.frame(x = cumsum(thcknss), y = slga[["sand"]]), 
                                                               xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y 
    soil_profile$soil$Nitrogen <- approx_soil_variable(data.frame(x = cumsum(thcknss), y = slga[["nitrogen"]]), 
                                                       xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y   
    soil_profile$soil$CEC <- approx_soil_variable(data.frame(x = cumsum(thcknss), y = slga[["cec"]]), 
                                                  xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y   
    soil_profile$soil$SAT <- approx_soil_variable(data.frame(x = cumsum(thcknss), y = (1- (slga[["bdod"]]/particle_density)))*0.99, 
                                                  xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y * 1e-3
    soil_profile$soil$DUL <- approx_soil_variable(data.frame(x = cumsum(thcknss), y = slga[["wv0033"]]), 
                                                  xout = sp.xout, soil.bottom = soil.bottom, method = method, nlayers = nlayers)$y * 1e-3  
    soil_profile$soil$LL15 <- approx_soil_variable(data.frame(x = cumsum(thcknss), y = slga[["wv1500"]]), 
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
    soil_profile$soil$SAT <- (1 - (soil_profile$soil$BD / particle_density))*0.99
    
  }
  
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

  B <- (log(1500) - log(33))/(log(soil_profile$soil$DUL) - log(soil_profile$soil$LL15))
  Lambda <- 1/B
  soil_profile$soil$KS <- (1930 * (soil_profile$soil$SAT - soil_profile$soil$DUL)^(3 - Lambda)) * 100
  
  soil_profile$soil$AirDry <- soil_profile$soil$LL15
  soil_profile$soil$AirDry[1] <- soil_profile$soil$LL15[1] * 0.5 ## AirDry is half of LL for the first layer
  
  for(i in soil_profile$crops){
    soil_profile$soil[[paste0(i,".LL")]] <- soil_profile$soil$LL15 ## Without better information  
  }
  
  #### Passing parameters from soilwat
  ## The soil texture class in the metadata will be based on the first layer only
  # note that this is the USA texture triangle - will need to update to the Australian one later
  txt_clss <- texture_class(soil_profile$soil$ParticleSizeClay * 1e-2, soil_profile$soil$ParticleSizeSilt * 1e-2)
  # Get soil parameters
  t2sp <- texture2soilParms(txt_clss)
  
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
  alist$DataSource <- paste("Original source is Soil and Landscape Grid of Australia www.isric.org. See: https://esoil.io/TERNLandscapes/Public/Pages/SLGA/index.html",Sys.time())
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

texture2soilParms <- function(texture.class = "NO DATA") { 
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

# redefine the texture_class function to accept vectors
texture_class <- function(usda_clay, usda_silt) {
  if (any(usda_clay < 0 | usda_clay > 1)) stop("All values in usda_clay should be between 0 and 1")
  if (any(usda_silt < 0 | usda_silt > 1)) stop("All values in usda_silt should be between 0 and 1")
  
  intl_clay <- usda_clay
  intl_silt <- usda_silt
  intl_sand <- 1.0 - intl_clay - intl_silt
  
  # Initialize result vector
  classes <- rep(NA, length(usda_clay))
  
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

get_slga_soil <- function(latitude, longitude) {
  # Define properties
  properties <- list(
    clay = "https://esoil.io/TERNLandscapes/Public/Products/TERN/SLGA/CLY/CLY_000_005_EV_N_P_AU_TRN_N_20210902.tif",
    sand = "https://esoil.io/TERNLandscapes/Public/Products/TERN/SLGA/SND/SND_000_005_EV_N_P_AU_TRN_N_20210902.tif",
    silt = "https://esoil.io/TERNLandscapes/Public/Products/TERN/SLGA/SLT/SLT_000_005_EV_N_P_AU_TRN_N_20210902.tif",
    wv1500 = "https://esoil.io/TERNLandscapes/Public/Products/TERN/SLGA/L15/L15_000_005_EV_N_P_AU_TRN_N_20210614.tif",
    wv0033 = "https://esoil.io/TERNLandscapes/Public/Products/TERN/SLGA/DUL/DUL_000_005_EV_N_P_AU_TRN_N_20210614.tif",
    bdod = "https://esoil.io/TERNLandscapes/Public/Products/TERN/SLGA/BDW/BDW_000_005_EV_N_P_AU_TRN_N_20230607.tif",
    nitrogen = "https://esoil.io/TERNLandscapes/Public/Products/TERN/SLGA/NTO/NTO_000_005_EV_N_P_AU_NAT_C_20231101.tif",
    phh2o = "https://esoil.io/TERNLandscapes/Public/Products/TERN/SLGA/PHW/PHW_000_005_EV_N_P_AU_TRN_N_20220520.tif",
    cec = "https://esoil.io/TERNLandscapes/Public/Products/TERN/SLGA/CEC/CEC_000_005_EV_N_P_AU_TRN_N_20220826.tif",
    # des = "https://esoil.io/TERNLandscapes/Public/Products/TERN/SLGA/DES/DES_000_200_EV_N_P_AU_TRN_C_20190901.tif",
    soc = "https://esoil.io/TERNLandscapes/Public/Products/TERN/SLGA/SOC/SOC_000_005_EV_N_P_AU_TRN_N_20220727.tif"
  )
  
  # Define depths
  depths <- c("000_005", "005_015", "015_030", "030_060", "060_100", "100_200")
  
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
      response <- GET(api_url)
      
      # Parse the response
      if (status_code(response) == 200) {
        data <- fromJSON(content(response, "text"))
        property_results[i] <- data$Value  # Assign to vector
      } else {
        property_results[i] <- NA  # Handle errors gracefully
      }
    }
    
    # Store the results for the property
    results[[property_name]] <- property_results
  }
  
  # Return the results
  return(results)
}

