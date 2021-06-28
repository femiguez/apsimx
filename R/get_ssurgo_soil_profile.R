#' Data source is USDA-NRCS Soil Data Access. See package soilDB for more details
#' 
#' @title Retrieve soil profile data and convert it to an object of class \sQuote{soil_profile}
#' @description Generate a synthetic soil profile based on the information in SSURGO database
#' @name get_ssurgo_soil_profile
#' @param lonlat Longitude and latitude vector (e.g. c(-93, 42))
#' @param shift simple mechanism for creating an area of interest by displacing the point indicated in 
#' lonlat by some amount of distance (e.g. 300 - in meters)
#' @param nmapunit number of mapunits to select (see \code{\link{ssurgo2sp}})
#' @param nsoil number of soils to select (see \code{\link{ssurgo2sp}})
#' @param xout see \code{\link{ssurgo2sp}}
#' @param soil.bottom see \code{\link{ssurgo2sp}}
#' @param method interpolation method see \code{\link{ssurgo2sp}}
#' @param nlayers number for layer for the new soil profile
#' @param verbose default FALSE. Whether to print messages.
#' @return this function will always return a list. Each element of the list will
#' be an object of class \sQuote{soil_profile}
#' @export
#' @examples 
#' \dontrun{
#' require(soilDB)
#' require(sp)
#' require(sf)
#' require(spData)
#' ## Soil inforation for a single point
#' sp <- get_ssurgo_soil_profile(lonlat = c(-93, 42))
#' plot(sp[[1]])
#' plot(sp[[1]], property = "water")
#' }
#' 
#' 

get_ssurgo_soil_profile <- function(lonlat, shift = -1,
                                    nmapunit = 1, nsoil = 1,
                                    xout = NULL, soil.bottom = 200,
                                    method = c("constant","linear"),
                                    nlayers = 10,
                                    verbose = FALSE){
  
  if(!requireNamespace("soilDB", quietly = TRUE)){
    stop("The soilDB package is required for this function")
    return(NULL)
  }
  
  if(!requireNamespace("sp", quietly = TRUE)){
    stop("The sp package is required for this function")
    return(NULL)
  }
  
  if(!requireNamespace("sf", quietly = TRUE)){
    stop("The sf package is required for this function")
    return(NULL)
  }
  
  if(!requireNamespace("spData", quietly = TRUE)){
    stop("The spData package is required for this function")
    return(NULL)
  }
  
  if(length(lonlat) != 2 || !is.numeric(lonlat)) 
    stop("lonlat should be a vector with length equal to 2")

  lon <- lonlat[1]
  lat <- lonlat[2]
  
  ## Determine if the location is in the US
  if(requireNamespace("maps")){
    country <- maps::map.where(x = lon, y = lat)
    if(country != "USA" || is.na(country))
      stop("These coordinates do not correspond to a location in the USA. \n Did you specify the coordinates correctly?")
  }
  
  if(shift <= 0){
    spg <- sp::SpatialPoints(cbind(x = lon, y = lat), 
                       proj4string = sp::CRS('+proj=longlat +datum=WGS84'))
  }else{
    shift <- (shift / 111) * 0.001 ## This is now in degrees
    lonlat.mat <- rbind(lonlat, ##root
                        lonlat + c(shift * 0.75, 0), ## x = 1, y = 0
                        lonlat + c(shift * 0.75, shift), ## x = 1, y = 1
                        lonlat + c(0, shift), ## x = 0, y = 1
                        lonlat) ## back to root
    rownames(lonlat.mat) <- NULL
    ## the previous matrix is a rectangle
    pg <- sp::Polygon(lonlat.mat)
    spg <- sp::SpatialPolygons(list(sp::Polygons(list(pg), "s1")),
                               proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  }
  
  if(verbose == FALSE){
    res <- suppressWarnings(soilDB::SDA_spatialQuery(spg, what = 'mukey'))
  }else{
    res <- soilDB::SDA_spatialQuery(spg, what = 'mukey')  
  }
  
  mu.is <- soilDB::format_SQL_in_statement(res$mukey)
  sql <- sprintf("mukey IN %s", mu.is)
  if(verbose == FALSE){
    fSDA <- suppressWarnings(suppressMessages(soilDB::fetchSDA(sql, duplicates = TRUE)))
  }else{
    fSDA <- soilDB::fetchSDA(sql, duplicates = TRUE)
  } 
  
  ### Mapunit ### -- this might contain the iacornsr
  if(verbose == FALSE){
    mapunit <- suppressWarnings(suppressMessages(soilDB::get_mapunit_from_SDA(sql)))
  }else{
    mapunit <- soilDB::get_mapunit_from_SDA(sql) 
  }
  
  ### Component ###
  cmpnt <- fSDA@site
  names(cmpnt) <- gsub("_", ".", names(cmpnt), fixed = TRUE)
  cmpnt$geomdesc <- cmpnt$geompos
  
  ## Retrieve the state from the areasymbol
  if(shift <= 0 || length(unique(mapunit$areasymbol)) == 1){
    cmpnt$state <- strtrim(mapunit$areasymbol, 2)
  }else{
    cmpnt$state <- NA
    warning("This area includes more than one state. 
            I have not though about how to get the state in this case. Please submit an issue 
            with a reproducible example to https://github.com/femiguez/apsimx/issues")
  }
  
  ### Chorizon ###
  chrzns <- fSDA@horizons
  names(chrzns) <- gsub("_", ".", names(chrzns), fixed = TRUE)
  ### Things missing from horizons: hzthk.r, partdensity, wsatiated.r, wfifteenbar.r, wtenthbar.r, wthirdbar.r,
  if(sum(grepl("partdensity", names(chrzns))) == 0) chrzns$partdensity <- NA
  if(sum(grepl("hzthk", names(chrzns))) == 0) chrzns$hzthk.r <- NA
  if(sum(grepl("wsatiated", names(chrzns))) == 0) chrzns$wsatiated.r <- NA
  if(sum(grepl("wfifteenbar", names(chrzns))) == 0) chrzns$wfifteenbar.r <- NA
  if(sum(grepl("wthirdbar", names(chrzns))) == 0) chrzns$wthirdbar.r <- NA
  
  if(shift <= 0){
    spg.sf <- sf::st_as_sf(spg)
    spg.sf[["MUKEY"]] <- res$mukey
    spg.sf[["AREASYMBOL"]] <- mapunit$areasymbol
    mapunit.shp <- spg.sf
  }else{
    mapunit.shp <- sf::st_as_sf(spg)
  }
  
  sp0 <- ssurgo2sp(mapunit = mapunit, component = cmpnt,
                   chorizon = chrzns, mapunit.shp = mapunit.shp,
                   nmapunit = nmapunit, nsoil = nsoil, xout = xout,
                   soil.bottom = soil.bottom, method = method, nlayers = nlayers,
                   verbose = verbose)
  
  ans <- vector("list", length(sp0))
  
  for(i in seq_along(sp0)){
    metadata <- attributes(sp0[[i]])
    metadata$DataSource <- paste("SSURGO (https://sdmdataaccess.nrcs.usda.gov/) through R package soilDB, R package apsimx function ssurgo2sp. Timestamp",Sys.time())
    metadata$names <- NULL; metadata$class <- NULL; metadata$row.names <- NULL;
    
    asp <- apsimx_soil_profile(nlayers = nlayers,
                               Thickness = sp0[[i]]$Thickness * 10,
                               BD = sp0[[i]]$BD,
                               AirDry = sp0[[i]]$AirDry,
                               LL15 = sp0[[i]]$LL15,
                               DUL = sp0[[i]]$DUL,
                               SAT = sp0[[i]]$SAT,
                               KS = sp0[[i]]$KS,
                               Carbon = sp0[[i]]$Carbon,
                               crop.LL = sp0[[i]]$LL15,
                               soil.bottom = soil.bottom,
                               metadata = metadata)
    
    check_apsimx_soil_profile(asp)
    
    ans[[i]] <- asp
  }
  
  return(ans)
}