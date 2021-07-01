#'
#' This function requires the \CRANpkg{FedData} pacakge.
#'
#' @title Get DAYMET data for an APSIM met file
#' @description Uses \code{\link[FedData]{get_daymet}} from the \CRANpkg{FedData} package to download data to create an APSIM met file.
#' @name get_daymet_apsim_met
#' @param lonlat Longitude and latitude vector
#' @param years a numeric vector of years to extract
#' @param wrt.dir write directory
#' @param filename file name for writing out to disk
#' @param width.height width and height of the cropped area (default 0.001, 0.001)
#' @param template A Raster or Spatial object to serve as a template for cropping (see \code{\link[FedData]{get_daymet}}).
#' @param label a character string naming the area (see \code{\link[FedData]{get_daymet}})
#' @param elements see \code{\link[FedData]{get_daymet}}
#' @param region see \code{\link[FedData]{get_daymet}}
#' @param tempo see \code{\link[FedData]{get_daymet}}
#' @param extraction.dir see \code{\link[FedData]{get_daymet}}
#' @param force.redo see \code{\link[FedData]{get_daymet}}
#' @param cleanup whether to delete download directories (default is FALSE). 
#' If the intention is for cleanup to delete all the files, \sQuote{raw.dir} 
#' and \sQuote{extraction.dir} should be supplied, supplying a sinlge name, 
#' such as \sQuote{RAW} and \sQuote{EXTRACTION}.
#' @details If the filename is not provided it will not write the file to disk, 
#' but it will return an object of class \sQuote{met}. This is useful in case manipulation
#' is required before writing to disk. The variable \sQuote{srad} as downloaded from
#' daymet is average solar radiation, so it is converted to total. 
#' Daily total radiation (MJ/m2/day) can be calculated as 
#' follows: ((srad (W/m2) * dayl (s/day)) / 1,000,000) \cr
#' Vapor Pressure Deficit (vp) should be in hecto Pascals
#' @source The data is retrieved using the \CRANpkg{FedData} package. For the original
#' source see: https://daymet.ornl.gov/
#' @export
#' @examples 
#' \dontrun{
#' require(FedData)
#' ## I write to a temp directory but replace as needed
#' tmp.dir <- tempdir()
#' dmet12 <- get_daymet_apsim_met(lonlat = c(-93,42),
#'                                extraction.dir = paste0(tmp.dir,"/FedData/extractions/daymet/"),
#'                                years = 2012)
#' summary(dmet12)
#' ## Check for reasonable ranges 
#' check_apsim_met(dmet12)
#' }
#' 

get_daymet_apsim_met <- function(lonlat, years, 
                                 wrt.dir=".", 
                                 filename=NULL, 
                                 width.height = c(1e-1 * 1.263012, 1e-1), 
                                 template, 
                                 label = NULL, 
                                 elements = c("dayl", "prcp", "srad", "swe", "tmax", "tmin", "vp"), 
                                 region = "na",
                                 tempo = "day",
                                 extraction.dir = paste0(tempdir(), "/FedData/extractions/daymet/", label, "/"),
                                 force.redo = FALSE, cleanup = FALSE){
  
  if(!requireNamespace("FedData", quietly = TRUE)){
    warning("The FedData is required for this function")
    return(NULL)
  }
  
  message("Function get_daymet2_apsim_met is preferred from now on. This function will be deprecated.")
  
  if(utils::packageVersion("FedData") < "3.0.0.9000")
    stop("FedData package version should be 3.0.0.9000 or higher")
  
  if(missing(filename)) filename <- "noname.met"
  
  if(!grepl(".met", filename, fixed=TRUE)) stop("filename should end in .met")
  
  if(missing(lonlat) && missing(template)) stop("either template or lonlat should be supplied")
  
  if(!missing(lonlat)){
    ## Set up the template
    ## lower left corner is x = 0, y = 0
    half.width.height <- width.height/2
    llc <- lonlat - half.width.height
    ## lower right corner is x = 1, y = 0
    lrc <- lonlat + c(half.width.height[1], -half.width.height[2])
    ## upper right corner is x = 1, y = 1
    urc <- lonlat + half.width.height
    ## upper left corner is x = 0, y = 1
    ulc <- lonlat + c(-half.width.height[1], half.width.height[2])
  
    lonlat.mat <- rbind(llc, lrc, urc, ulc)
    rownames(lonlat.mat) <- NULL
  
    pg <- sp::Polygon(lonlat.mat)
    tmplt <- sp::SpatialPolygons(list(sp::Polygons(list(pg), "s1")),
                           proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  }else{
    if(missing(template))
      stop("template and lonlat arguments are both missing")
    tmplt <- template
  }
  
  dmet0 <- FedData::get_daymet(template = tmplt,
                              label = label,
                              elements = elements,
                              years = years, 
                              region = region,
                              tempo = tempo,
                              extraction.dir = extraction.dir,
                              force.redo = force.redo)
  
  ## Process year and dates
  dmet.dates <-  gsub(".","-",gsub("X","\\1",names(dmet0$dayl)), fixed = TRUE)
  dmet.doy <- as.numeric(format(as.Date(dmet.dates), "%j"))
  dmet.year <- as.numeric(format(as.Date(dmet.dates), "%Y"))  
  ## Process max and min temperature
  dmet.maxt <- apply(raster::as.array(dmet0$tmax), 3, FUN = mean)
  dmet.tmin <- apply(raster::as.array(dmet0$tmin), 3, FUN = mean)
  ## Process solar radiation
  dmet.dayl <- apply(raster::as.array(dmet0$dayl), 3, FUN = mean)
  dmet.srad <- apply(raster::as.array(dmet0$srad), 3, FUN = mean)
  dmet.srad.total <- dmet.dayl * dmet.srad * 1e-6
  ## Process precipitation
  dmet.prcp <- apply(raster::as.array(dmet0$prcp), 3, FUN = mean)
  ## Process water vapor pressure
  ## Need to convert from Pa to hPa
  dmet.vp <- apply(raster::as.array(dmet0$vp), 3, FUN = mean) * 0.01 
  ## Process snow water equivalent
  dmet.swe <- apply(raster::as.array(dmet0$swe), 3, FUN = mean)
  
  dmet <- data.frame(dmet.year, dmet.doy, dmet.srad.total, dmet.maxt, dmet.tmin, dmet.prcp, dmet.vp, dmet.swe)
  
  names(dmet) <- c("year","day","radn","maxt","mint","rain","vp","swe")
  
  units <- c("()","()","(MJ/m2/day)","(oC)","(oC)","(mm)","hPa","kg/m2")
  
  comments <- paste("!data from DayMet obtained through FedData R pacakge. retrieved: ",Sys.time())
  
  attr(dmet, "filename") <- filename
  attr(dmet, "site") <- paste("site = ", sub(".met","", filename, fixed = TRUE))
  attr(dmet, "latitude") <- paste("latitude =",lonlat[2])
  attr(dmet, "longitude") <- paste("longitude =",lonlat[1])
  attr(dmet, "tav") <- paste("tav =",mean(colMeans(dmet[,c("maxt","mint")],na.rm=TRUE),na.rm=TRUE))
  attr(dmet, "amp") <- paste("amp =",mean(dmet$maxt, na.rm=TRUE) - mean(dmet$mint, na.rm = TRUE))
  attr(dmet, "colnames") <- names(dmet)
  attr(dmet, "units") <- units
  attr(dmet, "comments") <- comments
  ## No constants
  class(dmet) <- c("met","data.frame")
  
  if(filename != "noname.met"){
    write_apsim_met(dmet, wrt.dir = wrt.dir, filename = filename)
  }
  
  if(cleanup){
    unlink(extraction.dir, recursive = TRUE)
  }
  
  return(invisible(dmet))
}


#'
#' This function requires the \CRANpkg{daymetr} package. This function should replace 
#' the \code{\link{get_daymet_apsim_met}} function.
#'
#' @title Get DAYMET data for an APSIM met file
#' @description Uses \code{\link[daymetr]{download_daymet}} from the \CRANpkg{daymetr} package to download data to create an APSIM met file.
#' @name get_daymet2_apsim_met
#' @param lonlat Longitude and latitude vector
#' @param years a numeric vector of years to extract
#' @param wrt.dir write directory (default is the current directory)
#' @param filename file name for writing out to disk
#' @param silent argument passed to \code{\link[daymetr]{download_daymet}}
#' @details If the filename is not provided it will not write the file to disk, 
#' but it will return an object of class \sQuote{met}. This is useful in case manipulation
#' is required before writing to disk. The variable \sQuote{srad} as downloaded from
#' daymet is average solar radiation, so it is converted to total. 
#' Daily total radiation (MJ/m2/day) can be calculated as 
#' follows: ((srad (W/m2) * dayl (s/day)) / 1,000,000) \cr
#' Vapor Pressure Deficit (vp) should be in hecto Pascals
#' @source The data is retrieved using the \CRANpkg{daymetr} package. For the original
#' source see: https://daymet.ornl.gov/
#' @export
#' @examples 
#' \dontrun{
#' require(daymetr)
#' ## I write to a temp directory but replace as needed
#' dmet12 <- get_daymet2_apsim_met(lonlat = c(-93,42), years = 2012)
#' summary(dmet12)
#' ## Check for reasonable ranges 
#' check_apsim_met(dmet12)
#' }
#' 

get_daymet2_apsim_met <- function(lonlat, years, wrt.dir = ".", filename, silent = FALSE){
  
  if(!requireNamespace("daymetr", quietly = TRUE)){
    warning("The daymetr is required for this function")
    return(NULL)
  }
  
  if(length(years) == 1) years <- rep(years, 2)
  
  if(missing(filename)) filename <- "noname.met"
  
  if(!grepl(".met", filename, fixed=TRUE)) stop("filename should end in .met")
  
  dmet0 <- daymetr::download_daymet(site = filename,
                                      lat = lonlat[2], lon = lonlat[1],
                                      start = years[1], end = years[2],
                                      path = wrt.dir, silent = silent,
                                      internal = TRUE)

  dmet.year <- dmet0$data$year
  dmet.doy <- dmet0$data$yday
  dmet.dayl <- dmet0$data$dayl..s. 
  dmet.srad <- dmet0$data$srad..W.m.2.
  dmet.srad.total <- dmet.dayl * dmet.srad * 1e-6
  dmet.tmax <- dmet0$data$tmax..deg.c.
  dmet.tmin <- dmet0$data$tmin..deg.c.
  dmet.prcp <- dmet0$data$prcp..mm.day.
  dmet.vp <- dmet0$data$vp..Pa. * 0.01
  dmet.swe <- dmet0$data$swe..kg.m.2.
      
  dmet <- data.frame(dmet.year, dmet.doy, dmet.srad.total, dmet.tmax, dmet.tmin, dmet.prcp, dmet.vp, dmet.swe)    
  
  names(dmet) <- c("year","day","radn","maxt","mint","rain","vp","swe")
  
  units <- c("()","()","(MJ/m2/day)","(oC)","(oC)","(mm)","hPa","kg/m2")
  
  comments <- paste("!data from DayMet obtained through daymetr R pacakge. retrieved: ",Sys.time())
  
  attr(dmet, "filename") <- filename
  attr(dmet, "site") <- paste("site = ", sub(".met","", filename, fixed = TRUE))
  attr(dmet, "latitude") <- paste("latitude =",lonlat[2])
  attr(dmet, "longitude") <- paste("longitude =",lonlat[1])
  attr(dmet, "tav") <- paste("tav =",mean(colMeans(dmet[,c("maxt","mint")],na.rm=TRUE),na.rm=TRUE))
  attr(dmet, "amp") <- paste("amp =",mean(dmet$maxt, na.rm=TRUE) - mean(dmet$mint, na.rm = TRUE))
  attr(dmet, "colnames") <- names(dmet)
  attr(dmet, "units") <- units
  attr(dmet, "comments") <- comments
  ## No constants
  class(dmet) <- c("met", "data.frame")
  
  if(filename != "noname.met"){
    write_apsim_met(dmet, wrt.dir = wrt.dir, filename = filename)
  }  

  return(invisible(dmet))  
}
