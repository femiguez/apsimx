#'
#' This function requires the \CRANpkg{daymetr} package. This function should replace 
#' the \code{\link{get_daymet_apsim_met}} function.
#'
#' @title Get DAYMET data for an APSIM met file
#' @description Uses \code{\link[daymetr]{download_daymet}} from the \CRANpkg{daymetr} package to download data to create an APSIM met file.
#' @name get_daymet2_apsim_met
#' @param lonlat Longitude and latitude vector
#' @param years a numeric vector of years to extract (c(start, end)). For example, f you need 2012 through 2015, use c(2012, 2015).
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
#' @return It returns an object of class \sQuote{met} and writes a file to disk when filename is supplied.
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
  if(length(years) > 2) stop("years should be a numeric vector of size = 2", call. = FALSE)
  
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
  
  units <- c("()","()","(MJ/m2/day)","(oC)","(oC)","(mm)","(hPa)","(kg/m2)")
  
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

#'
#' This function requires the \CRANpkg{daymetr} package. This function should replace 
#' the \code{\link{get_daymet_apsim_met}} function.
#'
#' @title Get DAYMET data for an APSIM met file
#' @description Uses \code{\link[daymetr]{download_daymet}} from the \CRANpkg{daymetr} package to download data to create an APSIM met file.
#' @name get_daymet_apsim_met
#' @param lonlat Longitude and latitude vector
#' @param years a numeric vector of years to extract (c(start, end)). For example, f you need 2012 through 2015, use c(2012, 2015).
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
#' @return It returns an object of class \sQuote{met} and writes a file to disk when filename is supplied.
#' @export
#' @examples 
#' \dontrun{
#' require(daymetr)
#' ## I write to a temp directory but replace as needed
#' dmet12 <- get_daymet_apsim_met(lonlat = c(-93,42), years = 2012)
#' summary(dmet12)
#' ## Check for reasonable ranges 
#' check_apsim_met(dmet12)
#' }
#' 
get_daymet_apsim_met <- get_daymet2_apsim_met