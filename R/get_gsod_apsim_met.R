#'
#' This function requires the \CRANpkg{GSODR} package.
#'
#' @title Get GSOD data for an APSIM met file
#' @description Uses \code{\link[GSODR]{get_GSOD}} from the \CRANpkg{GSODR} package to download data to create an APSIM met file.
#' @name get_gsod_apsim_met
#' @param lonlat Longitude and latitude vector
#' @param dates date ranges
#' @param wrt.dir write directory
#' @param filename file name for writing out to disk
#' @param distance distance in kilometers for the nearest station
#' @param fillin.radn whether to fill in radiation data using the nasapower pacakge. Default is FALSE.
#' @details If the filename is not provided it will not write the file to disk, 
#' but it will return an object of class \sQuote{met}. This is useful in case manipulation
#' is required before writing to disk.
#' @note This source of data does not provide solar radiation. If \sQuote{fillin.radn} is
#' TRUE it fill in radiation data using the nasapower package.
#' @return returns an object of class \sQuote{met} and writes a file to disk when filename is supplied.
#' @export
#' @examples 
#' \dontrun{
#' require(GSODR)
#' ## This will not write a file to disk
#' gsd <- get_gsod_apsim_met(lonlat = c(-93,42), dates = c("2012-01-01","2012-12-31"),
#'                           fillin.radn = TRUE)
#' summary(gsd)
#' ## Check for reasonable ranges
#' check_apsim_met(gsd)
#' }
#' 

get_gsod_apsim_met <- function(lonlat, dates, wrt.dir = ".", filename = NULL, 
                               distance = 100, fillin.radn = FALSE){
  
  if(!requireNamespace("GSODR", quietly = TRUE)){
    warning("The GSODR package is required for this function")
    return(NULL)
  }
  
  if(missing(filename)) filename <- "noname.met"
  
  if(!grepl(".met", filename, fixed = TRUE)) stop("filename should end in .met")
  
  yr1 <- as.numeric(format(as.Date(dates[1]), "%Y"))
  yr2 <- as.numeric(format(as.Date(dates[2]), "%Y"))
  
  ## Get the station
  nr.st <- GSODR::nearest_stations(LAT = lonlat[2], LON = lonlat[1], distance = distance)
  
  if(length(nr.st) == 0) stop("No stations found. Try increasing the distance.")
  
  nr.st1 <- nr.st[1]
  
  dts <- as.numeric(format(as.Date(dates), "%Y"))
  yrs <- seq(from = dts[1], to = dts[2])
  
  gsd <- GSODR::get_GSOD(years = yrs, station = nr.st1)
  stnid <- gsd$STNID[1]
  lati <- gsd$LATITUDE[1]
  longi <- gsd$LONGITUDE[1]
  
  if(fillin.radn){
    if(!requireNamespace("nasapower", quietly = TRUE)){
      warning("The nasapower package is required for this function")
      return(NULL)
    }
    pwr <- get_power_apsim_met(lonlat = lonlat, 
                               dates = c(gsd$YEARMODA[1], gsd$YEARMODA[nrow(gsd)]))
    pwr <- add_column_apsim_met(pwr, 
                                value = as.Date(c(1:nrow(pwr)-1), origin = paste0(yr1,"-01-01")),
                                name = "date", units = "()")
    pwr <- subset(pwr, select = c("date", "radn"))
    names(pwr) <- c("date", "RADN")
    gsd$date <- gsd$YEARMODA
    gsd <- merge(gsd, pwr, by = "date")
  }else{
    gsd$RADN <- NA  
  }
  
  gsd <- subset(as.data.frame(gsd), select = c("YEAR", "YDAY","RADN",
                                               "MAX", "MIN", "PRCP", "RH", "WDSP"))
  
  
  names(gsd) <- c("year", "day", "radn", "maxt", "mint", "rain", "rh", "windspeed")
  units <- c("()", "()", "(MJ/m2/day)", "(oC)", "(oC)", "(mm)", "(%)", "(m/s)")
  
  if(fillin.radn){
    comments <- paste("!data from GSODR R package. Radiation from nasapower R. retrieved: ", Sys.time())
  }else{
    comments <- paste("!data from GSODR R package. retrieved: ", Sys.time())  
  }
  
  attr(gsd, "filename") <- filename
  attr(gsd, "site") <- paste("site =", "station-ID", stnid)
  attr(gsd, "latitude") <- paste("latitude =", lati)
  attr(gsd, "longitude") <- paste("longitude =", longi)
  attr(gsd, "tav") <- paste("tav =", mean(colMeans(gsd[,c("maxt","mint")], na.rm=TRUE), na.rm=TRUE))
  attr(gsd, "colnames") <- names(gsd)
  attr(gsd, "units") <- units
  attr(gsd, "comments") <- comments
  ## No constants
  class(gsd) <- c("met", "data.frame")
  
  gsd <- amp_apsim_met(gsd)
  
  if(filename != "noname.met"){
    write_apsim_met(gsd, wrt.dir = wrt.dir, filename = filename)
  }
  return(invisible(gsd))
}
