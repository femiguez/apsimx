#'
#' This function requires the \CRANpkg{nasapower} package version 4.0.0.
#' 
#' It looks like the earliest year you can request data for is 1984.
#'
#' @title Get NASA-POWER data for an APSIM met file
#' @description Uses \code{\link[nasapower]{get_power}} from the \CRANpkg{nasapower} package to download data to create an APSIM met file.
#' @name get_power_apsim_met
#' @param lonlat Longitude and latitude vector
#' @param dates date ranges
#' @param wrt.dir write directory
#' @param filename file name for writing out to disk
#' @return returns an object of class \sQuote{met} and writes a file to disk when filename is supplied.
#' @details If the filename is not provided it will not write the file to disk, 
#' but it will return an object of class \sQuote{met}. This is useful in case manipulation
#' is required before writing to disk.
#' @export
#' @examples 
#' \dontrun{
#' require(nasapower)
#' ## This will not write a file to disk
#' pwr <- get_power_apsim_met(lonlat = c(-93,42), dates = c("2012-01-01","2012-12-31"))
#' ## Let's insert a missing value
#' pwr[100, "radn"] <- NA
#' summary(pwr)
#' ## Check the met file 
#' check_apsim_met(pwr)
#' ## Impute using linear interpolation
#' pwr.imptd <- impute_apsim_met(pwr, verbose = TRUE)
#' summary(pwr.imptd)
#' check_apsim_met(pwr.imptd)
#' }
#' 

get_power_apsim_met <- function(lonlat, dates, wrt.dir = ".", filename = NULL){
  
  if(!requireNamespace("nasapower", quietly = TRUE)){
    warning("The nasapower package is required for this function")
    return(NULL)
  }
  
  if(packageVersion("nasapower") <= '3.0.1'){
   stop("Please upgrade the 'nasapower' package to the latest version", call. = FALSE) 
  }
  
  if(missing(filename)) filename <- "noname.met"
   
  if(!grepl(".met", filename, fixed = TRUE)) stop("filename should end in .met")
  
  pwr <- nasapower::get_power(community = "AG",
                                pars = c("T2M_MAX",
                                         "T2M_MIN",
                                         "ALLSKY_SFC_SW_DWN",
                                         "PRECTOTCORR",
                                         "RH2M",
                                         "WS2M"),
                                dates = dates,
                                lonlat = lonlat,
                                temporal_api = "daily")    

  pwr <- subset(as.data.frame(pwr), select = c("YEAR", "DOY",
                                               "ALLSKY_SFC_SW_DWN",
                                               "T2M_MAX", "T2M_MIN",
                                               "PRECTOTCORR", "RH2M", "WS2M"))
  
  names(pwr) <- c("year", "day", "radn", "maxt", "mint", "rain", "rh", "windspeed")
  units <- c("()", "()", "(MJ/m2/day)", "(oC)", "(oC)", "(mm)", "(%)", "(m/s)")
  
  comments <- paste("!data from nasapower R package. retrieved: ", Sys.time())
    
  ## Calculating annual amplitude in mean monthly temperature

  attr(pwr, "filename") <- filename
  attr(pwr, "site") <- paste("site =", sub(".met", "", filename, fixed = TRUE))
  attr(pwr, "latitude") <- paste("latitude =", lonlat[2])
  attr(pwr, "longitude") <- paste("longitude =", lonlat[1])
  attr(pwr, "tav") <- paste("tav =", mean(colMeans(pwr[,c("maxt","mint")], na.rm=TRUE), na.rm=TRUE))
  attr(pwr, "colnames") <- names(pwr)
  attr(pwr, "units") <- units
  attr(pwr, "comments") <- comments
  ## No constants
  class(pwr) <- c("met", "data.frame")
  
  pwr <- amp_apsim_met(pwr)
  
  if(filename != "noname.met"){
    write_apsim_met(pwr, wrt.dir = wrt.dir, filename = filename)
  }
  return(invisible(pwr))
}


