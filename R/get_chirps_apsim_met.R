#'
#' This function requires the \CRANpkg{chirps} package
#'
#' @title Get CHIRPS data for an APSIM met file
#' @description Uses \code{\link[chirps]{get_chirps}} from the \CRANpkg{chirps} package to download data to create an APSIM met file.
#' @name get_chirps_apsim_met
#' @param lonlat Longitude and latitude vector
#' @param dates date ranges
#' @param wrt.dir write directory
#' @param filename file name for writing out to disk
#' @param fillin.radn whether to fill in radiation data using the nasapower pacakge. Default is TRUE.
#' @param silent defaut is FALSE. Changing it will not do anything at the moment. A future feature.
#' @return returns an object of class \sQuote{met} and writes a file to disk when filename is supplied.
#' @details If the filename is not provided it will not write the file to disk,
#' but it will return an object of class \sQuote{met}. This is useful in case manipulation
#' is required before writing to disk.
#' @export
#' @examples
#' \dontrun{
#' require(chirps)
#' ## This will not write a file to disk
#' chrp <- get_chirps_apsim_met(lonlat = c(-93,42), dates = c("2012-01-01","2012-12-31"))
#' }
#'

get_chirps_apsim_met <- function(lonlat, dates, wrt.dir = ".", filename = NULL,
                                 fillin.radn = TRUE, silent = FALSE){

  if(!requireNamespace("chirps", quietly = TRUE)){
    warning("The chirps package is required for this function")
    return(NULL)
  }

  if(missing(filename)) filename <- "noname.met"

  if(!grepl(".met", filename, fixed = TRUE)) stop("filename should end in .met")

  objct <- data.frame(lon = lonlat[1], lat = lonlat[2])

  chrp <- chirps::get_chirps(object = objct, dates = dates) ## This is only precipitation

  pwr <- get_power_apsim_met(lonlat = lonlat, dates = dates)

  pwr$rain <- chrp$chirps
  pwr$rh <- NULL
  pwr$wind_speed <- NULL

  chrp <- pwr

  if(filename != "noname.met"){
    write_apsim_met(chrp, wrt.dir = wrt.dir, filename = filename)
  }
  return(invisible(chrp))
}