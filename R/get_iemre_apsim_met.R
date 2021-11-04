#'
#' The original data can be obtained from: https://mesonet.agron.iastate.edu/iemre/
#'
#' @title Get weather data from Iowa Environmental Mesonet Reanalysis
#' @description Retrieves weather data from Iowa Environmental Mesonet Reanalysis into an APSIM met file
#' @name get_iemre_apsim_met
#' @param lonlat Longitude and latitude vector
#' @param dates date ranges
#' @param wrt.dir write directory
#' @param filename file name for writing out to disk
#' @param fillin.radn whether to fill in radiation data using the nasapower pacakge. Default is FALSE.
#' @details If the filename is not provided it will not write the file to disk, 
#' but it will return an object of class \sQuote{met}. This is useful in case manipulation
#' is required before writing to disk.
#' @note Multi-year query is not supported for this product. 
#' @return returns an object of class \sQuote{met} and writes a file to disk when filename is supplied.
#' @export
#' @examples 
#' \dontrun{
#' ## This will not write a file to disk
#' iemre <- get_iemre_apsim_met(lonlat = c(-93,42), dates = c("2012-01-01","2012-12-31"))
#' ## Note that solar radiation is not available, but can be filled in
#' ## using the nasapower package
#' iemre2 <- get_iemre_apsim_met(lonlat = c(-93,42), 
#'                              dates = c("2012-01-01","2012-12-31"), 
#'                              fillin.radn = TRUE)
#' summary(iemre)
#' summary(iemre2)
#' 
#' ## Still it is important to check this object
#' ## Since there is one day with missing solar radiation
#' check_apsim_met(iemre2)
#' }
#' 

get_iemre_apsim_met <- function(lonlat, dates, wrt.dir=".", filename=NULL,
                                fillin.radn = FALSE){
  
  if(missing(filename)) filename <- "noname.met"
  
  if(!grepl("met$", filename)) stop("filename should end in .met")
  
  str0 <- "https://mesonet.agron.iastate.edu/iemre/multiday/"
  
  ## Build date
  day1 <- as.character(as.Date(dates[1]))
  dayn <- as.character(as.Date(dates[2]))
  
  yr1 <- as.numeric(format(as.Date(dates[1]), "%Y"))
  yr2 <- as.numeric(format(as.Date(dates[2]), "%Y"))
  
  if(yr2 > yr1) stop("Multi-year queries are not supported at the moment. See the source.")
  
  ## Longitude and latitude
  lon <- as.numeric(lonlat[1])
  lat <- as.numeric(lonlat[2])
  
  str1 <- paste0(str0, day1, "/", dayn, "/", lat, "/", lon, "/json")
  
  iem.json <- jsonlite::fromJSON(str1)
  
  iem.dat <- as.data.frame(iem.json[[1]])
  
  iem.dat$year <- as.numeric(format(as.Date(iem.dat$date),"%Y"))
  
  iem.dat$radn <- NA
  
  ## Dates sequence
  dates.seq <- seq(from = as.Date(dates[1]), to = as.Date(dates[2]), by = "day")
  
  iem.dat$day <- as.numeric(format(dates.seq, "%j"))
  
  iem.dat$daily_high_c <- (iem.dat$daily_high_f - 32) * (5/9)
  iem.dat$daily_low_c <- (iem.dat$daily_low_f - 32) * (5/9)
  iem.dat$daily_precip_mm <- iem.dat$daily_precip_in * 25.4 
  
  iem.dat2 <- subset(iem.dat, select = c("year","day",
                                         "radn",
                                         "daily_high_c",
                                         "daily_low_c",
                                         "daily_precip_mm"))
  
  if(fillin.radn){
    if(!requireNamespace("nasapower", quietly = TRUE)){
      warning("The nasapower package is required for this function")
      return(NULL)
    }
    pwr <- get_power_apsim_met(lonlat = lonlat, 
                               dates = as.Date(c(paste0(yr1, "-01-01"), paste0(yr2, "-12-31"))))
    pwr$date <- as.Date(c(1:nrow(pwr)-1), origin = paste0(yr1,"-01-01"))
    pwr <- subset(pwr, date >= as.Date(day1) & date <= as.Date(dayn))
    
    if(nrow(iem.dat2) != nrow(pwr))
      stop("Something went wrong. Number of rows do not match.")
    
    iem.dat2$radn <- pwr$radn
  }
  
  names(iem.dat2) <- c("year","day","radn","maxt","mint","rain")
  units <- c("()","()","(MJ/m2/day)","(oC)","(oC)","(mm)")
  
  comments <- paste("!data from IEM Reanalysis. retrieved: ", Sys.time())
  
  attr(iem.dat2, "filename") <- filename
  attr(iem.dat2, "site") <- paste("site = ", sub(".met","", filename, fixed = TRUE))
  attr(iem.dat2, "latitude") <- paste("latitude =", lonlat[2])
  attr(iem.dat2, "longitude") <- paste("longitude =", lonlat[1])
  attr(iem.dat2, "tav") <- paste("tav =",mean(colMeans(iem.dat2[,c("maxt","mint")], na.rm=TRUE), na.rm=TRUE))
  attr(iem.dat2, "amp") <- paste("amp =",mean(iem.dat2$maxt, na.rm=TRUE) - mean(iem.dat2$mint, na.rm = TRUE))
  attr(iem.dat2, "colnames") <- names(iem.dat2)
  attr(iem.dat2, "units") <- units
  attr(iem.dat2, "comments") <- comments
  ## No constants
  class(iem.dat2) <- c("met", "data.frame")
  
  if(filename != "noname.met"){
    write_apsim_met(iem.dat2, wrt.dir = wrt.dir, filename = filename)
  }
  return(invisible(iem.dat2))
}


#'
#' The original data can be obtained from: https://mesonet.agron.iastate.edu/request/coop/fe.phtml
#'
#' @title Get weather data from Iowa Environmental Ag Weather Stations
#' @description Retrieves weather data from Iowa Environmental Mesonet (AgWeather) into an APSIM met file
#' @name get_iem_apsim_met
#' @param lonlat Longitude and latitude vector (optional)
#' @param dates date ranges
#' @param wrt.dir write directory
#' @param state state which you choose climate data from
#' @param station station which you choose cliamte data from
#' @param filename file name for writing out to disk
#' @details If the filename is not provided it will not write the file to disk, 
#' but it will return an object of class \sQuote{met}. This is useful in case manipulation
#' is required before writing to disk. For this function either provide the longitude 
#' and latitude or the state and station, but not both. In fact, \sQuote{state} and
#' \sQuote{station} will be ignored if \sQuote{lonlat} is supplied.
#' @return returns an object of class \sQuote{met} and writes a file to disk when filename is supplied.
#' @export
#' @examples 
#' \dontrun{
#' ## This will not write a file to disk
#' iem.met <- get_iem_apsim_met(state = "IA", 
#'                              station = "IA0200", 
#'                              dates = c("2012-01-01","2012-12-31"))
#' 
#' summary(iem.met)
#' 
#' ## Alternatively, coordinates can be used
#' ## This should be equivalent to the previous request
#' iem.met2 <- get_iem_apsim_met(lonlat = c(-93.77, 42.02), 
#'                               dates = c("2012-01-01","2012-12-31"))
#'
#' summary(iem.met2)
#' }
#' 

get_iem_apsim_met <- function(lonlat, dates, wrt.dir = ".", 
                              state, station, filename){
  
  if(missing(filename)) filename <- "noname.met"
  
  if(!grepl("met$", filename)) stop("filename should end in .met")
  
  str0 <- "http://mesonet.agron.iastate.edu/cgi-bin/request/coop.py?network="
  
  ## Build date
  day1 <- as.character(as.Date(dates[1]))
  dayn <- as.character(as.Date(dates[2]))
  yr1 <- format(as.Date(dates[1]), "%Y")
  yr2 <- format(as.Date(dates[2]), "%Y")
  mnth1 <- format(as.Date(dates[1]), "%m")
  mnth2 <- format(as.Date(dates[2]), "%m")
  dy1 <- format(as.Date(dates[1]), "%j")
  dy2 <- format(as.Date(dates[2]), "%j")
  
  ## Longitude and latitude or State and Station
  stts <- datasets::state.abb
  
  if(!missing(lonlat)){
    lon <- as.numeric(lonlat[1])
    lat <- as.numeric(lonlat[2])
    ## Need to find the state based on lonlat
    pts <- sf::st_as_sf(data.frame(lon = lon, lat = lat), coords = 1:2, crs = 4326)
    
    if(!requireNamespace("sf",quietly = TRUE)){
      warning("The sf is required for this function")
      return(NULL)
    }
    
    if(!requireNamespace("spData",quietly = TRUE)){
      warning("The spData is required for this function")
      return(NULL)
    }
    
    states <- spData::us_states ## This object contains states and their geometries
    states <- sf::st_transform(states, crs = 3857)
    pts <- sf::st_transform(pts, crs = 3857)
    ii <- as.integer(sf::st_intersects(pts, states))
    
    stt0 <- states[["NAME"]][[ii]]
    state <- datasets::state.abb[which(stt0 == datasets::state.name)]
    stt.climate <- paste0(state, "CLIMATE")
    
    ## Now find the station
    ftrs <- jsonlite::fromJSON(paste0("http://mesonet.agron.iastate.edu/geojson/network.php?network=",stt.climate))$features
    
    ## Subset stations which include the dates request
    yr1 <- as.numeric(format(as.Date(dates[1]), "%Y"))
    yr2 <- as.numeric(format(as.Date(dates[2]), "%Y"))
    
    ## This chucnk of code selects stations which have data in the desired range
    start.year <- sapply(ftrs$properties$time_domain, function(x) as.numeric(gsub("(", "", strsplit(x, "-")[[1]][1], fixed = TRUE)))
    end.year0 <- sapply(ftrs$properties$time_domain, function(x) gsub("(", "", strsplit(x, "-")[[1]][2], fixed = TRUE))
    end.year <- ifelse(end.year0 == "Now)", 
                       as.numeric(format(Sys.Date(), "%Y")), 
                       suppressWarnings(as.numeric(gsub(")", "", end.year0, fixed = TRUE))))
    
    wch.yr <- which(start.year <= yr1 & end.year >= yr2)
    ftrs <- ftrs[wch.yr,]
    ## end of time span selection
    
    geo.station <- ftrs$geometry$coordinates
    geo.station.lon <- sapply(geo.station, FUN = function(x) x[[1]])
    geo.station.lat <- sapply(geo.station, FUN = function(x) x[[2]])
    
    station.coords <- sf::st_as_sf(data.frame(lon = geo.station.lon, 
                                              lat = geo.station.lat,
                                              station = ftrs$id), 
                                   coords = c("lon","lat"), 
                                   crs = sf::st_crs(4326))
    station.coords <- sf::st_transform(station.coords, crs = 3857)
    near.station.index <- sf::st_nearest_feature(pts, station.coords)
    station <- ftrs[near.station.index, "id"]
    
  }else{
  
    ## In this case, state and station should be given
    ## Get the state
    
    if(missing(state)) stop("state is missing with no default")
    
    if(!(state %in% stts)) stop("state must be incorrect")
    stt.climate <- paste0(state, "CLIMATE")
  
    ## Get the station
    ftrs <- jsonlite::fromJSON(paste0("http://mesonet.agron.iastate.edu/geojson/network.php?network=",stt.climate))$features
    if(!(station %in% ftrs$id)){
      cat("Available stations", ftrs$id, "\n")
      stop("station must be incorrect")
    } 
  }
   
  ## Build string
  str1 <- paste0(str0, stt.climate, "&stations=", station)
  str2 <- paste0(str1, "&year1=", yr1, "&month1=", mnth1, "&day1=", dy1,
                       "&year2=", yr2, "&month2=", mnth2, "&day2=", dy2)
  str3 <- paste0(str2, "&vars%5B%5D=apsim&what=view&delim=comma&gis=no")
  
  ## Retrieve data
  iem0 <- readLines(str3)
  write(iem0, file = file.path(wrt.dir, filename))
  iem.dat <- read_apsim_met(filename, src.dir = wrt.dir, verbose = FALSE)
  
  attr(iem.dat, "filename") <- paste0(state, "-", station, ".met")
  attr(iem.dat, "site") <- paste0("site = ", state, "-", station)
  
  if(!missing(lonlat)){
    attr(iem.dat, "longitude") <- paste("longitude =", lonlat[1], "(DECIMAL DEGREES)")
  }
  
  if(missing(lonlat)){
    stt <- ftrs[ftrs$id == station, ]
    attr(iem.dat, "longitude") <- paste("longitude =", stt$geometry$coordinates[[1]][1], "(DECIMAL DEGREES)")
    attr(iem.dat, "latitude") <- paste("latitude =", stt$geometry$coordinates[[1]][2], "(DECIMAL DEGREES)")
  }
  
  if(nrow(iem.dat) == 0)
    warning("No weather data for this location and dates combination")
  
  if(filename == "noname.met"){
    unlink(file.path(wrt.dir, filename))
    return(iem.dat)
  }else{
    return(invisible(iem.dat))
  } 
}
