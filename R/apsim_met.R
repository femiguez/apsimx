#' Read a met file into R
#' 
#' This function uses S3 classes and stores the additional information as attributes \cr
#' I use a more strict format than \acronym{APSIM} and reading and writing will not \cr
#' preserve all the details. For example, at this moment comments are lost through \cr
#' the process of read and write unless they are added back in manually. \cr
#' Also, empty lines are ignored so these will be lost as well in the read and write process.
#' 
#' @title Read in an APSIM met file
#' @name read_apsim_met
#' @description Read into R a met file and return an object of class \sQuote{met}
#' @param file path to met file
#' @param src.dir optional source directory
#' @param verbose whether to suppress all messages and warnings
#' @return an object of class \sQuote{met} with attributes
#' @export
#' @examples 
#' \donttest{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' ames.met <- read_apsim_met("Ames.met", src.dir = extd.dir)
#' ames.met
#' }
#' 

read_apsim_met <- function(file, src.dir = ".", verbose = TRUE){

  if(!grepl("met$",file)) stop("file should have a .met extension")
  
  file.path <- file.path(src.dir,file)

  ## Read the header
  header <- scan(file = file.path, 
                 what = "character", 
                 sep = "\n",
                 blank.lines.skip = FALSE,
                 nlines = 30, 
                 quiet = TRUE)
  ## hdrl is for keeping track of header lines
  hdrl <- 0; skip.lines <- 0 
  ## attrs <- c("name","site","latitude","longitude","tav","amp","clnms","clunits")
  name <- NULL; site <- NULL; latitude <- NULL; longitude <- NULL; 
  tav <- NULL; amp <- NULL; clnms <- NULL; clunits <- NULL; comments <- NULL
  constants <- vector(mode = "list",30); constant.count <- 0; fnd <- FALSE
  comment.lines <- 0
  ## This is as ugly as it gets but so are met files
  for(i in 1:30){
    if(grepl("^!",header[i])){comment.lines <- comment.lines + 1; next}
    if(grepl("[weather.met.weather]",header[i],fixed=TRUE)){name <- header[i];hdrl <- hdrl + 1; fnd <- TRUE}
    if(grepl("^site",header[i],ignore.case=TRUE)){site <- header[i];hdrl <- hdrl + 1; fnd <- TRUE}
    if(grepl("^latitude",header[i],ignore.case=TRUE)){latitude <- header[i];hdrl <- hdrl + 1; fnd <- TRUE} 
    if(grepl("^longitude",header[i],ignore.case=TRUE)){longitude <- header[i];hdrl <- hdrl + 1; fnd <- TRUE} 
    if(grepl("^tav",header[i])){tav <- header[i];hdrl <- hdrl + 1; fnd <- TRUE}
    if(grepl("^amp",header[i])){amp <- header[i];hdrl <- hdrl + 1; fnd <- TRUE}
    if(grepl("year",header[i]) && grepl("radn",header[i])){clnms <- header[i];hdrl <- hdrl + 1; fnd <- TRUE}
    if(grepl("()",header[i],fixed=TRUE)){clunits <- header[i];skip.lines <- i;hdrl <- hdrl + 1; fnd <- TRUE}
    if(grepl("=",header[i],fixed=TRUE) && fnd == FALSE){
      constant.count <- constant.count + 1
      constants[constant.count] <- header[i]
      hdrl <- hdrl + 1
    } 
    fnd <- FALSE
  }

  constants <- unlist(constants[1:constant.count])
  
  if(constant.count == 0){
    constants <- NA
  }

  if(verbose){
    cat("Found ",hdrl," header lines \n")
    cat("Found ",comment.lines," comment lines \n")
    cat("Found ",skip.lines," skip lines \n")
    cat("Found ",constant.count,"constants \n")
  }
  
  ## I only check the first 6 column names but there might be more
  clnms <- sub("^\\s+","",clnms)
  clnms.s <- strsplit(clnms,"\\s+")[[1]]
  if(sum(clnms.s %in% c("year","day","radn","maxt","mint","rain")) < 6){
    cat("All column names:",clnms,"\n") 
    warning("column names might be wrong")
  }
  
  clunits <- sub("^\\s+","",clunits)
  clunits.s <- strsplit(clunits,"\\s+")[[1]]
  ## Sounds like there is no point in checking units
  ## As they are a complete mess
    
  met <- utils::read.table(file = file.path, 
                           header = FALSE, 
                           as.is = TRUE,
                           na.strings = c(NA,-99),
                           comment.char = "!", 
                           col.names = clnms.s,
                           skip = skip.lines)
  
  attr(met, "filename") <- file
  attr(met, "site") <- ifelse(is.null(site),NA,site)
  attr(met, "latitude") <- latitude
  attr(met, "longitude") <- ifelse(is.null(longitude),NA,longitude)
  attr(met, "tav") <- tav
  attr(met, "amp") <- amp
  attr(met, "colnames") <- clnms.s
  attr(met, "units") <- clunits.s
  attr(met, "constants") <- constants
  attr(met, "comments") <- ifelse(is.null(comments),NA,comments)
  class(met) <- c("met","data.frame")
  return(met)
}

#' Write a met file to disk. It takes an object of class \sQuote{met}
#' 
#' @title Write an APSIM met file
#' @name write_apsim_met
#' @description Write an object of class \sQuote{met} to disk
#' @param met object of class \sQuote{met}
#' @param wrt.dir directory where the file will be written
#' @param filename optional alternative filename
#' @return does not create an R object, it only writes to disk
#' @details at the moment the read-write cycle will strip comments
#' @export
#' @examples 
#' \donttest{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' ames.met <- read_apsim_met("Ames.met", src.dir = extd.dir)
#' ames.met
#' tmp.dir <- tempdir()
#' write_apsim_met(ames.met, wrt.dir = tmp.dir, filename = "Ames.met")
#' ## Here I write to a temporary directory, but change this to where
#' ## you want to write to
#' }
#' 

write_apsim_met <- function(met, wrt.dir = NULL, filename = NULL){
  
  if(attr(met, "filename") != "noname.met" && is.null(filename)) filename <- attr(met, "filename")
    
  if(is.null(wrt.dir) && is.null(filename)){
    ## This assumes that the full path is in filename
    file.path <- attr(met, "filename")
  }
  if(!is.null(wrt.dir) && is.null(filename)){
    if(attr(met, "noname.met")){
      stop("Need to supply filename if 'wrt.dir' is not NULL")
    }else{
     file.path <- file.path(wrt.dir, attr(met, "filename")) 
    }
  }
  if(is.null(wrt.dir) && !is.null(filename)){
    stop("Need to supply 'wrt.dir' if filename is not NULL")
  }
  if(!is.null(wrt.dir) && !is.null(filename)){
    file.path <- file.path(wrt.dir, filename)
  }
  if(!is.null(filename)){
    if(!grepl(".met", filename, fixed=TRUE)) stop("filename should end in .met")
  }
  ## Open connection
  con <- file(description = file.path, open = "w")
  ## Write comments if they exist
  if(!is.na(attr(met,"comments")) && length(attr(met,"site")) > 0) 
    writeLines(attr(met,"comments"), con = con)
  ## Start header
  writeLines("[weather.met.weather]", con = con)
  ## Write site if it exists
  if(!is.na(attr(met,"site")) && length(attr(met,"site")) > 0){
    writeLines(attr(met,"site"), con = con)
  } 
  writeLines(attr(met,"latitude"), con = con)
  if(!is.na(attr(met,"longitude")) && length(attr(met,"longitude")) > 0){
    writeLines(attr(met,"longitude"), con = con)
  }
  writeLines(attr(met,"tav"), con = con)
  writeLines(attr(met,"amp"), con = con)
  ## Write constants
  if(!is.na(attr(met,"constants")) && length(attr(met,"constants")) > 0){
    for(i in seq_along(attr(met,"constants"))){
      writeLines(attr(met,"constants")[i], con = con)
    }
  }
  writeLines(paste(attr(met,"colnames"), collapse = " "), con = con)
  writeLines(paste(attr(met,"units"), collapse = " "), con = con)
  
  names(met) <- NULL
  utils::write.table(met, file = con,
                     append = TRUE, quote = FALSE,
                     row.names = FALSE, col.names = FALSE)
  close(con)
}

#' @title Printer-friendly version of a metfile
#' @name print.met
#' @description Print a met file in a friendly way
#' @param x an R object of class \sQuote{met}
#' @param ... additional printing arguments
#' @return It prints to console. Not used to return an R object.
#' @export
#' 
print.met <- function(x,...){
  
  ## print the header and just head
  cat(attr(x, "filename"),"\n")
  if(!is.na(attr(x,"site"))) cat(attr(x, "site"),"\n")
  cat(attr(x, "latitude"),"\n")
  if(!is.na(attr(x,"longitude"))) cat(attr(x, "longitude"),"\n")
  cat(attr(x, "tav"),"\n")
  cat(attr(x, "amp"),"\n")
  cat(attr(x, "colnames"),"\n")
  cat(attr(x, "units"),"\n")
  if(!any(is.na(attr(x,"constants"))) && !is.null(attr(x,"constants"))) cat(attr(x, "constants"), "\n")
  
  print(utils::head(as.data.frame(x),...))
}

#' @title Perform imputation for missing data in a met file
#' @name impute_apsim_met
#' @description Takes in an object of class \sQuote{met} and imputes values
#' @param met object of class \sQuote{met}
#' @param method method for imputation, \sQuote{approx} (\code{\link[stats]{approxfun}}),
#' \sQuote{spline} (\code{\link[stats]{splinefun}}) or \sQuote{mean} (\code{\link{mean}}).
#' @param verbose whether to print missing data to the console, default = FALSE
#' @param ... additional arguments to be passed to imputation method
#' @return an object of class \sQuote{met} with attributes
#' @export
#' 

impute_apsim_met <- function(met, method = c("approx","spline","mean"), verbose = FALSE, ...){
  
  if(!inherits(met, "met")) stop("met should be of class 'met'") 
  
  method <- match.arg(method)
  
  ## Someday I will do this when it is needed
  args <- list(...)
  
  ## If there is a missing value in the first row it won't be imputed
  if(any(is.na(met[1,]))){
    wn1r <- which(is.na(met[1,]))
    for(i in seq_along(wn1r)){
      met[1, wn1r[i]] <- mean(met[1:5, wn1r[i]], na.rm = TRUE)
      cat("Imputed first row for:", names(met)[wn1r[i]], "\n")
    }
    print(as.data.frame(met[1,]))
  }
  
  ## If there is a missing value in the last row it won't be imputed
  if(any(is.na(met[nrow(met),]))){
    wn1r <- which(is.na(met[nrow(met),]))
    for(i in seq_along(wn1r)){
      met[nrow(met), wn1r[i]] <- mean(met[(nrow(met) - 5):nrow(met), wn1r[i]], na.rm = TRUE)
      cat("Imputed last row for:", names(met)[wn1r[i]], "\n")
    }
    print(as.data.frame(met[nrow(met),]))
  }
  
  ## Which rows have missing data
  missing.vector <- vector(mode = "numeric", length = length(names(met)))
  
  if(all(sapply(sapply(met, function(x) which(is.na(x))),length) == 0))
    warning("No missing data found")
  
  missing.rows <- sapply(met, function(x) which(is.na(x)), simplify = FALSE)

  if(verbose){
    for(i in 1:ncol(met)){
      tmp.mr <- missing.rows[[i]]
      if(length(tmp.mr) > 0){
        cat("Missing values for", names(met)[i], "\n")
        print(as.data.frame(met)[tmp.mr,])
      }
    }
  }
  
  col.classes <- sapply(met, class) 
  ## I might need to prevent imputation on characters/factors 
  which.col.missing.values <- which(sapply(missing.rows, function(x) length(x)) > 0)

  for(i in which.col.missing.values){
    if(method == "approx"){
      imputed.values <- stats::approx(x=seq_len(nrow(met)),
                                      y=met[[i]],
                                      xout=missing.rows[[i]])$y
    }
    if(method == "spline"){
      imputed.values <- stats::spline(x=seq_len(nrow(met)),
                                      y=met[[i]],
                                      xout=missing.rows[[i]])$y
    }
    if(method == "mean"){
      imputed.values <- mean(met[[i]], na.rm = TRUE)
    }
    met[missing.rows[[i]],i] <- imputed.values
  }
  return(met)
}

#' @title Check a met file for possible errors
#' @name check_apsim_met
#' @description Takes in an object of class \sQuote{met} and checks for missing/valid/reasonable values
#' @param met object of class \sQuote{met}
#' @details It will only check for missing values and reasonable (within range) values for:
#'  \sQuote{year}: range (1500 to 3000); \cr
#'  \sQuote{day}: range (1 to 366); \cr 
#'  \sQuote{maxt}: range (-60 to 60) -- units (C); \cr
#'  \sQuote{mint}: range (-60 to 40) -- units (C); \cr
#'  \sQuote{radn}: range (0 to 40) -- units (MJ/m2/day);  \cr
#'  \sQuote{rain}: range (0 to 100) -- units (mm/day)
#' @return does not return anything unless possible errors are found
#' @export
#' 
check_apsim_met <- function(met){
  
  if(!inherits(met, "met")) stop("object should be of class 'met'")
  
  col.names <- c("year","day","radn","mint","maxt","rain")
  ## check for column names
  if(sum(names(met) %in% col.names) < 6) warning("column names might be wrong")
  ## Detect possible errors with years
  if(any(is.na(met[["year"]]))){
    print(met[is.na(met$year),])
    warning("Missing values found for year")
  }
  if(any(min(met[["year"]], na.rm = TRUE) < 1500)){
    print(met[met$year < 1500,])
    warning("year is less than 1500")
  }
  if(any(max(met[["year"]], na.rm = TRUE) > 3000)){
    print(met[met$year > 3000,])
    warning("year is greater than 3000")
  }
  ## Detect possible errors with day
  if(any(is.na(met[["day"]]))){
    print(met[is.na(met$day),])
    warning("Missing values found for day")
  }
  if(any(min(met[["day"]], na.rm = TRUE) < 1)){
    print(met[met$day < 1,])
    warning("day is less than 1")
  }
  if(any(max(met[["day"]], na.rm = TRUE) > 366)){
    print(met[met$day > 366,])
    warning("day is greater than 366")
  }
  ## Check for date discontinuities
  origin <- as.Date(paste0(met$year[1], "-01-01"))
  first.day <- doy2date(met$day[1], year = met$year[1])
  last.day <- doy2date(met$day[nrow(met)], year = met$year[nrow(met)])
  dates <- seq(from = first.day, to = last.day, by = 1)
  if(nrow(met) < length(dates)){
    warning(paste(length(dates) - nrow(met), "date discontinuities found. Consider using napad_apsim_met."))
  }
  ## Detect possible errors with minimum temperature
  if(any(is.na(met[["mint"]]))){
    print(met[is.na(met$mint),])
    warning("Missing values found for minimum temperature")
  }
  if(any(min(met[["mint"]], na.rm = TRUE) < -60)){
    print(met[met$mint < -60,])
    warning("Minimum temperature is less than -60")
  }
  if(any(max(met[["mint"]], na.rm = TRUE) > 40)){
    print(met[met$mint > 40,])
    warning("Minimum temperature is greater than 40")
  }
  ## Detect possible errors with maximum temperature
  if(any(is.na(met[["maxt"]]))){
    print(met[is.na(met$maxt),])
    warning("Missing values found for maximum temperature")
  }
  if(any(min(met[["maxt"]], na.rm = TRUE) < -60)){
    print(met[met$maxt < -60,])
    warning("Maximum temperature less than -60")
  }
  if(any(max(met[["maxt"]], na.rm = TRUE) > 60)){
    print(met[met$maxt > 60,])
    warning("Maximum temperature is greater than 60")
  }
  ## Detect possible errors with radiation
  if(any(is.na(met[["radn"]]))){
    print(met[is.na(met$radn),])
    warning("Missing values found for solar radiation")
  }
  if(any(min(met[["radn"]], na.rm = TRUE) < 0)){
    print(met[met$radn < 0,])
    warning("Radiation is negative")
  }
  if(any(max(met[["radn"]], na.rm = TRUE) > 40)){
    print(met[met$radn > 40,])
    warning("Radiation is greater than 40 (MJ/m2/day)")
  }
  ## Detect possible errors with rain
  if(any(is.na(met[["rain"]]))){
    print(met[is.na(met$rain),])
    warning("Missing values found for precipitation")
  }
  if(any(min(met[["rain"]], na.rm = TRUE) < 0)){
    warning("Rain is negative")
  }
  if(any(max(met[["rain"]], na.rm = TRUE) > 100)){
    warning("Rain is possibly too high")
  }
  
  if(!is.null(met$vp)){
    if(max(met[["vp"]], na.rm = TRUE) > 100){
      warning("Vapor Pressure units might be worng. It should be in hecto Pascals")
    }
  }
}

#' Fill in with missing data date discontinuities in a met file
#' 
#' @title Pad a met file with NAs when there are date discontinuities
#' @name napad_apsim_met
#' @description It will fill in or \sQuote{pad} a met object with NAs
#' @param met object of class \sQuote{met}
#' @note The purpose of this function is to allow for imputation using \code{\link{impute_apsim_met}}
#' @return It returns an object of class \sQuote{met} with padded NAs.
#' @export

napad_apsim_met <- function(met){
 
  if(!inherits(met, "met")) stop("object should be of class 'met'")
  
  ## First check if there are any discontinuities
  origin <- as.Date(paste0(met$year[1], "-01-01"))
  first.day <- doy2date(met$day[1], year = met$year[1])
  last.day <- doy2date(met$day[nrow(met)], year = met$year[nrow(met)])
  dates <- seq(from = first.day, to = last.day, by = "day")
  if(nrow(met) == length(dates)){
    stop("No discontinuities found")
  }  
  
  ## Create a data.frame with continuous dates
  namet <- data.frame(date = dates)
  
  ## Create date column for merging
  met$date <- rep(as.Date(NA), length.out = nrow(met))
  for(i in 1:nrow(met)){
    met$date[i] <- doy2date(met$day[i], year = met$year[i])
  }
  
  ans <- merge(namet, met, by = "date", all.x = TRUE)
  ans$year <- as.numeric(format(ans$date, "%Y"))
  ans$day <- as.numeric(format(ans$date, "%j"))
  ans$date <- NULL
 
  attr(ans, "filename") <- attr(met, "filename")
  attr(ans, "site") <- attr(met, "site")
  attr(ans, "latitude") <- attr(met, "latitude")
  attr(ans, "longitude") <- attr(met, "longitude")
  attr(ans, "tav") <- attr(met, "tav")
  attr(ans, "amp") <- attr(met, "amp")
  attr(ans, "colnames") <- attr(met, "colnames")
  attr(ans, "units") <- attr(met, "units")
  attr(ans, "constants") <- attr(met, "constants")
  attr(ans, "comments") <- attr(met, "comments")
  class(ans) <- c("met","data.frame")
  return(ans) 
}

#' Simple utility for converting a data frame to an object of class met
#' 
#' @title Conversion from data frame to met object
#' @name as_apsim_met
#' @description It makes minimum assumptions about the data so it is recommended to change defaults
#' @param x object of class \sQuote{data frame}
#' @param filename default \sQuote{noname.met}
#' @param site default \sQuote{nosite}
#' @param latitude default is zero (0)
#' @param longitude default is zero (0)
#' @param tav average temperature (calculated if not supplied)
#' @param amp temperature amplitude (calculated if not supplied)
#' @param colnames default are \dQuote{year}, \dQuote{day}, \dQuote{radn}, 
#'                  \dQuote{maxt}, \dQuote{mint}, \dQuote{rain}
#' @param units default are \dQuote{()}, \dQuote{()}, \dQuote{(MJ/m2/day)},
#'              \dQuote{(oC)}, \dQuote{(oC)}, \dQuote{(mm)}
#' @param constants default is \dQuote{NA}
#' @param comments default is \dQuote{NA}   
#' @param check whether to check the resulting met file using \code{\link{check_apsim_met}}.
#' default is TRUE.  
#' @return it returns an object of class \sQuote{met}.   
#' @export
#' 
as_apsim_met <- function(x,
                         filename = "noname.met",
                         site = "nosite",
                         latitude = 0,
                         longitude = 0,
                         tav = NA,
                         amp = NA,
                         colnames = c("year", "day", "radn", "maxt", "mint", "rain"),
                         units = c("()", "()", "(MJ/m2/day)", "(oC)", "(oC)", "(mm)"),
                         constants = NA,
                         comments = NA,
                         check = TRUE){
  
  if(!inherits(x, "data.frame"))
    stop("Object should be of class data.frame")

  if(ncol(x) != 6)
    stop("If number of columns is not 6 then provide column names")
  
  names(x) <- colnames
  
  attr(x, "filename") <- filename
  attr(x, "site") <- paste("site =", site)
  attr(x, "latitude") <- paste("latitude =", latitude)
  attr(x, "longitude") <- paste("longitude =", longitude)
  
  if(is.na(tav)){
    tav <- mean(colMeans(x[,c("maxt","mint")], na.rm=TRUE), na.rm=TRUE)
    attr(x, "tav") <- paste("tav =", tav, "(oC) ! annual average ambient temperature")  
  }else{
    attr(x, "tav") <- tav
  }
  
  if(is.na(amp)){
    attr(x, "amp") <- paste("amp =", mean(x$maxt, na.rm=TRUE) - mean(x$mint, na.rm = TRUE), "(oC) ! annual amplitude in mean monthly temperature")  
  }else{
    attr(x, "amp") <- amp  
  }
  
  attr(x, "colnames") <- colnames
  attr(x, "units") <- units
  attr(x, "constants") <- constants
  attr(x, "comments") <- comments
  class(x) <- c("met","data.frame")
  
  if(check) apsimx::check_apsim_met(x)
  
  return(x) 
}
  
#' Calculating Thermal Time using a variety of methods
#' 
#' @title Calculates Thermal Time taking a \sQuote{met} object
#' @name tt_apsim_met
#' @description Calculates Thermal Time using the \sQuote{Classic} formula, 
#' Heat Stress, Crop Heat Unit and other methods
#' @param met object of class \sQuote{met}
#' @param dates when the calculation starts and when it ends. At the moment
#' it needs to be a character vector (e.g. c(\sQuote{01-05}, \sQuote{10-10})). It will
#' use the same dates every year for multiple years.
#' @param method one of \sQuote{Classic_TT}, \sQuote{HeatStress_TT}, \sQuote{ASPIM_TT},
#' \sQuote{CERES_TT} and \sQuote{all}
#' @param x_temp cardinal temperatures (base, optimal and maximum)
#' @param y_tt thermal time accumulation for cardinal temperatures
#' @param base_temp base temperature for Classic TT calculation
#' @param max_temp maximum temperature for Classic TT calculation
#' @param dates.format default is \sQuote{\%d-\%m} which means day and month
#' @return it returns an object of class \sQuote{met} with additional columns
#' \sQuote{Date} and the corresponding TT calculation
#' @export
#' @references  Abendroth, L.J., Miguez, F.E., Castellano, M.J. and Hatfield, J.L. (2019),
#'  Climate Warming Trends in the U.S. Midwest Using Four Thermal Models. 
#'  Agron. J., 111: 3230-3243. (doi:10.2134/agronj2019.02.0118)
#' @examples 
#' \dontrun{
#' require(nasapower)
#' require(ggplot2)
#' 
#' pwr <- get_power_apsim_met(lonlat = c(-93,42), dates = c("2012-01-01","2015-12-31"))
#' check_apsim_met(pwr)
#' pwr <- impute_apsim_met(pwr)
#' 
#' pwr2 <- tt_apsim_met(pwr, dates = c("01-05", "30-10"), method = c("Classic", "Heat"))
#' 
#' ggplot(data = pwr2, aes(x = Date, y = Classic_TT)) + geom_point()
#' 
#' ggplot(data = pwr2, aes(x = Date, y = HeatStress_TT)) + geom_point()
#' 
#' }

tt_apsim_met <- function(met, dates, 
                         method = c("Classic_TT", "HeatStress_TT", "CropHeatUnit_TT",
                                    "APSIM_TT", "CERES_TT", "all"),
                         x_temp = c(0, 26, 34),
                         y_tt = c(0, 26, 0),
                         base_temp = 0,
                         max_temp = 30,
                         dates.format = c("%d-%m")){

  if(!missing(met) && !inherits(met, "met"))
    stop("Object met should be of class met")

  method <- match.arg(method, several.ok = TRUE)
  
  if("all" %in% method) method <- c("Classic_TT", "HeatStress_TT", "CropHeatUnit_TT")
  
  if("APSIM_TT" %in% method) stop("not implemented yet.")
  if("CERES_TT" %in% method) stop("not implemented yet.")
  
  if(is.na(as.Date(dates[1], format = dates.format))) stop("first date might be in the wrong format")
  if(is.na(as.Date(dates[2], format = dates.format))) stop("second date might be in the wrong format")
  
  start.date <- as.Date(paste0("01-01", "-", min(met$year)), format = paste0(dates.format, "-%Y"))
  end.date <- as.Date(paste0("31-12", "-", max(met$year)), format = paste0(dates.format, "-%Y"))
  
  tmpd <- data.frame(Dates = seq(from = start.date, 
                                 to = end.date, 
                                 by = "day"), index = 0)
  
  if("Classic_TT" %in% method) met$Classic_TT <- 0
  if("HeatStress_TT" %in% method) met$HeatStress_TT <- 0
  if("CropHeatUnit_TT" %in% method) met$CropHeatUnit_TT <- 0
  
  if(nrow(met) != nrow(tmpd))
    warning("Days for each year should be complete")
  
  ## This first instance will result in calculating TT for every year
  if(inherits(dates, "character")){
    doy1m <- format(as.Date(dates[1], format = dates.format), "%m-%d")
    doynm <- format(as.Date(dates[2], format = dates.format), "%m-%d")
    doy1 <- date2doy(doy1m)
    doyn <- date2doy(doynm)
    if(doy1 > doyn){
      ## We are in the southern hemisphere
      d1 <- as.Date(paste0(doy1m, "-", 2020), format = "%m-%d-%Y")
      dn <- as.Date(paste0(doynm, "-", 2021), format = "%m-%d-%Y")
      doy.seq <- as.numeric(format(seq(from = d1, to = dn, by = "day"), "%j")) 
      ## Writing it in this way will always include 366
      tmpd$index <- ifelse(!is.na(match(met$day, doy.seq)), 1, 0)
    }else{
      doy.seq <- doy1:doyn
      tmpd$index <- ifelse(!is.na(match(met$day, doy.seq)), 1, 0)
    }
  }

  cum.classic.tt <- 0
  cum.heatstress.tt <- 0
  cum.cropheatunit.tt <- 0
  k <- 0
  
  for(i in 1:nrow(met)){
    
    ## In the Southern hemisphere we start at k = 1
    if(tmpd$index[i] > 0.5 && i == 1) k <- 1
    ## In the Northern hemisphere we should just skip the first days until doy1
    if(tmpd$index[i] < 0.5 && k == 0) next
    
    if(tmpd$index[i] > 0.5 && met$day[i] == doy1) k <- k + 1  
    
    if(tmpd$index[i] > 0.5){
      if("Classic_TT" %in% method){
        mint.m <- max(met$mint[i], base_temp)
        maxt.m <- min(met$maxt[i], max_temp)
        classic.tt <- (maxt.m + mint.m)/2 - base_temp
        classic.tt <- ifelse(classic.tt >= 0, classic.tt, 0)
        cum.classic.tt <- cum.classic.tt + classic.tt
        met$Classic_TT[i] <- cum.classic.tt
      }
      if("HeatStress_TT" %in% method){
        heatstress.tt <- met$maxt[i] - max_temp
        heatstress.tt <- ifelse(heatstress.tt >= 0, heatstress.tt, 0)
        cum.heatstress.tt <- cum.heatstress.tt + heatstress.tt
        met$HeatStress_TT[i] <- cum.heatstress.tt
      }
      if("CropHeatUnit_TT" %in% method){
        mint.m <- 1.8 * met$mint[i] - 4.4
        maxt.m <- 3.33 * (met$maxt[i] - 10) - 0.084 * (met$maxt[i] - 10)^2
        cropheatunit.tt <- (maxt.m + mint.m)/2 
        cropheatunit.tt <- ifelse(cropheatunit.tt >= 0, cropheatunit.tt, 0)
        cum.cropheatunit.tt <- cum.cropheatunit.tt + cropheatunit.tt
        met$CropHeatUnit_TT[i] <- cum.cropheatunit.tt
      }
    }else{
      cum.classic.tt <- 0
      cum.heatstress.tt <- 0
      cum.cropheatunit.tt <- 0
    }
  }

  met$Date <- tmpd$Dates
  return(met)
}
  
  
  