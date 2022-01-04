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
  if(is.na(attr(met,"latitude")) || length(attr(met,"latitude")) == 0){
    stop("latitude should be present", call. = FALSE)
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
      if(all(is.na(met[1:15, wn1r[i]]))){
        warning("The mean was used to impute the first row as the first 15 values were NAs")
        met[1, wn1r[i]] <- mean(met[, wn1r[i]], na.rm = TRUE)
      }else{
        met[1, wn1r[i]] <- mean(met[1:15, wn1r[i]], na.rm = TRUE)  
      }
      cat("Imputed first row for:", names(met)[wn1r[i]], "\n")
    }
    print(as.data.frame(met[1,]))
  }
  
  ## If there is a missing value in the last row it won't be imputed
  if(any(is.na(met[nrow(met),]))){
    wn1r <- which(is.na(met[nrow(met),]))
    for(i in seq_along(wn1r)){
      if(all(is.na(met[(nrow(met) - 15):nrow(met), wn1r[i]]))){
        warning("The mean was used to impute the last row as the last 15 values were NAs")
        met[nrow(met), wn1r[i]] <- mean(met[, wn1r[i]], na.rm = TRUE)
      }else{
        met[nrow(met), wn1r[i]] <- mean(met[(nrow(met) - 15):nrow(met), wn1r[i]], na.rm = TRUE)  
      }
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
  
  if(!inherits(met, "met")) 
    stop("object should be of class 'met'", call. = FALSE)
  
  if(nrow(met) == 0)
    stop("No rows of data present in this object.", call. = FALSE)

  
  if(length(colnames(met)) != length(attr(met, "colnames")))
    stop("Length of column names does not match the length of 'colnames' in the attributes", call. = FALSE)
  
  col.names <- attr(met, "colnames")
  
  if(length(col.names) != ncol(met)){
    cat("Names in data.frame:", names(met), "\n")
    cat("Names in attribute column names", col.names, "\n")
    cat("Different names", setdiff(names(met), col.names), "\n")
    warning("Number of columns in data.frame do not match column names")
  } 
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
  ## Check for last day of leap year
  if(is_leap_year(met$year[nrow(met)]) && met$day[nrow(met)] == 365){
    warning("Last year in the met file is a leap year and it only has 365 days")
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
  ## Make sure that maxt is always higher or equal to mint
  temp.diff <- met[["maxt"]] - met[["mint"]]
  temp.diff <- temp.diff[!is.na(temp.diff)] ## Just look at data which is not NA
  if(any(temp.diff < 0)){
    print(met[which(temp.diff < 0),])
    warning("Minimum temperature is greater than maximum temperature")
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
    ## Apparently it is more or less reasonable to question 100mm in 24 hours
    ## https://journals.ametsoc.org/view/journals/hydr/20/12/jhm-d-19-0039_1.xml
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
  ## first.day <- doy2date(met$day[1], year = met$year[1])
  last.day <- doy2date(met$day[nrow(met)], year = met$year[nrow(met)])
  dates <- seq(from = origin, to = last.day, by = "day")
  
  if(nrow(met) == length(dates) && !is_leap_year(met$year[nrow(met)])){
    stop("No discontinuities found", call. = FALSE)
  }
  
  fix1 <- FALSE
  
  if(nrow(met) != length(dates)){
    ## Create a data.frame with continuous dates

    namet <- data.frame(date = dates)
    
    ## Create date column for merging
    met <- as.data.frame(met)
    met$date <- as.Date(paste0(met$year, "-", met$day), format = "%Y-%j")

    ans <- merge(namet, met, by = "date", all.x = TRUE)
    ans$year <- as.numeric(format(ans$date, "%Y"))
    ans$day <- as.numeric(format(ans$date, "%j"))
    ans$date <- NULL  
    fix1 <- TRUE
    ans <- as_apsim_met(ans, 
                        filename = attr(met, "filename"),
                        site = attr(met, "site"),
                        latitude = attr(met, "latitude"),
                        tav = attr(met, "tav"),
                        amp = attr(met, "amp"),
                        colnames = attr(met, "colnames"),
                        units = attr(met, "units"),
                        comments = attr(met, "comments"),
                        check = FALSE)
  }  
  
  fix2 <- FALSE
  ## Is the last year a leap year?
  if(is_leap_year(met$year[nrow(met)]) && met$day[nrow(met)] == 365){
      ## This adds a row at the end when it is a leap year and it only has 365 days
    if(fix1) met <- ans
    nms.met <- names(met)
    fill.dat <- as.data.frame(matrix(ncol = length(nms.met)))
    names(fill.dat) <- nms.met
    fill.row <- data.frame(year = met$year[nrow(met)],
                           day = 366,
                           fill.dat[, -c(1:2)])
    ans <- rbind(met, fill.row)
    fix2 <- TRUE
  }
  
  if(!fix1 && !fix2)
    stop("No discontinuities found", call. = FALSE)
  
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

is_leap_year <- function(year){
  if((year %% 4) == 0){
    if((year %% 100) == 0){
      if((year %% 400) == 0){
        ans <- TRUE
      }else{
        ans <- FALSE
      }
    }else{
      ans <- TRUE
    }
  }else{
    ans <- FALSE
  }
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
    stop("Object should be of class data.frame", call. = FALSE)

  if(ncol(x) != 6 && length(colnames) == 6)
    stop("If number of columns is not 6 then provide column names", call. = FALSE)
  
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
  
#' Calculating Thermal Time using a variety of methods.
#' The function will fail if the method is not selected.
#' Also, it does not work if each year does not have at least
#' 365 days.
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

  if(identical(method, c("Classic_TT", "HeatStress_TT", "CropHeatUnit_TT", "APSIM_TT", "CERES_TT", "all")))
    stop("Please select a method. Not all of them are implemented at the moment.", call. = FALSE)
  
  method <- match.arg(method, several.ok = TRUE)
  
  if("all" %in% method) method <- c("Classic_TT", "HeatStress_TT", "CropHeatUnit_TT", "APSIM_TT")
  
  if("CERES_TT" %in% method) stop("not implemented yet.")
  
  if(is.na(as.Date(dates[1], format = dates.format))) stop("first date might be in the wrong format")
  if(is.na(as.Date(dates[2], format = dates.format))) stop("second date might be in the wrong format")
  
  start.date <- as.Date(paste0("01-01", "-", min(met$year)), format = paste0(dates.format, "-%Y"))
  end.date <- as.Date(paste0("31-12", "-", max(met$year)), format = paste0(dates.format, "-%Y"))
  
  tmpd <- data.frame(Dates = seq(from = start.date, 
                                 to = end.date, 
                                 by = "day"), index = 0)
  
  if("Classic_TT" %in% method) met <- add_column_apsim_met(met, 0, "Classic_TT", units = "(Cd)")
  if("HeatStress_TT" %in% method) met <- add_column_apsim_met(met, 0, "HeatStress_TT", units = "(Cd)")
  if("CropHeatUnit_TT" %in% method) met <- add_column_apsim_met(met, 0, "CropHeatUnit_TT", units = "(Cd)")
  # if("APSIM_TT" %in% method) met$APSIM_TT <- 0
  if("APSIM_TT" %in% method) stop("Not implemented yet.", call. = FALSE)
  
  if(nrow(met) != nrow(tmpd))
    warning("Days for each year should be complete")
  
  if(any(table(met$year) < 365))
    stop("Each year should have at least 365 days", call. = FALSE)
  
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
  cum.apsim.tt <- 0
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
      if("APSIM_TT" %in% method){
        apsim.tt <- apsim_tt(met$maxt[i], met$mint[i], Tb = base_temp, To = max_temp, cardinal.temps = x_temp, gdd.coord = y_tt)
        cum.apsim.tt <- cum.apsim.tt + apsim.tt
        met$APSIM_TT[i] <- cum.apsim.tt
      }
    }else{
      cum.classic.tt <- 0
      cum.heatstress.tt <- 0
      cum.cropheatunit.tt <- 0
      cum.apsim.tt <- 0
    }
  }

  met <- add_column_apsim_met(met, tmpd$Dates, "Dates", units = "()")
  return(met)
}
  
## APSIM calculation of thermal time
########################################################################################################
########################################################################################################
## Functions to convert temperature to gdd following APSIM's XY interpolation method. 
## There are three options available in this script. Users can modify or add new functions. 
## We used the temp2gdd function in the paper (APSIM-soybean version 7.5).
## We provide two more options as examples (temp3gdd and temp4dd function)
## See supplementary materials, figure S1  for the shape of each function

## function 1 or method 1 (temp2gdd)
temp2gdd <- function(temp, cardinal.temps = c(10,30,40), gdd.coord=c(0,20,0)){
  
  gdd <- numeric(length(temp))
  
  for(i in 1:length(temp)){
    if(temp[i] <= cardinal.temps[2]){
      slp <- c(gdd.coord[2]-gdd.coord[1])/c(cardinal.temps[2]-cardinal.temps[1])    # slope = (20-0)/(30-10)=1
      int <- gdd.coord[1]                                                           # int   = 0
      gdd[i] <- int + slp * (temp[i] - cardinal.temps[1])    
    }else{
      slp <- c(gdd.coord[3]-gdd.coord[2])/c(cardinal.temps[3]-cardinal.temps[2])    # slope = (0-20)/(40-30)=-2
      int <- gdd.coord[2]                                                           # int   = 20
      gdd[i] <- int + slp * (temp[i] - cardinal.temps[2])
    }
  }
  
  gdd <- pmax(gdd,0)
  return(gdd)
}


## function 2 or method 2 (temp3gdd)
temp3gdd <- function(temp, cardinal.temps = c(0,15,30,40), gdd.coord=c(0,5,20,0)){
  
  gdd <- numeric(length(temp))
  
  for(i in 1:length(temp)){
    if(temp[i] <= cardinal.temps[2]){                                                 # temp < 15
      slp <- c(gdd.coord[2]-gdd.coord[1])/c(cardinal.temps[2]-cardinal.temps[1])      # slope = (5-0)/(15-0)=0.333
      int <- gdd.coord[1]                                                             # int = gdd.coord[1] = 0
      gdd[i] <- int + slp * (temp[i] - cardinal.temps[1])                             # if temp = 15, gdd=5
    }
    if(temp[i] > cardinal.temps[2] && temp[i] <= cardinal.temps[3]){                  # temp < 30
      slp <- c(gdd.coord[3]-gdd.coord[2])/c(cardinal.temps[3]-cardinal.temps[2])      # slope = (20-5)/(30-15)=1
      int <- gdd.coord[2]                                                             # int = gdd.coord[2] = 5
      gdd[i] <- int + slp * (temp[i] - cardinal.temps[2])                             # if temp = 30, gdd=20
    }
    if(temp[i] > cardinal.temps[3] && temp[i] <= cardinal.temps[4]){                  # temp < 40
      slp <- c(gdd.coord[4]-gdd.coord[3])/c(cardinal.temps[4]-cardinal.temps[3])      # slope = (0-20)/(40-30)=-2
      int <-  gdd.coord[3]                                                            # int = gd.coord[3]=20
      gdd[i] <- int + slp * (temp[i] - cardinal.temps[3])                             # if temp = 40, gdd=0
      #}else{
      # gdd[i] <- 0
    }
  }
  gdd <- pmax(gdd,0)
  return(gdd)
}


## function 3 or method 3 (temp4gdd)
temp4gdd <- function(temp, cardinal.temps = c(7,28,35,45), gdd.coord=c(0,21,21,0)){
  
  gdd <- numeric(length(temp))
  
  for(i in 1:length(temp)){
    if(temp[i] <= cardinal.temps[2]){                                                 # temp < 28
      slp <- c(gdd.coord[2]-gdd.coord[1])/c(cardinal.temps[2]-cardinal.temps[1])      # slope = (21-0)/(28-7)=1
      int <- gdd.coord[1]                                                             # int = gdd.coord[1] = 0
      gdd[i] <- int + slp * (temp[i] - cardinal.temps[1])                             # if temp = 28, gdd=21
    }
    if(temp[i] > cardinal.temps[2] && temp[i] <= cardinal.temps[3]){                  # temp < 35
      slp <- c(gdd.coord[3]-gdd.coord[2])/c(cardinal.temps[3]-cardinal.temps[2])      # slope = (21-21)/(35-28)=0
      int <- gdd.coord[2]                                                             # int = gdd.coord[2] = 21
      gdd[i] <- int + slp * (temp[i] - cardinal.temps[2])                             # if temp = 35, gdd=21
    }
    if(temp[i] > cardinal.temps[3] && temp[i] <= cardinal.temps[4]){                  # temp < 45
      slp <- c(gdd.coord[4]-gdd.coord[3])/c(cardinal.temps[4]-cardinal.temps[3])      # slope = (0-21)/(45-35)=-2.1
      int <-  gdd.coord[3]                                                            # int = gd.coord[3]=21
      gdd[i] <- int + slp * (temp[i] - cardinal.temps[3])                             # if temp = 45, gdd=0
    }
  }
  gdd <- pmax(gdd,0)
  return(gdd)
}


########################################################################################################
########################################################################################################
## This function calculates daily gdd using the 3-hour interval approach from APSIM
## Min and max daily temperature, Tb, To, and interpolation method are the inputs (see above for options).
## We used the "temp2gdd" interpolation method and APSIM's default Tb and To values (Table 1).   

apsim_tt <- function(maxt, mint, Tb=10, To=30, cardinal.temps, gdd.coord){
  
  h2c <- function(hr){
    ans <- 0.92105 + 0.114 * hr - 0.0703 * (hr^2) + 0.0053*(hr^3)   # apsim equation  
    ans
  } 
  
  method <- "temp2gdd"
  if(missing(cardinal.temps) && missing(gdd.coord)){
    method <- "temp2gdd"
  }else{
    if(length(cardinal.temps) == 3) method <- "temp2gdd"
    if(length(cardinal.temps) == 4) method <- "temp3gdd"
    if(length(cardinal.temps) == 5) 
      stop("Method not implemented yet", call. = FALSE)
  } 

  if(method == "temp2gdd"){
    if(mint < Tb || maxt > To){
      temp1 <- mint + h2c(1)*(maxt - mint)
      temp2 <- mint + h2c(2)*(maxt - mint)
      temp3 <- mint + h2c(3)*(maxt - mint)
      temp4 <- mint + h2c(4)*(maxt - mint)
      temp5 <- mint + h2c(5)*(maxt - mint)
      temp6 <- mint + h2c(6)*(maxt - mint)
      temp7 <- mint + h2c(7)*(maxt - mint)
      temp8 <- mint + h2c(8)*(maxt - mint)
      
      if(missing(cardinal.temps) && missing(gdd.coord)){
        gdd <- mean(temp2gdd(c(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8)))    
      }else{
        if(length(cardinal.temps) != 3 || length(gdd.coord) != 3)
          stop("Length of cardinal.temps and/or gdd.coord is not equal to 3", call. = FALSE)
        gdd <- mean(temp2gdd(c(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8),
                             cardinal.temps = cardinal.temps, gdd.coord = gdd.coord))    
      }
      
    }else{
      gdd <- temp2gdd((maxt + mint)*0.5)                                                 
    } 
  }
  
  if(method == "temp3gdd"){
    if(mint < Tb || maxt > To){
      temp1 <- mint + h2c(1)*(maxt - mint)
      temp2 <- mint + h2c(2)*(maxt - mint)
      temp3 <- mint + h2c(3)*(maxt - mint)
      temp4 <- mint + h2c(4)*(maxt - mint)
      temp5 <- mint + h2c(5)*(maxt - mint)
      temp6 <- mint + h2c(6)*(maxt - mint)
      temp7 <- mint + h2c(7)*(maxt - mint)
      temp8 <- mint + h2c(8)*(maxt - mint)
      
      if(missing(cardinal.temps) && missing(gdd.coord)){
        gdd <- mean(temp3gdd(c(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8)))   
      }else{
        if(length(cardinal.temps) != 4 || length(gdd.coord) != 4)
          stop("Length of cardinal.temps and/or gdd.coord is not equal to 4", call. = FALSE)
        gdd <- mean(temp3gdd(c(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8),
                             cardinal.temps = cardinal.temps, gdd.coord = gdd.coord))    
      }
    }else{
      gdd <- temp3gdd((maxt + mint)*0.5)                                                 
    }
  }
  
  return(gdd)
}

## Function to include the photoperiod in an APSIM met file
pp_apsim_met <- function(metfile, lat, sun_angle=0){
  
  if(!inherits(metfile, "met"))
    stop("Object 'metfile' should be of class 'met'", call. = FALSE)
  if(missing(lat)){
    lat0 <-  attr(metfile, "latitude") # read latitude from the Measured file
    lat <- suppressWarnings(as.numeric(strsplit(lat0, "=")[[1]][2]))
    if(is.na(lat)){
      lat00 <- strsplit(lat0, "=")[[1]][2]
      lat0 <- suppressWarnings(as.numeric(strsplit(lat00, " ")[[1]]))
      lat <- lat0[!is.na(lat0)]
    }    
  }
  day <-  metfile$day                  # read day from the Met file 
  
  aeqnox <- 79.25              
  pi     <- 3.14159265359
  dg2rdn <- (2.0 * pi) /360.0    
  decsol <- 23.45116 * dg2rdn  
  dy2rdn <- (2.0 * pi) /365.25   
  rdn2hr <- 24.0/(2.0 * pi)      
  
  sun_alt <- sun_angle * dg2rdn
  dec     <- decsol * sin (dy2rdn * (day - aeqnox))
  latrn   <- lat * dg2rdn
  slsd    <- sin(latrn) * sin(dec)
  clcd    <- cos(latrn) * cos(dec)
  altmn   <- asin(pmin(pmax(slsd - clcd, -1.0), 1.0))
  altmx   <- asin(pmin(pmax(slsd + clcd, -1.0), 1.0))
  alt     <- pmin(pmax(sun_alt, altmn), altmx)
  
  coshra1 <- (sin (alt) - slsd) /clcd
  coshra  <- pmin(pmax(coshra1, -1.0), 1.0)
  hrangl  <- acos (coshra)
  PP      <- hrangl * rdn2hr * 2.0          
  ##metfile$photoperiod <- PP
  metfile <- add_column_apsim_met(metfile, PP, "photoperiod", units = "(hour)")
  return(metfile)
}


#' @title Plot method for object of class \sQuote{met} 
#' @name plot.met
#' @description Some plots are similar to APSIM, others are different
#' and more useful in some respects
#' @param x object of class \sQuote{met}
#' @param ... additional arguments. None used at the moment.
#' @param years optional argument to subset years
#' @param met.var optional argument to choose a certain variable. By default,
#' temperature (min and max) is displayed
#' @param plot.type type of plot, default is \sQuote{ts} or time-series. 
#' The options \sQuote{area} and \sQuote{col} are only available when summary = TRUE.
#' @param cumulative default is FALSE. Especially useful for \sQuote{rain}.
#' @param facet whether to display the years in in different panels (facets). Not implemented yet.
#' @param climatology logical (default FALSE). Whether to display the \sQuote{climatology}
#' which would be the average of the data. 
#' Ideally, there are at least 20 years in the \sQuote{met} object.
#' @param summary whether to plot \sQuote{summary} data. (default FALSE).
#' @export
#' @examples 
#' \donttest{
#' ## Read in and plot a met file
#' extd.dir <- system.file("extdata", package = "apsimx")
#' ames <- read_apsim_met("Ames.met", src.dir = extd.dir)
#' plot(ames, years = 2012:2015)
#' ## Perhaps more informative
#' plot(ames, years = 2012:2015, cumulative = TRUE)
#' ## for rain
#' plot(ames, met.var = "rain", years = 2012:2015, cumulative = TRUE)
#' plot(ames, met.var = "rain", years = 2012:2015, cumulative = TRUE, climatology = TRUE)
#' ## It is possible to add ggplot elements
#' library(ggplot2)
#' p1 <- plot(ames, met.var = "rain", years = 2012:2015, cumulative = TRUE)
#' p1 + ggtitle("Cumulative rain for 2012-2015")
#' }
#' 
plot.met <- function(x, ..., years, met.var, 
                     plot.type = c("ts", "area", "col"), 
                     cumulative = FALSE,
                     facet = FALSE,
                     climatology = FALSE,
                     summary = FALSE){
  
  if(!requireNamespace("ggplot2", quietly = TRUE)){
    warning("ggplot2 is required for this plotting function")
    return(NULL)
  }
  
  plot.type <- match.arg(plot.type)
  
  if(plot.type %in% c("area", "col") && !summary)
    stop("plot.type = area and plot.type = col are only available when summary = TRUE", call. = FALSE)
  
  ## Global variables?
  day <- NULL; cum.maxt <- NULL; Years <- NULL; cum.mint <- NULL;
  value <- NULL; temperature <- NULL; maxt <- NULL; mint <- NULL;
  cum.met.var <- NULL; year <- NULL; 
  ## Calculate climatology before subsetting years
  if(climatology){
    maxt.climatology <- stats::aggregate(maxt ~ day, data = x, FUN = mean)
    mint.climatology <- stats::aggregate(mint ~ day, data = x, FUN = mean)
    rain.climatology <- stats::aggregate(rain ~ day, data = x, FUN = mean)
    radn.climatology <- stats::aggregate(radn ~ day, data = x, FUN = mean)
    met.var.climatology <- cbind(maxt.climatology, 
                                 mint.climatology[,"mint", drop = FALSE], 
                                 rain.climatology[,"rain", drop = FALSE], 
                                 radn.climatology[,"radn", drop = FALSE])
    if(any(grepl("vp", names(x), fixed = TRUE))){
      vp.climatology <- stats::aggregate(vp ~ day, data = x, FUN = mean)
      met.var.climatology <- cbind(met.var.climatology, vp.climatology[, "vp", drop = FALSE])
    }
    if(any(grepl("windspeed", names(x), fixed = TRUE))){
      windspeed.climatology <- stats::aggregate(windspeed ~ day, data = x, FUN = mean)
      met.var.climatology <- cbind(met.var.climatology, windspeed.climatology[, "windspeed", drop = FALSE])
    }
  }

  if(!missing(years)){
    if(max(years) > max(x$year) || min(years) < min(x$year))
      stop("Selected year is not within the range of the data", call. = FALSE)    
  }

  if(!missing(years)){
    x <- x[x$year %in% years,]
  }
  
  x <- add_column_apsim_met(x, value = as.factor(x$year), name = "Years", units = "()")
  
  if(isFALSE(summary)){
    if(!missing(met.var)){
      if(!met.var %in% names(x)){
        cat("Variables in met:", names(x), "\n")
        stop("met.var was not found in the names of the 'met' object", call. = FALSE)
      }
      if(met.var %in% c("day", "year"))
        stop("year or day cannot be used as a 'met.var'", call. = FALSE)
    }    
  }

  if(isFALSE(summary)){
    if(plot.type == "ts"){
      if(missing(met.var)){
        if(cumulative){
          dat <- NULL
          for(i in seq_along(sort(unique(x$year)))){
            yr <- sort(unique(x$year))[i]
            x.tmp <- x[x$year == yr,]
            x.ag.maxt <- cumsum(x.tmp$maxt)
            x.ag.mint <- cumsum(x.tmp$mint)
            dat <- rbind(dat, data.frame(year = yr, day = 1:nrow(x.tmp), cum.maxt = x.ag.maxt, cum.mint = x.ag.mint))
          }
          
          dat$Years <- as.factor(dat$year)
          
          if(climatology){
            maxt.climatology$cum.maxt <- cumsum(maxt.climatology$maxt)
            maxt.climatology$climatology <- "climatology"
            mint.climatology$cum.mint <- cumsum(mint.climatology$mint)
            mint.climatology$climatology <- "climatology"
            gp1 <- ggplot2::ggplot() + 
                            ggplot2::geom_line(data = dat, ggplot2::aes(day, cum.maxt, color = Years)) +
                            ggplot2::geom_line(data = dat, ggplot2::aes(day, cum.mint, color = Years), linetype = 2) +
                            ggplot2::geom_line(data = maxt.climatology, ggplot2::aes(day, cum.maxt, linetype = climatology), color = "black") + 
                            ggplot2::geom_line(data = mint.climatology, ggplot2::aes(day, cum.mint, linetype = climatology), color = "black", linetype = 2) +
                            ggplot2::scale_linetype_manual(name = NULL, values = 1) + 
                            ggplot2::geom_hline(yintercept = 0, linetype = 3) + 
                            ggplot2::ylab("Cumulative temperature (degree C)") 
          }else{
            dat1 <- data.frame(day = dat$day, temperature = "maxt", value = dat$cum.maxt, Years = dat$Years)
            dat2 <- data.frame(day = dat$day, temperature = "mint", value = dat$cum.mint, Years = dat$Years)
            dat <- rbind(dat1, dat2)
            gp1 <- ggplot2::ggplot(data = dat) + 
              ggplot2::geom_line(ggplot2::aes(day, value, color = Years, linetype = temperature)) +
              ggplot2::geom_line(ggplot2::aes(day, value, color = Years, linetype = temperature)) +
              ggplot2::scale_linetype_manual(name = NULL, values = c(1, 2)) + 
              ggplot2::geom_hline(yintercept = 0, linetype = 3) + 
              ggplot2::ylab("Cumulative temperature (degree C)") 
          }
          print(gp1)        
        }else{
          if(climatology){
            maxt.climatology$climatology <- "climatology"
            mint.climatology$climatology <- "climatology"
            x <- add_column_apsim_met(x, value = as.factor(x$year), name = "Years", units = "()")
            x <- as.data.frame(x)
            gp1 <- ggplot2::ggplot() + 
                            ggplot2::geom_line(data = x, ggplot2::aes(day, maxt, color = Years)) +
                            ggplot2::geom_line(data = x, ggplot2::aes(day, mint, color = Years), linetype = 2) +
                            ggplot2::geom_line(data = maxt.climatology, ggplot2::aes(day, maxt, linetype = climatology), color = "black") +
                            ggplot2::geom_line(data = mint.climatology, ggplot2::aes(day, mint, linetype = climatology), color = "black", linetype = 2) +
                            ggplot2::scale_linetype_manual(name = NULL, values = 1) + 
                            ggplot2::geom_hline(yintercept = 0, linetype = 3) + 
                            ggplot2::ylab("Temperature (degree C)")                        
          }else{
            x <- as.data.frame(x)
            gp1 <- ggplot2::ggplot(data = x) + 
              ggplot2::geom_line(ggplot2::aes(day, maxt, color = Years, linetype = "maxt")) +
              ggplot2::geom_line(ggplot2::aes(day, mint, color = Years, linetype = "mint")) +
              ggplot2::geom_hline(yintercept = 0, linetype = 3) + 
              ggplot2::scale_linetype_manual(name = NULL, values = c(1, 2)) + 
              ggplot2::ylab("Temperature (degree C)")            
          }
          print(gp1)        
        }
      }else{
        if(met.var %in% c("rain", "radn", "vp", "rh", "maxt", "mint", "windspeed", 
                          "Classic_TT", "HeatStress_TT", "CropHeatUnit_TT", "photoperiod")){
          
          met.var.units <- switch(met.var,
                                  rain = "(mm)",
                                  radn = "(MJ/m2)",
                                  rh = "(%)",
                                  maxt = "(degrees C)",
                                  mint = "(degrees C)",
                                  vp = "(hPa)",
                                  windspeed = "(m/s)",
                                  Classic_TT = "(Cd)",
                                  HeatStress_TT = "(Cd)",
                                  CropHeatUnit_TT = "(Cd)",
                                  photoperiod = "(hours)")

          if(climatology){
            met.var.clima <- met.var.climatology[ ,c("day", met.var)]
            met.var.clima$climatology <- "climatology"            
          }

          if(cumulative){
            ylabel <- paste("Cumulative ", met.var, met.var.units)
            dat <- NULL
            for(i in seq_along(sort(unique(x$year)))){
              yr <- sort(unique(x$year))[i]
              x.tmp <- x[x$year == yr,]
              x.ag.met.var <- cumsum(x.tmp[[met.var]])
              dat <- rbind(dat, data.frame(year = yr, day = 1:nrow(x.tmp), cum.met.var = x.ag.met.var))
            }
            
            dat$Years <- as.factor(dat$year)
            
            if(climatology){
              met.var.clima$cum.met.var <- cumsum(met.var.clima[[met.var]])
              
              gp1 <- ggplot2::ggplot() + 
                ggplot2::geom_line(data = dat, ggplot2::aes(day, cum.met.var, color = Years)) +
                ggplot2::geom_line(data = met.var.clima, ggplot2::aes(day, cum.met.var, linetype = climatology), color = "black") +
                ggplot2::scale_linetype_manual(name = NULL, values = 1) + 
                ggplot2::ylab(ylabel)                            
            }else{
              gp1 <- ggplot2::ggplot(data = dat) + 
                ggplot2::geom_line(ggplot2::aes(day, cum.met.var, color = Years)) +
                ggplot2::ylab(ylabel)              
            }
            print(gp1)        
          }else{
            ylabel <- paste(met.var, met.var.units)
            if(climatology){
              x <- as.data.frame(x)
              gp1 <- ggplot2::ggplot() + 
                ggplot2::geom_point(data = x, ggplot2::aes(day, 
                                                 y = eval(parse(text = eval(met.var))),
                                                 color = Years)) +
                ggplot2::geom_line(data = met.var.clima, ggplot2::aes(day, 
                                                 y = eval(parse(text = eval(met.var))),
                                                 linetype = climatology)) +
                ggplot2::scale_linetype_manual(name = NULL, values = 1) + 
                ggplot2::ylab(ylabel)                  
            }else{
              x <- as.data.frame(x)
              gp1 <- ggplot2::ggplot(data = x) + 
                              ggplot2::geom_point(ggplot2::aes(day, 
                                                 y = eval(parse(text = eval(met.var))),
                                                 color = Years)) +
                              ggplot2::ylab(ylabel)              
            }
            print(gp1)        
          }
        }
      }
    }    
  }else{
    
    if(missing(met.var)){
      met.var <- c("avg_maxt", "avg_mint")
    }
    
    if(is.null(list(...)$compute.frost)){
      valid.met.vars <- c("high_maxt", "high_mint", "avg_maxt", "avg_mint", "low_maxt", "low_mint", "rain_sum", "radn_sum", "radn_avg") 
    }else{
      valid.met.vars <- c("high_maxt", "high_mint", "avg_maxt", "avg_mint", "low_maxt", "low_mint", "rain_sum", "radn_sum", "radn_avg", "first_half_frost","second_half_frost","frost_free_period","frost_days") 
    }
    sel.met.var <- sapply(met.var, function(x) grep(x, valid.met.vars)) 
    if(length(sel.met.var) == 0 || is.list(sel.met.var)){
      cat("Valid met.vars:", valid.met.vars, "\n")
      stop("'met.var' is not a valid meteorological variable", call. = FALSE)
    }else{
      met.var <- valid.met.vars[sel.met.var] 
    }
    
    if(any(met.var %in% c("high_maxt", "high_mint", "avg_maxt", "avg_mint", "low_maxt", "low_mint")))
      ylabs <- "Temperature (degree C)"
    
    if(any(grepl("rain_sum", met.var)) && length(met.var) > 1)
      stop("If 'rain_sum' is chosen, it should be the only met variable", call. = FALSE)
    
    if(any(met.var %in% c("rain_sum")))
      ylabs <- "Rainfall (mm)"
    
    if(any(grepl("radn", met.var)) && length(met.var) > 2)
      stop("If 'radn' is chosen, it should not be mixed with other variables", call. = FALSE)
    
    if(any(met.var %in% c("radn_sum", "radn_avg")))
      ylabs <- "Radiation (MJ/m2)"
    
    if(any(grepl("frost", met.var)))
      ylabs <- "Days"
      
    stmp <- summary(x, ...)
    tmp <- NULL
    for(i in seq_along(met.var)){
      dat <- data.frame(year = stmp$year, met.var = met.var[i], value = stmp[[met.var[i]]])
      tmp <- rbind(tmp, dat)
    }
    
    if(plot.type == "ts" && !climatology){
      gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = year, y = value, color = met.var)) + 
        ggplot2::geom_point() + 
        ggplot2::geom_line() + 
        ggplot2::ylab(ylabs)
      print(gp1)      
    }
    
    if(plot.type == "ts" && climatology){
      y.ints <- aggregate(value ~ met.var, data = tmp, FUN = mean, na.rm = TRUE)$value
      gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = year, y = value, color = met.var)) + 
        ggplot2::geom_point() + 
        ggplot2::geom_line() +
        ggplot2::geom_hline(yintercept = y.ints, linetype = 2) + 
        ggplot2::ylab(ylabs)
      print(gp1)      
    }
    
    if(plot.type == "area"){
      gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = year, y = value, fill = met.var)) + 
        ggplot2::geom_area() + 
        ggplot2::ylab(ylabs)
      print(gp1)      
    }
    
    if(plot.type == "col"){
      gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = year, y = value, fill = met.var)) + 
        ggplot2::geom_col() + 
        ggplot2::ylab(ylabs)
      print(gp1)      
    }
  }
  invisible(gp1)
}

#' 
#' @title Add a column to an object of class \sQuote{met}
#' @rdname add_column_apsim_met
#' @description The usual way of adding a column to a data frame might
#' not work for an object of class \sQuote{met}, so this method is recommended
#' @param met object of class \sQuote{met}
#' @param value vector of the appropriate length
#' @param name optional name if the vector value is unnamed
#' @param units units for the new column (required)
#' @return an object of class \sQuote{met} with the additional column
#' @export
#' @examples
#' \donttest{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' ames <- read_apsim_met("Ames.met", src.dir = extd.dir)
#' 
#' ## The recommended method is
#' val <- abs(rnorm(nrow(ames), 10))
#' ames <- add_column_apsim_met(ames, value = val, name = "vp", units = "(hPa)")
#' 
#' ## This is also possible
#' vp <- data.frame(vp = abs(rnorm(nrow(ames), 10)))
#' attr(vp, "units") <- "(hPa)"
#' ames$vp <- vp
#' 
#' ## This is needed to ensure that units and related attributes are also removed
#' ames <- remove_column_apsim_met(ames, "vp")
#' ## However, ames$vp <- NULL will also work
#' }
#' 
#' 
add_column_apsim_met <- function(met, value, name, units){
  
  if(!inherits(met, "met"))
    stop("Object should be of class met.", call. = FALSE)
  
  if(missing(name)){
    name <- names(value)
    if(is.null(name))
      stop("If 'name' is not provided, 'value' should be a named vector", call. = FALSE)
  }
  
  if(missing(units))
    stop("argument 'units' is required for this function", call. = FALSE)
  
  units <- as.character(units)
  
  ## This invokes '$<-.data.frame' or not?
  met[[name]] <- value
  
  attr(met, "colnames") <- colnames(met)
  tmp.units <- attr(met, "units")
  if(length(tmp.units) < length(colnames(met))){
    attr(met, "units") <- c(tmp.units, units)    
  }
  return(met)
}

#' @rdname add_column_apsim_met
#' @param x object of class \sQuote{met}
#' @param name name of the variable to be added or modified
#' @param value value for the data.frame. It could be an integer, double or vector of length equal to the number of rows in x.
#' @export
`$<-.met` <- function(x, name, value){
  
  if(is.null(value) && !(name %in% names(x)))
    stop("Trying to remove a column which is not present in object of class 'met'", call. = FALSE)
  
  if(is.null(value) && name %in% names(x)){
    ## The thing here is that I also need to remove units and column names
    x[[name]] <- value
    attr(x, "colnames") <- attr(x, "colnames")[-which(names(x) == name)]
    attr(x, "units") <- attr(x, "units")[-which(names(x) == name)]
    return(x)
  }
  
  if(name %in% names(x)){
    x[[name]] <- value
    return(x)
  }
  
  if(is.null(attr(value, "units"))){
    stop("It is recommended to use function add_column_apsim_met for this operation instead.
         Partly because units are needed", call. = FALSE)    
  }else{
    if(!is.null(names(value))){
      return(add_column_apsim_met(x, value = value, units = attr(value, "units")))  
    }else{
      stop("It is recommended to use function add_column_apsim_met for this operation instead.
         Partly because units are needed", call. = FALSE)    
    }
  }
}

#' @rdname add_column_apsim_met
#' @name remove_column_apsim_met
#' @param met object of class \sQuote{met}
#' @param name name of the variable to be removed
#' @return an object of class \sQuote{met} without the variable (column) in \sQuote{name}
#' @export
remove_column_apsim_met <- function(met, name){
  
  if(!inherits(met, "met"))
    stop("Object should be of class met.", call. = FALSE)
  
  if(missing(name)){
      stop("'name' is not provided. It should be the name of the column to remove", call. = FALSE)
  }
  
  attr(met, "units") <- attr(met, "units")[-which(names(met) == name)]   
  met[[name]] <- NULL
  attr(met, "colnames") <- names(met)
  return(met)
}