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
#' @description Read into R a met file and return an object of class 'met'
#' @param file path to met file
#' @param src.dir optional source directory
#' @param verbose whether to suppress all messages and warnings
#' @return an object of class 'met' with attributes
#' @export
#' @examples 
#' \dontrun{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' ames.met <- read_apsim_met("Ames.met", src.dir = extd.dir)
#' ames.met
#' }
#' 

read_apsim_met <- function(file, src.dir = ".", verbose = TRUE){

  if(!grepl("met$",file)) stop("file should have a .met extension")
  
  file.path <- paste0(src.dir,"/",file)

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
    
  met <- read.table(file = file.path, 
                    header = FALSE, 
                    as.is = TRUE,
                    na.strings = c(NA,-99),
                    comment.char = "!", 
                    col.names = clnms.s,
                    skip = skip.lines)
  
  ## Detect possible errors with years
  if(any(min(met[["year"]]) < 1500) == TRUE || any(max(met[["year"]]) > 3000) == TRUE){
    if(!quiet) warning("year is either less than 1500 or greater than 3000")
  }
  ## Detect possible errors with day
  if(any(min(met[["day"]]) < 1) == TRUE || any(max(met[["day"]])> 366) == TRUE){
    if(!quiet) warning("day is either less than 1 or greater than 366")
  }

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
#' @return does not create an R object, it only writes to disk
#' @details at the moment the read-write cycle will strip comments
#' @export
#' @examples 
#' \dontrun{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' ames.met <- read_apsim_met("Ames.met", src.dir = extd.dir)
#' ames.met
#' write_apsim_met(ames.met, wrt.dir = ".", filename = "Ames.met")
#' }
#' 
write_apsim_met <- function(met, wrt.dir=NULL, filename = NULL){
  
  if(missing(wrt.dir) & missing(filename)){
    ## This assumes that the full path is in filename
    file.path <- attr(met,"filename")
  }
  if(!missing(wrt.dir) & missing(filename)){
    stop("Need to supply filename if 'wrt.dir' is not NULL")
  }
  if(!missing(wrt.dir) & !missing(filename)){
    file.path <- paste0(wrt.dir,"/",filename)
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
  write.table(met, file = con, append = TRUE, quote = FALSE,
              row.names = FALSE, col.names = FALSE)
  close(con)
}

#' @title Printer-friendly version of a metfile
#' @name print.met
#' @description Print a met file in a friendly way
#' @param x an R object of class \sQuote{met}
#' @export
#' 
print.met <- function(x){
  
  ## print the header and just head
  cat(attr(x, "filename"),"\n")
  if(!is.na(attr(x,"site"))) cat(attr(x, "site"),"\n")
  cat(attr(x, "latitude"),"\n")
  if(!is.na(attr(x,"longitude"))) cat(attr(x, "longitude"),"\n")
  cat(attr(x, "tav"),"\n")
  cat(attr(x, "amp"),"\n")
  cat(attr(x, "colnames"),"\n")
  cat(attr(x, "units"),"\n")
  if(!is.na(attr(x,"constants")) && !is.null(attr(x,"constants"))) cat(attr(x, "constants"), "\n")
  
  print(head(as.data.frame(x)))
}

#' @title Perform imputation for missing data in a met file
#' @name impute_apsim_met
#' @description Takes in an object of class \sQuote{met} and imputes values
#' @param met object of class \sQuote{met}
#' @param method method for imputation, \code{\link{approx}}, \code{\link{splines}} or \code{\link{mean}}
#' @param verbose whether to print missing data to the console, default = FALSE
#' @return an object of class \sQuote{met} with attributes
#' @export
#' 

impute_apsim_met <- function(met, method = c("approx","splines","mean"), verbose = FALSE){
  
  if(class(met)[1] != "met") stop("met should be of class 'met'")
  
  method <- match.arg(method)
  
  ## Which rows have missing data
  missing.vector <- vector(mode = "numeric",length = length(names(met)))
  
  if(all(sapply(sapply(met, function(x) which(is.na(x))),length) == 0))
    warning("No missing data found")
  
  missing.rows <- sapply(met, function(x) which(is.na(x)), simplify = FALSE)

  if(verbose){
    for(i in 1:ncol(met)){
      tmp.mr <- missing.rows[[i]]
      if(length(tmp.mr) > 0){
        cat("Missing values for",names(met)[i],"\n")
        print(as.data.frame(met)[tmp.mr,])
      }
    }
  }
  
  col.classes <- sapply(met, class) 
  ## I might need to prevent imputation on characters/factors 
  which.col.missing.values <- which(sapply(missing.rows, function(x) length(x)) > 0)

  for(i in which.col.missing.values){
    if(method == "approx"){
      imputed.values <- approx(x=seq_len(nrow(met)),
                              y=met[[i]],
                              xout=missing.rows[[i]])$y
    }
    if(method == "splines"){
      imputed.values <- spline(x=seq_len(nrow(met)),
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