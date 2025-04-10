#' Auxiliary unexported and \sQuote{hidden} functions
#' Find output file names in an APSIM \sQuote{Classic} file
#' @name .find_ouput_names
#' @description In APSIM \sQuote{Classic} the outputfiles are named internally and this
#' function can retrieve this information
#' @param .file file name
#' @param .src.dir source directory
#'
#' @return output file names
#'
#' @noRd
.find_output_names <- function(.file, .src.dir = "."){

  .apsim_xml <- xml2::read_xml(paste0(.src.dir,"/",.file))

  .find.output <- xml2::xml_find_all(.apsim_xml, ".//outputfile/filename")

  .ans <- xml2::xml_text(.find.output)

  return(.ans)
}

#' Check the name and disallow white spaces
#' These are not allowed as it would not work (easily)
#' at the command line
#' @name .check_apsim_name
#' @description Checks whether an APSIM file has spaces in it
#' @param .file an APSIM file
#' @return It only returns an error if there is a space in the file
#' @noRd
#'
.check_apsim_name <- function(.file){
  ## No spaces are allowed, provide informative error
  if(missing(.file))
    stop("argument .file is missing", call. = FALSE)
  if(grepl("\\s", .file))
    stop("White spaces are not allowed in file names or directories", call. = FALSE)
}

#' This is for when a list has only one element
#' But inside that element there is a children with multiple
#' elements, or at least is not null
#' @name .extract_bad_children
#' @description Possibly extract unnamed children more robustlly
#' @param x a list with (potentially) a \sQuote{Children} object
#' @return it returns a subset of a json list
#' @noRd
#'
.extract_bad_children <- function(x){

  if(!is.list(x)) stop("x should be a list")

  lnms <- names(x)
  nco <- is.null(x$Children)

  if(length(x) == 1 && lnms == 0 && !nco){
    x <- x$Children[[1]]
  }
  return(x)
}

#' This function extracts the APSIM-X year, month version
#' This used to be able to extract the date, but this is not
#' part of the name anymore, so it is not available
#' @name .favd
#' @description Extract the year, month version from an APSIM binary name
#' @param x an APISM binary name such as 'APSIM2020.06.5260.app'
#' @return it returns a string for the APSIM year, month, version
#' @noRd
#'
.favd <- function(x){
  x1 <- gsub("APSIM","\\1", x) ## Remove 'APSIM'
  x2 <- strsplit(x1, ".", fixed = TRUE)[[1]] ## Split by '.'
  x3 <- paste(paste(x2[1:2], collapse="-"), " version:", as.numeric(x2[3]))
  return(x3)
}

#' This function extracts the APSIM-X Version number
#' Find Apsim Version number
#' @name .favn
#' @description Extract the number from an APSIM binary name being used
#' @param x an APISM binary name such as 'APSIM2020.06.5260.app'
#' @return it returns a number for the APSIM version
#' @noRd
#'
.favn <- function(x){
  x1 <- gsub("APSIM","\\1", x) ## Remove 'APSIM'
  x2 <- strsplit(x1, ".", fixed = TRUE)[[1]] ## Split by '.'
  # x3 <- as.Date(paste(x2[1:3], collapse="-"), format = "%Y-%m-%d") ## Convert to 'Date'
  x3 <- as.numeric(x2[3]) + as.numeric(x2[4])
  return(x3)
}

#' This function converts thickness to depth for soil profiles
#' Thickness is assumed to be in mm and the returned depths would be in
#' cm
#' @name .t2d
#' @description Takes a \sQuote{Thickness} column and returns \sQuote{Depth}
#' @param x a soil profile \sQuote{Thickness} column (numeric)
#' @return it returns a column with strings such as \sQuote{0-20}
#' @noRd
#'
.t2d <- function(x){
  x2 <- c(0, x)/10 ## Divide by 10 to go from mm to cm
  ans <- character(length(x))
  csx2 <- cumsum(x2)
  for(i in 2:length(x2)){
    ans[i-1] <- paste0(csx2[i-1], "-", csx2[i])
  }
  ans
}
