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
  
  .ans <- xml_text(.find.output)
  
  return(.ans)
}

#' Check the name and disallow white spaces 
#' These are not allowed as it would not work (easily)
#' at the command line
#' @name .check_apsim_met
#' @description Checks whether an APSIM file has spaces in it
#' @param .file an APSIM file
#' @noRd
#' 
.check_apsim_name <- function(.file){
  ## No spaces are allowed, provide informative error
  if(grepl("\\s", .file))
    stop("White spaces are not allowed in file names")
}

#' This is for when a list has only one element
#' But inside that element there is a children with multiple
#' elements, or at least is not null
#' @name .extract_bad_children
#' @description Possibly extract unnamed children more robustlly
#' @param x a list with (potentially) a \sQuote{Children} object
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

#' This function extracts the APSIM-X Date version
#' Find Apsim Version Date
#' @name .favd
#' @description Extract the date from an APSIM binary name being used
#' @param x an APISM binary name such as 'APSIM2020.06.05.5260.app'
#' @noRd
#' 
.favd <- function(x){
  x1 <- gsub("APSIM","\\1",x) ## Remove 'APSIM'
  x2 <- strsplit(x1, ".", fixed = TRUE)[[1]] ## Split by '.'
  x3 <- as.Date(paste(x2[1:3], collapse="-"), format = "%Y-%m-%d") ## Convert to 'Date'
  return(x3)
}