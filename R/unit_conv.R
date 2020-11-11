#' Function which performs common unit conversions
#' 
#' At the moment possible conversions are:
#' \itemize{
#' 
#' \item \sQuote{g/m2} to \sQuote{kg/ha}
#' \item \sQuote{kg/ha} to \sQuote{g/m2}
#' \item \sQuote{lb} to \sQuote{kg}
#' \item \sQuote{kg} to \sQuote{lb}
#' \item \sQuote{maize bu} to \sQuote{kg}
#' \item \sQuote{kg} to \sQuote{maize bu}
#' \item \sQuote{soy bu} to \sQuote{kg}
#' \item \sQuote{kg} to \sQuote{soy bu}
#' \item \sQuote{maize bu/ac} to \sQuote{kg/ha}
#' \item \sQuote{kg/ha} to \sQuote{maize bu/ac}
#' \item \sQuote{soy bu/ac} to \sQuote{kg/ha}
#' \item \sQuote{kg/ha} to \sQuote{soy bu/ac}
#' 
#' }
#' 
#' This is for metric and Imperial conversions
#' Source: https://www.extension.iastate.edu/agdm/wholefarm/html/c6-80.html
#'
#' @title performs common unit conversions
#' @name unit_conv
#' @description It does not do much at the moment, but I will develop it as I have 
#' increasing needs for unit conversions.
#' @param x input variable
#' @param from original units 
#' @param to target units
#' @return value of the input variable with new units
#' @export
#' @examples 
#' \donttest{
#' grain.yield.gm2 <- 600
#' grain.yield.kgha <- unit_conv(grain.yield.gm2, from = "g/m2", to = "kg/ha")
#' grain.yield.kgha
#' }
#' 

unit_conv <- function(x, from, to){
  
  from.table <- c("g/m2","Mg/ha","kg/m2","kg/ha","lb/ac","kg","lb",
                  "maize bu", "soy bu", "maize bu/ac")
  to.table <- c("g/m2","Mg/ha","kg/m2","kg/ha","lb/ac","kg","lb",
                "maize bu", "soy bu", "maize bu/ac")
  
  from <- match.arg(from, from.table)
  to <- match.arg(to, to.table)
  
  ## Create conversion matrix
  
  if(from == "g/m2" && to == "kg/ha"){
    ## From m2 to ha multiply by 1e4
    ## From g to kg divide by 1e3
    ans <- x * 10
  }
  
  if(from == "kg/ha" && to == "g/m2"){
    ## From ha to m2 divide by 1e4
    ## from kg to grams multiply by 1e3
    ans <- x * 0.1
  }
  
  if(from == "kg" && to == "lb"){
    ## 1 lb (pound) = 0.454 kg
    ans <- x * 2.205
  }
  
  if(from == "lb" && to == "kg"){
    ## 1 lb (pound) = 0.454 kg
    ans <- x / 2.205
  }
  
  if(from == "maize bu" && to == "kg"){
    ## 1 maize bushel = 25.4 kg
    ans <- x * 25.4
  }
  
  if(from == "kg" && to == "maize bu"){
    ## 1 maize bushel = 25.4 kg
    ans <- x / 25.4
  }
  
  if(from == "soy bu" && to == "kg"){
    ## 1 soy bushel = 27.22 kg
    ans <- x * 27.22
  }
  
  if(from == "kg" && to == "soy bu"){
    ## 1 soy bushel = 27.22 kg
    ans <- x / 27.22
  }
  
  if(from == "maize bu/ac" && to == "kg/ha"){
    ## 1 maize bushel/acre = 56 
    ans <- x * 62.77
  }
  
  if(from == "kg/ha" && to == "maize bu/ac"){
    ## 1 maize bushel/acre = 56 
    ans <- x / 62.77
  }
  
  if(from == "soy bu/ac" && to == "kg/ha"){
    ## 1 soy bushel/acre = 60 
    ans <- x * 67.25
  }
  
  if(from == "kg/ha" && to == "soy bu/ac"){
    ## 1 soy bushel/acre = 60 
    ans <- x / 67.25
  }
  
  return(ans)
}

#' 
#' @title Converts from doy to date
#' @name doy2date
#' @rdname doy2date
#' @description Given a day of the year as julian (1-366) it converts to \sQuote{Date}
#' @param x either an integer 1-366 or a \sQuote{Date}
#' @param year year
#' @param inverse if TRUE it goes from date to day
#' @export
#' @examples 
#' doy2date(120)

doy2date <- function(x, year = 2001, inverse = FALSE){
  
  if(class(x) == "character" && inverse==FALSE){
    stop("Did you intend to use it in inverse mode?")
  }
  if(inverse == FALSE){
    if(!is.numeric(x)){
      stop("x should be numeric")
    }else{
      if(any(x < 1)) stop("x should be greater than 1")
      if(any(x > 366)) stop("x should be less than 365")
    }
    
    day1 <- as.Date(paste0(year, "-01-01"))
    dayn <- as.Date(paste0(year, "-12-31"))
    dates <- seq(day1, dayn, by="day")
    doy <- dates[x]
  }else{
    if(class(x) != "character") stop("x should be of 'character' class")
    
    doy <- as.numeric(format(as.Date(paste0(year, "-", x)), "%j"))
  }
  doy
}

#' 
#' @rdname doy2date
#' @description Given a \sQuote{Date} it converts to julian day (1-366) or day of the year
#' @param x either an integer 1-366 or a \sQuote{Date}
#' @param year year
#' @param inverse if TRUE it goes from doy to \sQuote{Date}
#' @export
#' @examples 
#' date2doy("04-30")
#' 
date2doy <- function(x, year = 2001, inverse = FALSE){
  ans <- doy2date(x, year = year, inverse = !inverse)
  return(ans)
}