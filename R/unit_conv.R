#' Function which performs common unit conversions
#' 
#' At the moment possible conversions are:
#' \itemize{
#' 
#' \item \sQuote{g/m2} to \sQuote{kg/ha}
#' \item \sQuote{kg/ha} to \sQuote{g/m2}
#' 
#' }
#' 
#'
#' @title performs common unit conversions
#' @name unit_conv
#' @param x input variable
#' @param from original units 
#' @param to target units
#' @return value of the input variable with new units
#' @export
#' @examples 
#' \dontrun{
#' grain.yield.gm2 <- 600
#' grain.yield.kgha <- unit_conv(grain.yield.gm2, from = "g/m2", to = "kg/ha")
#' grain.yield.kgha
#' }
#' 

unit_conv <- function(x, from, to){
  
  from.table <- c("g/m2","Mg/ha","kg/m2","kg/ha","lb/ac")
  to.table <- c("g/m2","Mg/ha","kg/m2","kg/ha","lb/ac")
  
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
  
  return(ans)
}
