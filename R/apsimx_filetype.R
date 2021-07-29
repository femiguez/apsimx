
#' Test whether an .apsimx file is XML or json
#' 
#' @title Test file format for .apsimx files
#' @name apsimx_filetype
#' @param file file ending in .apsimx to be tested
#' @param src.dir directory containing the .apsimx file to be tested; defaults to the current working directory
#' @return \sQuote{xml}, \sQuote{json} or \sQuote{unknown}
#' @note Minimal function which reads only the first line in a file and tries to guess whether it is
#'       an \sQuote{xml} or \sQuote{json} file type.
#' @export
#' @examples 
#' \donttest{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' apsimx_filetype("Wheat.apsimx", src.dir = extd.dir) 
#' }
#' 

apsimx_filetype <- function(file = "", src.dir = "."){
  
  
  file.names <- dir(path = src.dir, pattern=".apsimx$", ignore.case=TRUE)
  file.names.c <- dir(path = src.dir, pattern=".apsim$", ignore.case=TRUE)
  ## print(file.names.c)
  # file.names <- c(file.names.x, file.names.c)
  
  if(length(file.names)==0){
    stop("There are no .apsimx files in the specified directory to test.")
  }
  
  file <- match.arg(file, file.names, several.ok=FALSE)
  
  rfl1 <- utils::read.table(file = file.path(src.dir, file), nrows = 1)[1,1]
  ## The next line will work if all XML files contain xml in the first line
  if(length(grep("xml", as.character(rfl1))) != 0){
    ans <- "xml"
  }else{
    if(length(grep("{", as.character(rfl1), fixed = TRUE)) == 0){
      ans <- "unkown"
    }else{
      ans <- "json"
    }
  }
  return(ans)
}
