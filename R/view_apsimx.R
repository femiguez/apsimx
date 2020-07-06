#' 
#' @title Viewing an APSIM-X file interactively
#' @name view_apsimx
#' @description Generate an interactive viewer for an APSIM-X file
#' @param file 	a file ending in .apsimx to be inspected (JSON)
#' @param src.dir directory containing the .apsimx file to be inspected; defaults to the current working directory
#' @param viewer either \dQuote{json} or \dQuote{react}.
#' @param ... additional arguments passed to either \sQuote{jsonedit} or \sQuote{reactjson}.
#' These are functions in package \CRANpkg{listviewer}.
#' @note I do not know how to edit an APSIM-X file using this method yet.
#' @export
#' @examples 
#' \dontrun{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' ## View the structure of the APSIM-X simulation file
#' view_apsimx("Wheat.apsimx", src.dir = extd.dir)
#' }
#' 

view_apsimx <- function(file, src.dir, viewer = c("json","react"), ...){
  
  if(!requireNamespace("listviewer", quietly = TRUE)){
    warning("The listviewer package is required for this function")
    return(NULL)
  }
  
  if(missing(file)) stop("need to specify file name")
  
  .check_apsim_name(file)
  
  ## The might offer suggestions in case there is a typo in 'file'
  file.names <- dir(path = src.dir, pattern = ".apsimx$", ignore.case=TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsimx files in the specified directory to run.")
  }
  
  file <- match.arg(file, file.names)
  
  if(missing(src.dir)) src.dir <- "."

  viewer <- match.arg(viewer)
  
  file.name.path <- file.path(src.dir, file)
  
  apsimx_json <- jsonlite::read_json(file.name.path)
  
  if(viewer == "json"){
    ans <- listviewer::jsonedit(listdata = apsimx_json, ...) 
    return(ans)
  }
  
  if(viewer == "react"){
    
    if(!requireNamespace("reactR", quietly = TRUE)){
      warning("The reactR package is required for this function")
      return(NULL)
    }
    
    ans <- listviewer::reactjson(listdata = apsimx_json, ...)  
    return(ans)
  }
}


#' 
#' @title Viewing an APSIM Classic file interactively
#' @name view_apsim
#' @description Generate an interactive viewer for an APSIM file
#' @param file 	a file ending in .apsim to be inspected (XML)
#' @param src.dir directory containing the .apsim file to be inspected; defaults to the current working directory
#' @param viewer either \dQuote{json} or \dQuote{react}.
#' @param ... additional arguments passed to either \sQuote{jsonedit} or \sQuote{reactjson}.
#' These are functions in package \CRANpkg{listviewer}.
#' @note I do not know how to edit an APSIM file using this method yet.
#' @export
#' @examples 
#' \dontrun{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' ## View the structure of the APSIM-X simulation file
#' view_apsim("Millet.apsim", src.dir = extd.dir)
#' }
#' 

view_apsim <- function(file, src.dir, viewer = c("json","react"), ...){
  
  if(!requireNamespace("listviewer", quietly = TRUE)){
    warning("The listviewer package is required for this function")
    return(NULL)
  }
  
  if(missing(file)) stop("need to specify file name")
  
  .check_apsim_name(file)
  
  ## The might offer suggestions in case there is a typo in 'file'
  file.names <- dir(path = src.dir, pattern = ".apsim$", ignore.case=TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsim files in the specified directory to run.")
  }
  
  file <- match.arg(file, file.names)
  
  if(missing(src.dir)) src.dir <- "."
  
  viewer <- match.arg(viewer)
  
  file.name.path <- file.path(src.dir, file)
  
  apsim_xml <- xml2::as_list(xml2::read_xml(file.name.path))
  
  if(viewer == "json"){
    ans <- listviewer::jsonedit(listdata = apsim_xml, ...) 
    return(ans)
  }
  
  if(viewer == "react"){
    
    if(!requireNamespace("reactR", quietly = TRUE)){
      warning("The reactR package is required for this function")
      return(NULL)
    }
    
    ans <- listviewer::reactjson(listdata = apsim_xml, ...)  
    return(ans)
  }
}