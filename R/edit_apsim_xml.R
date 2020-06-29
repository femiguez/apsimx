#' Edit an APSIM (Classic) Simulation auxiliary xml file
#' 
#' This function allows editing of an APSIM (Classic) simulation xml file.
#' 
#' The variables specified by \code{parm} within the .apsim file specified by \code{file} 
#' in the source directory \code{src.dir} are edited. The old values are replaced with \code{value}, which
#' is a list that has the same number of elements as the length of the vector \code{parm}.  The current
#' .apsim file will be overwritten if \code{overwrite} is set to \code{TRUE}; otherwise the file
#' \sQuote{file} \emph{-edited.apsim} will be created.  If (verbose = TRUE) then the name
#'  of the written file is returned. The function is similar to the edit_apsim functin in the \sQuote{apsimr}
#'  package, but with the difference that only some variables (parameters) can be modified.
#'  
#'  The function inspect_apsim is for a quick look from within R. The APSIM GUI provides a more
#'  complete examination of the .apsim file
#' 
#' @name edit_apsim_xml
#' @param file file ending in .xml to be edited
#' @param src.dir directory containing the .apsim file to be edited; defaults to the current working directory
#' @param wrt.dir should be used if the destination directory is different from the src.dir
#' @param parm.path parameter path to be edited (see example)
#' @param value new values for the parameter to be edited 
#' @param overwrite logical; if \code{TRUE} the old file is overwritten, a new file is written otherwise
#' @param edit.tag if the file is edited a different tag from the default \sQuote{-edited} can be used.
#' @param verbose whether to print information about successful edit
#' @return (when verbose=TRUE) complete file path to edited .xml file is returned as a character string.
#' As a side effect this function creates a new XML file.
#' @note This function cannot check whether replacement is of the correct length. Also, there is
#' no inspect equivalent, so you need to inspect the file manually and/or use some trial and error
#' to find the correct parameter and double check that it was editied correctly. It is more
#' flexible than \sQuote{edit_apsim} and (perhaps) equivalent to \sQuote{apsimr::edit_sim_file}.
#' @export
#' @examples 
#' \donttest{
#' ## This example changes the RUE values
#' 
#' extd.dir <- system.file("extdata", package = "apsimx")
#' 
#' values <- paste(rep(1.7, 12), collapse = " ")
#' 
#' ## Writing to a temp directory, but replace as needed
#' tmp.dir <- tempdir()
#' 
#' edit_apsim_xml("Maize75.xml", 
#'                src.dir = extd.dir,
#'                wrt.dir = tmp.dir,
#'                parm.path = ".//Model/rue",
#'                value = values)
#' }

edit_apsim_xml <- function(file, src.dir = ".", wrt.dir = NULL,
                           parm.path=NULL, 
                           value=NULL, 
                           overwrite = FALSE,
                           edit.tag = "-edited",
                           verbose = TRUE){
  
  if(missing(wrt.dir)) wrt.dir <- src.dir
  
  file.names <- dir(path = src.dir, pattern=".xml$",ignore.case=TRUE)
  
  if(length(file.names)==0)
    stop("There are no .xml files in the specified directory to edit.")
  
  if(!identical(length(value),length(parm.path))) 
    stop("length of parm.path should be equal to length of value")
  
  file <- match.arg(file, file.names)
  
  ## Parse apsim file (XML)
  apsim_xml <- xml2::read_xml(paste0(src.dir,"/",file))
  
  xml.parm.path.out <- character(length(parm.path))
  
  for(i in seq_along(parm.path)){

    xml.path <- xml2::xml_find_all(apsim_xml, parm.path[i])
    
    if(is.na(xml.path)) stop("parameter not found")
    
    if(length(xml.path) > 1) stop("path is not unique")
    
    if(length(xml.path) != length(value[i])){
      stop("length of parm.path should be equal to length of value")
    }
    
    xml.parm.path.out[i] <- xml2::xml_path(xml.path)
    
    xml2::xml_set_text(xml.path, value[i])
    
  }

  if(overwrite == FALSE){
    wr.path <- paste0(wrt.dir,"/",
                      tools::file_path_sans_ext(file),
                      edit.tag,".xml")
  }else{
    wr.path <- paste0(wrt.dir,"/",file)
  }
  xml2::write_xml(apsim_xml, file = wr.path)
  
  if(verbose){
    cat("Edited",parm.path, "\n")
    cat("Paramter path:", xml.parm.path.out, "\n")
    cat("New values ",value, "\n")
    cat("Created ",wr.path,"\n")
  }
  invisible(xml.parm.path.out)
}


