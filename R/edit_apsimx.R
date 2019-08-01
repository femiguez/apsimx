#' Edit an APSIM-X Simulation
#' 
#' This function allows editing of an APSIM-X (XML or JSON) simulation file.
#' 
#' The variables specified by \code{parm} within the .apsimx file specified by \code{file} 
#' in the source directory \code{src.dir} are edited. The old values are replaced with \code{value}, which
#' is a vector that has the same number of elements as the length of the vector \code{parm}.  The current
#' .apsimx file will be overwritten if \code{overwrite} is set to \code{TRUE}; otherwise the file
#' \emph{'file'-edited.apsimx} will be created.  If (verbose = TRUE) then the name
#'  of the written file is returned. The function is similar to the edit_apsim functin in the 'apsimr'
#'  package, but with the difference that only some variables (parameters) can be modified.
#'  
#'  The function inspect_apsimx is for a quick look from within R. The APSIM GUI provides a more
#'  complete examination of the .apsimx file
#' 
#' @name edit_apsimx
#' @param file file ending in .apsimx to be edited
#' @param src.dir directory containing the .apsimx file to be edited; defaults to the current working directory
#' @param wrt.dir should be used if the destination directory is different from the src.dir
#' @param node either 'Weather', 'Soil', 'SurfaceOrganicMatter', 'MicroClimate', 'Crop', 'Manager' or 'Other' 
#' @param soil.child specific soil component to be edited
#' @param som.child specific soil organic matter component to be edited
#' @param manager.child specific manager component to be edited
#' @param parm parameter to be edited
#' @param value new values for the parameter to be edited 
#' @param overwrite logical; if \code{TRUE} the old file is overwritten, a new file is written otherwise
#' @param edit.tag if the file is edited a different tag from the default '-edited' can be used.
#' @param parm.path path to the attribute to edit when node is 'Other'
#' @param verbose whether to print information about successful edit
#' @return (when verbose=TRUE) complete file path to edited .apsimx file is returned as a character string.
#' As a side effect this function creates a new (XML or JSON) .apsimx file.
#' @note The components that can be edited are restricted becuase this is better in preventing
#' errors of editing unintended parts of the file. The disadvantage is that there is less flexibility
#' compared to the similar function in the 'apsimr' package. 
#' @export
#' @examples 
#' \dontrun{
#' ## This example will read one of the examples distributed with APSIM-X
#' ## but write to the current directory
#' 
#' ex.dir <- auto_detect_apsimx_examples()
#' ocs <- rep(0.5, 7)
#' edit_apsimx("Maize.apsimx", src.dir = ex.dir,
#'             wrt.dir = ".",
#'             node = "Soil",
#'             soil.child = "OrganicMatter", 
#'             parm = "OC", value = ocs,
#'             verbose = FALSE)
#' ## To delete the file...
#' file.remove("./Maize-edited.apsimx")
#' }
#' 

edit_apsimx <- function(file, src.dir = ".", wrt.dir = NULL,
                        node = c("Weather","Soil","SurfaceOrganicMatter",
                                 "MicroClimate","Crop","Manager"),
                        soil.child = c("Water","OrganicMatter",
                                       "Analysis","InitialWater","Sample"),
                        som.child = c("Pools","Other"),
                        manager.child = NULL,
                        parm=NULL, value=NULL, overwrite = FALSE,
                        edit.tag = "-edited",
                        parm.path = NULL,
                        verbose = TRUE){
  
  if(missing(wrt.dir)) wrt.dir <- src.dir

  fileNames <- dir(path = src.dir, pattern=".apsimx$",ignore.case=TRUE)
  
  if(length(fileNames)==0){
    stop("There are no .apsimx files in the specified directory to edit.")
  }
  
  node <- match.arg(node)
  soil.child <- match.arg(soil.child)
  som.child <- match.arg(som.child)
  
  ## For now we just edit one file at a time
  file <- match.arg(file, fileNames, several.ok=FALSE)
  
  if(apsimx_filetype(file = file, src.dir) == "unknown"){
    stop("file must be either XML or JSON")
  }
  ## Edit XML file type
  if(apsimx_filetype(file = file, src.dir = src.dir) == "xml"){
    edit_apsimx_xml(file = file, src.dir = src.dir, wrt.dir = wrt.dir,
                    node = node, soil.child = soil.child, som.child = som.child, 
                    manager.child = manager.child,
                    parm = parm, value = value, overwrite = overwrite,
                    edit.tag = edit.tag,
                    parm.path = parm.path,  verbose = verbose)
  }else{
    ## Edit JSON file type
    edit_apsimx_json(file = file, src.dir = src.dir, wrt.dir = wrt.dir,
                    node = node, soil.child = soil.child, som.child = som.child, 
                    manager.child = manager.child,
                    parm = parm, value = value, overwrite = overwrite,
                    edit.tag = edit.tag,
                    parm.path = parm.path, verbose = verbose)
  }
}
