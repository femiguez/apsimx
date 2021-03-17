#' Get APSIM-X Model Replacement from github
#' 
#' @title fetches the json file for a specific model from APSIMX github
#' @name wget_apsimx_json
#' @description Retreives the json replacement file for a specific model
#' @param model a model (e.g. \sQuote{Wheat} or \sQuote{Maize})
#' @param wrt.dir directory to save the JSON file (default is the current directory)
#' @param cleanup whether to delete the JSON file
#' @return a list read through the jsonlite package
#' @export
#' @examples 
#' \donttest{
#' tmp.dir <- tempdir()
#' wheat <- wget_apsimx_json(model = "Wheat", wrt.dir = tmp.dir)
#' }
#' 

wget_apsimx_json <- function(model = "Wheat", wrt.dir = ".", cleanup = FALSE){
  ### Construct the string
  st0 <- "https://raw.githubusercontent.com/APSIMInitiative/ApsimX/master/Models/Resources"
  str <- paste0(st0, "/", model, ".json")
  dst <- file.path(wrt.dir, paste0(model, ".json"))
  utils::download.file(url = str, destfile = dst)
  
  if(cleanup) unlink(dst)
    
  ans <- jsonlite::read_json(dst)
  
  invisible(ans)
}

## Work in progress
# inspect_apsimx_json <- function(file = "", src.dir = ".", parm){
#   
#   .check_apsim_name(file)
#   .check_apsim_name(src.dir)
#   
#   file.names.apsimx <- dir(path = src.dir, pattern = ".apsimx$", ignore.case = TRUE)
#   file.names.json <- dir(path = src.dir, pattern = ".json$", ignore.case = TRUE)
#   
#   if(length(file.names.apsimx) == 0 && length(file.names.json) == 0){
#     stop("There are no .json or .apsimx files in the specified directory to inspect.")
#   }
#   
#   apsimx_json <- jsonlite::read_json(file.path(src.dir, file)) ### This is a list
# 
#   ## This finds the element, but does not build the path
#   ## there is something called JSONpath that would be nice, but apparently not available for R
#   grep_fun <- function(x, parm) grep(parm, x, value = TRUE)  
#   rapply(apsimx_json, grep_fun, parm = parm)
#   ##.Simulations.SimulationBARC.Clock
# }