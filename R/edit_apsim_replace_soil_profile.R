#'
#' @title Edit APSIM 'Classic' file with a replaced soil profile
#' @name edit_apsim_replace_soil_profile
#' @description Edits an APSIM-X simulation by replacing the soil profile
#' @param file name of the .apsimx file to be edited
#' @param src.dir source directory
#' @param wrt.dir writing directory
#' @param soil.profile a soil profile object with class 'soil_profile'
#' @param swim list with SWIM specific parameters
#' @param soilwat list with SoilWat specific parameters
#' @param edit.tag default edit tag '-edited'
#' @param overwrite default FALSE
#' @param verbose default TRUE. Will print messages indicating what was done.
#' @return writes a file to disk with the supplied soil profile
#' @details This function is designed to batch replace the whole soil in an APSIM simulation. 
#' @note There is no such thing as a default soil, carefully build the profile for each simulation.
#'  dont export for now
#' @examples 
#' \dontrun{
#' sp <- apsimx_soil_profile()
#' extd.dir <- system.file("extdata", package = "apsimx")
#' edit_apsim_replace_soil_profile("Millet.apsim", soil.profile = sp, 
#'                                   src.dir = extd.dir, wrt.dir = ".")
#' inspect_apsim("Millet-edited.apsim", src.dir = ".",
#'                  node = "Soil")
#'  }
#'

edit_apsim_replace_soil_profile <-  function(file = "", src.dir = ".",
                                             wrt.dir = NULL,
                                             soil.profile = NULL,
                                             swim = NULL,
                                             soilwat = NULL,
                                             edit.tag = "-edited",
                                             overwrite = FALSE,
                                             verbose = TRUE){
  
  .check_apsim_name(file)
  
  if(missing(wrt.dir)) wrt.dir <- src.dir
  
  file.names <- dir(path = src.dir, pattern=".apsim$",ignore.case=TRUE)
  
  if(length(file.names)==0){
    stop("There are no .apsim files in the specified directory to edit.")
  }
  
  file <- match.arg(file, file.names)
  
  if(missing(soil.profile)) stop("soil profile is missing")
  
  stop("This is a placeholder. This function has not been developed yet.")
  
}