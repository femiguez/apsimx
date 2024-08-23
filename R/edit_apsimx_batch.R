#' Edit an APSIM-X (JSON) Simulation in Batch mode
#' 
#' This function allows editing of an APSIM-X (JSON) simulation file in batch mode.
#' 
#' from hol430
#' 
#' This allows the user to specify an .apsimx file and a config file when running Models.exe. The .apsimx file will not be run but instead, the changes listed in the config file will be applied to the .apsimx file, which will then be written to disk under the same filename.
#' 
#' The config file should contain lines of the form \sQuote{path = value}
#'
#' e.g.
#' 
#' [Clock].StartDate = 2019-1-20
#' .Simulations.Sim1.Name = SimulationVariant35
#' .Simulations.Sim2.Enabled = false
#' .Simulations.Sim1.Paddock.Soil.Thickness[1] = 50
#' Notes:
#'   
#'   Command line arguments should look like:
#'  Models.exe file.apsimx /Edit /path/to/config/file.conf
#' 
#' Relative paths will be resolved to the first match. ie
#' [Clock].StartDate will match the first clock found in the file.
#'
#' Dates can be specified as yyyy-mm-dd or mm/dd/yyyy.
#'
#' Strings should not be quoted
#' 
#' Array indices will be interpted as 1-indexed (mad face). So the first
#' element in the array should have index 1 in the config file.
#'
#' The file will be upgraded to the latest file version as part of this
#' process.
#' 
#' @name edit_apsimx_batch
#' @param file file ending in .apsimx to be edited (JSON)
#' @param src.dir directory containing the .apsimx file to be edited; defaults to the current working directory
#' @param wrt.dir should be used if the destination directory is different from the src.dir
#' @param parms parameter to be edited in the for of \sQuote{key = value}
#' @param silent controls the output of running APSIM at the command line
#' @param verbose whether to print information about successful edit
#' @return (when verbose=TRUE) complete file path to edited .apsimx file is returned as a character string.
#' As a side effect this function creates a new (JSON) .apsimx file.
#' @export
#' @examples 
#' \donttest{
#' ## This example will read one of the examples distributed with APSIM-X
#' ## but write to a temporary directory
#' 
#' tmp.dir <- tempdir()
#' 
#' ## Edit InitialResidueMass
#' extd.dir <- system.file("extdata", package = "apsimx")
#' parms <- list(`.Simulations.Simulation.Field.SurfaceOrganicMatter.InitialResidueMass` = 600)
#' edit_apsimx_batch("Wheat.apsimx", src.dir = extd.dir, wrt.dir = tmp.dir, parms = parms)
#' }
#' 

edit_apsimx_batch <- function(file, 
                              src.dir = ".", 
                              wrt.dir = NULL,
                              parms = NULL, 
                              silent = FALSE,
                              verbose = TRUE){
  
  if(missing(wrt.dir)) wrt.dir <- src.dir
  
  file.names <- dir(path = src.dir, pattern = ".apsimx$", ignore.case = TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsimx files in the specified directory to edit.")
  }
  
  if(missing(parms)) stop("parms is missing")
  
  if(length(grep(".apsimx$", file)) != 0){
    ## I assume the extention was included
    ## Only use the name from here 
    ## file <- strsplit(file, ".", fixed = TRUE)[[1]][1]
    file <- tools::file_path_sans_ext(file)
  }
  
  file.name.path <- paste0(src.dir, "/", file, ".apsimx")
  
  if(!is.null(wrt.dir)){
    ## Need to copy the file to wrt.dir and then clean up
    file.copy(from = file.name.path, 
              to = wrt.dir)
  }
  
  fns <- strsplit(file, ".", fixed = TRUE)[[1]][1]
  fn.po <- paste0(fns, "-prms.conf")
  write("", file = paste0(wrt.dir, "/", fn.po))
  
  prm.out <- character(length(parms))
  
  for(i in seq_along(parms)){
    prmo <- paste(names(parms[i]), "=", parms[[i]])
    write(prmo, file = paste0(wrt.dir, "/", fn.po), append = TRUE)
  }
  
  ## Auto detect apsimx
  ada <- auto_detect_apsimx()
  
  if(.Platform$OS.type == "unix"){
    mono <- system("which mono", intern = TRUE)
    run.strng <- paste0(mono, " ", ada, " ", paste0(wrt.dir, "/", file, ".apsimx"), " /Edit ", paste0(wrt.dir, "/", fn.po))
    ## Run APSIM-X on the command line
    system(command = run.strng, ignore.stdout = silent)
  }
  
  if(.Platform$OS.type == "windows"){
    ada <- auto_detect_apsimx()
    run.strng <- paste0(ada, " ", paste0(wrt.dir, "/", file, ".apsimx")," /Edit ", paste0(wrt.dir, "/", fn.po))
    cmd.out <- shell(cmd = run.strng, translate = FALSE, intern = TRUE)
  }
  
  if(verbose){
    cat("Edited parameters file: ", paste0(wrt.dir, "/", fn.po), "\n")
    cat("Edited parameters: ", names(parms), "\n")
    cat("Created new: ", paste0(wrt.dir, "/", file, ".apsimx"), "\n")
    if(.Platform$OS.type == "windows") print(cmd.out)
  }
}

