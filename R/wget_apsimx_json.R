#' Get APSIM-X Model Replacement from github
#' 
#' @title fetches the json file for a specific model from APSIMX github
#' @name get_apsimx_json
#' @description Retreives the json replacement file for a specific model
#' @param model a model (e.g. \sQuote{Wheat} or \sQuote{Maize})
#' @param wrt.dir directory to save the JSON file (default is the current directory)
#' @param cleanup whether to delete the JSON file
#' @return a list read through the jsonlite package
#' @seealso \code{\link{insert_replacement_node}}
#' @export
#' @examples 
#' \donttest{
#' tmp.dir <- tempdir()
#' wheat <- get_apsimx_json(model = "Wheat", wrt.dir = tmp.dir)
#' }
#' 

get_apsimx_json <- function(model = "Wheat", wrt.dir = ".", cleanup = FALSE){
  ### Construct the string
  st0 <- "https://raw.githubusercontent.com/APSIMInitiative/ApsimX/master/Models/Resources"
  str <- paste0(st0, "/", model, ".json")
  dst <- file.path(wrt.dir, paste0(model, ".json"))
  utils::download.file(url = str, destfile = dst)

  ans <- jsonlite::read_json(dst)
  
  if(cleanup) unlink(dst)
  
  invisible(ans)
}

#' @title Inserts a replacement node in a simple apsimx simulation file
#' @name insert_replacement_node
#' @description Inserts a replacement node in a simple apsimx simulation file
#' @param file file ending in .apsimx to be edited (JSON)
#' @param src.dir directory containing the .apsimx file to be edited; defaults to the current working directory
#' @param wrt.dir should be used if the destination directory is different from the src.dir
#' @param rep.node replacement node as obtained by the \code{\link{get_apsimx_json}} function
#' @param rep.node.position position where the replacement node will be inserted, default is 1
#' @param new.core.position this by default will place the core simulation below the replacement node position. 
#' With this option, this can be modified.
#' @param edit.tag if the file is edited a different tag from the default \sQuote{-edited} can be used.
#' @param overwrite logical; if \code{TRUE} the old file is overwritten, a new file is written otherwise
#' @param verbose whether to print information about successful edit
#' @param root supply the node postion in the case of multiple simulations such as factorials.
#' @return it does not return an R object but it writes an apsimx file to disk
#' @export
#' @examples
#' \dontrun{
#' tmp.dir <- tempdir()
#' wheat <- get_apsimx_json(model = "Wheat", wrt.dir = tmp.dir)
#' extd.dir <- system.file("extdata", package = "apsimx")
#' insert_replacement_node("Wheat.apsimx", 
#'                         src.dir = extd.dir, wrt.dir = tmp.dir,
#'                         rep.node = wheat)
#' }
#' 

insert_replacement_node <- function(file, src.dir, wrt.dir, rep.node, 
                                    rep.node.position = 1,
                                    new.core.position = rep.node.position + 1,
                                    edit.tag = "-edited", 
                                    overwrite = FALSE,
                                    verbose = TRUE,
                                    root){
  
  .check_apsim_name(file)
  
  if(missing(wrt.dir)) wrt.dir <- src.dir
  
  file.names <- dir(path = src.dir, pattern=".apsimx$", ignore.case=TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsimx files in the specified directory to edit.")
  }
  
    ## For now we just edit one file at a time
  file <- match.arg(file, file.names)
  
  if(apsimx_filetype(file = file, src.dir = src.dir) != "json")
    stop("This function only edits JSON files")
  
  ## Parse apsimx file (JSON)
  apsimx_json <- jsonlite::read_json(file.path(src.dir, file))
  
  wcore <- grep("Core.Simulation", apsimx_json$Children)
  wdatastore <- grep("Models.Storage.DataStore", apsimx_json$Children)
  
  if(verbose){
    cat("Simulation(s) is/are in node(s)", wcore, "\n")
    cat("Datastore(s) is/are in node(s)", wdatastore, "\n")
  }
  
  if(rep.node.position == wdatastore)
    warning("Replacement node will overwrite DataStore")
  
  if(new.core.position == wdatastore)
    warning("Simulations node will overwrite DataStore")

  ## Need to modify the replacement node?
  rep.node$ExplorerWidth <- NULL
  rep.node$Version <- NULL
  rep.node$Name <- "Replacements"
  rep.node$`$type` <- "Models.Core.Replacements, Models"
  
  if(length(wcore) != length(new.core.position))
    stop("length of new.core.position should be equal to the number of core simulations")
  
  ## Move core simulation nodes down to make room for replacement node
  for(i in seq_along(new.core.position)){
      apsimx_json$Children[[new.core.position[i]]] <- apsimx_json$Children[[wcore[i]]]    
  }
  
  apsimx_json$Children[[rep.node.position]] <- rep.node
  
  ## Write to file
  if(overwrite == FALSE){
    wr.path <- paste0(wrt.dir, "/",
                      tools::file_path_sans_ext(file),
                      edit.tag, ".apsimx")
  }else{
    wr.path <- paste0(wrt.dir, "/", file)
  }
  
  jsonlite::write_json(apsimx_json, path = wr.path, 
                       pretty = TRUE, digits = NA, 
                       auto_unbox = TRUE, null = "null")
  
  if(verbose){
    cat("Created: ", wr.path,"\n")
  }
}

