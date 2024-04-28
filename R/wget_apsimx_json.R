#' Get APSIM-X Model Replacement from github
#' 
#' @title fetches the json file for a specific model from APSIMX github
#' @name get_apsimx_json
#' @description Retrieves the json replacement file for a specific model
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
#' @param edit.tag if the file is edited a different tag from the default \sQuote{-edited} can be used.
#' @param overwrite logical; if \code{TRUE} the old file is overwritten, a new file is written otherwise
#' @param verbose whether to print information about successful edit
#' @return it does not return an R object but it writes an apsimx file to disk
#' @export
#' @examples
#' \dontrun{
#' ## It is not trivial to produce a reproducible example
#' ## because the model and file versions need to align.
#' ## The steps are:
#' ## 1. Get model: 
#' ##    wheat <- get_apsimx_json(model = "Wheat", wrt.dir = tmp.dir)
#' ## 2. Create file that matches current model version
#' ## 3. Edit the file by inserting the 'replacements' node
#' ##    insert_replacement_node("Wheat.apsimx", rep.node = wheat)
#'                         
#' }
#' 

insert_replacement_node <- function(file, src.dir, wrt.dir, rep.node, 
                                    edit.tag = "-edited", 
                                    overwrite = FALSE,
                                    verbose = FALSE){
  
  .check_apsim_name(file)
  
  if(missing(wrt.dir)) wrt.dir <- src.dir
  
  file.names <- dir(path = src.dir, pattern=".apsimx$", ignore.case=TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsimx files in the specified directory to edit.")
  }
  
  file <- match.arg(file, file.names)
  
  if(apsimx_filetype(file = file, src.dir = src.dir) != "json")
    stop("This function only edits JSON files")
  
  ## I should check that 'rep.node' is a proper 'replacements' node
  ## but I'm not sure how to do that, because there are many different types
  
  ## Parse apsimx file (JSON)
  apsimx_json <- jsonlite::read_json(file.path(src.dir, file))
  
  ## Replacement version and APSIMX version need to match
  replacement.node.version <- rep.node$Version
  apsimx.file.version <- apsimx_json$Version
  
  if(replacement.node.version != apsimx.file.version)
    stop("Replacement node version and apsimx version should match")

  wcore <- grep("Core.Simulation", apsimx_json$Children)

  if(verbose){
    cat("Simulation(s) is/are in node(s)", wcore, "\n")
    cat("Adding node", rep.node$Children[[1]]$Name, "\n")
  }
  
  ## Test if Replacements folder is already present
  simulation.names <- sapply(apsimx_json$Children, FUN = function(x) x$Name)
  wreplace <- grep("Replacements", simulation.names)
  
  if(length(wreplace) == 0){
    replacements.folder <- vector("list", length = 1)
    rep.node.list <- vector("list", length = 1)
    rep.node.list[[1]] <- rep.node$Children[[1]]
    
    replacements.folder.elements <- list(`$type` = "Models.Core.Folder, Models",
                                         ShowInDocs = FALSE,
                                         GraphsPerPage = 6,
                                         Name = "Replacements",
                                         ResourceName = NULL,
                                         Children = rep.node.list,
                                         Enabled = TRUE,
                                         ReadOnly = FALSE)
    
    replacements.folder[[1]] <- replacements.folder.elements
    
    apsimx_json$Children <- append(apsimx_json$Children, replacements.folder, after = 0)    
  }else{
    if(verbose)
      cat("Replacement folder already present. Adding model")
    replacements.folder <- apsimx_json$Children[[wreplace]]
    rep.node.list <- vector("list", length = 1)
    rep.node.list[[1]] <- rep.node$Children[[1]]
    replacements.folder$Children <- append(replacements.folder$Children, rep.node.list)
    apsimx_json$Children[[wreplace]] <- replacements.folder
  }

  
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

