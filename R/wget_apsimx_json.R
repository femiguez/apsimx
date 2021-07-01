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
#' \donttest{
#' tmp.dir <- tempdir()
#' wheat <- get_apsimx_json(model = "Wheat", wrt.dir = tmp.dir)
#' ex.dir <- auto_detect_apsimx_examples()
#' insert_replacement_node("Wheat.apsimx", 
#'                         src.dir = ex.dir, wrt.dir = tmp.dir,
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

## Work in progress
inspect_apsimx_json <- function(file = "", src.dir = ".", parm, verbose = TRUE){

  .check_apsim_name(file)
  .check_apsim_name(src.dir)

  if(missing(parm))
    stop("You need to specify the parm argument")
  
  file.names.apsimx <- dir(path = src.dir, pattern = ".apsimx$", ignore.case = TRUE)
  file.names.json <- dir(path = src.dir, pattern = ".json$", ignore.case = TRUE)

  if(length(file.names.apsimx) == 0 && length(file.names.json) == 0){
    stop("There are no .json or .apsimx files in the specified directory to inspect.")
  }

  apsimx_json <- jsonlite::read_json(file.path(src.dir, file)) ### This is a list

  jsonpath <- "$"
  ii <- 1
  notfound <- TRUE
  # grep_fun <- function(x, parm) grepl(parm, x)
  # rapply_fun <- function(x, parm) rapply(x, f = grep_fun(x, parm = parm))
  
  find_parm <- function(x, parm, verbose){
    
    while(notfound && ii < 50){
      ## Special handling of level 1
      x.names <- names(x)
      if(!is.null(x.names) && any(grepl(parm, x.names))){
        notfound <-  FALSE
        wpi <- grep(parm, x.names) ## Which parameter index
        if(length(wpi) > 1){
          cat("Found: ", x.names[wpi], "\n") 
          stop("Cannot handle multiple matches at the moment", call. = FALSE)
        }
        jsonpath <- paste0(jsonpath, ".", x.names[wpi])
        if(verbose){
          cat("Parm: ", x.names[wpi], " = ", x[[x.names[wpi]]], "\n")
        }
        break
      }else{
        ## Extracting the names can be tricky
        if(is.null(x.names) && !is.null(x$Children)){
          ## What is the children length?
          x.length <- length(x$Children)
          wchi <- logical(x.length)
          for(i in seq_len(x.length)){
              wchi[i] <- any(grepl(parm, x$Children[[i]])) 
          }
          chdi <- c(1:x.length)[wchi]
          x.names <- names(x$Children[[chdi]])
        }
        
        if(!is.null(x.names)){
          found <- grepl(parm, x.names)  
          if(any(found)){
            jsonpath <- paste0(jsonpath, ".", parm)
            notfound <- FALSE
            break
          }else{
            if(!is.null(x$Children)){
              jsonpath <- paste0(jsonpath, ".", x$Name)
              x.length <- length(x$Children)
              wchi <- logical(x.length)
              for(i in seq_len(x.length)){
                wchi[i] <- any(grepl(parm, x$Children[[i]]))
              }   
              if(sum(wchi) > 1){
                for(i in which(wchi > 0.5)){
                  cat("Node:", x$Children[[i]]$Name, "\n")
                }
                warning("Found multiple matches for parameter. Picking the first one")
                wchi <- which(wchi > 0.5)[1]
              } 
              chdi <- c(1:x.length)[wchi]
              x <- x$Children[[chdi]]
              ii <<- ii + 1
              find_parm(x, parm)              
            }
          }
        }        
      }
    }
    if(notfound){
      jsonpath <- NA
    } 
  }  
  
  ans <- find_parm(apsimx_json, parm, verbose = verbose)
  
  invisible(ans)
}

