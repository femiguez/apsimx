#' Get APSIM-X Model Replacement from github
#' 
#' @title fetches the json file for a specific model from APSIMX github
#' @name get_apsimx_json
#' @description Retreives the json replacement file for a specific model
#' @param model a model (e.g. \sQuote{Wheat} or \sQuote{Maize})
#' @param wrt.dir directory to save the JSON file (default is the current directory)
#' @param cleanup whether to delete the JSON file
#' @return a list read through the jsonlite package
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
  
  if(cleanup) unlink(dst)
    
  ans <- jsonlite::read_json(dst)
  
  invisible(ans)
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

