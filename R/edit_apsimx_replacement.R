#' 
#' @title Edit a replacement component in an .apsimx (JSON) file
#' @name edit_apsimx_replacement
#' @description edit the replacement componenet of an JSON apsimx file. It does not replace the GUI, but it can save time by quickly editing parameters and values.
#' @param file file ending in .apsimx to edit (JSON)
#' @param src.dir directory containing the .apsimx file; defaults to the current working directory
#' @param wrt.dir should be used if the destination directory is different from the src.dir
#' @param node specific node to edit
#' @param node.child specific node child component to edit.
#' @param node.subchild specific node sub-child to edit.
#' @param node.subsubchild specific node sub-subchild to edit.
#' @param node.sub3child specific node sub-sub-subchild to edit.
#' @param node.sub4child specific node sub-sub-sub-subchild to edit.
#' @param node.sub5child specific node sub-sub-sub-sub-subchild to edit.
#' @param node.string passing of a string instead of the node hierarchy. It can either start with a dot or not.
#' However, the \sQuote{best} form is not to start with a dot as it should be a more convenient form of passing
#' the nodes and their childs and not a real \sQuote{jsonpath}.
#' @param root \sQuote{root} node to explore (default = \dQuote{Models.Core.Replacements})
#' @param parm specific parameter to edit
#' @param value new values for the parameter
#' @param overwrite logical; if \code{TRUE} the old file is overwritten, a new file is written otherwise
#' @param edit.tag if the file is edited a different tag from the default \sQuote{-edited} can be used.
#' @param verbose whether to print information about successful edit
#' @details This is simply a script that prints the relevant parameters which are likely to need editing. It does not print all information from an .apsimx file.
#' @return (when verbose=TRUE) complete file path to edited .apsimx file is returned as a character string.
#' As a side effect this function creates a new (JSON) .apsimx file.
#' @note The components that can be edited are restricted becuase this is better in preventing
#' errors of editing unintended parts of the file.
#' @export
#' @examples 
#' \donttest{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' ## Writing to a temp directory, but change as needed
#' tmp.dir <- tempdir()
#' 
#' ## Inspect original values
#' inspect_apsimx_replacement("MaizeSoybean.apsimx", 
#'                            src.dir = extd.dir,
#'                            node = "Maize", 
#'                            node.child = "Phenology",
#'                            node.subchild = "ThermalTime", 
#'                            node.subsubchild = "BaseThermalTime",
#'                            node.sub3child = "Response")
#' 
#' edit_apsimx_replacement("MaizeSoybean.apsimx", 
#'                         src.dir = extd.dir, wrt.dir = tmp.dir,
#'                         node = "Maize", 
#'                         node.child = "Phenology",
#'                         node.subchild = "ThermalTime", 
#'                         node.subsubchild = "BaseThermalTime",
#'                         node.sub3child = "Response",
#'                         parm = "X",
#'                         value = c(10, 20, 30, 40, 50)) 
#' ## inspect it
#' inspect_apsimx_replacement("MaizeSoybean-edited.apsimx", 
#'                            src.dir = tmp.dir,
#'                            node = "Maize", 
#'                            node.child = "Phenology",
#'                            node.subchild = "ThermalTime", 
#'                            node.subsubchild = "BaseThermalTime",
#'                            node.sub3child = "Response")
#' 
#' ## Illustrating using 'node.string'
#' ## Equivalent to the code to edit above
#' 
#' edit_apsimx_replacement("MaizeSoybean-edited.apsimx", 
#'                         src.dir = tmp.dir, wrt.dir = tmp.dir,
#'                         node.string = "Maize.Phenology.ThermalTime.BaseThermalTime.Response",
#'                         parm = "X",
#'                         value = c(11, 21, 31, 41, 51),
#'                         edit.tag = "-ns") 
#'                         
#' inspect_apsimx_replacement("MaizeSoybean-edited-ns.apsimx", 
#'                            src.dir = tmp.dir,
#'                            node = "Maize", 
#'                            node.child = "Phenology",
#'                            node.subchild = "ThermalTime", 
#'                            node.subsubchild = "BaseThermalTime",
#'                            node.sub3child = "Response")
#' }
#'

edit_apsimx_replacement <- function(file = "", src.dir = ".", wrt.dir = ".",
                                    node = NULL, node.child = NULL,
                                    node.subchild = NULL, node.subsubchild = NULL,
                                    node.sub3child = NULL, node.sub4child = NULL, 
                                    node.sub5child = NULL, node.string = NULL,
                                    root = list("Models.Core.Replacements", NA),
                                    parm = NULL, value = NULL, overwrite = FALSE,
                                    edit.tag = "-edited", verbose = TRUE){
  
  file.names <- dir(path = src.dir, pattern=".apsimx$",ignore.case=TRUE)
  
  if(length(file.names)==0){
    stop("There are no .apsimx files in the specified directory to edit.")
  }
  
  ## This matches the specified file from a list of files
  ## Notice that the .apsimx extension will be added here
  file <- match.arg(file, file.names, several.ok=FALSE)
  
  if(missing(parm) || missing(value)) stop("'parm' and/or 'value' are missing")
  
  apsimx_json <- jsonlite::read_json(paste0(src.dir, "/", file))
  
  ## Select Replacements node
  frn <- grep(root[[1]], apsimx_json$Children, fixed = TRUE)
  
  if(length(frn) == 0) stop(paste0(root," not found"))

  if(length(frn) > 1){
    if(is.na(root[[2]])){
      cat("These positions matched ",root[[1]]," ",frn, "\n")
      stop("Multiple root nodes found. Please provide a position")
    }else{
      replacements.node <- apsimx_json$Children[[frn[root[[2]]]]]
    }
  }else{
    replacements.node <- apsimx_json$Children[[frn]]
  }
  
  ## Print names of replacements
  replacements.node.names <- vapply(replacements.node$Children, function(x) x$Name, 
                                    FUN.VALUE = "character")
  if(verbose) cat("Replacements: ", replacements.node.names, "\n")
  
  if(!missing(node.string)){
    ## strip the dot automatically if it is included
    if(strsplit(node.string, "")[[1]][1] == "."){
      node.string <- substring(node.string, 2)
    }
    ## If I want to provide the path returned by inspect_apsimx_replacement
    ## directly I need to strip the first part which is likely to be '.Simulations.Replacement.'
    if(grepl(".Replacements.", node.string)){
      node.string <- strsplit(node.string, ".Replacements.", fixed = TRUE)[[1]][2]
    }
    nodes <- strsplit(node.string, ".", fixed = TRUE)[[1]]
    node <- nodes[1]
    if(!is.na(nodes[2])) node.child <- nodes[2]
    if(!is.na(nodes[3])) node.subchild <- nodes[3]
    if(!is.na(nodes[4])) node.subsubchild <- nodes[4]
    if(!is.na(nodes[5])) node.sub3child <- nodes[5]
    if(!is.na(nodes[6])) node.sub4child <- nodes[6]
    if(!is.na(nodes[7])) node.sub5child <- nodes[7]
  }
  
  if(missing(node)) return(cat("Please provide a node \n"))
  ## Let's call this level = 0, at the 'node' level (nothing to edit)
  lvl <- -1
  wrn <- grep(node, replacements.node.names)
  rep.node <- replacements.node$Children[[wrn]]
  
  if(!is.null(rep.node$CropType) && verbose) cat("CropType", rep.node$CropType,"\n")
  
  if(parm %in% names(rep.node)){
    lvl <- 0
    rep.node <- edit_node(rep.node, parm = parm, value = value)
    replacements.node$Children[[wrn]] <- rep.node
    if(length(frn) == 1){
      apsimx_json$Children[[frn]] <- replacements.node
    }else{
      apsimx_json$Children[[frn[root[[2]]]]] <- replacements.node
    }
  } 
  
  rep.node.children.names <- sapply(rep.node$Children, function(x) x$Name)
  if(verbose) cat("Available node children: ", rep.node.children.names, "\n")
  
  ## Select a specific node.child
  if(missing(node.child) && lvl == -1) return(cat("missing node.child\n"))
  ## Node.child would be level = 1, still nothing to edit
  if(!missing(node.child)){
    wrnc <- grep(node.child, rep.node.children.names)
    rep.node.child <- rep.node$Children[[wrnc]]

    if(any(parm %in% names(rep.node.child))){
      lvl <- 1
      rep.node.child <- edit_node(rep.node.child, parm = parm, value = value)
      rep.node$Children[[wrnc]] <- rep.node.child
      replacements.node$Children[[wrn]] <- rep.node
      if(length(frn) == 1){
        apsimx_json$Children[[frn]] <- replacements.node
      }else{
        apsimx_json$Children[[frn[root[[2]]]]] <- replacements.node
      }
    }
    
    ## Cultivar parameters can be in 'Command'
    if(length(rep.node.child$Command) > 0){
      if(any(grepl(parm, unlist(rep.node.child$Command)))){
        lvl <- 2
        wrnsc <- grep(parm, unlist(rep.node.child$Command))
        ## Break it up and reassemble
        cmdstrng <- strsplit(rep.node.child$Command[[wrnsc]], "=")[[1]][1]
        rep.node.child$Command[[wrnsc]] <- paste0(cmdstrng, "=", value)
        ## Now write back to list
        rep.node$Children[[wrnc]] <- rep.node.child
        replacements.node$Children[[wrn]] <- rep.node
        if(length(frn) == 1){
          apsimx_json$Children[[frn]] <- replacements.node
        }else{
          apsimx_json$Children[[frn[root[[2]]]]] <- replacements.node
        }
      }
    }
    
    if(missing(node.subchild) && lvl == -1)
      stop("Failed to find and edit parameter")
    
    rep.node.subchildren.names <- sapply(rep.node.child$Children, function(x) x$Name)
  }
  
  ## Select a specific node.subchild
  if(missing(node.subchild) && verbose && missing(parm)) cat("missing node.subchild\n")
  
  if(!missing(node.subchild)){
    wrnsc <- grep(node.subchild, rep.node.subchildren.names)
    rep.node.subchild <- rep.node.child$Children[[wrnsc]]
  
    if(verbose) cat("Subchild Name: ", rep.node.subchild$Name,"\n")
  
    if(parm %in% names(rep.node.subchild)){
      rep.node.subchild <- edit_node(rep.node.subchild, parm = parm, value = value)
      lvl <- 3
      rep.node.child$Children[[wrnsc]] <- rep.node.subchild
      rep.node$Children[[wrnc]] <- rep.node.child
      replacements.node$Children[[wrn]] <- rep.node
      if(length(frn) == 1){
        apsimx_json$Children[[frn]] <- replacements.node
      }else{
        apsimx_json$Children[[frn[root[[2]]]]] <- replacements.node
      }
      ## apsimx_json is ready to be written back to file
    }
    
    if(missing(node.subsubchild) && lvl == -1)
      stop("Failed to find and edit parameter")
  }
  
  if(missing(node.subsubchild) && verbose && missing(parm)) cat("missing node.subsubchild\n")
  
  if(!missing(node.subsubchild)){
    rep.node.subsubchildren.names <- vapply(rep.node.subchild$Children, function(x) x$Name,
                                            FUN.VALUE = "character")
    ## Select a specific node.subsubchild
    wrnssc <- grep(node.subsubchild, rep.node.subsubchildren.names)
    rep.node.subsubchild <- rep.node.subchild$Children[[wrnssc]]
  
    if(verbose) cat("Subsubchild Name: ", rep.node.subsubchild$Name,"\n")
  
    if(parm %in% names(rep.node.subsubchild)){
      rep.node.subsubchild <- edit_node(rep.node.subsubchild, parm = parm, value = value)
      lvl <- 4
      rep.node.subchild$Children[[wrnssc]] <- rep.node.subsubchild
      rep.node.child$Children[[wrnsc]] <- rep.node.subchild
      rep.node$Children[[wrnc]] <- rep.node.child
      replacements.node$Children[[wrn]] <- rep.node
      if(length(frn) == 1){
        apsimx_json$Children[[frn]] <- replacements.node
      }else{
        apsimx_json$Children[[frn[root[[2]]]]] <- replacements.node
      }
      ## apsimx_json is ready to be written back to file
    }
    
    ## Cultivar parameters can be in 'Command'
    if(length(rep.node.subsubchild$Command) > 0){
      if(any(grepl(parm, unlist(rep.node.subsubchild$Command)))){
        lvl <- 5
        wrnsscc <- grep(parm, unlist(rep.node.subsubchild$Command))
        ## Break it up and reassemble
        cmdstrng <- strsplit(rep.node.subsubchild$Command[[wrnsscc]],"=")[[1]][1]
        rep.node.subsubchild$Command[[wrnsscc]] <- paste0(cmdstrng,"=",value)
        ## Now write back to list
        rep.node.subchild$Children[[wrnssc]] <- rep.node.subsubchild
        rep.node.child$Children[[wrnsc]] <- rep.node.subchild
        rep.node$Children[[wrnc]] <- rep.node.child
        replacements.node$Children[[wrn]] <- rep.node
        if(length(frn) == 1){
          apsimx_json$Children[[frn]] <- replacements.node
        }else{
          apsimx_json$Children[[frn[root[[2]]]]] <- replacements.node
        }
      }
    }
    
    if(missing(node.sub3child) && lvl == -1)
      stop("Failed to find and edit parameter")
  }

  ## Inserting deeper level June 15th 2020
  if(missing(node.sub3child) && verbose && missing(parm)) cat("missing node.sub3child\n")  
  
  if(!missing(node.sub3child)){
    rep.node.sub3children.names <- vapply(rep.node.subsubchild$Children, function(x) x$Name,
                                            FUN.VALUE = "character")
    ## Select a specific node.subchild
    wrnsssc <- grep(node.sub3child, rep.node.sub3children.names)
    rep.node.sub3child <- rep.node.subsubchild$Children[[wrnsssc]]
    
    if(verbose) cat("Sub-sub-subchild Name: ", rep.node.sub3child$Name,"\n")
    
    if(any(parm %in% names(rep.node.sub3child))){
      rep.node.sub3child <- edit_node(rep.node.sub3child, parm = parm, value = value)
      lvl <- 6
      # node is edited, now put it back in place
      rep.node.subsubchild$Children[[wrnsssc]] <- rep.node.sub3child
      rep.node.subchild$Children[[wrnssc]] <- rep.node.subsubchild
      rep.node.child$Children[[wrnsc]] <- rep.node.subchild
      rep.node$Children[[wrnc]] <- rep.node.child
      replacements.node$Children[[wrn]] <- rep.node
      if(length(frn) == 1){
        apsimx_json$Children[[frn]] <- replacements.node
      }else{
        apsimx_json$Children[[frn[root[[2]]]]] <- replacements.node
      }
      ## apsimx_json is ready to be written back to file
    }
    
    ## Cultivar parameters can be in 'Command'
    if(length(rep.node.sub3child$Command) > 0){
      if(any(grepl(parm, unlist(rep.node.sub3child$Command)))){
        lvl <- 7
        wrnssscc <- grep(parm, unlist(rep.node.sub3child$Command))
        ## Break it up and reassemble
        cmdstrng <- strsplit(rep.node.sub3child$Command[[wrnssscc]],"=")[[1]][1]
        rep.node.sub3child$Command[[wrnssscc]] <- paste0(cmdstrng,"=",value)
        ## Now write back to list
        rep.node.subsubchild$Children[[wrnsssc]] <- rep.node.sub3child
        rep.node.subchild$Children[[wrnssc]] <- rep.node.subsubchild
        rep.node.child$Children[[wrnsc]] <- rep.node.subchild
        rep.node$Children[[wrnc]] <- rep.node.child
        replacements.node$Children[[wrn]] <- rep.node
        if(length(frn) == 1){
          apsimx_json$Children[[frn]] <- replacements.node
        }else{
          apsimx_json$Children[[frn[root[[2]]]]] <- replacements.node
        }
      }
    }
    
    if(missing(node.sub4child) && lvl == -1)
      stop("Failed to find and edit parameter")
  }
  
  if(missing(node.sub4child) && verbose && missing(parm)) cat("missing node.sub4child\n")  

  #### Inserting node.sub4child option March 5th 2021
  if(!missing(node.sub4child)){
    rep.node.sub4children.names <- vapply(rep.node.sub3child$Children, function(x) x$Name,
                                          FUN.VALUE = "character")
    ## Select a specific node.subchild
    wrnssssc <- grep(node.sub4child, rep.node.sub4children.names)
    rep.node.sub4child <- rep.node.sub3child$Children[[wrnssssc]]
    
    if(verbose) cat("Sub-sub-sub-subchild Name: ", rep.node.sub4child$Name,"\n")
    
    if(any(parm %in% names(rep.node.sub4child))){
      rep.node.sub4child <- edit_node(rep.node.sub4child, parm = parm, value = value)
      lvl <- 8
      # node is edited, now put it back in place
      rep.node.sub3child$Children[[wrnssssc]] <- rep.node.sub4child
      rep.node.subsubchild$Children[[wrnsssc]] <- rep.node.sub3child
      rep.node.subchild$Children[[wrnssc]] <- rep.node.subsubchild
      rep.node.child$Children[[wrnsc]] <- rep.node.subchild
      rep.node$Children[[wrnc]] <- rep.node.child
      replacements.node$Children[[wrn]] <- rep.node
      if(length(frn) == 1){
        apsimx_json$Children[[frn]] <- replacements.node
      }else{
        apsimx_json$Children[[frn[root[[2]]]]] <- replacements.node
      }
      ## apsimx_json is ready to be written back to file
    }
    
    ## Cultivar parameters can be in 'Command'
    if(length(rep.node.sub4child$Command) > 0){
      if(any(grepl(parm, unlist(rep.node.sub4child$Command)))){
        lvl <- 9
        wrnsssscc <- grep(parm, unlist(rep.node.sub4child$Command))
        ## Break it up and reassemble
        cmdstrng <- strsplit(rep.node.sub4child$Command[[wrnsssscc]], "=")[[1]][1]
        rep.node.sub4child$Command[[wrnsssscc]] <- paste0(cmdstrng, "=", value)
        ## Now write back to list
        rep.node.sub3child$Children[[wrnssssc]] <- rep.node.sub4child
        rep.node.subsubchild$Children[[wrnsssc]] <- rep.node.sub3child
        rep.node.subchild$Children[[wrnssc]] <- rep.node.subsubchild
        rep.node.child$Children[[wrnsc]] <- rep.node.subchild
        rep.node$Children[[wrnc]] <- rep.node.child
        replacements.node$Children[[wrn]] <- rep.node
        if(length(frn) == 1){
          apsimx_json$Children[[frn]] <- replacements.node
        }else{
          apsimx_json$Children[[frn[root[[2]]]]] <- replacements.node
        }
      }
    }
    
    if(missing(node.sub5child) && lvl == -1)
      stop("Failed to find and edit parameter")
  }  
  
  if(missing(node.sub5child) && verbose && missing(parm)) cat("missing node.sub5child\n")  
  
  #### Inserting node.sub5child option March 5th 2021
  if(!missing(node.sub5child)){
    rep.node.sub5children.names <- vapply(rep.node.sub4child$Children, function(x) x$Name,
                                          FUN.VALUE = "character")
    ## Select a specific node.subchild
    wrnsssssc <- grep(node.sub5child, rep.node.sub5children.names)
    rep.node.sub5child <- rep.node.sub4child$Children[[wrnsssssc]]
    
    if(verbose) cat("Sub-sub-sub-sub-subchild Name: ", rep.node.sub5child$Name,"\n")
    
    if(any(parm %in% names(rep.node.sub5child))){
      rep.node.sub5child <- edit_node(rep.node.sub5child, parm = parm, value = value)
      lvl <- 10
      # node is edited, now put it back in place
      rep.node.sub4child$Children[[wrnsssssc]] <- rep.node.sub5child
      rep.node.sub3child$Children[[wrnssssc]] <- rep.node.sub4child
      rep.node.subsubchild$Children[[wrnsssc]] <- rep.node.sub3child
      rep.node.subchild$Children[[wrnssc]] <- rep.node.subsubchild
      rep.node.child$Children[[wrnsc]] <- rep.node.subchild
      rep.node$Children[[wrnc]] <- rep.node.child
      replacements.node$Children[[wrn]] <- rep.node
      if(length(frn) == 1){
        apsimx_json$Children[[frn]] <- replacements.node
      }else{
        apsimx_json$Children[[frn[root[[2]]]]] <- replacements.node
      }
      ## apsimx_json is ready to be written back to file
    }
    
    ## Cultivar parameters can be in 'Command'
    if(length(rep.node.sub5child$Command) > 0){
      if(any(grepl(parm, unlist(rep.node.sub5child$Command)))){
        lvl <- 11
        wrnssssscc <- grep(parm, unlist(rep.node.sub5child$Command))
        ## Break it up and reassemble
        cmdstrng <- strsplit(rep.node.sub5child$Command[[wrnssssscc]], "=")[[1]][1]
        rep.node.sub5child$Command[[wrnssssscc]] <- paste0(cmdstrng, "=", value)
        ## Now write back to list
        rep.node.sub4child$Children[[wrnsssssc]] <- rep.node.sub5child
        rep.node.sub3child$Children[[wrnssssc]] <- rep.node.sub4child
        rep.node.subsubchild$Children[[wrnsssc]] <- rep.node.sub3child
        rep.node.subchild$Children[[wrnssc]] <- rep.node.subsubchild
        rep.node.child$Children[[wrnsc]] <- rep.node.subchild
        rep.node$Children[[wrnc]] <- rep.node.child
        replacements.node$Children[[wrn]] <- rep.node
        if(length(frn) == 1){
          apsimx_json$Children[[frn]] <- replacements.node
        }else{
          apsimx_json$Children[[frn[root[[2]]]]] <- replacements.node
        }
      }
    }
    
    if(lvl == -1)
      stop("Failed to find and edit parameter")
  }
  
  ## Write back to a file
  if(overwrite == FALSE){
    wr.path <- paste0(wrt.dir,"/",
                      strsplit(file,".",fixed = TRUE)[[1]][1],
                      edit.tag,".apsimx")
  }else{
    wr.path <- paste0(wrt.dir,"/",file)
  }
  
  jsonlite::write_json(apsimx_json, path = wr.path, 
                       pretty = TRUE, digits = NA, 
                       auto_unbox = TRUE, null = "null")
  
  if(verbose){
    if(!missing(node)) cat("Edited (node): ",node, "\n")
    if(!missing(node.child)) cat("Edited (node.child): ", node.child,"\n")
    if(!missing(node.subchild)) cat("Edited (node.subchild): ", node.subchild,"\n")
    if(!missing(node.subsubchild)) cat("Edited (node.subsubchild): ", node.subsubchild,"\n")
    if(!missing(node.sub3child)) cat("Edited (node.sub3child): ", node.sub3child,"\n")
    cat("Edit (level): ", lvl,"\n")
    cat("Edited parameter: ",parm, "\n")
    cat("New values: ",value, "\n")
    cat("Created: ",wr.path,"\n")
  }
}

edit_node <- function(x, parm = NULL, value = NULL){
  
  ## parm is presumably a component in names of x
  
  if(length(names(x)) == 0) stop("no 'names' in node to edit")
  
  if(is.null(parm) | is.null(value)) stop("parm or value are missing")
  
  if(length(parm) == 1){
    x.nms <- names(x)
##    print(x.nms)
    ## wne <- which(x.nms == parm)
    wne <- grep(parm, x.nms)
##    if(length(x[[wne]]) != 1) stop("value should be of length = 1")
    x[[wne]] <- value
  }
  
  if(length(parm) > 1){
    x.nms <- names(x)
    wne <- grepl(parm, x.nms)
    j <- 1
    for(i in wne){
      x[[i]] <- value[j]
      j <- j + 1
    }
  }
  return(x)
}





