
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
#' @param root 'root' node to explore (default = "Models.Core.Replacements")
#' @param parm specific parameter to edit
#' @param value new values for the parameter
#' @param overwrite logical; if \code{TRUE} the old file is overwritten, a new file is written otherwise
#' @param edit.tag if the file is edited a different tag from the default '-edited' can be used.
#' @param verbose whether to print information about successful edit
#' @details This is simply a script that prints the relevant parameters which are likely to need editing. It does not print all information from an .apsimx file.
#' @return (when verbose=TRUE) complete file path to edited .apsimx file is returned as a character string.
#' As a side effect this function creates a new (JSON) .apsimx file.
#' @note The components that can be edited are restricted becuase this is better in preventing
#' errors of editing unintended parts of the file.
#' @export
#' @examples 
#' \dontrun{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' edit_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
#'                            node = "Maize", node.child = "Phenology",
#'                            node.subchild = "ThermalTime", parm = c("X","Y"),
#'                            value = c(1,2,3,4,5)) 
#' }
#'

edit_apsimx_replacement <- function(file = "", src.dir = ".", wrt.dir = ".",
                                    node = NULL, node.child = NULL,
                                    node.subchild = NULL, node.subsubchild = NULL,
                                    root = "Models.Core.Replacements",
                                    parm = NULL, value = NULL, overwrite = FALSE,
                                    edit.tag = "-edited", verbose = TRUE){
  
  fileNames <- dir(path = src.dir, pattern=".apsimx$",ignore.case=TRUE)
  
  if(length(fileNames)==0){
    stop("There are no .apsimx files in the specified directory to edit.")
  }
  
  ## This matches the specified file from a list of files
  ## Notice that the .apsimx extension will be added here
  file <- match.arg(file, fileNames, several.ok=FALSE)
  
  if(missing(parm) | missing(value)) stop("'parm' and/or 'value' are missing")
  
  apsimx_json <- read_json(paste0(src.dir,"/",file))
  
  ## Select Replacements node
  frn <- grep(root, apsimx_json$Children, fixed = TRUE)
  replacements.node <- apsimx_json$Children[[frn]]
  
  ## Print names of replacements
  replacements.node.names <- sapply(replacements.node$Children, function(x) x$Name)
  if(verbose) cat("Replacements: ", replacements.node.names, "\n")
  
  if(missing(node)) return(cat("Please provide a node \n"))
  ## Let's call this level = 0, at the 'node' level (nothing to edit)
  lvl <- 0
  wrn <- grep(node, replacements.node$Children)
  rep.node <- replacements.node$Children[[wrn]]
  
  if(!is.null(rep.node$CropType) & verbose) cat("CropType", rep.node$CropType,"\n")
  
  rep.node.children.names <- sapply(rep.node$Children, function(x) x$Name)
  if(verbose) cat("Available node children: ",rep.node.children.names,"\n")
  
  ## Select a specific node.child
  if(missing(node.child)) return(cat("missing node.child\n"))
  ## Node.child would be level = 1, still nothing to edit
  wrnc <- grep(node.child, rep.node.children.names)
  rep.node.child <- rep.node$Children[[wrnc]]
  
  ## Is it possible that we want to edit things at this level?
  ## Unlikely, but here it is
  if(parm %in% names(rep.node.child)){
    lvl <- 1
    rep.node.child <- edit_node(rep.node.child, parm = parm, value = value)
    rep.node$Children[[wrnc]] <- rep.node.child
    replacements.node$Children[[wrn]] <- rep.node
    apsimx_json$Children[[frn]] <- replacements.node
    ## apsimx_json is ready to be written back to file
  } 
  
  rep.node.subchildren.names <- sapply(rep.node.child$Children, function(x) x$Name)
  ## Select a specific node.subchild
  if(missing(node.subchild)) cat("missing node.subchild\n")
  wrnsc <- grep(node.subchild, rep.node.subchildren.names)
  rep.node.subchild <- rep.node.child$Children[[wrnsc]]
  
  if(verbose) cat("Subchild Name: ", rep.node.subchild$Name,"\n")
  
  if(parm %in% names(rep.node.subchild)){
    rep.node.subchild <- edit_node(rep.node.subchild, parm = parm, value = value)
    lvl <- 2
    rep.node.child$Children[[wrnsc]] <- rep.node.subchild
    rep.node$Children[[wrnc]] <- rep.node.child
    replacements.node$Children[[wrn]] <- rep.node
    apsimx_json$Children[[frn]] <- replacements.node
    ## apsimx_json is ready to be written back to file
  } 
  
  rep.node.subsubchildren.names <- sapply(rep.node.subchild$Children, function(x) x$Name)
  ## Select a specific node.subchild
  if(missing(node.subsubchild)) return(cat("missing node.subsubchild\n"))
  wrnssc <- grepl(node.subsubchild, rep.node.subsubchildren.names)
  rep.node.subsubchild <- rep.node.subchild$Children[[wrnssc]]
  
  if(verbose) cat("Subsubchild Name: ", rep.node.subsubchild$Name,"\n")
  
  if(length(names(rep.node.subsubchild$Children)) == 0){
    rep.node.sub3child <- rep.node.subsubchild$Children[[1]] 
  }else{
    rep.node.sub3child <- rep.node.subsubchild$Children
  }
  
  if(parm %in% names(rep.node.sub3child)){
    rep.node.sub3child <- edit_node(rep.node.sub3child, parm = parm, value = value)
    lvl <- 4
    if(length(names(rep.node.subsubchild$Children)) == 0){
      rep.node.subsubchild$Children[[1]] <- rep.node.sub3child
    }else{
      rep.node.subsubchild$Children <- rep.node.sub3child 
    }
    rep.node.subchild$Children[[wrnssc]] <- rep.node.subsubchild
    rep.node.child$Children[[wrnsc]] <- rep.node.subchild
    rep.node$Children[[wrnc]] <- rep.node.child
    replacements.node$Children[[wrn]] <- rep.node
    apsimx_json$Children[[frn]] <- replacements.node
    ## apsimx_json is ready to be written back to file
  } 

  ## Write back to a file
  if(overwrite == FALSE){
    wr.path <- paste0(wrt.dir,"/",
                      strsplit(file,".",fixed = TRUE)[[1]][1],
                      edit.tag,".apsimx")
  }else{
    wr.path <- paste0(wrt.dir,"/",file)
  }
  
  write_json(apsimx_json, path = wr.path, 
             pretty = TRUE, digits = NA, 
             auto_unbox = TRUE, null = "null")
  
  if(verbose){
    cat("Edited (node): ",node, "\n")
    cat("Edited (node.child): ", node.child,"\n")
    cat("Edited (node.subchild): ", node.subchild,"\n")
    cat("Edited (node.subsubchild): ", node.subsubchild,"\n")
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
    wne <- which(x.nms == parm)
    ## x should be a list
##    if(length(x[[wne]]) != 1) stop("value should be of length = 1")
    x[[wne]] <- value
  }
  
  if(length(parm) > 1){
    x.nms <- names(x)
    wne <- grepl(parm, n.nms)
    j <- 1
    for(i in wne){
      x[[i]] <- value[j]
      j <- j + 1
    }
  }
  return(x)
}





