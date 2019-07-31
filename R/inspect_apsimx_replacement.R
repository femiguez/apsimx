
#' 
#' @title Inspect an replacement component in an .apsimx (JSON) file
#' @name inspect_apsimx_replacement
#' @description inspect the replacement componenet of an JSON apsimx file. It does not replace the GUI, but it can save time by quickly checking parameters and values.
#' @param file file ending in .apsimx to be inspected (JSON)
#' @param src.dir directory containing the .apsimx file to be inspected; defaults to the current working directory
#' @param node specific node to be inspected
#' @param node.child specific node child component to be inspected.
#' @param node.subchild specific node sub-child to be inspected.
#' @param node.subsubchild specific node sub-subchild to be inspected.
#' @param parm specific parameter to display
#' @param display.available logical. Whether to display available components to be inspected (default = FALSE)
#' @param digits number of decimals to print (default 3)
#' @details This is simply a script that prints the relevant parameters which are likely to need editing. It does not print all information from an .apsimx file.
#' @return table with inspected parameters and values
#' @export
#' @examples 
#' \dontrun{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
#'                            node = "Maize", node.child = "Phenology",
#'                            node.subchild = "ThermalTime", parm = c("X","Y")) 
#' }
#'

inspect_apsimx_replacement <- function(file = "", src.dir = ".", node = NULL, node.child = NULL,
                                        node.subchild = NULL, node.subsubchild = NULL,
                                       parm = NULL, display.available = FALSE,
                                       digits = 3){
  
  fileNames <- dir(path = src.dir, pattern=".apsimx$",ignore.case=TRUE)
  
  if(length(fileNames)==0){
    stop("There are no .apsimx files in the specified directory to inspect.")
  }
  
  ## This matches the specified file from a list of files
  ## Notice that the .apsimx extension will be added here
  file <- match.arg(file, fileNames, several.ok=FALSE)
  
  apsimx_json <- read_json(paste0(src.dir,"/",file))
  
  ## Select Replacements node
  frn <- grep("Models.Core.Replacements", apsimx_json$Children, fixed = TRUE)
  replacements.node <- apsimx_json$Children[[frn]]
  
  ## Print names of replacements
  replacements.node.names <- sapply(replacements.node$Children, function(x) x$Name)
  cat("Replacements: ", replacements.node.names, "\n")
  
  if(missing(node)) return(cat("Please provide a node\n"))
  
  wrn <- grep(node, replacements.node$Children)
  rep.node <- replacements.node$Children[[wrn]]
  
  if(!is.null(rep.node$CropType)) cat("CropType", rep.node$CropType,"\n")
  
  ## Available node children
  rep.node.children.names <- sapply(rep.node$Children, function(x) x$Name)
  if(display.available) cat("Available node children: ",rep.node.children.names,"\n")
  
  ## Select a specific node.child
  if(missing(node.child)) return(cat("missing node.child\n"))
  wrnc <- which(rep.node.children.names == node.child)
  rep.node.child <- rep.node$Children[[wrnc]]
  
  ## If children are missing display data at this level
  if(length(rep.node.child$Children) == 0){
    unpack_node(rep.node.child, parm = parm, display.available = display.available)
    return(cat("no node sub-children available \n"))
  }else{
    rep.node.subchildren.names <- sapply(rep.node.child$Children, function(x) x$Name)
    if(display.available){ 
      cat("Available node sub-children: ",rep.node.subchildren.names,"\n")
    }
  }
  
  ## Select a specific node.subchild
  if(missing(node.subchild)) return(cat("missing node.subchild\n"))
  wrnsc <- which(rep.node.subchildren.names == node.subchild)
  rep.node.subchild <- rep.node.child$Children[[wrnsc]]
  
  ## Let's just print this information somehow
  cat("Subchild Name: ", rep.node.subchild$Name,"\n")
  
  if(is.atomic(rep.node.subchild$Children)){
    unpack_node(rep.node.subchild, parm = parm, display.available = display.available)
    return(cat("no node sub-sub-children available \n"))
  }else{

    if(length(names(rep.node.subchild$Children)) == 0){
      ## For some reason at this level the Children are not named
      rep.node.subsubchild <- rep.node.subchild$Children[[1]]
    }else{
      rep.node.subsubchild <- rep.node.subchild$Children
    }
    cat("Name sub-sub-child: ", rep.node.subsubchild$Name,"\n")
    
    rep.node.subsubchild.names <- names(rep.node.subsubchild)
    if(display.available){ 
      cat("Available node sub-sub-child: ",rep.node.subsubchild.names,"\n")
    }
    
    if(is.atomic(rep.node.subsubchild$Children)){
      unpack_node(rep.node.subsubchild, parm = parm, display.available = display.available)
      return(cat("no node sub-sub-sub-children available \n"))
    }
    
    if(missing(node.subsubchild)){ 
      if(length(rep.node.subsubchild.names) == 0){
        rep.node.sub3child <- rep.node.subsubchild$Children[[1]]
      }else{
        rep.node.sub3child <- rep.node.subsubchild$Children
      }
    }else{
      wrnssc <- which(rep.node.subsubchild.names == node.subsubchild)
      rep.node.sub3child <- rep.node.subsubchild$Children[[wrnssc]]
    }
    unpack_node(rep.node.sub3child, parm = parm, display.available = display.available)
  }
}


## I will use this function to unpack a node when it is time to print
## Not exported
unpack_node <- function(x, parm=NULL, display.available = FALSE){
  
  if(!is.list(x)) stop("x should be a list")
  ## I will do just three levels of unpacking
  ## x is a node
  lnode <- length(x)
  
  node.names <- names(x)
  if(display.available) cat("Available node children (unpack_node): ",node.names,"\n")
  
  ## Let's try to handle the different elements that x can be
  ## If it is a list of length one and just one element, just cat
  ## the key, value pair
  if(is.list(x) & length(x) == 1 & length(x[[1]]) == 1){
    return(cat("Key: ",names(x),"; Value: ",x[[1]],"\n"))
  }
  
  if(all(sapply(x, is.atomic))){
    ## This is for a list which has all atomic elements
    ## If one of the elements is an empty list
    ## This will fail
    return(cat_parm(x, parm = parm))
  }
  
  for(i in seq_len(lnode)){
    ## The components can either have multiple elements such as
    ## Command, or have children, in either case I need to unpack
    node.child <- x[i]
    ## If x is a list node.child will be a list
    ## If it has just one element, then 
    if(is.list(node.child) & length(node.child) == 1 & length(node.child[[1]]) == 1){
      cat_parm(node.child, parm = parm)
    }else{
      ## Let's assume this is a list with multiple elements
      ## Shouldn't 'cat_parm' be able to handle this?
      if(length(node.child) == 1 & length(x[[i]]) > 1){
        ## This is an element such as 'Command'
        cat(names(node.child),"\n")
        cat_parm(x[[i]], parm = parm)
      }
      ## Let's handle 'Children' now
      if(length(node.child$Children) !=0){
        for(j in seq_len(node.child$Children)){
          if(names(node.child) == 0){
            node.subchild <- node.child$Children[[1]]
          }else{
            node.subchild <- node.child$Children
          }
          cat_parm(nodel.subchild, parm = parm)
        }
      }
    }
  }
}
      
        
cat_parm <- function(x, parm = NULL){
  
  ## This will print an element or multiple elements 
  ## When x is a simple list structure, with no 
  ## Children
  lx <- length(x)
  x.nms <- names(x)
  for(i in seq_len(lx)){
    if(is.null(parm)){
      cat(x.nms[i], ":",unlist(x[i]),"\n")
    }else{
      if(x.nms[i] %in% parm){
        cat(x.nms[i], ":",unlist(x[i]),"\n")
      }
    }
  }
}

