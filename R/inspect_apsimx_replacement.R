
#' 
#' @title Inspect an replacement component in an .apsimx (JSON) file
#' @name inspect_apsimx_replacement
#' @description inspect the replacement componenet of an JSON apsimx file. It does not replace the GUI, but it can save time by quickly checking parameters and values.
#' @param file file ending in .apsimx to be inspected (JSON)
#' @param src.dir directory containing the .apsimx file to be inspected; defaults to the current working directory
#' @param node specific node to be inspected
#' @param node.child specific node child component to be inspected.
#' @param node.subchild specific node sub-child to be inspected.
#' @param parm specific parameter to display
#' @display.available logical. Whether to display available components to be inspected (default = FALSE)
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
                                        node.subchild = NULL, parm = NULL, display.available = FALSE,
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
  
  if(missing(node) & length(replacements.node.names == 1)) node <- replacements.node.names
  
  wrn <- grep(node, replacements.node$Children)
  rep.node <- replacements.node$Children[[wrn]]
  
  if(!is.null(rep.node$CropType)) cat("CropType", rep.node$CropType,"\n")
  
  ## Available node children
  rep.node.children.names <- sapply(rep.node$Children, function(x) x$Name)
  if(display.available) cat("Available node children: ", rep.node.children.names,"\n")
  
  ## Select a specific node.child
  if(missing(node.child)) stop("missing node.child")
  wrnc <- which(rep.node.children.names == node.child)
  rep.node.child <- rep.node$Children[[wrnc]]
  
  ## Available node sub-children
  rep.node.subchildren.names <- sapply(rep.node.child$Children, function(x) x$Name)
  if(display.available) cat("Available node sub-children: ", rep.node.subchildren.names,"\n")
  
  ## Select a specific node.subchild
  wrnsc <- which(rep.node.subchildren.names == node.subchild)
  rep.node.subchild <- rep.node.child$Children[[wrnsc]]
  
  ## Let's just print this information somehow
  cat("Subchild Name: ", rep.node.subchild$Name,"\n")
  
  for(i in seq_len(length(rep.node.subchild$Children))){
    children.subchild <- rep.node.subchild$Children[[i]]
    if(length(children.subchild$Children) != 0){
      cat("Subchild children name: ", children.subchild$Name,"\n")
      ## Here I make the assumtion that at this level there
      ## is only one component within children.subchild$Children
      lscc <- length(children.subchild$Children[[1]])
      for(j in seq_len(lscc)){
        if(missing(parm)){
        cat("Name: ",names(children.subchild$Children[[1]][j]),";",
              "Value: ", unlist(children.subchild$Children[[1]][j]),"\n")
        }else{
          if(names(children.subchild$Children[[1]][j]) %in% parm){
            cat("Name: ",names(children.subchild$Children[[1]][j]),";",
                "Value: ", unlist(children.subchild$Children[[1]][j]),"\n")
          }
        }
      }
    }
  }
}
