#' 
#' @title Inspect a replacement component in an .apsimx (JSON) file
#' @name inspect_apsimx_replacement
#' @description inspect the replacement componenet of an JSON apsimx file. It does not replace the GUI, but it can save time by quickly checking parameters and values.
#' @param file file ending in .apsimx to be inspected (JSON)
#' @param src.dir directory containing the .apsimx file to be inspected; defaults to the current working directory
#' @param node specific node to be inspected
#' @param node.child specific node child component to be inspected.
#' @param node.subchild specific node sub-child to be inspected.
#' @param node.subsubchild specific node sub-subchild to be inspected.
#' @param node.sub3child specific node sub-sub-subchild to be inspected.
#' @param node.string passing of a string instead of the node hierarchy.
#' Do not use this and also the other node arguments. This argument will
#' overwrite the other node specifications.
#' @param root \sQuote{root} for the inspection of a replacement file (it gives flexibility to inspect other types of files).
#' @param parm specific parameter to display
#' @param display.available logical. Whether to display available components to be inspected (default = FALSE)
#' @param digits number of decimals to print (default 3)
#' @param print.path print the path to the inspected parameter (default FALSE)
#' @param verbose whether to print additional information, default: TRUE
#' @details This is simply a script that prints the relevant parameters which are likely to need editing. It does not print all information from an .apsimx file.
#' @note I need to make some changes in order to be able to handle multiple parameters. At this point, it
#' might work but it will generate warnings.
#' @return table with inspected parameters and values
#' @export
#' @examples 
#' \donttest{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
#'                            node = "Maize", node.child = "Phenology",
#'                            node.subchild = "ThermalTime", 
#'                            node.subsubchild = "BaseThermalTime",
#'                            node.sub3child = "TemperatureResponse") 
#'}
#'\dontrun{  
#' ## This function can also be used to inspect more complex APSIM-X files
#' ## For example the 'Factorial' example
#' ex.dir <- auto_detect_apsimx_examples()
#' inspect_apsimx_replacement("Factorial", src.dir = ex.dir, 
#' root = list("Experiment", 1), node = "Base", node.child = "Weather")
#' }
#'

inspect_apsimx_replacement <- function(file = "", src.dir = ".", 
                                       node = NULL, node.child = NULL,
                                       node.subchild = NULL, node.subsubchild = NULL,
                                       node.sub3child = NULL, node.string = NULL,
                                       root = list("Models.Core.Replacements", NA),
                                       parm = NULL, display.available = FALSE,
                                       digits = 3, print.path = FALSE,
                                       verbose = TRUE){
  
  .check_apsim_name(file)
  
  file.names <- dir(path = src.dir, pattern=".apsimx$", ignore.case=TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsimx files in the specified directory to inspect.")
  }
  
  ## This matches the specified file from a list of files
  ## Notice that the .apsimx extension will be added here
  file <- match.arg(file, file.names)
  
  apsimx_json <- jsonlite::read_json(paste0(src.dir, "/", file))
  
  parm.path.0 <- paste0(".", apsimx_json$Name)
  ans <- parm.path.0
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
  
  parm.path.0.1 <- paste0(parm.path.0, ".",replacements.node$Name)
  ## Print names of replacements
  replacements.node.names <- vapply(replacements.node$Children, function(x) x$Name, 
                                    FUN.VALUE = "character")
  if(verbose) cat("Replacements: ", replacements.node.names, "\n")
  
  if(!missing(node.string)){
    nodes <- strsplit(node.string, ".", fixed = TRUE)[[1]]
    node <- nodes[1]
    if(!is.na(nodes[2])) node.child <- nodes[2]
    if(!is.na(nodes[3])) node.subchild <- nodes[3]
    if(!is.na(nodes[4])) node.subsubchild <- nodes[4]
    if(!is.na(nodes[5])) node.sub3child <- nodes[5]
  }
  ## This handles a missing node 'gracefully'
  if(missing(node)){
    parm.path <- parm.path.0.1
    if(print.path) cat("Parm path:",parm.path,"\n") 
    if(verbose) cat("Please provide a node \n")
    return(invisible(parm.path))
  } 
  
  if(verbose) cat("node:", node, "\n")

  # if(display.available){
  #   cat("Level: node \n")
  #   str_list(node)
  # }
  
  ## wrn <- grep(node, replacements.node$Children) old version
  wrn <- grep(node, replacements.node.names)
  if(length(wrn) > 1) stop("node should result in a unique result")
  if(length(wrn) == 0) stop("node not found")
  rep.node <- replacements.node$Children[[wrn]] ## Is this robust enough?
  ## This last object is a list with Children
  
  parm.path.0.1.1 <- paste0(parm.path.0.1,".",rep.node$Name)
  
  if(!is.null(rep.node$CropType) && verbose) cat("CropType", rep.node$CropType, "\n")
  
  ## This tries to handle the fact that ther paramter might be at this
  ## high of a level
  if(!missing(parm) && any(parm %in% names(rep.node))){
    unpack_node(rep.node, parm = parm, display.available = display.available)
  }
  
  ## Available node children
  rep.node.children.names <- vapply(rep.node$Children, function(x) x$Name,
                                    FUN.VALUE = "character")
  if(display.available){
    cat("Level: node \n")
    str_list(rep.node)
  } 
  
  ## If node.child is missing try to handle it gracefully
  if(missing(node.child)){
    parm.path <- parm.path.0.1.1
    if(print.path) cat("Parm path: ", parm.path, "\n") 
    if(verbose) cat("missing node.child \n")
    return(invisible(parm.path))
  } 
  
  if(verbose) cat("node child:", node.child, "\n")
  
  wrnc <- grep(node.child, rep.node.children.names)
  if(length(wrnc) == 0) stop("node.child not found")
  rep.node.child <- rep.node$Children[[wrnc]]
  
  parm.path.0.1.1.1 <- paste0(parm.path.0.1.1,".",rep.node.child$Name)
  
  ## If children are missing display data at this level
  ## Conditions for stopping here:
  ## 1. There are no children OR parm is not null AND
  ## 2. node.subchild is missing
  if((length(rep.node.child$Children) == 0 || !is.null(parm)) && missing(node.subchild)){
    unpack_node(rep.node.child, parm = parm, display.available = display.available)
    parm.path <- parm.path.0.1.1.1
    if(print.path) cat("Parm path: ", format_parm_path(parm.path, parm), "\n")
    if(verbose) cat("no node sub-children available or parm not equal to null \n")
    return(invisible(format_parm_path(parm.path, parm)))
  }else{
    if(display.available) str_list(rep.node.child)
  }
  
  if(verbose) cat("node subchild:", node.subchild, "\n")
  ## This is intended to be used to handle a missing node.subchild gracefully
  if(missing(node.subchild)){
    parm.path <- parm.path.0.1.1.1
    if(print.path) cat("Parm path: ", parm.path, "\n") 
    if(verbose) cat("missing node.subchild \n")
    return(invisible(parm.path))
  } 
  
  rep.node.subchildren.names <- vapply(rep.node.child$Children, function(x) x$Name,
                                       FUN.VALUE = "character")
  wrnsc <- grep(node.subchild, rep.node.subchildren.names)
  if(length(wrnsc) == 0) stop("node.subchild not found")
  rep.node.subchild <- rep.node.child$Children[[wrnsc]]
  
  parm.path.0.1.1.1.1 <- paste0(parm.path.0.1.1.1,".",rep.node.subchild$Name)
  ## Let's just print this information somehow
  if(verbose) cat("Subchild Name: ", rep.node.subchild$Name, "\n")
  
  if((length(rep.node.subchild$Children) == 0 || !is.null(parm)) && missing(node.subsubchild)){
    unpack_node(rep.node.subchild, parm = parm, display.available = display.available)
    parm.path <- parm.path.0.1.1.1.1
    if(print.path) cat("Parm path:", parm.path, "\n")
    if(verbose) cat("no node sub-sub-children available or parm is not null \n")
    return(invisible(format_parm_path(parm.path)))
  }else{
    ## The problem here is that rep.node.subchild can either be
    ## named or nameless
    if(display.available) str_list(rep.node.subchild)
  }
  
  ## This is intended to be used to handle a missing node.subsubchild gracefully
  if(missing(node.subsubchild)){
    parm.path <- parm.path.0.1.1.1.1
    if(print.path) cat("Parm path:", parm.path, "\n") 
    if(verbose) cat("missing node.subsubchild \n")
    return(invisible(parm.path))
  }
  
  ## The thing here is that I need to be able to handle an object with either
  ## named Children or
  ## unnamed Children
  rep.node.subsubchildren.names <- vapply(rep.node.subchild$Children, function(x) x$Name,
                                          FUN.VALUE = "character")
  wrnssc <- grep(node.subsubchild, rep.node.subsubchildren.names)
  if(length(wrnssc) == 0) stop("node.subsubchild not found")
  rep.node.subsubchild <- rep.node.subchild$Children[[wrnssc]]
  
  if(verbose) cat("Name sub-sub-child: ", rep.node.subsubchild$Name, "\n")
  parm.path.0.1.1.1.1.1 <- paste0(parm.path.0.1.1.1.1,".",rep.node.subsubchild$Name)
  
##  if(FALSE){
##    if(length(names(rep.node.subchild$Children)) == 0){
##        ## For some reason at this level the Children are not named
##        rep.node.subsubchild <- rep.node.subchild$Children[[1]]
##      }else{
##        rep.node.subsubchild <- rep.node.subchild$Children
##      }
##  }

  rep.node.subsubchild.names <- names(rep.node.subsubchild)

  ## I also need to check that parameter is not in 'Command'
  ## ispic: Is Parameter In Command?
  if(length(rep.node.subsubchild$Command) > 0 && !is.null(parm)){
    ispic <- any(grepl(parm, unlist(rep.node.subsubchild$Command)))
  }else{
    ispic <- FALSE
  }
  
  if((length(rep.node.subsubchild$Children) == 0 || !is.null(parm)) && !ispic && missing(node.sub3child)){
      unpack_node(rep.node.subsubchild, parm = parm, display.available = display.available)
      ## It might be that 'unpack' is not always the best choice and simply cat_parm might bbe better
      ## cat_parm(rep.node.subsubchild, parm = parm)
      parm.path <- parm.path.0.1.1.1.1.1
      if(print.path) cat("Parm path:", format_parm_path(parm.path,parm), "\n") 
      if(verbose) cat("no node sub-sub-sub-children available or parm is not null \n")
      return(invisible(format_parm_path(parm.path,parm)))
  }
    
  if(is.null(parm)){ 
    if(length(rep.node.subsubchild.names) == 0){
        rep.node.sub3child <- rep.node.subsubchild$Children[[1]]
    }else{
        rep.node.sub3child <- rep.node.subsubchild$Children
    }
    if(is.null(node.sub3child)){
      unpack_node(rep.node.sub3child, parm = NULL, display.available = display.available)
    }
  }
  
  if(!is.null(parm) && any(parm %in% rep.node.subsubchild.names)){
      wrnsspc <- grep(parm, rep.node.subsubchild.names)
      rep.node.sub3child <- rep.node.subsubchild[wrnsspc]
      cat_parm(rep.node.sub3child, parm = parm)
  }else{
    if(!is.null(parm) && any(parm %in% unlist(rep.node.subsubchild$Command))){
      wcp <- grep(parm, unlist(rep.node.subsubchild$Command))
      cat(unlist(rep.node.subsubchild$Command)[wcp])
    }else{
      if(!is.null(parm) && missing(node.sub3child)) stop("Parameter not found")
    }
  }
  
  ## At this point it seems that this should work
  ## Node at the third level should go here
  ## This is intended to be used to handle a missing node.subsubchild gracefully
  if(missing(node.sub3child)){
    parm.path <- parm.path.0.1.1.1.1.1
    if(print.path) cat("Parm path:", parm.path, "\n") 
    if(verbose) cat("missing node.sub3child \n")
    return(invisible(parm.path))
  }
  
  rep.node.sub3children.names <- vapply(rep.node.subsubchild$Children, function(x) x$Name,
                                       FUN.VALUE = "character")
  wrnsssc <- grep(node.sub3child, rep.node.sub3children.names)
  if(length(wrnsssc) == 0) stop("node.sub3child not found")
  rep.node.sub4child <- rep.node.subsubchild$Children[[wrnsssc]]
  
  if(verbose) cat("Name sub-sub-sub-child: ", rep.node.sub4child$Name, "\n")
  parm.path.0.1.1.1.1.1.1 <- paste0(parm.path.0.1.1.1.1.1,".",rep.node.sub4child$Name)

  ## Why does this work instead of the extraction through x$Name?
  rep.node.sub4child.names <- names(rep.node.sub4child)
  
  ## I also need to check that parameter is not in 'Command'
  ## ispic: Is Parameter In Command?
  if(length(rep.node.sub4child$Command) > 0 && !is.null(parm)){
    ispic <- any(grepl(parm, unlist(rep.node.sub4child$Command)))
  }else{
    ispic <- FALSE
  }
  
  if((length(rep.node.sub4child$Children) == 0 || !is.null(parm)) && !ispic){
    ## unpack_node(rep.node.sub4child, parm = parm, display.available = display.available)
    cat_parm(rep.node.sub4child, parm = parm)
    parm.path <- parm.path.0.1.1.1.1.1.1
    if(print.path) cat("Parm path:", format_parm_path(parm.path,parm), "\n") 
    ## if(verbose) cat("no node sub-sub-sub-sub-children available or parm is not null \n")
    return(invisible(format_parm_path(parm.path,parm)))
  }
  
  if(is.null(parm)){ 
    if(length(rep.node.sub4child.names) == 0){
      rep.node.sub4child <- rep.node.sub3child$Children[[1]]
    }else{
      rep.node.sub4child <- rep.node.sub3child$Children
    }
    unpack_node(rep.node.sub4child, parm = NULL, display.available = display.available)
  }
  
  if(!is.null(parm) && any(grepl(parm, rep.node.sub4child.names))){
    wrnssspc <- grep(parm, rep.node.sub4child.names)
    rep.node.sub4child <- rep.node.sub4child[wrnssspc]
    cat_parm(rep.node.sub4child, parm = parm) ## This might be wrong, without parm argument
  }else{
    if(!is.null(parm) && any(grepl(parm, unlist(rep.node.sub4child$Command)))){
      wcp <- grep(parm, unlist(rep.node.sub4child$Command))
      cat(unlist(rep.node.sub4child$Command)[wcp])
    }else{
      if(!is.null(parm)) stop("Parameter not found")
    }
  }
  
  if(print.path){
    cat("Parm path:", parm.path, "\n")
    if(is.null(parm)){
      ans <- parm.path
    }else{
      if(length(parm) == 1){
        ans <- paste0(parm.path,".",parm)
      }else{
        ans <- paste0(parm.path,".",parm[[1]])
      }
    }
  }
  invisible(ans)
}


## I will use this function to unpack a node when it is time to print
## Not exported
unpack_node <- function(x, parm = NULL, display.available = FALSE){
  
  if(!is.list(x)) stop("x should be a list")
  ## I will do just three levels of unpacking
  ## x is a node
  lnode <- length(x)
  
  node.names <- names(x)
  if(display.available) cat("Available node children (unpack_node): ", node.names, "\n")
  
  ## Let's try to handle the different elements that x can be
  ## If it is a list of length one and just one element, just cat
  ## the key, value pair
  if(is.list(x) & lnode == 1 & length(x[[1]]) == 1){
    return(cat("Key: ", names(x), "; Value: ", x[[1]], "\n"))
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
    if(is.list(node.child) && 
       length(node.child$Children) == 0 &&
       length(node.child) == 1 && 
       length(node.child[[1]]) == 1){
      cat_parm(node.child, parm = parm)
    }else{
      ## Let's assume this is a list with multiple elements
      ## Shouldn't 'cat_parm' be able to handle this?
      if(length(node.child) == 1 & length(x[[i]]) > 1){
        ## This is an element such as 'Command'
        tcp <- try(cat_parm(x[[i]], parm = parm), silent = TRUE)
        if(class(tcp) != "try-error"){
          cat(names(node.child), "\n")
          tcp
        }
      }
      ## Let's handle 'Children' now
      if(length(node.child$Children) != 0){
        for(j in seq_along(node.child$Children)){
          if(names(node.child) == 0 || !is.null(node.child$Children[[1]])){
            node.subchild <- node.child$Children[[1]]
          }else{
            node.subchild <- node.child$Children
          }
          try(cat_parm(node.subchild, parm = parm), silent = TRUE)
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
      if(x.nms[i] %in% parm || any(sapply(parm, function(x) grepl(x, unlist(x[i]))))){
        cat(x.nms[i], ":",unlist(x[i]),"\n")
      }
    }
  }
}

## list structure
str_list <- function(x){
  ## List name
  cat("list Name:",x$Name,"\n")
  ## Number of elements
  ln <- length(x)
  cat("list length:",ln,"\n")
  ## What are the names of the elements
  lnms <- names(x)
  cat("list names:",lnms,"\n")
  ## Are Children present?
  if(!is.null(x$Children)){
    cat("Children: Yes \n")
    cln <- length(x$Children)
    cat("Children length:",cln,"\n")
    cnms <- names(x$Children)
    if(length(cnms) != 0) cat("Children names:",cnms,"\n")
    if(length(cnms) == 0 && cln > 0){
      cNms <- sapply(x$Children, function(x) x$Name)
      cat("Children Names:",cNms,"\n")
    }
  }
  invisible(list(ln=ln,lnms=lnms,cln=cln,cnms=cnms,cNms=cNms))
}

format_parm_path <- function(x, parm = NULL){
  if(is.null(parm)){
    ans <- x
  }else{
    if(length(parm) == 1){
      ans <- paste0(x,".",parm)
    }else{
      ans <- paste0(x,".",parm[[1]])
    }    
  }
 ans
}

## Note: In the package I distribute some files which have additional 'replacements'
## The specific replacement needs to be incorporated by copying code from
## ApsimX/Models/Resources/
## Or: https://github.com/APSIMInitiative/ApsimX/tree/master/Models/Resources
