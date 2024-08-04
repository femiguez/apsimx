#' Edit an APSIM-X (JSON) Simulation
#' 
#' This function allows editing of an APSIM-X (JSON) simulation file.
#' 
#' The variables specified by \code{parm} within the .apsimx file specified by \code{file} 
#' in the source directory \code{src.dir} are edited. The old values are replaced with \code{value}, which
#' is a list that has the same number of elements as the length of the vector \code{parm}.  The current
#' .apsimx file will be overwritten if \code{overwrite} is set to \code{TRUE}; otherwise the file
#' \sQuote{file} \emph{-edited.apsimx} will be created.  If (verbose = TRUE) then the name
#'  of the written file is returned. 
#'  
#'  When node equals \sQuote{Report}, the editing allows to add variables, but not to remove them at the moment.
#' 
#'  When node equals Operations, \sQuote{parm} should have a list with two elements. The first should be the line(s) to edit and 
#'  the second should be the component(s) to edit. Either \sQuote{Date}, \sQuote{Action} or \sQuote{Line}.
#'  When more than one line is edited, \sQuote{value} should be a character vector of the same length as the number of
#'  lines to edit. It is possible to remove, say, line 10 by using \sQuote{parm = list(-10, NA)}. It is safer to remove
#'  lines at the end of \sQuote{Operations}. To remove several use the following \sQuote{parm = list(-c(10:12), NA)}. 
#'  This assumes that \sQuote{12} is the maximum number of lines present. Trying to remove lines in the middle will have 
#'  unexpected effects. It is possible to create additional lines, but only by using \sQuote{Date} first. This feature
#'  has not been tested much so use it carefully.
#'  
#' @name edit_apsimx
#' @param file file ending in .apsimx to be edited (JSON)
#' @param src.dir directory containing the .apsimx file to be edited; defaults to the current working directory
#' @param wrt.dir should be used if the destination directory is different from the src.dir
#' @param node either \sQuote{Clock}, \sQuote{Weather}, \sQuote{Soil}, 
#' \sQuote{SurfaceOrganicMatter}, \sQuote{MicroClimate}, \sQuote{Crop}, \sQuote{Manager}, \sQuote{Report}, \sQuote{Operations} or \sQuote{Other} 
#' @param soil.child specific soil component to be edited
#' @param manager.child specific manager component to be edited
#' @param parm parameter to be edited. It can be a regular expression.
#' @param value new values for the parameter to be edited 
#' @param overwrite logical; if \code{TRUE} the old file is overwritten, a new file is written otherwise
#' @param edit.tag if the file is edited a different tag from the default \sQuote{-edited} can be used.
#' @param parm.path path to the attribute to edit when node is \sQuote{Other}
#' @param root supply the node position in the case of multiple simulations such as factorials.
#' @param verbose whether to print information about successful edit
#' @return (when verbose=TRUE) complete file path to edited .apsimx file is returned as a character string.
#' As a side effect this function creates a new (JSON) .apsimx file.
#' @export
#' @examples 
#' \donttest{
#' ## This example will read one of the examples distributed with APSIM-X
#' ## but write to a temporary directory
#' tmp.dir <- tempdir()
#' 
#' ## Edit Bulk density
#' extd.dir <- system.file("extdata", package = "apsimx")
#' bds <- c(1.02, 1.03, 1.09, 1.16, 1.18, 1.19, 1.20)
#' edit_apsimx("Wheat.apsimx", src.dir = extd.dir,
#'             wrt.dir = tmp.dir,
#'             node = "Soil",
#'             soil.child = "Physical", 
#'             parm = "BD", value = bds,
#'             verbose = FALSE)
#' ## Inspect file
#' inspect_apsimx("Wheat-edited.apsimx", src.dir = tmp.dir,
#'                 node = "Soil", soil.child = "Physical")
#' ## To delete the file...
#' file.remove(file.path(tmp.dir, "Wheat-edited.apsimx"))
#' 
#' ## Edit the fertilizer amount in 'Maize.apsimx'
#' edit_apsimx("Maize.apsimx", src.dir = extd.dir,
#'              wrt.dir = tmp.dir, node = "Manager",
#'              manager.child = "SowingFertiliser",
#'              parm = "Amount", value = 200, verbose = TRUE)
#'              
#' ## Make sure it worked
#' inspect_apsimx("Maize-edited.apsimx", src.dir = tmp.dir, 
#'                node = "Manager",
#'                parm = list("SowingFertiliser", NA))
#' 
#' ## Remove the file
#' file.remove(file.path(tmp.dir, "Maize-edited.apsimx"))
#' }
#' 

edit_apsimx <- function(file, src.dir = ".", wrt.dir = NULL,
                        node = c("Clock", "Weather", "Soil", "SurfaceOrganicMatter", "MicroClimate", "Crop", "Manager", "Report", "Operations", "Other"),
                        soil.child = c("Metadata", "Water", "SoilWater", "Organic", "Physical", "Analysis", "Chemical", "InitialWater", "Sample", "Solute", "NO3", "NH4", "Urea", "Swim3"),
                        manager.child = NULL,
                        parm = NULL, value = NULL, 
                        overwrite = FALSE,
                        edit.tag = "-edited",
                        parm.path = NULL,
                        root = NULL,
                        verbose = TRUE){
  
  if(isFALSE(apsimx.options$allow.path.spaces)) .check_apsim_name(file)
  
  if(missing(wrt.dir)) wrt.dir <- src.dir
  
  file.names <- dir(path = src.dir, pattern = ".apsimx$", ignore.case = TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsimx files in the specified directory to edit.")
  }
  
  node <- match.arg(node)
  soil.child <- match.arg(soil.child)
  edited.child <- "none"
  
  ## For now we just edit one file at a time
  file <- match.arg(file, file.names)
  
  if(apsimx_filetype(file = file, src.dir = src.dir) != "json")
    stop("This function only edits JSON files")

  if(!is.null(parm.path) && node != "Other")
    stop("When parm.path is supplied node should be 'Other'", call. = FALSE)
  
  if(is.null(value))
    stop("'value' is missing", call. = FALSE)
  
  ## Parse apsimx file (JSON)
  apsimx_json <- jsonlite::read_json(file.path(src.dir, file))
  
  ## apsimx_json <<- apsimx_json
  ## Where is the 'Core' simulation?
  children.names <- sapply(apsimx_json$Children, FUN = function(x) x$`$type`)
  wcore <- grep("Core.Simulation", children.names)
  
  parm.path.0 <- paste0(".", apsimx_json$Name) ## Root
  ## When node == "Other" root should always be missing
  ## The problem is that I have to guess it
  ## We should be able to use node == "Other" even if root is not missing
  if(node == "Other" && is.null(root) && length(wcore) > 1){
    ## Process 'parm'
    if(is.null(parm.path))
      stop("parm.path is missing")
    if(!grepl(".", parm.path, fixed = TRUE))
      stop("parm.path is not a proper json path")
    cparm <- paste0(parm.path, ".", parm)
    pparm <- strsplit(cparm, split = ".", fixed = TRUE)[[1]]
    root.name.level.0 <- gsub(".", "", parm.path.0, fixed = TRUE)
    if(pparm[2] != root.name.level.0)
      stop(paste("First parm element does not match:", root.name.level.0), call. = FALSE)
    root1 <- pparm[3] 
    if(is.na(pparm[4])){
      root <- root1
    }else{
      ## Guess if 'root' is contained in the first level of names
      root.names.level.1 <- vapply(apsimx_json$Children, FUN = function(x) x$Name, 
                                   FUN.VALUE = "character")
      wroot1 <- grep(as.character(root1), root.names.level.1)    
      if(length(wroot1) == 0)
        stop(paste("Second element of parm did not match:", root.names.level.1), call. = FALSE)
      ## Need to test if the fourth element of pparm is a node
      nodes <- c("Clock", "Weather", "Soil", "SurfaceOrganicMatter", "MicroClimate", "Crop", "Manager","Report", "Operations", "Other", "Field")
      if(pparm[4] %in% nodes){
        ## This amounts to guessing that root should be of length 1
        root <- list(pparm[3])
      }else{
        ## This amounts to guessing that pparm[4] should be the second element in 
        root.names.level.2 <- vapply(apsimx_json$Children[[wroot1]]$Children, 
                                     FUN = function(x) x$Name, 
                                     FUN.VALUE = "character")
        root2 <- pparm[4]
        wroot2 <- grep(as.character(root2), root.names.level.2)  
        if(length(wroot2) == 0)
          stop(paste("Third element of parm did not match:", root.names.level.2), call. = FALSE)
        if(pparm[5] %in% nodes){
          root <- list(pparm[3], pparm[4])
        }else{
          root.names.level.3 <- vapply(apsimx_json$Children[[wroot1]]$Children[[wroot2]]$Children, 
                                       FUN = function(x) x$Name, 
                                       FUN.VALUE = "character")
          root3 <- pparm[5]
          wroot3 <- grep(as.character(root3), root.names.level.3)    
          if(length(wroot3) == 0)
            stop(paste("Fourth element of parm did not match:", root.names.level.3), call. = FALSE)
        }
      }      
    }
  }

  if(length(wcore) > 1 || !is.null(root)){
    if(is.null(root)){
      cat("Simulation structure: \n")
      str_list(apsimx_json)
      stop("more than one simulation found and no root node label has been specified \n select one of the children names above")   
    }else{
      ## Parse root
      root <- parse_root(root)
      if(length(root) > 3)
        stop("At the moment 3 is the maximum length for root", call. = TRUE)
      if(length(root) == 1){
        root.node.0.names <- sapply(apsimx_json$Children, function(x) x$Name)
        wcore1 <- grep(as.character(root), root.node.0.names)
        if(length(wcore1) == 0 || length(wcore1) > 1)
          stop("no root node label found or root is not unique")
        parent.node <- apsimx_json$Children[[wcore1]]$Children
      }
      if(length(root) == 2){
        root.node.0.names <- sapply(apsimx_json$Children, function(x) x$Name)
        wcore1 <- grep(as.character(root[1]), root.node.0.names)
        if(length(wcore1) == 0 || length(wcore1) > 1)
          stop("no root node label in position 1 found or root is not unique")
        root.node.0 <- apsimx_json$Children[[wcore1]]
        root.node.0.child.names <- sapply(root.node.0$Children, function(x) x$Name)  
        wcore2 <- grep(as.character(root[2]), root.node.0.child.names)
        if(length(wcore2) == 0 || length(wcore2) > 1)
          stop("no root node label in position 2 found or root is not unique")
        parent.node <- apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Children        
      }
      if(length(root) == 3){
        root.node.0.names <- sapply(apsimx_json$Children, function(x) x$Name)
        wcore1 <- grep(as.character(root[1]), root.node.0.names)
        if(length(wcore1) == 0 || length(wcore1) > 1)
          stop("no root node label in position 1 found or root is not unique")
        root.node.0 <- apsimx_json$Children[[wcore1]]
        root.node.0.child.names <- sapply(root.node.0$Children, function(x) x$Name)
        wcore2 <- grep(as.character(root[2]), root.node.0.child.names)
        if(length(wcore2) == 0 || length(wcore2) > 1)
          stop("no root node label in position 2 found or root is not unique")
        root.node.1 <- apsimx_json$Children[[wcore1]]$Children[[wcore2]]
        root.node.1.child.names <- sapply(root.node.1$Children, function(x) x$Name)  
        wcore3 <- grep(as.character(root[3]), root.node.1.child.names)
        if(length(wcore3) == 0 || length(wcore3) > 1)
          stop("no root node label in position 3 found or root is not unique")
        parent.node <- apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Children[[wcore3]]$Children        
      }
    }
  }else{
    parent.node <- apsimx_json$Children[[wcore]]$Children  
  }
  
  ## Edit the 'Clock'
  if(node == "Clock"){
    parm.choices <- c("Start","End")
    parm <- match.arg(parm, choices = parm.choices, several.ok = TRUE)
    ## Find what the 'Start' and 'End' are actually called
    ## Find the 'Clock'
    wlc <- function(x) grepl("Models.Clock", x$`$type`, ignore.case = TRUE)
    wlcl <- sapply(parent.node, FUN = wlc)
    
    if(sum(wlcl) < 1)
      stop("Clock not found", call. = FALSE)
    
    start <- grep("Start", names(parent.node[wlcl][[1]]), 
                  ignore.case = TRUE, value = TRUE)
    
    end <- grep("End", names(parent.node[wlcl][[1]]), 
                  ignore.case = TRUE, value = TRUE)
     
    if(length(parm) == 1){
     if(parm == "Start"){
       parent.node[wlcl][[1]][start] <- value
     }
     if(parm == "End"){
        parent.node[wlcl][[1]][end] <- value
      }
    }
    
    if(length(parm) == 2){
      if(parm[1] == "Start"){
        parent.node[wlcl][[1]][start] <- value[1]
      }
      if(parm[2] == "End"){
        parent.node[wlcl][[1]][end] <- value[2]
      }
    }
    ## The code below is not needed, I think
    # if(missing(root)){
    #   apsimx_json$Children[[1]]$Children <- parent.node  
    # }else{
    #   if(length(root) == 1){
    #     apsimx_json$Children[[wcore1]]$Children <- parent.node  
    #   }else{
    #     apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Children <- parent.node
    #   }
    # }
  }
  
  ## Edit the met file
  if(node == "Weather"){
    wlw <- function(x) grepl("Models.Climate.Weather|Models.Weather", x$`$type`)
    wlwl <- sapply(parent.node, FUN = wlw)
    
    if(sum(wlwl) < 1)
      stop("Weather node not found", call. = FALSE)
    
    parent.node[wlwl][[1]]$FileName <- value
  }
  
  ## Extract 'Core' simulation
  wcz <- grepl("Models.Core.Zone", parent.node)
  
  if(sum(wcz) < 1)
    stop("Models.Core.Zone not found", call. = FALSE)
  
  core.zone.node <- parent.node[wcz][[1]]$Children
  
  ## Edit the soil
  if(node == "Soil"){
    ## Extract soil
    wsn <- grepl("Models.Soils.Soil", core.zone.node)
    
    if(sum(wsn) < 1)
      stop("Models.Soils.Soil not found", call. = FALSE)
    
    soil.node <- core.zone.node[wsn]
    
    soil.node0 <- soil.node[[1]]$Children
    
    if(soil.child == "Metadata"){
      edited.child <- soil.child
      ## Perhaps too many possible metadata parms to list here?
      metadata.parms <- c("RecordNumber", "ASCOrder", "ASCSubOrder", "SoilType",
                          "LocalName", "Site", "NearestTown", "Region", 
                          "State", "Country", "NaturalVegetation", "ApsoilNumber",
                          "Latitude", "Longitude", "LocationAccuracy", "DataSource",
                          "Comments")
      if(!all(parm %in% metadata.parms)) stop("parm name(s) might be wrong")
      for(i in seq_along(parm)){
        soil.node[[1]][[parm[i]]] <- value[i]
      }
    }
  
    if(soil.child == "Physical"){
      edited.child <- soil.child
      
      ## In older versions of APSIM Next Gen instead of 'Physical' it was called
      ## 'Water'. Now "InitialWater" has been renamed to simply "Water"
      ## So here we now just look for "Physical"
      wwn <- grep("Physical", sapply(soil.node[[1]]$Children, function(x) x$Name)) 
      soil.water.node <- soil.node[[1]]$Children[[wwn]]

      if(soil.water.node$Name != "Water" && soil.water.node$Name != "Physical"){
        cat("Found: ", soil.water.node$Name, "instead of Physical or Water \n")
        stop("Wrong node (Physical or Water)")
      }
      
      crop.parms <- c("XF", "KL", "LL")
      
      if(parm %in% crop.parms || any(sapply(crop.parms, function(x) grepl(x, parm))) && parm != "LL15"){
        ## Maybe we are trying to edit the parameter for a specific crop
        ## The first options matches the parameter exactly 
        if(parm %in% crop.parms){
          for(i in seq_along(soil.water.node$Children[[1]][[parm]])){
            soil.water.node$Children[[1]][[parm]][[i]] <- value[i]
          }          
        }else{
         ## This assumes that the parameter to be edited is "Wheat LL" for example
          parm0 <- strsplit(parm, " ")[[1]]
          crop.name <- parm0[1]
          sparm <- parm0[2]
          wcnp <- which(gsub("Soil", "", sapply(soil.water.node$Children, function(x) x$Name)) == crop.name)
          for(i in seq_along(soil.water.node$Children[[1]][[sparm]])){
            soil.water.node$Children[[wcnp]][[sparm]][[i]] <- value[i]  
          }
        }
      }else{
        for(i in 1:length(soil.water.node[[parm]])){
          soil.water.node[[parm]][[i]] <- value[i]
        }
      }
      soil.node[[1]]$Children[[wwn]] <- soil.water.node
    }
    
    if(soil.child == "SoilWater"){
      edited.child <- soil.child
      wswn <- grep("^SoilWater", sapply(soil.node[[1]]$Children, function(x) x$Name)) 
      soil.soilwater.node <- soil.node[[1]]$Children[[wswn]]
      
      soilwat.parms <- c("SummerDate", "SummerU", "SummerCona", "WinterDate",
                         "WinterU", "WinterCona", "DiffusConst", "DiffusSlope",
                         "Salb", "CN2Bare", "CNRed", "CNCov", "Slope", "DischargeWidth",
                         "CatchmentArea")
      
      if(parm %in% soilwat.parms){
        ## This allows for editing multiple parameters and values
        for(i in seq_along(parm)){
          soil.soilwater.node[[parm[i]]] <- value[i]
        }
      }else{
        ## This case is most likely SWCON
        if(!parm %in% c("SWCON", "KLAT")) stop("parameter is likely incorrect")
        for(i in 1:length(soil.soilwater.node[[parm]])){
          soil.soilwater.node[[parm]][[i]] <- value[i]
        }
      }
      soil.node[[1]]$Children[[wswn]] <- soil.soilwater.node
    }
    
    if(soil.child == "Nitrogen"){
      wnn <- grepl("Nitrogen", soil.node0)
      soil.nitrogen.node <- soil.node0[wnn][[1]]
      
      for(i in 1:length(soil.nitrogen.node[[parm]])){
        soil.nitrogen.node[[parm]][[i]] <- value[i]
      }
      soil.node[[1]]$Children[wnn][[1]] <- soil.nitrogen.node
    }
    
    if(soil.child == "Organic"){
      edited.child <- "Organic"
      wsomn <- grepl("Organic", soil.node0)
      soil.om.node <- soil.node0[wsomn][[1]]
      
      som.parms1 <- c("RootCN", "EnrACoeff", "EnrBCoeff")
      
      if(parm %in% som.parms1){
          soil.om.node[[parm]] <- value
      }else{
        for(i in 1:length(soil.om.node[[parm]])){
          soil.om.node[[parm]][[i]] <- value[i]
        }
      }
      soil.node[[1]]$Children[wsomn][[1]] <- soil.om.node
    }
    
    if(soil.child %in% c("Analysis", "Chemical", "Solute", "NO3", "NH4", "Urea")){
      edited.child <- soil.child
      wan <- grepl(soil.child, soil.node0)

      if(sum(wan) == 1){
        soil.analysis.node <- soil.node0[wan][[1]]  
      }else{
        soil.node0.names <- sapply(soil.node0[wan], FUN = function(x) x$Name)
        if(soil.child == "Solute"){
          if(length(parm) != 2)
            stop("When 'soil.child' is 'Solute' 'parm' should be of length = 2", call. = FALSE)
          if(!parm[[1]] %in% c("NO3", "NH4", "Urea"))
            stop("The first element of 'parm' should be one of :", soil.node0.names, call. = FALSE)
          wsan <- which(soil.node0.names == parm[[1]])
          soil.analysis.node <- soil.node0[wan][[wsan]]
        }
      }
      
      ## Check for length
      if(length(parm) == 1){
        if(length(soil.analysis.node[[parm]]) != length(value)){
          cat("Length of value:", length(value), "\n")
          cat("Length of 'node':", length(soil.analysis.node[[parm]]), "\n")
          stop("Length of 'value' should equalto length of 'node'", call. = FALSE)
        }
        if(parm %in% c("PH", "NO3", "NH4", "Urea", "Thickness", "InitialValues")){
          for(i in 1:length(soil.analysis.node[[parm]])){
            soil.analysis.node[[parm]][[i]] <- value[i]
          }
        }else{
          stop("'parm' should be one of 'PH', 'NO3', 'NH4', 'Urea', 'InitialValues' or 'Thickness'", call. = FALSE)
        }
      }
      
      if(length(parm) == 2){
        if(length(soil.analysis.node[[parm[[2]]]]) > length(value))
          stop("Length of 'value' should not be less than length of 'node'", call. = FALSE)
        if(length(soil.analysis.node[[parm[[2]]]]) < length(value))
          stop("Length of 'value' should not be greater than length of 'node'", call. = FALSE)
        if(!parm[[2]] %in% names(soil.analysis.node))
          stop("The second element of 'parm' should be one of: ", names(soil.analysis.node), call. = FALSE)
        for(i in 1:length(soil.analysis.node[[parm[[2]]]])){
          soil.analysis.node[[parm[[2]]]][[i]] <- value[i]
        }
      }
      ### Need to fix this
      if(sum(wan) == 1){
        soil.node[[1]]$Children[wan][[1]] <- soil.analysis.node  
      }else{
        soil.node[[1]]$Children[wan][[wsan]] <- soil.analysis.node
      }
    }
    
    if(soil.child == "InitialWater" || soil.child == "Water"){
      edited.child <- "InitialWater"
      soil.node0.names <- sapply(soil.node0, function(x) x$Name)
      wiwn <- grep("InitialWater", soil.node0.names)
      if(length(wiwn) == 0){
        ### Maybe find just water?
        wiwn <- grep("^Water", soil.node0.names)
        if(length(wiwn) == 0){
          wiwn <- grep("initial water", soil.node0.names, ignore.case = TRUE)
        }
      }
      if(length(wiwn) == 0)
        stop("InitialWater node not found", call. = FALSE)
      
      soil.initialwater.node <- soil.node0[wiwn][[1]]
      
      ## Only three can be edited: PercentMethod, FractionFull, DepthWetSoil
      siw.parms <- c("PercentMethod", "FractionFull", "DepthWetSoil", "Thickness", "InitialValues")
      parm <- match.arg(parm, choices = siw.parms)
      
      if(parm %in% c("Thickness", "InitialValues")){
        soil.initialwater.node.vector <- soil.initialwater.node[[parm]]
        if(length(value) != length(soil.initialwater.node.vector))
          stop("Length of 'value' should match the length of ", parm, call. = FALSE)
        for(i in seq_along(soil.initialwater.node.vector))
          soil.initialwater.node.vector[[1]][[i]] <- value[i]
        soil.initialwater.node[[parm]] <- soil.initialwater.node.vector[[1]]
        soil.node[[1]]$Children[wiwn][[1]] <- soil.initialwater.node
      }else{
        soil.initialwater.node[[parm]] <- value
        soil.node[[1]]$Children[wiwn][[1]] <- soil.initialwater.node        
      }
    }
    
    if(soil.child == "Sample"){
      edited.child <- "Sample"
      wsn <- grepl("Sample", soil.node0)
      soil.sample.node <- soil.node0[wsn][[1]]
      
      for(i in 1:length(soil.sample.node[[parm]])){
        soil.sample.node[[parm]][[i]] <- value[i]
      }
      soil.node[[1]]$Children[wsn][[1]] <- soil.sample.node
    }
    
    if(soil.child == "Swim3"){
      edited.child <- "Swim3"
      wswimn <- grepl("Swim3", soil.node0)
      soil.swim.node <- soil.node0[wswimn][[1]]
      
      for(i in 1:length(soil.swim.node[[parm]])){
        soil.swim.node[[parm]][[i]] <- value[i]
      }
      soil.node[[1]]$Children[wswimn][[1]] <- soil.swim.node
    }
    
    core.zone.node[wsn] <- soil.node
  }
  
  if(node == "SurfaceOrganicMatter"){
    wsomn <- grepl("Models.Surface.SurfaceOrganicMatter", core.zone.node)
    som.node <- core.zone.node[wsomn][[1]]
    
    if(som.node$Name != "SurfaceOrganicMatter"){
      stop("Wrong node")
    }
    som.node[[parm]] <- value
    core.zone.node[wsomn][[1]] <- som.node
  }
  
  if(node == "MicroClimate"){
    wmcn <- grepl("Models.MicroClimate", core.zone.node)
    microclimate.node <- core.zone.node[wmcn][[1]]
    
    if(microclimate.node$Name != "MicroClimate"){
      stop("Wrong node")
    }
    microclimate.node[[parm]] <- value
    core.zone.node[wmcn][[1]] <- microclimate.node
  }
  
  if(node == "Crop"){
    ## Which is 'Crop'
    wmmn <- grepl("Models.Manager", core.zone.node)
    manager.node <- core.zone.node[wmmn]
    ## Which element has the crop information?
    wcn <- grepl("CultivarName", manager.node)
    crop.node <- manager.node[wcn][[1]]$Parameters
    
    for(i in 1:length(crop.node)){
      if(crop.node[[i]]$Key == parm){
        crop.node[[i]]$Value <- value
      }
    }
    core.zone.node[wmmn][wcn][[1]]$Parameters <- crop.node
  }
  
  if(node == "Manager"){
    wmmn <- grepl("Models.Manager", core.zone.node)
    manager.node <- core.zone.node[wmmn]
    manager.node.names <- sapply(manager.node, FUN = function(x) x$Name)

    if(missing(manager.child)) stop("need to specify manager.child")
    edited.child <- manager.child
    ## Which child should we edit?
    wmc <- grep(manager.child, manager.node.names)
    ## Maybe if it is inside a folder try looking inside manager.node?
    if(length(wmc) == 0){
      manager.node.names <- sapply(manager.node[[1]]$Children, FUN = function(x) x$Name)
      wmc2 <- grep(manager.child, manager.node.names)
      manager.child.node <- manager.node[[1]]$Children[[wmc2]]$Parameters  
    }else{
      manager.child.node <- manager.node[[wmc]]$Parameters  
    }
    
    for(i in 1:length(manager.child.node)){
      if(manager.child.node[[i]]$Key == parm){
        manager.child.node[[i]]$Value <- value
      }
    }
    
    if(length(wmc) == 0){
      manager.node[[1]]$Children[[wmc2]]$Parameters <- manager.child.node 
    }else{
      manager.node[[wmc]]$Parameters <- manager.child.node  
    }
    core.zone.node[wmmn] <- manager.node
  }
  
  if(node == "Operations"){
    won <- grepl("Models.Operations", core.zone.node)
    operations.node <- core.zone.node[won]
    
    if(length(operations.node) > 1)
      stop("Not ready to handle multiple 'Operations'", call. = FALSE)
    
    if(is.null(operations.node[[1]]$Operation))
      stop("'Operation' child node not found", call. = FALSE)
    
    ## Here the assumption is that 'parm' is a line and a component to edit
    if(length(parm) != 2)
      stop("'parm' should be a list of length 2. The first should be the line and the second should be the component", call. = FALSE)
    
    if(all(parm[[1]] > 0)){
      if(length(parm[[1]]) != length(value))
        stop("lenght of the first 'parm' element should be equal to the length of 'value'", call. = FALSE)      
    }

    length.operation <- length(operations.node[[1]]$Operation)
    
    if(all(parm[[1]] > 0)){
      if(parm[[2]] == "Date"){
        for(i in seq_along(value)){
          date.exists <- try(operations.node[[1]]$Operation[[parm[[1]][i]]]$Date, silent = TRUE)
          if(!inherits(date.exists, 'try-error')){
            operations.node[[1]]$Operation[[parm[[1]][i]]]$Date <- value[i]      
          }else{
            ## Need to add Date if not present 
            ##stop("Adding a row is not available yet", call. = FALSE)
            if(verbose) cat("Added a new 'Date' element in position", parm[[1]][i], "\n")
            input.list <- vector("list", length = 1) ## Create empty list
            list.elements <- operations.node[[1]]$Operation[[1]] ## Copying the first one
            list.elements$Date <- value[i] ## replacing Date
            list.elements$Action <- "" 
            list.elements$Line <- ""
            input.list[[1]] <- list.elements
            operations.node[[1]]$Operation <- append(operations.node[[1]]$Operation, input.list) 
          }
        }
      }
      
      if(parm[[2]] == "Action"){
        for(i in seq_along(value)){
          action.exists <- try(operations.node[[1]]$Operation[[parm[[1]][i]]]$Action, silent = TRUE)
          if(!inherits(action.exists, 'try-error')){
            operations.node[[1]]$Operation[[parm[[1]][i]]]$Action <- value[i]      
          }else{
            stop("Trying to edit an 'Action' item but it is not present. Add 'Date' first.", call. = FALSE)
          }
        }
      }
      
      if(parm[[2]] == "Line"){
        for(i in seq_along(value)){
          line.exists <- try(operations.node[[1]]$Operation[[parm[[1]][i]]]$Date, silent = TRUE)
          if(!inherits(line.exists, 'try-error')){
            operations.node[[1]]$Operation[[parm[[1]][i]]]$Line <- value[i]      
          }else{
            stop("Trying to edit a 'Line' item but it is not present. Add 'Date' first.", call. = FALSE)
          }
        }
      }      
    }else{
      ### In this case the format should be parm = list(-10, 'NA')
      if(!is.na(parm[[2]][1]))
        stop("Second element of the 'parm' list should be 'NA'", call. = FALSE)
      
      for(i in rev(seq_along(parm[[1]]))){
        idx.rm <- abs(parm[[1]][i])
        operations.node[[1]]$Operation[[idx.rm]] <- NULL      
      }
    }
      
    if(all(parm[[1]] > 0)){
      if(!parm[[2]] %in% c("Date", "Action", "Line"))
        stop("The second 'parm' component should be either 'Date', 'Action', or 'Line'", call. = FALSE)      
    }

    core.zone.node[won][[1]]$Operation <- operations.node[[1]]$Operation
    parm <- unlist(parm)
  }
  
  if(node == "Report"){
    wrn <- grepl("Models.Report", core.zone.node)
    report.node <- core.zone.node[wrn]
    report.node.names <- sapply(report.node, FUN = function(x) x$Name)
    
    if(missing(parm))
      stop("parm argument is missing")
    ## There is only one report in this case
    if(!is.list(parm)){
      if(length(report.node.names) > 1)
        stop("More than one Report is present. Use a list to choose one.")
      if(!grepl(parm, "VariableNames") && !grepl(parm, "EventNames"))
        stop("parm should contain either VariableNames or EventNames")
      if(grepl(parm, "VariableNames")){
        lvn <- length(report.node[[1]]$VariableNames)
        for(i in seq_along(value)){
          vnindx <- lvn + i
          report.node[[1]]$VariableNames[[vnindx]] <- value[i]
        }
      }else{
        evn <- length(report.node[[1]]$EventNames)
        for(i in seq_along(value)){
          evnindx <- evn + i
          report.node[[1]]$EventNames[[evnindx]] <- value[i]
        }
      }  
    }else{
      if(length(parm) < 2)
        stop("parm should be a list of length greater than 1")
      wrn <- grep(parm[[1]], report.node.names)  
      if(length(wrn) == 0L)
        stop("Report Name not found")
      if(!grepl(parm[[2]], "VariableNames") && !grepl(parm[[2]], "EventNames"))
        stop("parm should contain either VariableNames or EventNames")
      if(grepl(parm[[2]], "VariableNames")){
        lvn <- length(report.node[[wrn]]$VariableNames)
        for(i in seq_along(value)){
          vnindx <- lvn + i
          report.node[[wrn]]$VariableNames[[vnindx]] <- value[i]
        }
      }else{
        evn <- length(report.node[[wrn]]$EventNames)
        for(i in seq_along(value)){
          evnindx <- evn + i
          report.node[[wrn]]$EventNames[[evnindx]] <- value[i]
        }
      }
    }
    core.zone.node[wrn] <- report.node 
  }
  
  if(node == "Other"){
    ## Note: this strategy might not always work well
    ## because cultivar parameters are under Command
    ## which I think behaves differently
    ## Here the path should be to the full parameter path
    ## Unpack the parm.path
    ## root node starts at .
    
    if(!missing(parm))
      parm.path <- paste0(parm.path, ".", parm)
    
    if(missing(parm.path))
      stop("When node = 'Other' parm.path should be provided")
    
    ## Strip dollar sign if present
    if(substr(parm.path, 1, 1) == "$") 
      parm.path <- substr(parm.path, 2, nchar(parm.path))
    
    upp <- strsplit(parm.path, ".", fixed = TRUE)[[1]]
    upp.lngth <- length(upp)
    if(upp.lngth < 5) stop("Parameter path too short?")
    if(upp.lngth > 10) stop("Cannot handle this yet")
    ## upp[2] is typically "Simulations"
    if(apsimx_json$Name != upp[2])
      stop("Simulation root name does not match")
    wl3 <- which(upp[3] == sapply(apsimx_json$Children, function(x) x$Name))
    if(length(wl3) == 0) stop("Could not find parameter at level 3")
    ## At this level I select among simulation children
    ## upp[3] is typically "Simulation"
    n3 <- apsimx_json$Children[[wl3]]
    ## Look for the first reasonable parameter
    wl4 <- which(upp[4] == sapply(n3$Children, function(x) x$Name))
    if(length(wl4) == 0) stop("Could not find parameter at level 4")
    ## This is super dumb but I do not know how to do it otherwise
    if(upp.lngth == 5){
      if(upp[5] %in% names(n3$Children[[wl4]])){
        apsimx_json$Children[[wl3]]$Children[[wl4]][[upp[5]]] <- value
      }else{
        wl5 <- which(upp[5] == sapply(n3$Children[[wl4]]$Children, function(x) x$Name))
        if(length(wl5) == 0) stop("Could not find parameter at level 5")
        apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]][[upp[5]]] <- value
      }
    }
    ## Handling level 6
    if(upp.lngth == 6){
      n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
      wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
      if(length(wl5) == 0) stop("Could not find parameter at level 5")
      if(upp[6] %in% names(n4$Children[[wl5]])){
        apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]][[upp[6]]] <- value
      }else{
        if("Parameters" %in% names(n4$Children[[wl5]])){
          wp <- grep(upp[6], n4$Children[[wl5]]$Parameters)
          if(length(wp) == 0) stop("Could not find parameter at level 6 (Parameter)")
          apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Parameters[[wp]]$Value <- value
        }else{
          wl6 <- which(upp[6] == sapply(n4$Children[[wl5]]$Children, function(x) x$Name))
          if(length(wl6) == 0) stop("Could not find parameter at level 6")
          apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]][[upp[6]]] <- value         
        }
      }
    }
    ## Handling level 7, I don't think this works. Need to test.
    if(upp.lngth == 7){
      n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
      wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
      n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
      wl6 <- which(upp[6] == sapply(n5$Children, function(x) x$Name))
      apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]][[upp[7]]] <- value
    }
    if(upp.lngth == 8){
      n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
      wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
      n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
      wl6 <- which(upp[6] == sapply(n5$Children, function(x) x$Name))
      n6 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]
      wl7 <- which(upp[7] == sapply(n6$Children, function(x) x$Name))
      apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]][[upp[8]]] <- value
    }
    if(upp.lngth == 9){
      n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
      wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
      n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
      wl6 <- which(upp[6] == sapply(n5$Children, function(x) x$Name))
      n6 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]
      wl7 <- which(upp[7] == sapply(n6$Children, function(x) x$Name))
      n7 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]
      wl8 <- which(upp[8] == sapply(n7$Children, function(x) x$Name))
      apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]$Children[[wl8]][[upp[9]]] <- value
    }
    if(upp.lngth == 10){
      n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
      wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
      n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
      wl6 <- which(upp[6] == sapply(n5$Children, function(x) x$Name))
      n6 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]
      wl7 <- which(upp[7] == sapply(n6$Children, function(x) x$Name))
      n7 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]
      wl8 <- which(upp[8] == sapply(n7$Children, function(x) x$Name))
      n8 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]$Children[[wl8]]
      wl9 <- which(upp[9] == sapply(n8$Children, function(x) x$Name))
      apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]$Children[[wl8]]$Children[[wl9]][[upp[10]]] <- value
    }
  }
  
  if(node != "Other"){
    parent.node[wcz][[1]]$Children <- core.zone.node
    
    if(length(wcore) > 1 || !is.null(root)){
      ## I have to assume that root was supplied 
      ## otherwise an error would have been triggered before
      if(length(root) == 1){
        apsimx_json$Children[[wcore1]]$Children <- parent.node 
      }
      if(length(root) == 2){
        apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Children <- parent.node 
      }
      if(length(root) == 3){
        apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Children[[wcore3]]$Children <- parent.node 
      }
    }else{
      apsimx_json$Children[[wcore]]$Children <- parent.node 
    }    
  }

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
    cat("Edited (node): ", node, "\n")
    cat("Edited (child): ", edited.child,"\n")
    cat("Edited parameters: ", parm, "\n")
    cat("New values: ", value, "\n")
    cat("Created: ", wr.path,"\n")
  }
}

## This function is not exported at the moment
## It is not ready to be used yet.
edit_apsimx_json <- function(file, src.dir = ".", wrt.dir = NULL,
                             parm.path = NULL, value = NULL, 
                             overwrite = FALSE, edit.tag = "-edited",
                             verbose = TRUE){
  
  .check_apsim_name(file)
  
  if(missing(wrt.dir)) wrt.dir <- src.dir
  
  file.names.apsimx <- dir(path = src.dir, pattern=".apsimx$", ignore.case=TRUE)
  file.names.json <- dir(path = src.dir, pattern=".json$", ignore.case=TRUE)
  file.names <- c(file.names.apsimx, file.names.json)
  
  if(length(file.names) == 0){
    stop("There are no .apsimx or.json files in the specified directory to edit.")
  }
  
  file <- match.arg(file, file.names)
  
  ## Parse apsimx file (JSON)
  apsimx_json <- jsonlite::read_json(file.path(src.dir, file))
  
  ## Should add code to deal with scoped paths
  if(substr(parm.path, 1, 1) == "$") 
    parm.path <- substr(parm.path, 2, nchar(parm.path))
    
  upp <- strsplit(parm.path, ".", fixed = TRUE)[[1]]
  upp.lngth <- length(upp)
  keep.going <- TRUE
  
  if(upp.lngth == 2){
    wp2p <- which(names(apsimx_json) == upp[2])
    if(length(wp2p) == 0)
      stop("Parameter not found")
    apsimx_json[[wp2p]] <- value
  }
  
  if(upp.lngth >= 3 && keep.going){
    if(apsimx_json$Name != upp[2])
      stop("First position name does not match")    
    wl3 <- which(upp[3] == sapply(apsimx_json$Children, function(x) x$Name))
    if(length(wl3) == 0) stop("Could not find parameter at level 3")
    if(upp.lngth == 3){
      apsimx_json$Children[[wl3]] <- value
      keep.going <- FALSE
    }
  }
  
  if(upp.lngth >= 4 && keep.going){
    n3 <- apsimx_json$Children[[wl3]]
    ## Look for the first reasonable parameter
    wl4 <- which(upp[4] == sapply(n3$Children, function(x) x$Name))
    if(length(wl4) == 0) stop("Could not find parameter at level 4")
    if(upp.lngth == 4){
      apsimx_json$Children[[wl3]]$Children[[wl4]] <- value
      keep.going <- FALSE
    }
  }
  
  if(upp.lngth >= 5 && keep.going){
    
    if(upp[5] %in% names(n3$Children[[wl4]])){
      apsimx_json$Children[[wl3]]$Children[[wl4]][[upp[5]]] <- value
      keep.going <- FALSE
    }else{
      wl5 <- which(upp[5] == sapply(n3$Children[[wl4]]$Children, function(x) x$Name))
      if(length(wl5) == 0) stop("Could not find parameter at level 5")
      if(upp.lngth == 5){
        apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]][[upp[5]]] <- value
        keep.going <- FALSE
      }
    }
  }
  ## Handling level 6
  if(upp.lngth >= 6 && keep.going){
    n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
    if(upp[6] %in% names(n4$Children[[wl5]])){
      apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]][[upp[6]]] <- value
      keep.going <- FALSE
    }else{
      if("Parameters" %in% names(n4$Children[[wl5]])){
        wp <- grep(upp[6], n4$Children[[wl5]]$Parameters)
        if(length(wp) == 0) stop("Could not find parameter at level 6 (Parameter)")
        apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Parameters[[wp]]$Value <- value
        keep.going <- FALSE
      }else{
        wl6 <- which(upp[6] == sapply(n4$Children[[wl5]]$Children, function(x) x$Name))
        if(length(wl6) == 0) stop("Could not find parameter at level 6")
        if(upp.lngth == 6){
          apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]][[upp[6]]] <- value         
          keep.going <- FALSE
        }
      }
    }
  }
  
  ## Handling level 7
  if(upp.lngth >= 7 && keep.going){
    n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
    if(upp[7] %in% names(n5$Children[[wl6]])){
      apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]][[upp[7]]] <- value
      keep.going <- FALSE
    }else{
      if("Parameters" %in% names(n5$Children[[wl6]])){
        wp <- grep(upp[7], n5$Children[[wl6]]$Parameters)
        if(length(wp) == 0) stop("Could not find parameter at level 7 (Parameter)")
        apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Parameters[[wp]]$Value <- value
        keep.going <- FALSE
      }else{
        wl7 <- which(upp[7] == sapply(n5$Children[[wl6]]$Children, function(x) x$Name))
        if(length(wl6) == 0) stop("Could not find parameter at level 7")
        if(upp.lngth == 7){
          apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]][[upp[7]]] <- value
          keep.going <- FALSE
        }
      }
    }
  }

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
    cat("Edited parameters: ", parm.path, "\n")
    cat("New values: ", value, "\n")
    cat("Created: ", wr.path,"\n")
  }
}
  
  
  
  
  
