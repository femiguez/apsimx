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
#'  When node equals Report, the editing allows to add variables, but not to remove them at the moment.
#' 
#' @name edit_apsimx
#' @param file file ending in .apsimx to be edited (JSON)
#' @param src.dir directory containing the .apsimx file to be edited; defaults to the current working directory
#' @param wrt.dir should be used if the destination directory is different from the src.dir
#' @param node either \sQuote{Clock}, \sQuote{Weather}, \sQuote{Soil}, 
#' \sQuote{SurfaceOrganicMatter}, \sQuote{MicroClimate}, \sQuote{Crop}, \sQuote{Manager}, \sQuote{Report} or \sQuote{Other} 
#' @param soil.child specific soil component to be edited
#' @param manager.child specific manager component to be edited
#' @param parm parameter to be edited
#' @param value new values for the parameter to be edited 
#' @param overwrite logical; if \code{TRUE} the old file is overwritten, a new file is written otherwise
#' @param edit.tag if the file is edited a different tag from the default \sQuote{-edited} can be used.
#' @param parm.path path to the attribute to edit when node is \sQuote{Other}
#' @param root supply the node postion in the case of multiple simulations such as factorials.
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
#'             soil.child = "Water", 
#'             parm = "BD", value = bds,
#'             verbose = FALSE)
#' ## Inspect file
#' inspect_apsimx("Wheat-edited.apsimx", src.dir = tmp.dir,
#'                 node = "Soil", soil.child = "Water")
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
#' inspect_apsimx("Maize-edited.apsimx", src.dir = tmp.dir, node = "Manager")
#' 
#' ## Remove the file
#' file.remove(file.path(tmp.dir, "Maize-edited.apsimx"))
#' }
#' 

edit_apsimx <- function(file, src.dir = ".", wrt.dir = NULL,
                        node = c("Clock", "Weather", "Soil", "SurfaceOrganicMatter", "MicroClimate", "Crop", "Manager", "Report", "Other"),
                        soil.child = c("Metadata", "Water", "SoilWater", "Organic", "Physical", "Analysis", "Chemical", "InitialWater", "Sample"),
                        manager.child = NULL,
                        parm = NULL, value = NULL, 
                        overwrite = FALSE,
                        edit.tag = "-edited",
                        parm.path = NULL,
                        root,
                        verbose = TRUE){
  
  .check_apsim_name(file)
  
  if(missing(wrt.dir)) wrt.dir <- src.dir
  
  file.names <- dir(path = src.dir, pattern=".apsimx$", ignore.case=TRUE)
  
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
  
  ## Parse apsimx file (JSON)
  apsimx_json <- jsonlite::read_json(file.path(src.dir, file))
  
  ## Where is the 'Core' simulation?
  wcore <- grep("Core.Simulation", apsimx_json$Children)

  if(length(wcore) > 1){
    if(missing(root)){
      cat("Simulation structure: \n")
      str_list(apsimx_json)
      stop("more than one simulation found and no root node label has been specified \n select one of the children names above")   
    }else{
      if(length(root) == 1){
        wcore1 <- grep(as.character(root), apsimx_json$Children)
        if(length(wcore1) == 0 || length(wcore1) > 1)
          stop("no root node label found or root is not unique")
        parent.node <- apsimx_json$Children[[wcore1]]$Children
      }else{
        root.node.0.names <- sapply(apsimx_json$Children, function(x) x$Name)
        wcore1 <- grep(as.character(root[1]), root.node.0.names)
        root.node.0 <- apsimx_json$Children[[wcore1]]
        root.node.0.child.names <- sapply(root.node.0$Children, function(x) x$Name)
        wcore2 <- grep(as.character(root[2]), root.node.0.child.names)
        parent.node <- apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Children
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
    wlc <- function(x) grepl("Clock", x$Name)
    wlcl <- sapply(parent.node, FUN = wlc)
    
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
    apsimx_json$Children[[1]]$Children <- parent.node
  }
  
  ## Edit the met file
  if(node == "Weather"){
    wlw <- function(x) grepl("Weather", x$Name)
    wlwl <- sapply(parent.node, FUN = wlw)
    parent.node[wlwl][[1]]$FileName <- value
  }
  
  ## Extract 'Core' simulation
  wcz <- grepl("Models.Core.Zone", parent.node)
  core.zone.node <- parent.node[wcz][[1]]$Children
  
  ## Edit the soil
  if(node == "Soil"){
    ## Extract soil
    wsn <- grepl("Models.Soils.Soil", core.zone.node)
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
  
    if(soil.child == "Water" || soil.child == "Physical"){
      edited.child <- soil.child
      
      wwn <- grep("^Water|Physical", sapply(soil.node[[1]]$Children, function(x) x$Name)) 
      soil.water.node <- soil.node[[1]]$Children[[wwn]]

      if(soil.water.node$Name != "Water" && soil.water.node$Name != "Physical"){
        cat("Found: ", soil.water.node$Name, "instead of Physical or Water \n")
        stop("Wrong node (Physical or Water)")
      }
      
      crop.parms <- c("XF", "KL", "LL")
      
      if(parm %in% crop.parms || any(sapply(crop.parms, function(x) grepl(x, parm)))){
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
    
    if(soil.child == "Analysis" || soil.child == "Chemical"){
      edited.child <- soil.child
      wan <- grepl(soil.child, soil.node0)
      soil.analysis.node <- soil.node0[wan][[1]]
      
      ## Only PH can be edited
      if(parm != "PH") stop("only PH can be edited, use 'edit_apsimx_replace_soil_profile instead")
      if(parm == "PH"){
        for(i in 1:length(soil.analysis.node[[parm]])){
          soil.analysis.node[[parm]][[i]] <- value[i]
        }
      }
      soil.node[[1]]$Children[wan][[1]] <- soil.analysis.node
    }
    
    if(soil.child == "InitialWater"){
      edited.child <- "InitialWater"
      wiwn <- grepl("InitialWater", soil.node0)
      soil.initialwater.node <- soil.node0[wiwn][[1]]
      
      ## Only three can be edited: PercentMethod, FractionFull, DepthWetSoil
      siw.parms <- c("PercentMethod", "FractionFull", "DepthWetSoil")
      parm <- match.arg(parm, choices = siw.parms)
      
      soil.initialwater.node[[parm]] <- value
      
      soil.node[[1]]$Children[wiwn][[1]] <- soil.initialwater.node
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
    
    if(length(wcore) > 1){
      ## I have to assume that root was supplied 
      ## otherwise an error would have been triggered before
      if(length(root) == 1){
        apsimx_json$Children[[wcore1]]$Children <- parent.node 
      }else{
        apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Children <- parent.node 
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
  
  
  
  
  
