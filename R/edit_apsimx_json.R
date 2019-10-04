#' Edit an APSIM-X (JSON) Simulation
#' 
#' This function allows editing of an APSIM-X (JSON) simulation file.
#' 
#' The variables specified by \code{parm} within the .apsimx file specified by \code{file} 
#' in the source directory \code{src.dir} are edited. The old values are replaced with \code{value}, which
#' is a list that has the same number of elements as the length of the vector \code{parm}.  The current
#' .apsimx file will be overwritten if \code{overwrite} is set to \code{TRUE}; otherwise the file
#' \emph{'file'-edited.apsimx} will be created.  If (verbose = TRUE) then the name
#'  of the written file is returned. The function is similar to the edit_apsim functin in the 'apsimr'
#'  package, but with the difference that only some variables (parameters) can be modified.
#'  
#'  The function inspect_apsimx is for a quick look from within R. The APSIM GUI provides a more
#'  complete examination of the .apsimx file
#' 
#' @name edit_apsimx_json
#' @param file file ending in .apsimx to be edited (JSON)
#' @param src.dir directory containing the .apsimx file to be edited; defaults to the current working directory
#' @param wrt.dir should be used if the destination directory is different from the src.dir
#' @param node either 'Clock', 'Weather', 'Soil', 'SurfaceOrganicMatter', 'MicroClimate', 'Crop', 'Manager' or 'Other' 
#' @param soil.child specific soil component to be edited
#' @param som.child specific surface organic matter component to be edited (not used)
#' @param manager.child specific manager component to be edited
#' @param parm parameter to be edited
#' @param value new values for the parameter to be edited 
#' @param overwrite logical; if \code{TRUE} the old file is overwritten, a new file is written otherwise
#' @param edit.tag if the file is edited a different tag from the default '-edited' can be used.
#' @param parm.path path to the attribute to edit when node is 'Other'
#' @param verbose whether to print information about successful edit
#' @return (when verbose=TRUE) complete file path to edited .apsimx file is returned as a character string.
#' As a side effect this function creates a new (JSON) .apsimx file.
#' @note The components that can be edited are restricted becuase this is better in preventing
#' errors of editing unintended parts of the file. The disadvantage is that there is less flexibility
#' compared to the similar function in the 'apsimr' package. 
#' @export
#' @examples 
#' \dontrun{
#' ## This example will read one of the examples distributed with APSIM-X
#' ## but write to the current directory
#' 
#' ## Edit Bulk density
#' ex.dir <- auto_detect_apsimx_examples()
#' bds <- c(1.02, 1.03, 1.09, 1.16, 1.18, 1.19, 1.20)
#' edit_apsimx_json("Barley.apsimx", src.dir = ex.dir,
#'                  wrt.dir = ".",
#'                  node = "Soil",
#'                  soil.child = "Water", 
#'                  parm = "BD", value = bds,
#'                  verbose = FALSE)
#' ## Inspect file
#' inspect_apsimx("Barley-edited.apsimx", node = "Soil", soil.child = "Water")
#' ## To delete the file...
#' file.remove("./Barley-edited.apsimx")
#' 
#' ## Edit the fertilizer amount in 'Maize.apsimx'
#' edit_apsimx_json("Maize.apsimx", src.dir = ex.dir,
#'                  wrt.dir = ".",
#'                  node = "Manager",
#'                  manager.child = "SowingFertiliser",
#'                  parm = "Amount",
#'                  value = 200, verbose = TRUE)
#' ## Make sure it worked
#' inspect_apsimx("Maize-edited.apsimx", node = "Manager")
#' ## Remove the file
#' file.remove("./Maize-edited.apsimx")
#' }
#' 

edit_apsimx_json <- function(file, src.dir = ".", wrt.dir = NULL,
                            node = c("Clock","Weather","Soil","SurfaceOrganicMatter",
                                     "MicroClimate","Crop","Manager"),
                            soil.child = c("Water","OrganicMatter",
                                           "Analysis","InitialWater","Sample"),
                            som.child = c("Pools","Other"),
                            manager.child = NULL,
                            parm=NULL, value=NULL, overwrite = FALSE,
                            edit.tag = "-edited",
                            parm.path = NULL,
                            verbose = TRUE){
  
  if(missing(wrt.dir)) wrt.dir <- src.dir
  
  fileNames <- dir(path = src.dir, pattern=".apsimx$",ignore.case=TRUE)
  
  if(length(fileNames)==0){
    stop("There are no .apsimx files in the specified directory to edit.")
  }
  
  node <- match.arg(node)
  soil.child <- match.arg(soil.child)
  som.child <- match.arg(som.child)
  edited.child <- "none"
  
  ## For now we just edit one file at a time
  file <- match.arg(file, fileNames, several.ok=FALSE)
  
  if(apsimx_filetype(file = file, src.dir = src.dir) != "json")
    stop("This function only edits JSON files")
  
  ## Parse apsimx file (JSON)
  apsimx_json <- read_json(paste0(src.dir,"/",file))
  
  parent.node <- apsimx_json$Children[[1]]$Children

  ## Edit the 'Clock'
  if(node == "Clock"){
    parm.choices <- c("StartDate","EndDate")
    parm <- match.arg(parm, choices = parm.choices, several.ok = TRUE)
    ## Find the 'Clock'
    wlc <- function(x) grepl("Clock", x$Name)
    wlcl <- sapply(parent.node, FUN = wlc)
     
    if(length(parm) == 1){
     if(parm == "StartDate"){
       parent.node[wlcl][[1]]["StartDate"] <- value
     }
     if(parm == "EndDate"){
        parent.node[wlcl][[1]]["EndDate"] <- value
      }
    }
    
    if(length(parm) == 2){
      if(parm == "StartDate"){
        parent.node[wlcl][[1]]["StartDate"] <- value[1]
      }
      if(parm == "EndDate"){
        parent.node[wlcl][[1]]["EndDate"] <- value[2]
      }
    }
    apsimx_json$Children[[1]]$Children <- parent.node
  }
  
  ## Edit the met file
  if(node == "Weather"){
    wlw <- function(x) grepl("Weather", x$Name)
    wlwl <- sapply(parent.node, FUN = wlw)
    parent.node[wlwl][[1]]$FileName <- value
    apsimx_json$Children[[1]]$Children <- parent.node
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
  
    if(soil.child == "Water"){
      edited.child <- "Water"
      soil.water.node <- soil.node[[1]]$Children[[1]]
      ## I select this one based on position, so problems
      ## can arise
      
      if(soil.water.node$Name != "Water"){
        stop("Wrong node (Soil Water)")
      }
      
      crop.parms <- c("XF", "KL", "LL")
      
      if(parm %in% crop.parms ){
        for(i in 1:length(soil.water.node[[parm]])){
          soil.water.node$Children[[1]][[parm]][[i]] <- value[i]
        }
      }else{
        for(i in 1:length(soil.water.node[[parm]])){
          soil.water.node[[parm]][[i]] <- value[i]
        }
      }
      soil.node[[1]]$Children[[1]] <- soil.water.node
    }
    
    if(soil.child == "Nitrogen"){
      wnn <- grepl("Nitrogen", soil.node0)
      soil.nitrogen.node <- soil.node0[wnn][[1]]
      
      for(i in 1:length(soil.nitrogen.node[[parm]])){
        soil.nitrogen.node[[parm]][[i]] <- value[i]
      }
      soil.node[[1]]$Children[wnn][[1]] <- soil.nitrogen.node
    }
    
    if(soil.child == "OrganicMatter"){
      edited.child <- "OrganicMatter"
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
    
    if(soil.child == "Analysis"){
      edited.child <- "Analysis"
      wan <- grepl("Analysis", soil.node0)
      soil.analysis.node <- soil.node0[wan][[1]]
      
      ## Only PH can be edited
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
      siw.parms <- c("PercentMethod", "FractionFull","DepthWetSoil")
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
    
    manager.child.node <- manager.node[[wmc]]$Parameters
    
    for(i in 1:length(manager.child.node)){
      if(manager.child.node[[i]]$Key == parm){
        manager.child.node[[i]]$Value <- value
      }
    }
    manager.node[[wmc]]$Parameters <- manager.child.node
    core.zone.node[wmmn] <- manager.node
  }
  
  parent.node[wcz][[1]]$Children <- core.zone.node
  apsimx_json$Children[[1]]$Children <- parent.node
  
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
    cat("Edited (child): ", edited.child,"\n")
    cat("Edited parameter: ",parm, "\n")
    cat("New values: ",value, "\n")
    cat("Created: ",wr.path,"\n")
  }
}

