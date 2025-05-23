#'
#' @title Edit APSIM-X file with a replaced soil profile
#' @name edit_apsimx_replace_soil_profile
#' @description Edits an APSIM-X simulation by replacing the soil profile
#' @param file name of the .apsimx file to be edited
#' @param src.dir source directory
#' @param wrt.dir writing directory
#' @param soil.profile a soil profile object with class \sQuote{soil_profile}
#' @param edit.tag default edit tag \sQuote{-edited}
#' @param overwrite default FALSE
#' @param verbose default TRUE and it will print messages to console
#' @param root supply the node position in the case of multiple simulations such as factorials.
#' @return writes a file to disk with the supplied soil profile
#' @details This function is designed to batch replace the whole soil in an APSIM simulation file. 
#' @note There is no such thing as a default soil, carefully build the profile for each simulation.
#' @export
#' @examples 
#' \donttest{
#' sp <- apsimx_soil_profile()
#' extd.dir <- system.file("extdata", package = "apsimx")
#' 
#' ## I write to a temp directory but replace as needed
#' tmp.dir <- tempdir()
#' 
#' edit_apsimx_replace_soil_profile("Maize.apsimx", soil.profile = sp, 
#'                                   src.dir = extd.dir, wrt.dir = tmp.dir)
#' inspect_apsimx("Maize-edited.apsimx", src.dir = tmp.dir,
#'                  node = "Soil")
#'  }
#'

edit_apsimx_replace_soil_profile <-  function(file = "", src.dir = ".",
                                              wrt.dir = NULL,
                                              soil.profile = NULL,
                                              edit.tag = "-edited",
                                              overwrite = FALSE,
                                              verbose = TRUE,
                                              root = NULL){
  
  if(isFALSE(get("allow.path.spaces", envir = apsimx::apsimx.options))){
    .check_apsim_name(file)
    .check_apsim_name(src.dir)
  } 
  
  if(missing(wrt.dir)) wrt.dir <- src.dir
  
  file.names <- dir(path = src.dir, pattern=".apsimx$", ignore.case=TRUE)
  
  if(length(file.names)==0){
    stop("There are no .apsimx files in the specified directory to edit.")
  }
  
  file <- match.arg(file, file.names)
  
  if(missing(soil.profile)) stop("soil profile is missing")
  
  if(!inherits(soil.profile, "soil_profile"))
    stop("Object soil.profile should be of class 'soil_profile'", call. = FALSE)
  
  ## Parse apsimx file (JSON)
  apsimx_json <- jsonlite::read_json(file.path(src.dir, file))
  
  ## Where is the 'Core' simulation?
  parent.node <- apsimx_json$Children
  wcore <- grep("Models.Core.Simulation", parent.node)
  
  if(length(wcore) > 1 || !is.null(root)){
    if(missing(root)){
      cat("Simulation structure: \n")
      str_list(apsimx_json)
      stop("more than one simulation found and no root node label has been specified \n select one of the children names above")   
    }else{
      ### Parse root
      root <- parse_root(root)
      if(length(root) > 3)
        stop("At the moment 3 is the maximum length for root", call. = TRUE)
      
      if(length(root) == 1){
        root.node.0.names <- sapply(apsimx_json$Children, function(x) x$Name)
        wcore1 <- grep(as.character(root[1]), root.node.0.names)
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

  ## Extract 'Core' simulation
  wcz <- grepl("Models.Core.Zone", parent.node)
  core.zone.node <- parent.node[wcz][[1]]$Children
  
  ## Extract soil
  wsn <- grepl("Models.Soils.Soil", core.zone.node)
  soil.node <- core.zone.node[wsn]
  
  soil.node0 <- soil.node[[1]]$Children ## Is the 1 safe here because there is only one soil?
  
  ## First edit: soil 'Physical'
  ## Depth, Thickness, ParticleSizeClay,
  ## BD, AirDry, LL15, DUL, SAT, KS
  wspn <- grepl("Models.Soils.Physical", soil.node0)
  soil.physical.node <- soil.node0[wspn][[1]]
  for(i in c("Depth", "Thickness", "ParticleSizeClay", "ParticleSizeSilt", "ParticleSizeSand", "BD", "AirDry", "LL15", "DUL", "SAT", "KS")){
    ## Format the variable
    if(is.null(soil.profile$soil[[i]])) next
    tmp <- as.vector(soil.profile$soil[[i]], mode = "list")
    ## Replace the variable
    soil.physical.node[[i]] <- tmp 
  }
  ## Preliminary setup of soil
  if(length(soil.profile$crops) > length(soil.physical.node$Children)){
    for(i in seq_along(soil.profile$crops)){
      soil.physical.node$Children[[i]] <- soil.physical.node$Children[[1]]
      soil.physical.node$Children[[i]]$Name <- paste0(soil.profile$crops[i], "Soil")
    }
  }
  ## Crop parameters
  for(i in 1:length(soil.profile$crops)){
    crop.vars.names <- grep("KL|XF|LL$", names(soil.profile$soil), value = TRUE)
    for(j in crop.vars.names){
      tmp <- as.vector(soil.profile$soil[[j]], mode = "list")
      strpcrop <- strsplit(j, ".", fixed = TRUE)[[1]][2]
      soil.physical.node$Children[[i]][[strpcrop]] <- tmp
    }
  }
  soil.node0[wspn][[1]] <- soil.physical.node
  soil.node[[1]]$Children <- soil.node0
  
  ## Next edit the soil organic component
  wsomn <- grepl("Models.Soils.Organic", soil.node0)
  soil.om.node <- soil.node0[wsomn][[1]]
  
  for(i in c("Depth", "Thickness", "Carbon", "SoilCNRatio", "FBiom", "FInert", "FOM")){
    ## Format the variable
    tmp <- as.vector(soil.profile$soil[[i]], mode = "list")
    ## Replace the variable
    soil.om.node[[i]] <- tmp 
  }
  soil.node0[wsomn][[1]] <- soil.om.node
  soil.node[[1]]$Children <- soil.node0
  
  ## Next edit the Chemical component
  wschn <- grepl("Models.Soils.Chemical", soil.node0)
  soil.chemical.node <- soil.node0[wschn][[1]]
  
  for(i in c("Depth", "Thickness", "PH")){
    ## Format the variable
    tmp <- as.vector(soil.profile$soil[[i]], mode = "list")
    ## Replace the variable
    soil.chemical.node[[i]] <- tmp 
  }
  soil.node0[wschn][[1]] <- soil.chemical.node
  soil.node[[1]]$Children <- soil.node0
  
  ## Next edit the Solute component, this is not present in older versions of APSIM
  wssoln <- grepl("Models.Soils.Solute", soil.node0)
  if(sum(wssoln) > 0){
    soil.solute.nodes <- soil.node0[wssoln]
    for(i in seq_along(soil.solute.nodes)){
      soil.solute.node <- soil.solute.nodes[[i]]
      vname <- soil.solute.node$Name
      vname.in.soil <- grep(vname, names(soil.profile$soil), value = TRUE)
      if(length(vname.in.soil) > 0){
        tmp <- as.vector(soil.profile$soil[[vname.in.soil]], mode = "list") ## This now works because these are initial values?
        soil.solute.node$InitialValues <- tmp  
        soil.solute.node$Thickness <- soil.profile$soil$Thickness
        soil.node0[wssoln][[i]] <- soil.solute.node
      }
    }
    soil.node[[1]]$Children <- soil.node0    
  }

  ## Edit metadata
  if(!is.null(soil.profile$metadata)){
    soil.node.names <- names(soil.node[[1]])
    skp.nms <- c("$type", "Name", "Children", "IncludeInDocumentation", "Enabled", "ReadOnly")
    for(i in soil.node.names){
      if(i %in% skp.nms) next
      if(i %in% names(soil.profile$metadata)){
        soil.node[[1]][[i]] <- soil.profile$metadata[[i]]
      }else{
        if(i %in% c("RecordNumber","ApsoilNumber")){
          soil.node[[1]][[i]] <- 0 
        }else{
          soil.node[[1]][[i]] <- NA  
        }
      }
    }
  }
  
  ## Use metadata to edit the name of the soil
  if(!is.null(soil.profile$metadata$SoilType)){
    soil.node[[1]]$Name <- soil.profile$metadata$SoilType
  }
  
  ## Edit soilWat if present
  if(inherits(soil.profile$soilwat, "soilwat_parms")){
    
    wsswn <- grepl("Models.WaterModel", soil.node0) 
    soil.soilwat.node <- soil.node0[wsswn][[1]]
    for(i in names(soil.profile$soilwat)){
      if(any(is.na(soil.profile$soilwat[[i]]))){
        next
      }else{
          soil.soilwat.node[[i]] <- soil.profile$soilwat[[i]]  
      } 
    }
    soil.node0[wsswn][[1]] <- soil.soilwat.node
    soil.node[[1]]$Children <- soil.node0
  }
  
  ## Edit SWIM if present
  if(inherits(soil.profile$swim, "swim_parms")){
    
    wsswmn <- grepl("Models.Soils.Swim3", soil.node0) 
    ### This is an error instead of a warning because
    ### if 'swim' is provided, then it is likely that 
    ### editing is needed
    if(length(wsswmn) == 0)
      stop("Could not find Swim3 model in simulation", call. = FALSE)
    ## Which soil swim node?
    soil.swim3.node <- soil.node0[wsswmn][[1]]
    for(i in names(soil.profile$swim)){
      if(any(is.na(soil.profile$swim[[i]]))){
        next
      }else{
        soil.swim3.node[[i]] <- soil.profile$swim[[i]]  
      } 
    }
    soil.node0[wsswmn][[1]] <- soil.swim3.node
    soil.node[[1]]$Children <- soil.node0
  }
  
  ## Edit InitialWater if present
  if(inherits(soil.profile$initialwater, "initialwater_parms")){
    
    wsiswn <- grepl("Models.Soils.Water", soil.node0) 
    soil.initialwater.node <- soil.node0[wsiswn][[1]]
    for(i in names(soil.profile$initialwater)){
      if(any(is.na(soil.profile$initialwater[[i]]))){
        next
      }else{
        soil.initialwater.node[[i]] <- soil.profile$initialwater[[i]]  
      } 
    }
    soil.node0[wsiswn][[1]] <- soil.initialwater.node
    soil.node[[1]]$Children <- soil.node0
  }
  
  ## Edit Solutes if present
  if(inherits(soil.profile$solutes, "solutes_parms")){
    
    wssoln <- grepl("Models.Soils.Solute", soil.node0) 
    soil.solute.nodes <- soil.node0[wssoln]
    for(i in seq_along(soil.solute.nodes)){
      soil.solute.node <- soil.solute.nodes[[i]]
      vname <- soil.solute.node$Name
      vname.in.solutes <- grep(vname, soil.profile$solutes$Solutes, value = TRUE)
      if(length(vname.in.solutes) > 0){
        for(j in seq_along(soil.profile$solutes)){
          if(any(is.na(soil.profile$solutes[[j]]))){
            next
          }else{
              sspntc <- names(soil.profile$solutes)[[j]] ## Soil Solute Parameter Name to Change
              ## Is the parameter above present in soil.solute.node?
              soil.solute.node.names <- names(soil.solute.node)
              if(!sspntc %in% soil.solute.node.names)
                stop("soil solute parameter to change is not in soil.solute.node")
              soil.solute.node[sspntc] <- soil.profile$solutes[[j]]    
          } 
        }        
      }
      soil.solute.nodes[[i]] <- soil.solute.node
    }
    soil.node0[wssoln] <- soil.solute.nodes
    soil.node[[1]]$Children <- soil.node0
  }
  
  if(is.null(root)){
    ## Replace the soil
    ## 1. soil.node to core.zone.node
    core.zone.node[wsn] <- soil.node
    ## 2. core.one.node to parent.node
    parent.node[wcz][[1]]$Children <- core.zone.node
    ## parent.node to core
    apsimx_json$Children[[wcore]]$Children <- parent.node    
  }else{
    core.zone.node[wsn] <- soil.node
    parent.node[wcz][[1]]$Children <- core.zone.node
    if(length(root) == 1){
      apsimx_json$Children[[wcore1]]$Children <- parent.node 
    }
    if(length(root) == 2){
      apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Children <- parent.node 
    }
    if(length(root) == 3){
      apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Children[[wcore3]]$Children <- parent.node 
    }
  }

  if(overwrite == FALSE){
    wr.path <- paste0(wrt.dir, "/",
                      tools::file_path_sans_ext(file),
                      edit.tag,".apsimx")
  }else{
    wr.path <- paste0(wrt.dir, "/", file)
  }
  
  jsonlite::write_json(apsimx_json, path = wr.path, 
                       pretty = TRUE, digits = NA, 
                       auto_unbox = TRUE, null = "null",
                       na = "null")
  
  if(verbose){
    cat("Created: ", wr.path, "\n")
  }
}

