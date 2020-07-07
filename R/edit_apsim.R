#' Edit an APSIM (Classic) Simulation
#' 
#' This function allows editing of an APSIM (Classic) simulation file.
#' 
#' The variables specified by \code{parm} within the .apsim file specified by \code{file} 
#' in the source directory \code{src.dir} are edited. The old values are replaced with \code{value}, which
#' is a list that has the same number of elements as the length of the vector \code{parm}.  The current
#' .apsim file will be overwritten if \code{overwrite} is set to \code{TRUE}; otherwise the file
#' \sQuote{file} \emph{-edited.apsim} will be created.  If (verbose = TRUE) then the name
#'  of the written file is returned. The function is similar to the edit_apsim functin in the \sQuote{apsimr}
#'  package, but with the difference that only some variables (parameters) can be modified.
#'  
#'  The function inspect_apsim is for a quick look from within R. The APSIM GUI provides a more
#'  complete examination of the .apsim file
#' 
#' @name edit_apsim
#' @param file file ending in .apsim to be edited
#' @param src.dir directory containing the .apsim file to be edited; defaults to the current working directory
#' @param wrt.dir should be used if the destination directory is different from the src.dir
#' @param node either \sQuote{Clock}, \sQuote{Weather}, \sQuote{Soil}, 
#' \sQuote{SurfaceOrganicMatter}, \sQuote{MicroClimate}, \sQuote{Crop}, \sQuote{Manager} or \sQuote{Other} 
#' @param soil.child specific soil component to be edited
#' @param manager.child specific manager component to be edited (not implemented yet)
#' @param parm parameter to be edited
#' @param value new values for the parameter to be edited 
#' @param overwrite logical; if \code{TRUE} the old file is overwritten, a new file is written otherwise
#' @param edit.tag if the file is edited a different tag from the default \sQuote{-edited} can be used.
#' @param parm.path path to the attribute to edit when node is \sQuote{Other}
#' @param verbose whether to print information about successful edit
#' @param check.length check whether vectors are of the correct length
#' @return (when verbose=TRUE) complete file path to edited .apsimx file is returned as a character string.
#' As a side effect this function creates a new (XML) .apsimx file.
#' @note The components that can be edited are restricted becuase this is better in preventing
#' errors of editing unintended parts of the file. The disadvantage is that there is less flexibility
#' compared to the similar function in the 'apsimr' package. 
#' @export
#' @examples 
#' \donttest{
#' ## This example will read one of the examples distributed with APSIM
#' ## but write to a temporary directory
#' 
#' tmp.dir <- tempdir()
#' 
#' extd.dir <- system.file("extdata", package = "apsimx")
#' edit_apsim("Millet", src.dir = extd.dir, wrt.dir = tmp.dir, 
#'            node = "Clock",
#'            parm = "start_date", value = "01/02/1940")
#' }
#' 

edit_apsim <- function(file, src.dir = ".", wrt.dir = NULL,
                       node = c("Clock", "Weather", "Soil", "SurfaceOrganicMatter",
                                "MicroClimate", "Crop", "Manager", "Other"),
                       soil.child = c("Metadata", "Water", "OrganicMatter", "Chemical",
                                      "Analysis", "InitialWater", "Sample", "SWIM"),
                       manager.child = NULL,
                       parm = NULL, value = NULL, 
                       overwrite = FALSE,
                       edit.tag = "-edited",
                       parm.path = NULL,
                       verbose = TRUE,
                       check.length = TRUE){
  
  if(missing(wrt.dir)) wrt.dir <- src.dir
  
  file.names <- dir(path = src.dir, pattern=".apsim$",ignore.case=TRUE)
  
  if(identical(length(file.names),0))
    stop("There are no .apsim files in the specified directory to edit.")
  
  node <- match.arg(node)
  soil.child <- match.arg(soil.child)
  
  ## For now we just edit one file at a time
  file <- match.arg(file, file.names)
  
  ## Parse apsim file (XML)
  apsim_xml <- xml2::read_xml(paste0(src.dir, "/", file))
  
  ## Edit the 'Clock'
  if(node == "Clock"){
    parm.choices <- c("start_date", "end_date")
    parm <- match.arg(parm, choices = parm.choices, several.ok = TRUE)
    j <- 1
    for(i in parm){
      parm.path <- paste0(".//clock","/", i)
      startend.node <- xml2::xml_find_first(apsim_xml, parm.path)
      ## apsim requires %Y-%m-%d
      xml2::xml_set_text(startend.node, as.character(value[j]))
      j <- j + 1
    }
  }
  
  ## Editing the weather file name only
  if(node == "Weather"){
    if(missing(parm)){
      parm.path <- paste0("//metfile/filename")
      weather.filename.node <- xml2::xml_find_first(apsim_xml, parm.path)
      if(length(grep(".met$", value)) == 0) 
        stop("value should be a .met file")
      xml2::xml_set_text(weather.filename.node, value)
    }else{
      stop("parm not appropriate for node = Weather")
    }
  }
  
  ## Editing the 'Soil' component
  if(node == "Soil"){
    
    if(soil.child == "Metadata"){
      ## Not sure if there is minimum set of required parameters
      parm.path <- paste0(".//Soil/", parm)
      soil.metadata.node <- xml2::xml_find_first(apsim_xml, parm.path)
      xml2::xml_set_text(soil.metadata.node, as.character(value))
    }
    
    if(soil.child == "Water"){
      ## First set of parameters are crop specific
      if(parm %in% c("LL", "KL", "XF")){
        parm.path <- paste0(".//Soil/Water/SoilCrop", "/", parm)
      }
      ## Second set of parameters are soil specific
      if(parm %in% c("Thickness", "BD", "AirDry", "LL15", "DUL", "SAT", "KS")){
        parm.path <- paste0(".//Soil/Water", "/", parm)
      }
      
      ## Soil Water
      soil.water.parms <- c("SummerCona", "SummerU", "SummerDate",
                            "WinterCona", "WinterU", "WinterDate",
                            "DiffusConst","DiffusSlope", "Salb",
                            "CN2Bare", "CNRed", "CNCov")
      
      if(parm %in% soil.water.parms){
        ## These are of length 1
        parm.path <- paste0(".//Soil/Water", "/", parm)
        soil.water.node <- xml2::xml_find_first(apsim_xml, parm.path)
        ## Error checking
        if(check.length){
          if(length(value) != length(soil.water.node))
            stop("value vector of incorrect length")
        }
        xml2::xml_set_text(soil.water.node, as.character(value))
      }else{
        soil.water.node <- xml2::xml_find_first(apsim_xml, parm.path)
        if(check.length){
          if(length(value) != length(xml2::xml_children(soil.water.node)))
            stop("value vector of incorrect length")
        }
        ## With this code it is still possible to provide an incorrect parameter
        ## Not sure...
        len.child.soil.water.node <- length(xml2::xml_children(soil.water.node))
        ## If value is larger I grow the children to match lengths
        if(length(value) > len.child.soil.water.node){
            for(i in seq_len(length(value) - len.child.soil.water.node)){
              xml2::xml_add_child(soil.water.node, xml2::xml_children(soil.water.node)[[len.child.soil.water.node]])
            }
        }
        if(length(value) < len.child.soil.water.node){
          cat("length of value is shorter than length of soil water node.\n")
          stop("Don't really know how to do this yet")
        }
        xml2::xml_set_text(xml2::xml_children(soil.water.node), as.character(value))
      }
    }

    if(soil.child == "SWIM"){
      
      parm.path.0 <- ".//Soil/Swim"
      ## This first set are single values
      swim.parms1 <- c("Salb", "CN2Bare", "CNRed", "CNCov", "KDul",
                       "PSIDul", "VC", "DTmin", "DTmax", "MaxWaterIncrement",
                       "SpaceWeightingFactor", "SoluteSpaceWeightingFactor",
                       "Diagnostics")

      swim.parms2 <- c("DrainDepth", "DrainSpacing", "DrainRadius", "Klat", "ImpermDepth")
      
      if(parm %in% swim.parms1){
        parm.path <- paste0(".//Soil/Swim", "/", parm)
      }
      if(parm == "WaterTableDepth"){
        parm.path <- ".//Soil/Swim/SwimWaterTable/WaterTableDepth"
      }
      if(parm %in% swim.parms2){
        parm.path <- paste0(".//Soil/Swim/SwimSubsurfaceDrain", "/", parm)
      }
      soil.swim.node <- xml2::xml_find_first(apsim_xml, parm.path)
      xml2::xml_set_text(soil.swim.node, as.character(value))
    }
        
    if(soil.child == "Nitrogen"){
      stop("not implemented yet")
      parm.choices <- c("fom_type", "fract_carb", "fract_cell", "fract_lign")
      parm <- match.arg(parm, choices = parm.choices)
      
      ## for all of these the length should be 6
      if(length(value) != 6) stop("value should be of length=6")
      
      parm.path <- paste0(".//Soil/SoilNitrogen", "/", parm)
      soil.nitrogen.node <- xml2::xml_find_first(apsim_xml, parm.path)
      xml2::xml_set_text(xml2::xml_children(soil.nitrogen.node), as.character(value))
    }
    
    if(soil.child == "OrganicMatter"){
      ## State what are organic matter possible parameters
      parm.ch1 <- c("RootCN", "RootWt", "SoilCN", "EnrACoeff", "EnrBCoeff")
      parm.ch2 <- c("Thickness", "OC", "FBiom", "FInert")
      
      parm.path <- paste0(".//Soil/SoilOrganicMatter", "/", parm)
      
      if(parm %in% parm.ch1){
        if(length(value) != 1) stop("value should be of length = 1")
        soil.OM.node <- xml2::xml_find_first(apsim_xml, parm.path)
        xml2::xml_set_text(soil.OM.node, as.character(value))
      }
      
      if(parm %in% parm.ch2){
        soil.OM.node <- xml2::xml_find_first(apsim_xml, parm.path)
        
        if(check.length){
          if(length(value) != length(xml2::xml_children(soil.OM.node)))
            stop("value vector of incorrect length")
        }
        
        len.child.soil.OM.node <- length(xml2::xml_children(soil.OM.node))
        ## If value is larger I grow the children to match lengths
        if(length(value) > len.child.soil.OM.node){
          for(i in seq_len(length(value) - len.child.soil.OM.node)){
            xml2::xml_add_child(soil.OM.node, xml2::xml_children(soil.OM.node)[[len.child.soil.OM.node]])
          }
        }
        if(length(value) < len.child.soil.OM.node){
          cat("length of value is shorter than length of soil water node. \n At the moment I think I can grow XML nodes but not shrink them.\n")
          stop("Don't really know how to do this yet.")
        }
        xml2::xml_set_text(xml2::xml_children(soil.OM.node), as.character(value))
      }
    }
    
    if(soil.child == "Analysis"){
      ## State what are possible analysis parameters
      parm.ch <- c("Thickness", "PH", "EC")
      
      parm <- match.arg(parm, choices = parm.ch)
      
      parm.path <- paste0(".//Soil/Analysis", "/", parm)
      
      soil.Analysis.node <- xml2::xml_find_first(apsim_xml, parm.path)
      
      if(check.length){
        if(length(value) != length(xml2::xml_children(soil.Analysis.node)))
          stop("value vector of incorrect length")
      }
      
      len.child.soil.Analysis.node <- length(xml2::xml_children(soil.Analysis.node))
      ## If value is larger I grow the children to match lengths
      if(length(value) > len.child.soil.Analysis.node){
        for(i in seq_len(length(value) - len.child.soil.Analysis.node)){
          xml2::xml_add_child(soil.Analysis.node, xml2::xml_children(soil.Analysis.node)[[len.child.soil.Analysis.node]])
        }
      }
      if(length(value) < len.child.soil.Analysis.node){
        cat("length of value is shorter than length of soil water node. \n At the moment I think I can grow XML nodes but not shrink them.\n")
        stop("Don't really know how to do this yet.")
      }
      
      xml2::xml_set_text(xml2::xml_children(soil.Analysis.node), as.character(value))
    }
    
    if(soil.child == "InitialWater"){
      ## State what are possible InitialWater parameters
      parm.ch <- c("PercentMethod", "FractionFull", "DepthWetSoil")
      
      parm <- match.arg(parm, choices = parm.ch)
      
      parm.path <- paste0(".//Soil/InitialWater", "/", parm)
      
      soil.InitialWater.node <- xml2::xml_find_first(apsim_xml, parm.path)
      
      if(check.length){
        if(length(value) != length(soil.InitialWater.node))
          stop("value vector of incorrect length")
      }
      
      len.child.soil.InitialWater.node <- length(xml2::xml_children(soil.InitialWater.node))
      ## If value is larger I grow the children to match lengths
      if(length(value) > len.child.soil.InitialWater.node){
        for(i in seq_len(length(value) - len.child.soil.InitialWater.node)){
          xml2::xml_add_child(soil.InitialWater.node, xml2::xml_children(soil.InitialWater.node)[[len.child.soil.InitialWater.node]])
        }
      }
      if(length(value) < len.child.soil.InitialWater.node){
        cat("length of value is shorter than length of soil water node. \n At the moment I think I can grow XML nodes but not shrink them.\n")
        stop("Don't really know how to do this yet.")
      }
      
      xml2::xml_set_text(soil.InitialWater.node, as.character(value))
    }
    
    if(soil.child == "Sample"){
      ## State what are possible InitialWater parameters
      parm.ch <- c("Thickness", "NO3", "NH4", "SW", "OC", "EC", "CL", "ESP", "PH")
      
      parm <- match.arg(parm, choices = parm.ch)
      
      parm.path <- paste0(".//Soil/Sample", "/", parm)
      
      soil.Sample.node <- xml2::xml_find_first(apsim_xml, parm.path)
      
      if(check.length){
        if(length(value) != length(soil.Sample.node))
          stop("value vector of incorrect length")
      }
      
      len.child.soil.Sample.node <- length(xml2::xml_children(soil.Sample.node))
      ## If value is larger I grow the children to match lengths
      if(length(value) > len.child.soil.Sample.node){
        for(i in seq_len(length(value) - len.child.soil.Sample.node)){
          xml2::xml_add_child(soil.Sample.node, xml2::xml_children(soil.Sample.node)[[len.child.soil.Sample.node]])
        }
      }
      if(length(value) < len.child.soil.Sample.node){
        cat("length of value is shorter than length of soil water node. \n At the moment I think I can grow XML nodes but not shrink them.\n")
        stop("Don't really know how to do this yet.")
      }
      
      xml2::xml_set_text(xml2::xml_children(soil.Sample.node), as.character(value))
    }
    ## For now changing other components of the 'Soil' might make little sense
  }
  
  if(node == "SurfaceOrganicMatter"){
      ## State what are possible Pools parameters
      parm.ch <- c("PoolName", "ResidueType", "Mass", "CNRatio",
                   "CPRatio", "StandingFraction", "type", "mass",
                   "cnr", "standing_fraction")
      
      parm <- match.arg(parm, choices = parm.ch)
      
      parm.path <- paste0(".//surfaceom/", parm)
      
      soil.Pools.Pool.node <- xml2::xml_find_first(apsim_xml, parm.path)
      
      if(check.length){
        if(length(value) != length(soil.Pools.Pool.node))
          stop("value vector of incorrect length")
      }
      
      len.child.soil.Pools.Pool.node <- length(xml2::xml_children(soil.Pools.Pool.node))
      ## If value is larger I grow the children to match lengths
      if(length(value) > len.child.soil.Pools.Pool.node){
        for(i in seq_len(length(value) - len.child.soil.Pools.Pool.node)){
          xml2::xml_add_child(soil.Pools.Pool.node, xml2::xml_children(soil.Pools.Pool.node)[[len.child.soil.Pools.Pool.node]])
        }
      }
      if(length(value) < len.child.soil.Pools.Pool.node){
        cat("length of value is shorter than length of soil water node. \n At the moment I think I can grow XML nodes but not shrink them.\n")
        stop("Don't really know how to do this yet.")
      }
      
      xml2::xml_set_text(soil.Pools.Pool.node, as.character(value))
  }
  
  if(node == "Crop" || node == "Manager"){
    
    if(!missing(manager.child)){
      
      manager.node <- xml2::xml_find_all(apsim_xml, ".//manager")
      manager.node.names <- xml2::xml_attr(manager.node, "name")
      wmnn <- grep(manager.child, manager.node.names, ignore.case = TRUE)
      
      if(length(wmnn) == 0L) stop("manager child not found")
      
      select.manager.node <- manager.node[[wmnn]]
      
      ## If parameter is missing I should print available options, maybe...
      parm.select.manager.node <- xml2::xml_find_first(select.manager.node, paste0(".//",parm))
      
      xml2::xml_set_text(parm.select.manager.node, as.character(value))
      
    }else{
      crop.node <- xml2::xml_find_all(apsim_xml, parm)
      
      if(length(crop.node) == 0) stop("parm not found")
      
      if(check.length){
        if(length(value) != length(crop.node))
            stop("value vector of incorrect length")
      }
      
      xml2::xml_set_text(crop.node, as.character(value))
      
    }
  }
  
  if(node == "Other"){
    other.node <- xml2::xml_find_first(apsim_xml, parm.path)
    
    if(length(other.node) == 0) stop("other node parameter not found")
    
    xml2::xml_set_text(other.node, as.character(value))
  }
  
  if(overwrite == FALSE){
    wr.path <- paste0(wrt.dir, "/",
                      tools::file_path_sans_ext(file),
                      edit.tag, ".apsim")
  }else{
    wr.path <- paste0(wrt.dir, "/", file)
  }
  xml2::write_xml(apsim_xml, file = wr.path)
  
  if(verbose){
    cat("Edited", parm.path, "\n")
    cat("Edited parameter", parm, "\n")
    cat("New values ", value, "\n")
    cat("Created ", wr.path, "\n")
  }
}


