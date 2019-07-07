#' Edit an APSIM-X (XML) Simulation
#' 
#' This function allows editing of an APSIM-X (XML) simulation file.
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
#' @name edit_apsimx_xml
#' @param file file ending in .apsimx to be edited
#' @param src.dir directory containing the .apsimx file to be edited; defaults to the current working directory
#' @param wrt.dir should be used if the destination directory is different from the src.dir
#' @param node either 'Weather', 'Soil', 'SurfaceOrganicMatter', 'MicroClimate', 'Crop', 'Manager' or 'Other' 
#' @param soil.child specific soil component to be edited
#' @param som.child specific soil organic matter component to be edited
#' @param parm parameter to be edited
#' @param value new values for the parameter to be edited 
#' @param overwrite logical; if \code{TRUE} the old file is overwritten, a new file is written otherwise
#' @param parm.path path to the attribute to edit when node is 'Other'
#' @param verbose whether to print information about successful edit
#' @return (when verbose=TRUE) complete file path to edited .apsimx file is returned as a character string.
#' As a side effect this function created a new (XML) .apsimx file.
#' @note The components that can be edited are restricted becuase this is better in preventing
#' errors of editing unintended parts of the file. The disadvantage is that there is less flexibility
#' compared to the similar function in the 'apsimr' package. Only XML files are supported at the moment.
#'  JSON files will be supported in the future.
#' @export
#' @examples 
#' \dontrun{
#' ## This example will read one of the examples distributed with APSIM-X
#' ## but write to the current directory
#' 
#' ex.dir <- auto_detect_apsimx_examples()
#' ocs <- rep(0.5, 7)
#' edit_apsimx("Maize.apsimx", src.dir = ex.dir,
#'             wrt.dir = ".",
#'             node = "Soil",
#'             soil.child = "OrganicMatter", 
#'             parm = "OC", value = ocs,
#'             verbose = FALSE)
#' ## To delete the file run
#' system("rm Maize-edited.apsimx")
#' }
#' 

edit_apsimx_xml <- function(file, src.dir = ".", wrt.dir = NULL,
                            node = c("Weather","Soil","SurfaceOrganicMatter",
                                     "MicroClimate","Crop","Manager"),
                            soil.child = c("Water","Nitrogen","OrganicMatter",
                                           "Analysis","InitialWater","Sample"),
                            som.child = c("Pools","Other"),
                            parm=NULL, value=NULL, overwrite = FALSE,
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
  
  ## For now we just edit one file at a time
  file <- match.arg(file, fileNames, several.ok=FALSE)
  
  if(!is_apsimx_xml(file = file, src.dir = src.dir))
    stop("Only XML files are supported. JSON files will be supported in the future")
  
  ## Parse apsimx file (XML file)
  apsimx_xml <- read_xml(paste0(src.dir,"/",file))
  
  ## Editing the weather file name only
  if(node == "Weather"){
    if(missing(parm)){
      parm.path <- paste0("//Weather/FileName")
      weather.filename.node <- xml_find_first(apsimx_xml, parm.path)
      if(length(grep(".met$",value)) == 0) 
        stop("value should be a .met file")
      xml_set_text(weather.filename.node, value)
    }else{
      stop("parm not appropriate for node = Weather")
    }
  }
  
  ## Editing the 'Soil' component
  if(node == "Soil"){
    if(soil.child == "Water"){
      ## First set of parameters are crop specific
      if(parm %in% c("LL","KL","XF")){
        parm.path <- paste0(".//Soil/Water/SoilCrop","/",parm)
      }
      ## Second set of parameters are soil specific
      if(parm %in% c("Thickness","BD","AirDry","LL15","DUL","SAT","KS")){
        parm.path <- paste0(".//Soil/Water","/",parm)
      }
      ## With this code it is possible to provide an incorrect parameter
      soil.water.node <- xml_find_first(apsimx_xml, parm.path)
      
      if(length(value) != length(xml_children(soil.water.node)))
        stop("value vector of incorrect length")
      
      xml_set_text(xml_children(soil.water.node), as.character(value))
    }
    
    if(soil.child == "Nitrogen"){
      
      parm.choices <- c("fom_type","fract_carb","fract_cell","fract_lign")
      parm <- match.arg(parm, choices = parm.choices)
      
      ## for all of these the length should be 6
      if(length(value) != 6) stop("value should be of length=6")
      
      parm.path <- paste0(".//Soil/SoilNitrogen","/",parm)
      soil.nitrogen.node <- xml_find_first(apsimx_xml, parm.path)
      xml_set_text(xml_children(soil.nitrogen.node), as.character(value))
    }
    
    if(soil.child == "OrganicMatter"){
      ## State what are organic matter possible parameters
      parm.ch1 <- c("RootCN","RootWt","SoilCN","EnrACoeff",
                    "EnrBCoeff")
      parm.ch2 <- c("Thickness","Depth","OC","FBiom","FInert")
      
      parm.path <- paste0(".//Soil/SoilOrganicMatter","/",parm)
      
      if(parm %in% parm.ch1){
        if(length(value) != 1) stop("value should be of length=1")
        soil.OM.node <- xml_find_first(apsimx_xml, parm.path)
        xml_set_text(soil.OM.node, as.character(value))
      }
      
      if(parm %in% parm.ch2){
        soil.OM.node <- xml_find_first(apsimx_xml, parm.path)
        
        if(length(value) != length(xml_children(soil.OM.node)))
          stop("value vector of incorrect length")
        
        xml_set_text(xml_children(soil.OM.node), as.character(value))
      }
    }
    
    if(soil.child == "Analysis"){
      ## State what are possible analysis parameters
      parm.ch <- c("Thickness","PH")
      
      parm <- match.arg(parm, choices = parm.ch)
      
      parm.path <- paste0(".//Soil/Analysis","/",parm)
      
      soil.Analysis.node <- xml_find_first(apsimx_xml, parm.path)
      
      if(length(value) != length(xml_children(soil.Analysis.node)))
        stop("value vector of incorrect length")
      
      xml_set_text(xml_children(soil.Analysis.node), as.character(value))
    }
    
    if(soil.child == "InitialWater"){
      ## State what are possible InitialWater parameters
      parm.ch <- c("PercentMethod","FractionFull","DepthWetSoil")
      
      parm <- match.arg(parm, choices = parm.ch)
      
      parm.path <- paste0(".//Soil/InitialWater","/",parm)
      
      soil.InitialWater.node <- xml_find_first(apsimx_xml, parm.path)
      
      if(length(value) != length(soil.InitialWater.node))
        stop("value vector of incorrect length")
      
      xml_set_text(soil.InitialWater.node, as.character(value))
    }
    
    if(soil.child == "Sample"){
      ## State what are possible InitialWater parameters
      parm.ch <- c("Thickness","NO3","NH4","SW","OC","EC","CL","ESP","PH")
      
      parm <- match.arg(parm, choices = parm.ch)
      
      parm.path <- paste0(".//Soil/Sample","/",parm)
      
      soil.Sample.node <- xml_find_first(apsimx_xml, parm.path)
      
      if(length(value) != length(soil.Sample.node))
        stop("value vector of incorrect length")
      
      xml_set_text(soil.Sample.node, as.character(value))
    }
    ## For now changing other components of the 'Soil' might make little sense
  }
  
  if(node == "SurfaceOrganicMatter"){
    if(som.child == "Pools"){
      ## State what are possible Pools parameters
      parm.ch <- c("PoolName","ResidueType","Mass","CNRatio",
                   "CPRatio","StandingFraction")
      
      parm <- match.arg(parm, choices = parm.ch)
      
      parm.path <- paste0(".//SurfaceOrganicMatter/Pools/Pool","/",parm)
      
      soil.Pools.Pool.node <- xml_find_first(apsimx_xml, parm.path)
      
      if(length(value) != length(soil.Pools.Pool.node))
        stop("value vector of incorrect length")
      
      xml_set_text(soil.Pools.Pool.node, as.character(value))
      
      ## The structure here is confusing because there are two places
      ## to state what the Mass, CNRAtio and StandingFraction are
      ## I'm assuming that if I change one, I need to change both
      if(parm %in% c("Mass","CNRatio","StandingFraction")){
        if(parm == "Mass")
          parm.path2 <- ".//SurfaceOrganicMatter/mass"
        if(parm == "CNRatio")
          parm.path2 <- ".//SurfaceOrganicMatter/cnr"
        if(parm == "StandingFraction")
          parm.path2 <- ".//SurfaceOrganicMatter/standing_fraction"
        
        soil.Pools.node <- xml_find_first(apsimx_xml, parm.path2)
        
        if(length(value) != length(soil.Pools.node))
          stop("value vector of incorrect length")
        
        xml_set_text(soil.Pools.node, as.character(value))
      }
    }
    
    if(som.child == "Other"){
      
      parm.ch <- c("CriticalResidueWeight",
                   "OptimumDecompTemp","MaxCumulativeEOS",
                   "CNRatioDecompCoeff","CNRatioDecompThreshold",
                   "TotalLeachRain","MinRainToLeach",
                   "CriticalMinimumOrganicC","DefaultCPRatio",
                   "DefaultStandingFraction","StandingExtinctCoeff",
                   "FractionFaecesAdded")
      
      parm <- match.arg(parm, choices = parm.ch)
      
      parm.path <- paste0(".//SurfaceOrganicMatter","/",parm)
      
      soil.Other.node <- xml_find_first(apsimx_xml, parm.path)
      
      if(length(value) != length(soil.Other.node))
        stop("value vector of incorrect length")
      
      xml_set_text(soil.Other.node, as.character(value))
      
    }
  }
  
  if(node == "MicroClimate"){
    ## These are hard coded, might use them in the future
    ## parm.ch <- c("a_interception","b_interception","c_interception",
    ##             "d_interception", "soil_albedo", "sun_angle",
    ##             "soil_heat_flux_fraction", "night_interception_fraction",
    ##             "refheight","albedo","emissivity","RadIntTotal")
    
    parm.ch <- xml_name(xml_children(xml_find_first(apsimx_xml, ".//MicroClimate")))[-c(1:2)]
    
    parm <- match.arg(parm, choices = parm.ch)
    
    parm.path <- paste0(".//MicroClimate","/",parm)
    
    soil.MicroClimate.node <- xml_find_first(apsimx_xml, parm.path)
    
    if(length(value) != length(soil.MicroClimate.node))
      stop("value vector of incorrect length")
    
    xml_set_text(soil.MicroClimate.node, as.character(value))
    
  }
  
  if(node == "Crop"){
    ## I will assume that 'SowingRule' and 'Harvesing' are always present
    ## Sowing is a case where it might not be easy to automatically 
    ## get the parameter choices
    parms.ch <- c("StartDate","EndDate","MinESW","MinRain",
                  "RainDays","CultivarName","SowingDepth",
                  "RowSpacing","Population")
    
    parm <- match.arg(parm, choices = parm.ch)
    
    parm.path <- paste0(".//Manager/Script","/",parm)
    
    soil.manager.sowingrule.node <- xml_find_first(apsimx_xml, parm.path)
    
    if(length(value) != length(soil.manager.sowingrule.node))
      stop("value vector of incorrect length")
    
    xml_set_text(soil.manager.sowingrule.node, as.character(value))
    
    ## At the moment it seems that there is nothing to edit for 'Harvesting'
  }
  
  if(node == "Manager"){
    stop("This is an advanced feature, not implemented at the moment")
  }
  
  if(node == "Other"){
    stop("This is an advanced feature, not implemented at the moment")
  }
  
  ## Still to do: Manager
  ## It might not make sense to change all of these variables
  ## For example, the plant is 'Maize' and if you really need to change this
  ## create a different .apsimx file.
  ## I'm against partial replacement as in the apsimr package
  ## In general, I think strict total replacement might 
  ## lead to fewer errors
  
  if(overwrite == FALSE){
    wr.path <- paste0(wrt.dir,"/",
                      strsplit(file,".",fixed = TRUE)[[1]][1],
                      "-edited",".apsimx")
  }else{
    wr.path <- paste0(wrt.dir,"/",file)
  }
  write_xml(apsimx_xml, file = wr.path)
  
  if(verbose){
    cat("Edited",parm.path, "\n")
    cat("Edited parameter",parm, "\n")
    cat("New values ",value, "\n")
    cat("Created ",wr.path,"\n")
  }
}
