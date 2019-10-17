#' Edit an APSIM (Classic) Simulation
#' 
#' This function allows editing of an APSIM (Classic) simulation file.
#' 
#' The variables specified by \code{parm} within the .apsim file specified by \code{file} 
#' in the source directory \code{src.dir} are edited. The old values are replaced with \code{value}, which
#' is a list that has the same number of elements as the length of the vector \code{parm}.  The current
#' .apsim file will be overwritten if \code{overwrite} is set to \code{TRUE}; otherwise the file
#' \emph{'file'-edited.apsim} will be created.  If (verbose = TRUE) then the name
#'  of the written file is returned. The function is similar to the edit_apsim functin in the 'apsimr'
#'  package, but with the difference that only some variables (parameters) can be modified.
#'  
#'  The function inspect_apsim is for a quick look from within R. The APSIM GUI provides a more
#'  complete examination of the .apsim file
#' 
#' @name edit_apsim
#' @param file file ending in .apsim to be edited
#' @param src.dir directory containing the .apsim file to be edited; defaults to the current working directory
#' @param wrt.dir should be used if the destination directory is different from the src.dir
#' @param node either 'Clock', 'Weather', 'Soil', 'SurfaceOrganicMatter', 'MicroClimate', 'Crop', 'Manager' or 'Other' 
#' @param soil.child specific soil component to be edited
#' @param manager.child specific manager component to be edited (not implemented yet)
#' @param parm parameter to be edited
#' @param value new values for the parameter to be edited 
#' @param overwrite logical; if \code{TRUE} the old file is overwritten, a new file is written otherwise
#' @param edit.tag if the file is edited a different tag from the default '-edited' can be used.
#' @param parm.path path to the attribute to edit when node is 'Other'
#' @param verbose whether to print information about successful edit
#' @return (when verbose=TRUE) complete file path to edited .apsimx file is returned as a character string.
#' As a side effect this function creates a new (XML) .apsimx file.
#' @note The components that can be edited are restricted becuase this is better in preventing
#' errors of editing unintended parts of the file. The disadvantage is that there is less flexibility
#' compared to the similar function in the 'apsimr' package. 
#' @export
#' @examples 
#' \dontrun{
#' ## This example will read one of the examples distributed with APSIM
#' ## but write to the current directory
#' 
#' extd.dir <- system.file("extdata", package = "apsimx")
#' edit_apsim("Millet", src.dir = extd.dir, node = "Clock",
#'            parm = "start_date", value = "01/02/1940")
#' }
#' 

edit_apsim <- function(file, src.dir = ".", wrt.dir = NULL,
                       node = c("Clock","Weather","Soil","SurfaceOrganicMatter",
                                     "MicroClimate","Crop","Manager","Other"),
                       soil.child = c("Water","OrganicMatter", "Chemical",
                                           "Analysis","InitialWater","Sample"),
                       manager.child = NULL,
                       parm=NULL, value=NULL, 
                       overwrite = FALSE,
                       edit.tag = "-edited",
                       parm.path = NULL,
                       verbose = TRUE){
  
  if(missing(wrt.dir)) wrt.dir <- src.dir
  
  fileNames <- dir(path = src.dir, pattern=".apsim$",ignore.case=TRUE)
  
  if(length(fileNames)==0){
    stop("There are no .apsim files in the specified directory to edit.")
  }
  
  node <- match.arg(node)
  soil.child <- match.arg(soil.child)
  
  ## For now we just edit one file at a time
  file <- match.arg(file, fileNames, several.ok=FALSE)
  
  ## Parse apsim file (XML)
  apsim_xml <- read_xml(paste0(src.dir,"/",file))
  
  ## Edit the 'Clock'
  if(node == "Clock"){
    parm.choices <- c("start_date","end_date")
    parm <- match.arg(parm, choices = parm.choices, several.ok = TRUE)
    j <- 1
    for(i in parm){
      parm.path <- paste0(".//clock","/",i)
      startend.node <- xml_find_first(apsim_xml, parm.path)
      ## apsim requires %Y-%m-%d
      xml_set_text(startend.node, as.character(value[j]))
      j <- j + 1
    }
  }
  
  ## Editing the weather file name only
  if(node == "Weather"){
    if(missing(parm)){
      parm.path <- paste0("//metfile/filename")
      weather.filename.node <- xml_find_first(apsim_xml, parm.path)
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
      
      ## Soil Water
      soil.water.parms <- c("SummerCona", "SummerU", "SummerDate",
                            "WinterCona", "WinterU", "WinterDate",
                            "DiffusConst","DiffusSlope", "Salb",
                            "CN2Bare","CNRed","CNCov")
      
      if(parm %in% soil.water.parms){
        parm.path <- paste0(".//Soil/Water","/",parm)
        soil.water.node <- xml_find_first(apsim_xml, parm.path)
        ## Error checking
        if(length(value) != length(soil.water.node))
          stop("value vector of incorrect length")
      }else{
        soil.water.node <- xml_find_first(apsim_xml, parm.path)
        if(length(value) != length(xml_children(soil.water.node)))
          stop("value vector of incorrect length")
      }
      ## With this code it is still possible to provide an incorrect parameter
      ## Not sure...
      xml_set_text(xml_children(soil.water.node), as.character(value))
    }
    
    if(soil.child == "Nitrogen"){
      stop("not implemented yet")
      parm.choices <- c("fom_type","fract_carb","fract_cell","fract_lign")
      parm <- match.arg(parm, choices = parm.choices)
      
      ## for all of these the length should be 6
      if(length(value) != 6) stop("value should be of length=6")
      
      parm.path <- paste0(".//Soil/SoilNitrogen","/",parm)
      soil.nitrogen.node <- xml_find_first(apsim_xml, parm.path)
      xml_set_text(xml_children(soil.nitrogen.node), as.character(value))
    }
    
    if(soil.child == "OrganicMatter"){
      ## State what are organic matter possible parameters
      parm.ch1 <- c("RootCN","RootWt","SoilCN","EnrACoeff",
                    "EnrBCoeff")
      parm.ch2 <- c("Thickness","OC","FBiom","FInert")
      
      parm.path <- paste0(".//Soil/SoilOrganicMatter","/",parm)
      
      if(parm %in% parm.ch1){
        if(length(value) != 1) stop("value should be of length=1")
        soil.OM.node <- xml_find_first(apsim_xml, parm.path)
        xml_set_text(soil.OM.node, as.character(value))
      }
      
      if(parm %in% parm.ch2){
        soil.OM.node <- xml_find_first(apsim_xml, parm.path)
        
        if(length(value) != length(xml_children(soil.OM.node)))
          stop("value vector of incorrect length")
        
        xml_set_text(xml_children(soil.OM.node), as.character(value))
      }
    }
    
    if(soil.child == "Analysis"){
      ## State what are possible analysis parameters
      parm.ch <- c("Thickness","PH","EC")
      
      parm <- match.arg(parm, choices = parm.ch)
      
      parm.path <- paste0(".//Soil/Analysis","/",parm)
      
      soil.Analysis.node <- xml_find_first(apsim_xml, parm.path)
      
      if(length(value) != length(xml_children(soil.Analysis.node)))
        stop("value vector of incorrect length")
      
      xml_set_text(xml_children(soil.Analysis.node), as.character(value))
    }
    
    if(soil.child == "InitialWater"){
      ## State what are possible InitialWater parameters
      parm.ch <- c("PercentMethod","FractionFull","DepthWetSoil")
      
      parm <- match.arg(parm, choices = parm.ch)
      
      parm.path <- paste0(".//Soil/InitialWater","/",parm)
      
      soil.InitialWater.node <- xml_find_first(apsim_xml, parm.path)
      
      if(length(value) != length(soil.InitialWater.node))
        stop("value vector of incorrect length")
      
      xml_set_text(soil.InitialWater.node, as.character(value))
    }
    
    if(soil.child == "Sample"){
      ## State what are possible InitialWater parameters
      parm.ch <- c("Thickness","NO3","NH4","SW","OC","EC","CL","ESP","PH")
      
      parm <- match.arg(parm, choices = parm.ch)
      
      parm.path <- paste0(".//Soil/Sample","/",parm)
      
      soil.Sample.node <- xml_find_first(apsim_xml, parm.path)
      
      if(length(value) != length(soil.Sample.node))
        stop("value vector of incorrect length")
      
      xml_set_text(soil.Sample.node, as.character(value))
    }
    ## For now changing other components of the 'Soil' might make little sense
  }
  
  if(node == "SurfaceOrganicMatter"){
      ## State what are possible Pools parameters
      parm.ch <- c("PoolName","ResidueType","Mass","CNRatio",
                   "CPRatio","StandingFraction", "type", "mass",
                   "cnr","standing_fraction")
      
      parm <- match.arg(parm, choices = parm.ch)
      
      parm.path <- paste0(".//surfaceom/",parm)
      
      soil.Pools.Pool.node <- xml_find_first(apsim_xml, parm.path)
      
      if(length(value) != length(soil.Pools.Pool.node))
        stop("value vector of incorrect length")
      
      xml_set_text(soil.Pools.Pool.node, as.character(value))
  }
  
  if(node == "Crop" | node == "Manager"){
    
    ## Trying to figure this out
    ## xml_text(xml_find_all(apsim_xml, ".//manager[4]/ui/fert_date"))
    ## xml_path(xml_find_all(apsim_xml,".//fert_date"))

    crop.node <- xml_find_all(apsim_xml, parm)
    
    if(length(value) != length(crop.node))
      stop("value vector of incorrect length")
    
    xml_set_text(crop.node, as.character(value))
  }
  
  if(node == "Other"){
    other.node <- xml_find_first(apsim_xml, parm.path)
    
    if(length(value) != length(other.node))
      stop("value vector of incorrect length")
    
    xml_set_text(other.node, as.character(value))
  }
  
  if(overwrite == FALSE){
    wr.path <- paste0(wrt.dir,"/",
                      strsplit(file,".",fixed = TRUE)[[1]][1],
                      edit.tag,".apsim")
  }else{
    wr.path <- paste0(wrt.dir,"/",file)
  }
  write_xml(apsim_xml, file = wr.path)
  
  if(verbose){
    cat("Edited",parm.path, "\n")
    cat("Edited parameter",parm, "\n")
    cat("New values ",value, "\n")
    cat("Created ",wr.path,"\n")
  }
}


