#' Edit an APSIM-X Simulation
#' 
#' This function allows editing of an APSIM-X simulation file.
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
#' @name edit_apsimx
#' @param file file ending in .apsimx to be edited
#' @param src.dir directory containing the .apsimx file to be edited; defaults to the current working directory
#' @param wrt.dir should be used if the destination directory is different from the src.dir
#' @param node either 'Weather', 'Soil', 'SurfaceOrganicMatter', 'MicroClimate', 'Crop' or 'Manager' (for now) 
#' @param soil.child specific soil component to be edited
#' @param som.child specific soil organic matter component to be edited
#' @param parm parameter to be edited
#' @param value new values for the parameter to be edited 
#' @param overwrite logical; if \code{TRUE} the old file is overwritten, a new file is written otherwise
#' @param verbose whether to print information about successful edit
#' @return (when verbose=TRUE) complete file path to edited .apsimx file is returned as a character string.
#' As a side effect this function created a new (XML) .apsimx file.
#' @note The components that can be edited are restricted becuase this is better in preventing
#' errors of editing unintended parts of the file. The disadvantage is that there is less flexibility
#' compared to the similar function in the 'apsimr' package
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

edit_apsimx <- function(file, src.dir = ".", wrt.dir = NULL,
                        node = c("Weather","Soil","SurfaceOrganicMatter",
                                 "MicroClimate","Crop","Manager"),
                        soil.child = c("Water","Nitrogen","OrganicMatter",
                                       "Analysis","InitialWater","Sample"),
                        som.child = c("Pools","Other"),
                        parm=NULL, value=NULL, overwrite = FALSE,
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

#' Similar structure to the 'edit_apsimx' file but aimed at a simple way
#' of inspecting a file
#' 
#' @title Inspect and .apsimx file
#' @name inspect_apsimx
#' @param file file ending in .apsimx to be inspected
#' @param src.dir directory containing the .apsimx file to be inspected; defaults to the current working directory
#' @param node either 'Weather', 'Soil', 'SurfaceOrganicMatter', 'MicroClimate', 'Crop' or 'Manager'
#' @param soil.child specific soil component to be inspected
#' @param som.child specific soil organic matter component to be inspected
#' @param digits number of decimals to print
#' @return table with inspected parameters and values
#' @export
#' @note node 'Manager' can be complicated and it is not guranteed to work
#' @examples 
#' \dontrun{
#' ex.dir <- auto_detect_apsimx_examples()
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Weather")        
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Soil", soil.child = "Water")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Soil", soil.child = "Nitrogen")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Soil", soil.child = "OrganicMatter")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Soil", soil.child = "Analysis")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Soil", soil.child = "InitialWater")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Soil", soil.child = "Sample")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "SurfaceOrganicMatter", som.child = "Pools")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "SurfaceOrganicMatter", som.child = "Other")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "MicroClimate")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Crop")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Manager")
#' }
#' 

inspect_apsimx <- function(file = "", src.dir = ".", 
                           node = c("Weather","Soil","SurfaceOrganicMatter",
                                    "MicroClimate","Crop","Manager"),
                           soil.child = c("Water","Nitrogen","OrganicMatter",
                                          "Analysis","InitialWater","Sample"),
                           som.child = c("Pools","Other"),
                           digits = 3){
  
  fileNames <- dir(path = src.dir, pattern=".apsimx$",ignore.case=TRUE)
  
  if(length(fileNames)==0){
    stop("There are no .apsimx files in the specified directory to edit.")
  }
  
  node <- match.arg(node)
  soil.child <- match.arg(soil.child)
  som.child <- match.arg(som.child)
  
  ## For now we just edit one file at a time
  file <- match.arg(file, fileNames, several.ok=FALSE)
  
  ## Parse apsimx file (XML file)
  apsimx_xml <- read_xml(paste0(src.dir,"/",file))
  
  if(node == "Weather"){
      parm.path <- paste0("//Weather/FileName")
      weather.filename.node <- xml_find_first(apsimx_xml, parm.path)
      return(xml_text(weather.filename.node))
  }
  
  if(node == "Soil"){
    ## Soil depths for naming columns
    depths.path <- ".//Soil/SoilOrganicMatter/Depth"
    soil.depths <- xml_text(xml_children(xml_find_first(apsimx_xml, depths.path)))
    soil.depths <- gsub("-","_to_",soil.depths, fixed = TRUE)
    soil.depths <- paste0("L_",soil.depths)
    ## Determine the number of soil layers
    number.soil.layers <- length(xml_children(xml_find_first(apsimx_xml, 
                                                             ".//Soil/Water/SoilCrop/LL")))
    
    if(soil.child == "Water"){
      ## Crop specific
      crop.parms <- c("LL","KL","XF")
      
      val.mat <- matrix(NA, nrow = length(crop.parms),
                        ncol = number.soil.layers)
      colnames(val.mat) <- soil.depths
      crop.d <- data.frame(parm = crop.parms, val.mat)
      for(i in crop.parms){
        parm.path <- paste0(".//Soil/Water/SoilCrop","/",i)
        soil.water.crop.node <- xml_find_first(apsimx_xml, parm.path)
        crop.d[crop.d$parm == i,2:(number.soil.layers+1)] <- xml_double(xml_children(soil.water.crop.node))
      }
      soil.parms <- c("Thickness","BD","AirDry","LL15","DUL","SAT","KS")
      val.mat <- matrix(NA, nrow = length(soil.parms),
                        ncol = number.soil.layers)
      colnames(val.mat) <- soil.depths
      soil.d <- data.frame(parm = soil.parms, val.mat)
      for(i in soil.parms){
        parm.path <- paste0(".//Soil/Water","/",i)
        soil.water.node <- xml_find_first(apsimx_xml, parm.path)
        soil.d[soil.d$parm == i,2:(number.soil.layers+1)] <- xml_double(xml_children(soil.water.node))
      }
      return(kable(rbind(crop.d,soil.d), digits = digits))
    }
    
    if(soil.child == "Nitrogen"){
      
      nitrogen.parms <- c("fom_type","fract_carb","fract_cell","fract_lign")
      
      val.mat <- matrix(NA, ncol = length(nitrogen.parms),
                        nrow = 6, 
                        dimnames = list(NULL,nitrogen.parms))
      
      nitro.d <- data.frame(val.mat)
      k <- 1
      for(i in nitrogen.parms){
        parm.path <- paste0(".//Soil/SoilNitrogen","/",i)
        soil.nitrogen.node <- xml_find_first(apsimx_xml, parm.path)
        nitro.d[,k] <- xml_text(xml_children(soil.nitrogen.node))
        k <- k + 1
      }
      return(kable(nitro.d, digits = digits))
    }
    
    if(soil.child == "OrganicMatter"){
      ## State what are organic matter possible parameters
      ## Will keep these ones hard coded
      som.parms1 <- c("RootCN","RootWt","SoilCN","EnrACoeff",
                    "EnrBCoeff")
      som.parms2 <- c("Thickness","Depth","OC","FBiom","FInert")
      
      som.d1 <- data.frame(parm = som.parms1, value = NA)
      
      for(i in som.parms1){
        parm.path <- paste0(".//Soil/SoilOrganicMatter","/",i)
        soil.som1.node <- xml_find_first(apsimx_xml, parm.path)
        som.d1[som.d1$parm == i,2] <- xml_text(soil.som1.node)
      }
      print(kable(som.d1, digits = digits))
      
      val.mat <- matrix(NA, nrow = length(som.parms2),
                        ncol = number.soil.layers)
      colnames(val.mat) <- soil.depths
      som.d2 <- data.frame(parm = som.parms2, val.mat)
      
      for(i in som.parms2){
        parm.path <- paste0(".//Soil/SoilOrganicMatter","/",i)
        soil.som2.node <- xml_find_first(apsimx_xml, parm.path)
        som.d2[som.d2$parm == i,2:(number.soil.layers+1)] <- xml_text(xml_children(soil.som2.node))
      }
      return(kable(som.d2, digits = digits))
    }
    
    if(soil.child == "Analysis"){
      
      ## I will keep this one hard coded because it is simple
      analysis.parms <- c("Thickness","PH")
      
      val.mat <- matrix(NA, nrow = length(analysis.parms),
                        ncol = number.soil.layers)
      colnames(val.mat) <- soil.depths
      analysis.d <- data.frame(parm = analysis.parms, val.mat)

      for(i in analysis.parms){
        parm.path <- paste0(".//Soil/Analysis","/",i)
        soil.analysis.node <- xml_find_first(apsimx_xml, parm.path)
        analysis.d[analysis.d$parm == i,2:(number.soil.layers+1)] <- xml_text(xml_children(soil.analysis.node))
      }
      return(kable(analysis.d, digits = digits))
    }
    
    if(soil.child == "InitialWater"){
      initialwater.parms <- c("PercentMethod","FractionFull","DepthWetSoil")
      
      initial.water.d <- data.frame(parm = initialwater.parms, value = NA)
      
      for(i in initialwater.parms){
        parm.path <- paste0(".//Soil/InitialWater","/",i)
        soil.InitialWater.node <- xml_find_first(apsimx_xml, parm.path)
        initial.water.d[initial.water.d$parm == i,2] <- xml_text(soil.InitialWater.node)
      }
      return(kable(initial.water.d, digits = digits))
    }
    
    if(soil.child == "Sample"){
      sample.parms <- c("Thickness","NO3","NH4","SW","OC","EC","CL","ESP","PH")
      
      val.mat <- matrix(NA, nrow = length(sample.parms),
                        ncol = number.soil.layers)
      colnames(val.mat) <- soil.depths
      sample.d <- data.frame(parm = sample.parms, val.mat)
      
      for(i in sample.parms){
        parm.path <- paste0(".//Soil/Sample","/",i)
        soil.sample.node <- xml_find_first(apsimx_xml, parm.path)
        sample.d[sample.d$parm == i,2:(number.soil.layers+1)] <- xml_text(xml_children(soil.sample.node))
      }
      return(kable(sample.d, digits = digits))
    }
  }
  
  if(node == "SurfaceOrganicMatter"){
    if(som.child == "Pools"){
      ## The lines below are hardcoded, which could be used if we want to
      ## restrict the variables to inspect
      ## pools.parms <- c("PoolName","ResidueType","Mass","CNRatio",
      ##             "CPRatio","StandingFraction")
      pools.path <- ".//SurfaceOrganicMatter/Pools/Pool"
      pools.parms <- xml_name(xml_children(xml_find_first(apsimx_xml, pools.path)))
      
      pools.d <- data.frame(parm = pools.parms, value = NA)
      
      for(i in pools.parms){
        parm.path <- paste0(pools.path,"/",i)
        soil.pools.node <- xml_find_first(apsimx_xml, parm.path)
        pools.d[pools.d$parm == i,2] <- xml_text(soil.pools.node)
      }
      return(kable(pools.d, digits = digits))
    }
    
    if(som.child == "Other"){
      ## The lines below are hardcoded, which could be used if we want to
      ## restrict the variables to inspect
      ## other.parms <- c("CriticalResidueWeight",
      ##             "OptimumDecompTemp","MaxCumulativeEOS",
      ##             "CNRatioDecompCoeff","CNRatioDecompThreshold",
      ##             "TotalLeachRain","MinRainToLeach",
      ##             "CriticalMinimumOrganicC","DefaultCPRatio",
      ##             "DefaultStandingFraction","StandingExtinctCoeff",
      ##             "FractionFaecesAdded")
      
      other.parms <- xml_name(xml_children(xml_find_first(apsimx_xml, ".//SurfaceOrganicMatter")))[-c(1:2)]
      
      other.d <- data.frame(parm = other.parms, value = NA)
      
      for(i in other.parms){
        parm.path <- paste0(".//SurfaceOrganicMatter","/",i)
        soil.other.node <- xml_find_first(apsimx_xml, parm.path)
        other.d[other.d$parm == i,2] <- xml_text(soil.other.node)
      }
      return(kable(other.d, digits = digits))
    }
  }
  
  if(node == "MicroClimate"){
    ## The lines below would be the hard coded version which could work
    ## If we want to restrict the variables which can be inspected
    ## microclimate.parms <- c("a_interception","b_interception","c_interception",
    ##             "d_interception", "soil_albedo", "sun_angle",
    ##             "soil_heat_flux_fraction", "night_interception_fraction",
    ##             "refheight","albedo","emissivity","RadIntTotal")
    
    ## Names 'Name' and 'IncludeInDocumentation' will not be inspected
    microclimate.parms <- xml_name(xml_children(xml_find_first(apsimx_xml, ".//MicroClimate")))[-c(1:2)]
    
    microclimate.d <- data.frame(parm = microclimate.parms, value = NA)
    
    for(i in microclimate.parms){
      parm.path <- paste0(".//MicroClimate","/",i)
      soil.microclimate.node <- xml_find_first(apsimx_xml, parm.path)
      microclimate.d[microclimate.d$parm == i,2] <- xml_text(soil.microclimate.node)
    }
    return(kable(microclimate.d, digits = digits))
  }
  
  if(node == "Crop"){
    ## I will assume that 'SowingRule' and 'Harvesting' are
    ## always present, but this might not be true for some crops
    ## Print name of Crop
    cat("Crop Type: ",xml_text(xml_find_first(apsimx_xml, ".//Plant/CropType")),"\n")
    ## Make sure sowing rule is present
    if(length(grep("SowingRule",xml_find_all(apsimx_xml, ".//Manager/Name"))) == 0)
         stop("SowingRule not found")
    
    sowingrule.parms <- c("StartDate","EndDate","MinESW","MinRain",
                          "RainDays","CultivarName","SowingDepth",
                          "RowSpacing","Population")
    
    sowingrule.d <- data.frame(parm = sowingrule.parms, value = NA)
    
    for(i in sowingrule.parms){
      parm.path <- paste0(".//Manager/Script","/",i)
      manager.sowingrule.node <- xml_find_first(apsimx_xml, parm.path)
      sowingrule.d[sowingrule.d$parm == i,2] <- xml_text(manager.sowingrule.node)
    }
    
    ## Just check that there is Harvesting
    if(length(grep("Harvesting",xml_find_all(apsimx_xml, ".//Manager/Name"))) == 0)
      stop("Harvesting not found")
    
    return(kable(sowingrule.d, digits = digits))
  }
  
  if(node == "Manager"){
    ## First print available 'Manager' options
    ## This is not bullet-proof as I do not know what to expect with
    ## 'Other' Manager options
    amo <- xml_text(xml_find_all(apsimx_xml, ".//Manager/Name"))
    w.amo <- which(amo0 != "Harvesting" & amo0 != "SowingRule")
    cat("Other Manager Components: \n",amo[w.amo],"\n")
    
    ## This is rough at the moment
    ## Available components within Other Manager Components
    ms.attr <- xml_children(xml_find_all(apsimx_xml, paste0(".//Manager/Script"))[w.amo])
    ms.nm <- xml_name(ms.attr)
    ms.vl <- xml_text(ms.attr)
    
    return(kable(data.frame(parm = ms.nm, value = ms.vl), digits = digits))

  }
}






