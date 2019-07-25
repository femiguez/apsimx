
#' 
#' @title Inspect an .apsimx (XML) file
#' @name inspect_apsimx_xml
#' @description inspect an XML apsimx file. It does not replace the GUI, but it can save time by quickly checking parameters and values.
#' @param file file ending in .apsimx to be inspected (XML)
#' @param src.dir directory containing the .apsimx file to be inspected; defaults to the current working directory
#' @param node either 'Weather', 'Soil', 'SurfaceOrganicMatter', 'MicroClimate', 'Crop', 'Manager' or 'Other'
#' @param soil.child specific soil component to be inspected
#' @param som.child specific surface organic matter component to be inspected ('Pools' or 'Other')
#' @param parm parameter to inspect when node = 'Other'
#' @param digits number of decimals to print (default 3)
#' @details This is simply a script that prints the relevant parameters which are likely to need editing. It does not print all information from an .apsimx file.
#' @return table with inspected parameters and values
#' @export
#' @note node 'Manager' can be complicated and it is not guranteed to work. 
#' @examples 
#' \dontrun{
#' ex.dir <- auto_detect_apsimx_examples()
#' inspect_apsimx_xml("Maize", src.dir = ex.dir, node = "Clock")
#' inspect_apsimx_xml("Maize", src.dir = ex.dir, node = "Weather") 
#' inspect_apsimx_xml("Maize", src.dir = ex.dir, node = "Soil", soil.child = "Water")
#' inspect_apsimx_xml("Maize", src.dir = ex.dir, node = "Soil", soil.child = "Nitrogen")
#' inspect_apsimx_xml("Maize", src.dir = ex.dir, node = "Soil", soil.child = "OrganicMatter")
#' inspect_apsimx_xml("Maize", src.dir = ex.dir, node = "Soil", soil.child = "Analysis")
#' inspect_apsimx_xml("Maize", src.dir = ex.dir, node = "Soil", soil.child = "InitialWater")
#' inspect_apsimx_xml("Maize", src.dir = ex.dir, node = "Soil", soil.child = "Sample")
#' inspect_apsimx_xml("Maize", src.dir = ex.dir, node = "SurfaceOrganicMatter", som.child = "Pools")
#' inspect_apsimx_xml("Maize", src.dir = ex.dir, node = "SurfaceOrganicMatter", som.child = "Other")
#' inspect_apsimx_xml("Maize", src.dir = ex.dir, node = "MicroClimate") 
#' inspect_apsimx_xml("Maize", src.dir = ex.dir, node = "Crop") 
#' }
#' 

inspect_apsimx_xml <- function(file = "", src.dir = ".", 
                               node = c("Clock","Weather","Soil","SurfaceOrganicMatter",
                                        "MicroClimate","Crop","Manager"),
                               soil.child = c("Water","Nitrogen","OrganicMatter",
                                              "Analysis","InitialWater","Sample"),
                               som.child = c("Pools","Other"),
                               parm = NULL,
                               digits = 3){
  
  fileNames <- dir(path = src.dir, pattern=".apsimx$",ignore.case=TRUE)
  
  if(length(fileNames)==0){
    stop("There are no .apsimx files in the specified directory to inspect.")
  }
  
  node <- match.arg(node)
  soil.child <- match.arg(soil.child)
  som.child <- match.arg(som.child)
  
  ## This matches the specified file from a list of files
  ## Notice that the .apsimx extension will be added here
  file <- match.arg(file, fileNames, several.ok=FALSE)
  
  ## Parse apsimx file (XML file)
  if(apsimx_filetype(file = file, src.dir = src.dir) == "xml"){
    apsimx_xml <- read_xml(paste0(src.dir,"/",file))
  }else{
    if(apsimx_filetype(file = file, src.dir = src.dir) == "json"){
      stop("use function inspect_apsimx_json for this type of file")
    }else{
      stop("unknown filetype")
    }
  }
  
  if(node == "Clock"){
    parm.path <- "//Clock"
    parms <- c("StartDate", "EndDate")
    for(i in parms){
      clock.node <- xml_find_first(apsimx_xml, paste0(parm.path,"/",i))
      cat(i,":",xml_text(clock.node),"\n")
    }
  }
  
  if(node == "Weather"){
    parm.path <- paste0("//Weather/FileName")
    weather.filename.node <- xml_find_first(apsimx_xml, parm.path)
    cat("Met file:",(xml_text(weather.filename.node)),"\n")
  }
  
  if(node == "Soil"){
    ## Soil depths for naming columns
    depths.path <- ".//Soil/SoilOrganicMatter/Depth"
    soil.depths <- xml_text(xml_children(xml_find_first(apsimx_xml, depths.path)))
    soil.depths <- gsub("-","_to_",soil.depths, fixed = TRUE)
    ##soil.depths <- paste0("L_",soil.depths)
    ## Determine the number of soil layers
    number.soil.layers <- length(xml_children(xml_find_first(apsimx_xml, 
                                                             ".//Soil/Water/SoilCrop/LL")))
    ## Print soil type, latitude and longitude
    cat("Soil Type: ",xml_text(xml_find_first(apsimx_xml, ".//Soil/SoilType")),"\n")
    cat("Latitude: ",xml_text(xml_find_first(apsimx_xml, ".//Soil/Latitude")),"\n")
    cat("Longitude: ",xml_text(xml_find_first(apsimx_xml, ".//Soil/Longitude")),"\n")
    
    if(soil.child == "Water"){
      ## Crop specific
      crop.parms <- c("LL","KL","XF")
      
      val.mat <- matrix(NA, ncol = (length(crop.parms)+1),
                        nrow = number.soil.layers)
      crop.d <- data.frame(val.mat)
      crop.d[,1] <- soil.depths
      names(crop.d) <- c("Depth",crop.parms)
      j <- 2
      for(i in crop.parms){
        parm.path <- paste0(".//Soil/Water/SoilCrop","/",i)
        soil.water.crop.node <- xml_find_first(apsimx_xml, parm.path)
        crop.d[,j] <- xml_double(xml_children(soil.water.crop.node))
        j <- j + 1
      }
      
      soil.parms <- c("Thickness","BD","AirDry","LL15","DUL","SAT","KS")
      val.mat <- matrix(NA, ncol = length(soil.parms),
                        nrow = number.soil.layers)
      soil.d <- data.frame(val.mat)
      names(soil.d) <- soil.parms
      j <- 1
      for(i in soil.parms){
        parm.path <- paste0(".//Soil/Water","/",i)
        soil.water.node <- xml_find_first(apsimx_xml, parm.path)
        soil.d[,j] <- xml_double(xml_children(soil.water.node))
        j <- j + 1
      }
      print(kable(cbind(crop.d,soil.d), digits = digits))
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
      print(kable(nitro.d, digits = digits))
    }
    
    if(soil.child == "OrganicMatter"){
      ## State what are organic matter possible parameters
      ## Will keep these ones hard coded
      som.parms1 <- c("RootCN","RootWt","SoilCN","EnrACoeff",
                      "EnrBCoeff")
      
      som.d1 <- data.frame(parm = som.parms1, value = NA)
      
      for(i in som.parms1){
        parm.path <- paste0(".//Soil/SoilOrganicMatter","/",i)
        soil.som1.node <- xml_find_first(apsimx_xml, parm.path)
        som.d1[som.d1$parm == i,2] <- xml_text(soil.som1.node)
      }
      print(kable(som.d1, digits = digits))
      
      ## I delete Depth because it was extracted before
      som.parms2 <- c("Thickness","OC","FBiom","FInert")
      
      val.mat <- matrix(NA, ncol = (length(som.parms2)+1),
                        nrow = number.soil.layers)
      val.mat[,1] <- soil.depths
      som.d2 <- as.data.frame(val.mat)
      names(som.d2) <- c("Depth", som.parms2)
      
      j <- 2
      for(i in som.parms2){
        parm.path <- paste0(".//Soil/SoilOrganicMatter","/",i)
        soil.som2.node <- xml_find_first(apsimx_xml, parm.path)
        som.d2[,j] <- xml_text(xml_children(soil.som2.node))
        j <- j + 1
      }
      print(kable(som.d2, digits = digits))
    }
    
    if(soil.child == "Analysis"){
      ## I will keep this one hard coded because it is simple
      analysis.parms <- c("Thickness","PH")
      
      val.mat <- matrix(NA, ncol = (length(analysis.parms)+1),
                        nrow = number.soil.layers)
      analysis.d <- as.data.frame(val.mat)
      analysis.d[,1] <- soil.depths
      names(analysis.d) <- c("Depth", analysis.parms)
      
      j <- 2
      for(i in analysis.parms){
        parm.path <- paste0(".//Soil/Analysis","/",i)
        soil.analysis.node <- xml_find_first(apsimx_xml, parm.path)
        analysis.d[,j] <- xml_text(xml_children(soil.analysis.node))
        j <- j + 1
      }
      print(kable(analysis.d, digits = digits))
    }
    
    if(soil.child == "InitialWater"){
      initialwater.parms <- c("PercentMethod","FractionFull","DepthWetSoil")
      
      initial.water.d <- data.frame(parm = initialwater.parms, value = NA)
      
      for(i in initialwater.parms){
        parm.path <- paste0(".//Soil/InitialWater","/",i)
        soil.InitialWater.node <- xml_find_first(apsimx_xml, parm.path)
        initial.water.d[initial.water.d$parm == i,2] <- xml_text(soil.InitialWater.node)
      }
      print(kable(initial.water.d, digits = digits))
    }
    
    if(soil.child == "Sample"){
      sample.parms <- c("Thickness","NO3","NH4","SW","OC","EC","CL","ESP","PH")
      
      val.mat <- matrix(NA, ncol = (length(sample.parms)+1),
                        nrow = number.soil.layers)
      sample.d <- data.frame(val.mat)
      sample.d[,1] <- soil.depths
      names(sample.d) <- c("Depth", sample.parms)
      
      j <- 2
      for(i in sample.parms){
        parm.path <- paste0(".//Soil/Sample","/",i)
        soil.sample.node <- xml_find_first(apsimx_xml, parm.path)
        sample.d[,j] <- xml_text(xml_children(soil.sample.node))
        j <- j + 1
      }
      print(kable(sample.d, digits = digits))
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
      print(kable(pools.d, digits = digits))
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
      
      other.parms <- xml_name(xml_children(xml_find_first(apsimx_xml, ".//SurfaceOrganicMatter")))[-c(1:10)]
      
      other.d <- data.frame(parm = other.parms, value = NA)
      
      for(i in other.parms){
        parm.path <- paste0(".//SurfaceOrganicMatter","/",i)
        soil.other.node <- xml_find_first(apsimx_xml, parm.path)
        other.d[other.d$parm == i,2] <- xml_text(soil.other.node)
      }
      print(kable(other.d, digits = digits))
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
    print(kable(microclimate.d, digits = digits))
  }
  
  if(node == "Crop"){
    ## It seems that for nodes Crop and Manager, the names can be arbitrary which
    ## makes this hard parsing complicated
    cat("Crop Type: ",xml_text(xml_find_first(apsimx_xml, ".//Plant/CropType")),"\n")
    ## Make sure sowing rule is present
    xfa.manager <- xml_text(xml_find_all(apsimx_xml, ".//Manager/Name"))
    cat("Management Scripts:", xfa.manager,"\n")
    
    ## this makes a strong assumption that a sowing rule is present
    if(length(grep("sowingrule",xfa.manager, ignore.case = TRUE)) == 0)
      warning("Sowing Rule not found")
    
    ## This makes a strong assumption that these components are present 
    ## in the sowing rule
    sowingrule.parms <- c("StartDate","EndDate","MinESW","MinRain",
                          "RainDays","CultivarName","SowingDepth",
                          "RowSpacing","Population")
    
    sowingrule.d <- data.frame(parm = sowingrule.parms, value = NA)
    
    for(i in sowingrule.parms){
      parm.path <- paste0(".//Manager/Script","/",i)
      manager.sowingrule.node <- xml_find_first(apsimx_xml, parm.path)
      sowingrule.d[sowingrule.d$parm == i,2] <- xml_text(manager.sowingrule.node)
    }
    
    ## Just check that there is a harvest rule
    ## So far I looked into the Maize and Wheat examples. For Maize it is called
    ## Harvesting. For Wheat it is called 'Harvest'. I suspect the user can give it
    ## Other names too
    if(length(grep("harvest", xfa.manager, ignore.case = TRUE)) == 0){
      warning("Harvest[ing] not found")
    }else{
      cat("Harvest rule found \n")
      ## For now it does not seem to make sense to print the harvest rule
      if(FALSE){
        w.harvest <- grep("harvest", xfa.manager, ignore.case = TRUE)
        hrv <- xml_find_all(apsimx_xml, paste0(".//Manager"))[w.harvest]
        hrv.attr <- xml_children(hrv)
        hrv.nm <- xml_name(hrv.attr)
        hrv.vl <- xml_text(hrv.attr)
        w.hrv.nm.code <- which(hrv.nm == "Code" & hrv.nm == "Script")
        ## Eliminate "Code"
        hrv.nm <- hrv.nm[-w.hrv.nm.code]
        hrv.vl <- hrv.vl[-w.hrv.nm.code]
        print(kable(data.frame(parm = hrv.nm, value = hrv.vl)))
      }
    }
    
    print(kable(sowingrule.d, digits = digits))
  }
  
  if(node == "Manager"){
    ## First print available 'Manager' options
    ## This is not bullet-proof as I do not know what to expect with
    ## 'Other' Manager options
    xfa.manager <- xml_text(xml_find_all(apsimx_xml, ".//Manager/Name"))
    cat("Management Scripts:", xfa.manager,"\n")
    ## Inspect other manager scripts other than sowing and harvesting
    w.sow <- grep("sowingrule", xfa.manager, ignore.case = TRUE)
    w.harvest <- grep("harvest", xfa.manager, ignore.case = TRUE)
    if(length(c(w.sow,w.harvest)) != 0){
      other.manager <- xfa.manager[-c(w.sow,w.harvest)]
      w.other.manager <- which(xfa.manager == other.manager)
    }else{
      other.manager <- xfa.manager
      w.other.manager <- seq_along(other.manager)
    }
    cat("Other Manager: \n",other.manager,"\n")
    
    ## This is rough at the moment
    ## Available components within Other Manager Components
    manager.d <- NULL
    for(i in w.other.manager){
      amo <- xml_find_all(apsimx_xml, paste0(".//Manager/Script"))[w.other.manager]
      ms.attr <- xml_children(amo)
      ms.nm <- xml_name(ms.attr)
      ms.vl <- xml_text(ms.attr)
      tmp <- data.frame(parm = ms.nm, value = ms.vl)
      manager.d <- rbind(manager.d, tmp)
    }
    print(kable(manager.d, digits = digits))
  }
  
  if(node == 'Other'){
    if(missing(parm.path)){
      stop("parm.path should be specified when node = 'Other'")
    }
    
    other.d <- NULL
    for(i in other){
      other <- xml_find_all(apsimx_xml, paste0(parm.path,"/",parm))
      ms.attr <- xml_children(other)
      ms.nm <- xml_name(ms.attr)
      ms.vl <- xml_text(ms.attr)
      tmp <- data.frame(parm = ms.nm, value = ms.vl)
      other.d <- rbind(other.d, tmp)
    }
    print(kable(other.d))
  }
}
