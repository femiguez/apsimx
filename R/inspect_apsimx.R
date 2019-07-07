
#' Similar structure to the 'edit_apsimx' file but aimed at a simple way
#' of inspecting a file
#' 
#' @title Inspect and .apsimx file
#' @name inspect_apsimx
#' @description inspect either an XML or JSON apsimx file. It does not replace the GUI, but it can save time by quickly checking parameters and values.
#' @param file file ending in .apsimx to be inspected
#' @param src.dir directory containing the .apsimx file to be inspected; defaults to the current working directory
#' @param node either 'Weather', 'Soil', 'SurfaceOrganicMatter', 'MicroClimate', 'Crop', 'Manager' or 'Other'
#' @param soil.child specific soil component to be inspected
#' @param som.child specific soil organic matter component to be inspected
#' @param parm.path path to the attribute to edit when node is 'Other'
#' @param digits number of decimals to print
#' @details This is simply a script that prints the relevant parameters which are likely to need editing. It does not print all information from an .apsimx file.
#' @return table with inspected parameters and values
#' @export
#' @note node 'Manager' can be complicated and it is not guranteed to work. First version of a file which supports
#' both XML and JSON files.
#' @examples 
#' \dontrun{
#' ex.dir <- auto_detect_apsimx_examples()
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Clock")        
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
    stop("There are no .apsimx files in the specified directory to inspect.")
  }
  
  node <- match.arg(node)
  soil.child <- match.arg(soil.child)
  som.child <- match.arg(som.child)
  
  ## This matches the specified file from a list of files
  ## Notice that the .apsimx extension will be added here
  file <- match.arg(file, fileNames, several.ok=FALSE)
  
  ## Inspect XML file type
  if(apsimx_filetype(file = file, src.dir = src.dir) == "xml"){
    inspect_apsimx_xml(file = file, src.dir = src.dir, node = node,
                       soil.child = soil.child, som.child = som.child, 
                       digits = digits)
  }
  
  ## Inspect JSON file type
  if(apsimx_filetype(file = file, src.dir = src.dir) == "json"){
    inspect_apsimx_json(file = file, src.dir = src.dir, node = node,
                        soil.child = soil.child, som.child = som.child, 
                        digits = digits) 
  }
  
  if(apsimx_filetype(file = file, src.dir = src.dir) == "unknown"){
    stop("file type unknown")
  }
} 


#' 
#' @title Inspect an .apsimx (XML) file
#' @name inspect_apsimx_xml
#' @description inspect an XML apsimx file. It does not replace the GUI, but it can save time by quickly checking parameters and values.
#' @param file file ending in .apsimx to be inspected (XML)
#' @param src.dir directory containing the .apsimx file to be inspected; defaults to the current working directory
#' @param node either 'Weather', 'Soil', 'SurfaceOrganicMatter', 'MicroClimate', 'Crop', 'Manager' or 'Other'
#' @param soil.child specific soil component to be inspected
#' @param som.child specific surface organic matter component to be inspected ('Pools' or 'Other')
#' @param digits number of decimals to print (default 3)
#' @details This is simply a script that prints the relevant parameters which are likely to need editing. It does not print all information from an .apsimx file.
#' @return table with inspected parameters and values
#' @export
#' @note node 'Manager' can be complicated and it is not guranteed to work. 
#' @examples 
#' \dontrun{
#' ex.dir <- auto_detect_apsimx_examples()
#' inspect_apsimx_xml("Maize", src.dir = ex.dir, node = "Weather") 
#' inspect_apsimx_xml("Maize", src.dir = ex.dir, node = "Soil", soil.child = "OrganicMatter")
#' inspect_apsimx_xml("Maize", src.dir = ex.dir, node = "MicroClimate") 
#' }
#' 

inspect_apsimx_xml <- function(file = "", src.dir = ".", 
                               node = c("Clock","Weather","Soil","SurfaceOrganicMatter",
                                        "MicroClimate","Crop","Manager"),
                               soil.child = c("Water","Nitrogen","OrganicMatter",
                                              "Analysis","InitialWater","Sample"),
                               som.child = c("Pools","Other"),
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
    print(xml_text(weather.filename.node))
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
      print(kable(rbind(crop.d,soil.d), digits = digits))
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
      print(kable(som.d2, digits = digits))
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
      
      val.mat <- matrix(NA, nrow = length(sample.parms),
                        ncol = number.soil.layers)
      colnames(val.mat) <- soil.depths
      sample.d <- data.frame(parm = sample.parms, val.mat)
      
      for(i in sample.parms){
        parm.path <- paste0(".//Soil/Sample","/",i)
        soil.sample.node <- xml_find_first(apsimx_xml, parm.path)
        sample.d[sample.d$parm == i,2:(number.soil.layers+1)] <- xml_text(xml_children(soil.sample.node))
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
      
      other.parms <- xml_name(xml_children(xml_find_first(apsimx_xml, ".//SurfaceOrganicMatter")))[-c(1:2)]
      
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

#' 
#' @title Inspect an .apsimx (JSON) file
#' @name inspect_apsimx_json
#' @description inspect a JSON apsimx file. It does not replace the GUI, but it can save time by quickly checking parameters and values.
#' @param file file ending in .apsimx to be inspected (JSON)
#' @param src.dir directory containing the .apsimx file to be inspected; defaults to the current working directory
#' @param node either 'Clock', 'Weather', 'Soil', 'SurfaceOrganicMatter', 'MicroClimate', 'Crop', 'Manager' or 'Other'
#' @param soil.child specific soil component to be inspected. The options are: 'Water', 'Nitrogen', 'OrganicMatter', 'Analysis', 'InitalWater', 'Sample'
#' @param som.child specific surface organic matter component to be inspected (not used)
#' @param parm.path path to the attribute to be inspected when node is 'Other'
#' @param digits number of decimals to print (default 3)
#' @details This is simply a script that prints the relevant parameters which are likely to need editing. It does not print all information from an .apsimx file.
#' @return table with inspected parameters and values
#' @export
#' @examples 
#' \dontrun{
#' ex.dir <- auto_detect_apsimx_examples()
#' inspect_apsimx_json("Barley", src.dir = ex.dir, node = "Clock") 
#' inspect_apsimx_json("Barley", src.dir = ex.dir, node = "Weather")
#' inspect_apsimx_json("Barley", src.dir = ex.dir, node = "Soil", soil.child = "Water") 
#' inspect_apsimx_json("Barley", src.dir = ex.dir, node = "Soil", soil.child = "Nitrogen") 
#' inspect_apsimx_json("Barley", src.dir = ex.dir, node = "Soil", soil.child = "OrganicMatter")
#' inspect_apsimx_json("Barley", src.dir = ex.dir, node = "Soil", soil.child = "Analysis")
#' inspect_apsimx_json("Barley", src.dir = ex.dir, node = "Soil", soil.child = "InitialWater")
#' inspect_apsimx_json("Barley", src.dir = ex.dir, node = "Soil", soil.child = "Sample")
#' inspect_apsimx_json("Barley", src.dir = ex.dir, node = "SurfaceOrganicMatter")
#' inspect_apsimx_json("Barley", src.dir = ex.dir, node = "MicroClimate")
#' inspect_apsimx_json("Barley", src.dir = ex.dir, node = "Crop")
#' 
#' }
#'

inspect_apsimx_json <- function(file = "", src.dir = ".", 
                                node = c("Clock","Weather","Soil","SurfaceOrganicMatter",
                                         "MicroClimate","Crop","Manager"),
                                soil.child = c("Water","Nitrogen","OrganicMatter",
                                               "Analysis","InitialWater","Sample"),
                                som.child = c("Pools","Other"),
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
  
  apsimx_json <- read_json(paste0(src.dir,"/",file))
  
  ## I think that everything I might want to look at 
  ## is under this Children/Children node
  parent.node <- apsimx_json$Children[[1]]$Children
  
  ## The previous creates a list
  if(node == "Clock"){
    ## Clock seems to be the first element in the list
    ## parent.node[[1]]
    ## Extract the list which has a component Name == "Clock"
    wlc <- function(x) grepl("Clock", x$Name)
    wlcl <- sapply(parent.node, FUN = wlc)
    clock.node <- unlist(parent.node[wlcl])
    cat("StartDate:", clock.node["StartDate"],"\n")
    cat("EndDate:", clock.node["EndDate"],"\n")
  }
  
  ## The previous creates a list
  if(node == "Weather"){
    ## Extract the list which has a component Name == "Weather"
    wlw <- function(x) grepl("Weather", x$Name)
    wlwl <- sapply(parent.node, FUN = wlw)
    weather.node <- parent.node[wlwl]
    ## Select the string which has a met file
    gf1 <- function(x) grep(".met$", x, value = TRUE)
    cat("Weather file:", as.character(sapply(weather.node, gf1)),"\n")
  }
  
  ## From here on there is an important component that lives inside
  ## 'Models.Core.Zone'
  wcz <- grepl("Models.Core.Zone", parent.node)
  core.zone.node <- parent.node[wcz][[1]]$Children
  
  if(node == "Soil"){
    ## Which soils node
    wsn <- grepl("Models.Soils.Soil", core.zone.node)
    soil.node <- core.zone.node[wsn]
    
    ## Print some basic soil information
    cat("Soil Type: ", soil.node[[1]]$SoilType,"\n")
    cat("Latitude: ", soil.node[[1]]$Latitude,"\n")
    cat("Longitude: ", soil.node[[1]]$Longitude,"\n")
    
    if(soil.child == "Water"){
      
      soil.water.node <- soil.node[[1]]$Children[[1]]
      
      soil.water.d <- data.frame(Thickness = unlist(soil.water.node$Thickness),
                                 BD = unlist(soil.water.node$Thickness),
                                 AirDry = unlist(soil.water.node$AirDry),
                                 LL15 = unlist(soil.water.node$LL15),
                                 DUL = unlist(soil.water.node$DUL),
                                 SAT = unlist(soil.water.node$SAT),
                                 KS = unlist(soil.water.node$KS))
      
      cat("\n Soil Water \n")
      print(kable(soil.water.d, digits = digits))
      
      crop.soil.water.d <- data.frame(LL = unlist(soil.water.node$Children[[1]]$LL),
                                      KL = unlist(soil.water.node$Children[[1]]$KL),
                                      XF = unlist(soil.water.node$Children[[1]]$XF))
      
      cat("\n Crop Soil Water \n")
      print(kable(crop.soil.water.d, digits = digits))
      
      ## Which soils water model
      wswmn <- grepl("Models.Soils.SoilWater", soil.node[[1]]$Children)
      soil.water.model.node <- soil.node[[1]]$Children[wswmn][[1]]
      
      tmp <- soil.water.model.node
      tmp1 <- data.frame(SummerDate = tmp$SummerDate,
                         SummerU = tmp$SummerU,
                         SummerCona = tmp$SummerCona,
                         WinterDate = tmp$WinterDate,
                         WinterU = tmp$WinterU,
                         WinterCona = tmp$WinterCona,
                         DiffusConst = tmp$DiffusConst,
                         DiffusSlope = tmp$DiffusSlope,
                         Salb = tmp$Salb,
                         CNBare = tmp$CN2Bare,
                         CNRed = tmp$CNRed,
                         CNCov = tmp$CNCov,
                         slope = tmp$slope,
                         discharge_width = tmp$discharge_width,
                         catchment_area = tmp$catchment_area,
                         max_pond = tmp$max_pond)
      
      soil.water.model.node.d1 <- as.data.frame(t(as.matrix(tmp1)))
      
      soil.water.model.node.d2 <- data.frame(Thickness = unlist(tmp$Thickness),
                                             SWCON = unlist(tmp$SWCON))
      
      cat("\n Soil Water Model \n")
      print(kable(soil.water.model.node.d1, digits = digits))
      print(kable(soil.water.model.node.d2, digits = digits))
      
    }
    
    if(soil.child == "Nitrogen"){
      ## Which soil nitrogen
      wsnn <- grepl("Models.Soils.SoilNitrogen", soil.node[[1]]$Children)
      soil.nitrogen.node <- soil.node[[1]]$Children[wsnn][[1]]
      
      tmp <- soil.nitrogen.node
      
      soil.nitrogen.node.d <- data.frame(fom_types = unlist(tmp$fom_types),
                                         fract_carb = unlist(tmp$fract_carb),
                                         fract_cell = unlist(tmp$fract_cell),
                                         fract_lign = unlist(tmp$fract_lign))
      
      cat("\n Soil Nitrogen \n")
      print(kable(soil.nitrogen.node.d, digits = digits))
    }
    
    if(soil.child == "OrganicMatter"){
      ## Which soil organc matter
      wsomn <- grepl("Models.Soils.SoilOrganicMatter", soil.node[[1]]$Children)
      soil.om.node <- soil.node[[1]]$Children[wsomn][[1]]
      
      tmp <- soil.om.node
      soil.om.d1 <- data.frame(parm = names(tmp)[2:6],
                               value = as.vector(unlist(tmp[2:6])))
      cat("\n Soil Organic Matter 1: \n")
      print(kable(soil.om.d1, digits = digits))
      
      soil.om.d2 <- data.frame(Thickness = unlist(tmp$Thickness),
                               Depth = unlist(tmp$Depth),
                               OC = unlist(tmp$OC),
                               FBiom = unlist(tmp$FBiom),
                               FInert = unlist(tmp$FInert))
      
      cat("\n Soil Organic Matter 2: \n")
      print(kable(soil.om.d2, digits = digits))
    }
    
    if(soil.child == "Analysis"){
      ## Which soil analysis
      wsan <- grepl("Models.Soils.Analysis", soil.node[[1]]$Children)
      soil.analysis.node <- soil.node[[1]]$Children[wsan][[1]]
      
      tmp <- soil.analysis.node
      soil.analysis.d <- data.frame(Thickness = unlist(tmp$Thickness),
                                    PH = unlist(tmp$PH))
      
      cat("\n Analysis: \n")
      print(kable(soil.analysis.d, digits = digits))
    }
    
    if(soil.child == "InitialWater"){
      ## Which soil initialwater
      wsiwn <- grepl("Models.Soils.InitialWater", soil.node[[1]]$Children)
      soil.initialwater.node <- soil.node[[1]]$Children[wsiwn][[1]]
      
      tmp <- soil.initialwater.node
      soil.initialwater.d <- data.frame(PercentMethod = tmp$PercentMethod,
                                        FractionFull = tmp$FractionFull,
                                        DepthWetSoil = tmp$DepthWetSoil)
      
      cat("\n Initial Water: \n")
      print(kable(soil.initialwater.d, digits = digits))
    }
    
    if(soil.child == "Sample"){
      ## Which soil sample
      wssn <- grepl("Models.Soils.Sample", soil.node[[1]]$Children)
      soil.sample.node <- soil.node[[1]]$Children[wssn][[1]]
      
      tmp <- soil.sample.node
      mat <- rbind(unlist(tmp$Thickness), unlist(tmp$NO3),
                   unlist(tmp$NH4), unlist(tmp$SW), unlist(tmp$OC),
                   unlist(tmp$EC), unlist(tmp$CL), unlist(tmp$ESP),
                   unlist(tmp$PH))
      soil.sample.d <- data.frame(parm = names(tmp)[2:10],
                                  value = mat)
      cat("Soil Sample \n")
      print(kable(soil.sample.d, digits = digits))
    }
  }
  
  if(node == "SurfaceOrganicMatter"){
    ## Which is 'SurfaceOrganicMatter'
    ## som.child is not relevant at the moment
    wsomn <- grepl("Models.Surface.SurfaceOrganicMatter", core.zone.node)
    som.node <- core.zone.node[wsomn][[1]]
    
    som.d <- data.frame(parm = names(som.node)[2:8],
                        value = as.vector(unlist(som.node)[2:8]))
    cat("Surface Organic Matter: \n")
    print(kable(som.d, digits = digits))
  }
  
  if(node == "MicroClimate"){
    ## Which is 'MicroClimate'
    wmcn <- grepl("Models.MicroClimate", core.zone.node)
    microclimate.node <- core.zone.node[wmcn][[1]]
    
    microclimate.d <- data.frame(parm = names(microclimate.node)[2:9],
                                 value = as.vector(unlist(microclimate.node)[2:9]))
    cat("MicroClimate: \n")
    print(kable(microclimate.d, digits = digits))
  }
  
  if(node == "Crop"){
    ## Which is 'Crop'
    wmmn <- grepl("Models.Manager", core.zone.node)
    manager.node <- core.zone.node[wmmn]
    ## Which element has the crop information?
    wcn <- grepl("CultivarName", manager.node)
    crop.node <- manager.node[wcn][[1]]$Parameters
    
    mat <- matrix(NA, nrow = 5, ncol = 2,
                  dimnames = list(NULL,c("parm","value")))
    j <- 1
    for(i in 2:6){
      mat[j,1] <- crop.node[[i]]$Key
      mat[j,2] <- crop.node[[i]]$Value
      j <- j + 1
    }
    
    cat("Crop: \n")
    print(kable(as.data.frame(mat), digits = digits))
  }
  
  if(node == "Manager"){
    stop("Not implemented yet")
  }
}





