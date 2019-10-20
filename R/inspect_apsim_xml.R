#' 
#' @title Inspect an .apsim (XML) file
#' @name inspect_apsim
#' @description inspect an XML apsim file. It does not replace the GUI, but it can save time by quickly checking parameters and values.
#' @param file file ending in .apsim (Classic) to be inspected (XML)
#' @param src.dir directory containing the .apsim file to be inspected; defaults to the current working directory
#' @param node either 'Weather', 'Soil', 'SurfaceOrganicMatter', 'MicroClimate', 'Crop', 'Manager' or 'Other'
#' @param soil.child specific soil component to be inspected
#' @param som.child specific surface organic matter component to be inspected ('Pools' or 'Other')
#' @param parm parameter to inspect when node = 'Crop', 'Manager' or 'Other'
#' @param digits number of decimals to print (default 3)
#' @param print.path whether to print the parameter path (default = FALSE)
#' @details This is simply a script that prints the relevant parameters which are likely to need editing. It does not print all information from an .apsim file.
#'          For 'Crop', 'Manager' and 'Other' 'parm' should be indicated with a first element to look for and a second with the relative position in case there are
#'          multiple results.
#' @return table with inspected parameters and values
#' @export
#' @examples 
#' \dontrun{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' ## Testing using 'Millet'
#' inspect_apsim("Millet", src.dir = extd.dir, node = "Clock")
#' inspect_apsim("Millet", src.dir = extd.dir, node = "Weather") 
#' inspect_apsim("Millet", src.dir = extd.dir, node = "Soil", soil.child = "Water")
#' inspect_apsim("Millet", src.dir = extd.dir, node = "Soil", soil.child = "OrganicMatter")
#' inspect_apsim("Millet", src.dir = extd.dir, node = "Soil", soil.child = "Analysis")
#' inspect_apsim("Millet", src.dir = extd.dir, node = "Soil", soil.child = "InitialWater")
#' inspect_apsim("Millet", src.dir = extd.dir, node = "Soil", soil.child = "Sample")
#' inspect_apsim("Millet", src.dir = extd.dir, node = "SurfaceOrganicMatter")
#' inspect_apsim("Millet", src.dir = extd.dir, node = "Crop", parm = list("sow",NA)) 
#' inspect_apsim("Millet", src.dir = extd.dir, node = "Crop", parm = list("sow",7))
#' 
#' 
#' ## Testing with maize-soybean-rotation.apsim
#' inspect_apsim("maize-soybean-rotation", src.dir = extd.dir, node = "Clock")
#' inspect_apsim("maize-soybean-rotation", src.dir = extd.dir, node = "Weather")
#' inspect_apsim("maize-soybean-rotation", src.dir = extd.dir, node = "Soil")
#' inspect_apsim("maize-soybean-rotation", src.dir = extd.dir, node = "Soil", 
#'                soil.child = "OrganicMatter")
#' inspect_apsim("maize-soybean-rotation", src.dir = extd.dir, node = "Soil", 
#'                soil.child = "Analysis")
#' inspect_apsim("maize-soybean-rotation", src.dir = extd.dir, node = "Soil", 
#'                soil.child = "InitialWater")
#' inspect_apsim("maize-soybean-rotation", src.dir = extd.dir, node = "Soil", 
#'                soil.child = "Sample")
#' inspect_apsim("maize-soybean-rotation", src.dir = extd.dir, 
#'                node = "SurfaceOrganicMatter")
#' inspect_apsim("maize-soybean-rotation", src.dir = extd.dir, node = "Crop")
#' ## This has many options and a complex structure
#' ## It is possible to select unique managements, but not non-unique ones
#' ## The first element in parm can be a regular expression
#' inspect_apsim("maize-soybean-rotation", src.dir = extd.dir, 
#'                node = "Manager", parm = list("rotat",NA))
#' inspect_apsim("maize-soybean-rotation", src.dir = extd.dir, 
#'                node = "Manager", 
#'                parm = list("sow on a fixed date - maize",NA))
#' ## Select an individual row by position
#' inspect_apsim("maize-soybean-rotation", src.dir = extd.dir, 
#'               node = "Manager", 
#'               parm = list("sow on a fixed date - maize",7))
#' }
#' 

inspect_apsim <- function(file = "", src.dir = ".", 
                          node = c("Clock","Weather","Soil","SurfaceOrganicMatter",
                                        "MicroClimate","Crop","Manager","Other"),
                          soil.child = c("Water","OrganicMatter",
                                              "Analysis","InitialWater","Sample"),
                          som.child = c("Pools","Other"),
                          parm = NULL,
                          digits = 3,
                          print.path = FALSE){
  
  file.names <- dir(path = src.dir, pattern=".apsim$",ignore.case=TRUE)
  
  if(length(file.names)==0){
    stop("There are no .apsim files in the specified directory to inspect.")
  }
  
  node <- match.arg(node)
  soil.child <- match.arg(soil.child)
  som.child <- match.arg(som.child)
  
  ## This matches the specified file from a list of files
  ## Notice that the .apsimx extension will be added here
  file <- match.arg(file, file.names, several.ok=FALSE)
  
  ## Read the file
  apsim_xml <- read_xml(paste0(src.dir,"/",file))

  if(node == "Clock"){
    parm.path <- ".//clock"
    parms <- c("start_date", "end_date")
    for(i in parms){
      clock.node <- xml_find_first(apsim_xml, paste0(parm.path,"/",i))
      cat(i,":",xml_text(clock.node),"\n")
    }
  }
  
  if(node == "Weather"){
    parm.path <- paste0(".//metfile/filename")
    weather.filename.node <- xml_find_first(apsim_xml, parm.path)
    cat("Met file:",(xml_text(weather.filename.node)),"\n")
  }
  
  ## Extracting soil Depths
  ## t2d is "thickness" to "depth"
  t2d <- function(x){
    x2 <- c(0,x)/10 ## Divide by 10 to go from mm to cm
    ans <- character(length(x))
    csx2 <- cumsum(x2)
    for(i in 2:length(x2)){
        ans[i-1] <- paste0(csx2[i-1],"-",csx2[i]) 
    }
    ans
  }
  
  if(node == "Soil"){
    ## Soil depths for naming columns
    ## It seems that Depth is not explicitly exposed
    ## But Thickness is
    thickness.path <- ".//Thickness"
    soil.thickness <- as.numeric(xml_text(xml_children(xml_find_first(apsim_xml, thickness.path))))
    soil.depths <- t2d(soil.thickness)
    ## Determine the number of soil layers
    number.soil.layers <- length(soil.thickness)
    
    ## Print soil type, latitude and longitude
    cat("Soil Type: ",xml_text(xml_find_first(apsim_xml, ".//Soil/SoilType")),"\n")
    cat("Latitude: ",xml_text(xml_find_first(apsim_xml, ".//Soil/Latitude")),"\n")
    cat("Longitude: ",xml_text(xml_find_first(apsim_xml, ".//Soil/Longitude")),"\n")
    
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
        soil.water.crop.node <- xml_find_first(apsim_xml, parm.path)
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
        soil.water.node <- xml_find_first(apsim_xml, parm.path)
        soil.d[,j] <- xml_double(xml_children(soil.water.node))
        j <- j + 1
      }
      print(kable(cbind(crop.d,soil.d), digits = digits))
      
      ## Soil Water
      soil.water.parms <- c("SummerCona", "SummerU", "SummerDate",
                            "WinterCona", "WinterU", "WinterDate",
                            "DiffusConst","DiffusSlope", "Salb",
                            "CN2Bare","CNRed","CNCov")
 
      soil.water.d <- data.frame(soil.water = soil.water.parms,
                                 value = NA)
      j <- 1
      for(i in soil.water.parms){
        parm.path <- paste0(".//Soil/SoilWater","/",i)
        soil.water.node <- xml_find_first(apsim_xml, parm.path)
        soil.water.d[j,"value"] <- xml_text(soil.water.node)
        j <- j + 1
      }
      print(kable(soil.water.d, digits = digits))
    }
    
    if(soil.child == "Nitrogen"){
      stop("not implemented yet")
      nitrogen.parms <- c("fom_type","fract_carb","fract_cell","fract_lign")
      
      val.mat <- matrix(NA, ncol = length(nitrogen.parms),
                        nrow = 6, 
                        dimnames = list(NULL,nitrogen.parms))
      
      nitro.d <- data.frame(val.mat)
      k <- 1
      for(i in nitrogen.parms){
        parm.path <- paste0(".//Soil/SoilNitrogen","/",i)
        soil.nitrogen.node <- xml_find_first(apsim_xml, parm.path)
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
        soil.som1.node <- xml_find_first(apsim_xml, parm.path)
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
        soil.som2.node <- xml_find_first(apsim_xml, parm.path)
        som.d2[,j] <- xml_text(xml_children(soil.som2.node))
        j <- j + 1
      }
      print(kable(som.d2, digits = digits))
    }
    
    if(soil.child == "Analysis"){
      ## I will keep this one hard coded because it is simple
      analysis.parms <- c("Thickness","PH","EC")
      
      val.mat <- matrix(NA, ncol = (length(analysis.parms)+1),
                        nrow = number.soil.layers)
      analysis.d <- as.data.frame(val.mat)
      analysis.d[,1] <- soil.depths
      names(analysis.d) <- c("Depth", analysis.parms)
      
      j <- 2
      for(i in analysis.parms){
        parm.path <- paste0(".//Soil/Analysis","/",i)
        soil.analysis.node <- xml_find_first(apsim_xml, parm.path)
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
        soil.InitialWater.node <- xml_find_first(apsim_xml, parm.path)
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
        soil.sample.node <- xml_find_first(apsim_xml, parm.path)
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
      pools.path <- ".//surfaceom"
      pools.parms <- xml_name(xml_children(xml_find_first(apsim_xml, pools.path)))
      
      pools.d <- data.frame(parm = pools.parms, value = NA)
      
      for(i in pools.parms){
        parm.path <- paste0(pools.path,"/",i)
        soil.pools.node <- xml_find_first(apsim_xml, parm.path)
        pools.d[pools.d$parm == i,2] <- xml_text(soil.pools.node)
      }
      print(kable(pools.d, digits = digits))
    }
    
    if(som.child == "Other"){
      stop("Not relevant at the moment")
      ## The lines below are hardcoded, which could be used if we want to
      ## restrict the variables to inspect
      ## other.parms <- c("CriticalResidueWeight",
      ##             "OptimumDecompTemp","MaxCumulativeEOS",
      ##             "CNRatioDecompCoeff","CNRatioDecompThreshold",
      ##             "TotalLeachRain","MinRainToLeach",
      ##             "CriticalMinimumOrganicC","DefaultCPRatio",
      ##             "DefaultStandingFraction","StandingExtinctCoeff",
      ##             "FractionFaecesAdded")
      
      other.parms <- xml_name(xml_children(xml_find_first(apsim_xml, ".//SurfaceOrganicMatter")))[-c(1:10)]
      
      other.d <- data.frame(parm = other.parms, value = NA)
      
      for(i in other.parms){
        parm.path <- paste0(".//SurfaceOrganicMatter","/",i)
        soil.other.node <- xml_find_first(apsim_xml, parm.path)
        other.d[other.d$parm == i,2] <- xml_text(soil.other.node)
      }
      print(kable(other.d, digits = digits))
    }
  }
  
  if(node == "MicroClimate"){
    stop("not relevant for APSIM Classic")
    ## The lines below would be the hard coded version which could work
    ## If we want to restrict the variables which can be inspected
    ## microclimate.parms <- c("a_interception","b_interception","c_interception",
    ##             "d_interception", "soil_albedo", "sun_angle",
    ##             "soil_heat_flux_fraction", "night_interception_fraction",
    ##             "refheight","albedo","emissivity","RadIntTotal")
    
    ## Names 'Name' and 'IncludeInDocumentation' will not be inspected
    microclimate.parms <- xml_name(xml_children(xml_find_first(apsim_xml, ".//MicroClimate")))[-c(1:2)]
    
    microclimate.d <- data.frame(parm = microclimate.parms, value = NA)
    
    for(i in microclimate.parms){
      parm.path <- paste0(".//MicroClimate","/",i)
      soil.microclimate.node <- xml_find_first(apsim_xml, parm.path)
      microclimate.d[microclimate.d$parm == i,2] <- xml_text(soil.microclimate.node)
    }
    print(kable(microclimate.d, digits = digits))
  }
  
  if(node == "Crop" | node == "Manager"){
    ## It seems that for nodes Crop and Manager, the names can be arbitrary which
    ## makes this hard parsing complicated
    cat("Crop Type: ",xml_text(xml_find_first(apsim_xml, ".//manager/ui/crop")),"\n")
    ## Make sure sowing rule is present
    xfa.manager <- as.vector(unlist(xml_attrs(xml_find_all(apsim_xml, ".//manager"))))
    cat("Management Scripts:", xfa.manager,"\n", sep = "\n")
    
    if(!missing(parm)){
      parm1 <- parm[[1]]
      position <- parm[[2]]
    
      all.manager.names <- xml_attrs(xml_find_all(apsim_xml, ".//manager"))
    
      find.parm <- grep(parm1, all.manager.names, ignore.case = TRUE)
      
      if(length(find.parm) == 0)
        stop(paste(parm1, " not found"))
      if(length(find.parm) > 1) stop("parm selection should be unique")
      
      cat("Selected manager: ", all.manager.names[[find.parm]],"\n")

      crop <- xml_find_all(apsim_xml, paste0(".//manager"))[find.parm]
      crop2 <- xml_find_first(crop, "ui")
      if(print.path){
        cat("parm path:", xml_path(xml_children(crop2)[[position]]),"\n")
      }
      descr <- sapply(xml_attrs(xml_children(crop2)),function(x) x[["description"]])
      vals <- xml_text(xml_children(crop2))
    
      if(is.na(position)){
        position <- 1:length(vals)
      }
      crop.d <- data.frame(parm = descr, value = vals)[position,]
      print(kable(crop.d, digits = digits))
    }
  }
  
  if(node == 'Other'){
    if(missing(parm)){
      stop("parm should be specified when node = 'Other'")
    }
    
    other.d <- NULL
    other <- xml_find_all(apsim_xml, parm)
    for(i in other){
      ms.attr <- xml_children(other)
      ms.nm <- xml_name(ms.attr)
      ms.vl <- xml_text(ms.attr)
      tmp <- data.frame(parm = ms.nm, value = ms.vl)
      other.d <- rbind(other.d, tmp)
    }
    print(kable(other.d))
  }
}
