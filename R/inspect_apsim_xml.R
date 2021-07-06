#' 
#' @title Inspect an .apsim (XML) file
#' @name inspect_apsim
#' @description inspect an XML apsim file. It does not replace the GUI, but it can save time by quickly checking parameters and values.
#' @param file file ending in .apsim (Classic) to be inspected (XML)
#' @param src.dir directory containing the .apsim file to be inspected; defaults to the current working directory
#' @param node either \sQuote{Weather}, \sQuote{Soil}, \sQuote{SurfaceOrganicMatter}, 
#' \sQuote{MicroClimate}, \sQuote{Crop}, \sQuote{Manager}, \sQuote{Outputfile} or \sQuote{Other}
#' @param soil.child specific soil component to be inspected
#' @param parm parameter to inspect when node = \sQuote{Crop}, \sQuote{Manager}, \sQuote{Outputfile} or \sQuote{Other}
#' @param digits number of decimals to print (default 3)
#' @param print.path whether to print the parameter path (default = FALSE)
#' @param root root node label. In simulation structures such as factorials there will be multiple possible nodes. This can be specified by supplying an appropriate character.
#' @details This is simply a script that prints the relevant parameters which are likely to need editing. It does not print all information from an .apsim file.
#'          For \sQuote{Crop}, \sQuote{Manager} and \sQuote{Other}, \sQuote{parm} should be indicated with a first element to look for and a second with the relative position in case there are
#'          multiple results.
#' @note When multiple folders are present as it is the case when there are factorials. Inspect will find
#'       the instance in the first folder unless 'root' is supplied. By providing the name of the folder to root
#'       (or a regular expression), the appropriate node can be selected. In this case the printed path will
#'       be absolute instead of relative.
#' @return table with inspected parameters and values
#' @export
#' @examples 
#' \donttest{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' ## Testing using 'Millet'
#' inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Clock")
#' inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Weather") 
#' inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil", soil.child = "Metadata")
#' inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil", soil.child = "OrganicMatter")
#' inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil", soil.child = "Analysis")
#' inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil", soil.child = "InitialWater")
#' inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil", soil.child = "Sample")
#' inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "SurfaceOrganicMatter")
#' inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Crop", parm = list("sow",NA)) 
#' inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Crop", parm = list("sow",7))
#' 
#' ## when soil.child = "Water" there are potentially many crops to chose from 
#' ## This selects LL, KL and XF for Barley
#' inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil", 
#'               soil.child = "Water", parm = "Barley")
#' ## This selects LL for all the crops
#' inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil", 
#'               soil.child = "Water", parm = "LL")
#' ## To print the parm.path the selection needs to be unique
#' ## but still there will be multiple soil layers
#' ## 'parm' can be a list or a character vector of length equal to two
#' inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil", 
#'               soil.child = "Water", parm = list("Barley", "LL"),
#'               print.path = TRUE)
#' 
#' ## Inspect outputfile
#' inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Outputfile",
#'               parm = "filename")
#'  
#' inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Outputfile",
#'               parm = "variables")
#' 
#' ## Testing with maize-soybean-rotation.apsim
#' inspect_apsim("maize-soybean-rotation.apsim", src.dir = extd.dir, node = "Clock")
#' inspect_apsim("maize-soybean-rotation.apsim", src.dir = extd.dir, node = "Weather")
#' inspect_apsim("maize-soybean-rotation.apsim", src.dir = extd.dir, node = "Soil",
#'               soil.child = "Metadata")
#' inspect_apsim("maize-soybean-rotation.apsim", src.dir = extd.dir, node = "Soil", 
#'                soil.child = "OrganicMatter")
#' inspect_apsim("maize-soybean-rotation.apsim", src.dir = extd.dir, node = "Soil", 
#'                soil.child = "Analysis")
#' inspect_apsim("maize-soybean-rotation.apsim", src.dir = extd.dir, node = "Soil", 
#'                soil.child = "InitialWater")
#' inspect_apsim("maize-soybean-rotation.apsim", src.dir = extd.dir, node = "Soil", 
#'                soil.child = "Sample")
#' inspect_apsim("maize-soybean-rotation.apsim", src.dir = extd.dir, 
#'                node = "SurfaceOrganicMatter")
#' inspect_apsim("maize-soybean-rotation.apsim", src.dir = extd.dir, node = "Crop")
#' ## This has many options and a complex structure
#' ## It is possible to select unique managements, but not non-unique ones
#' ## The first element in parm can be a regular expression
#' inspect_apsim("maize-soybean-rotation.apsim", src.dir = extd.dir, 
#'                node = "Manager", parm = list("rotat",NA))
#' inspect_apsim("maize-soybean-rotation.apsim", src.dir = extd.dir, 
#'                node = "Manager", 
#'                parm = list("sow on a fixed date - maize",NA))
#' ## Select an individual row by position
#' inspect_apsim("maize-soybean-rotation.apsim", src.dir = extd.dir, 
#'               node = "Manager", 
#'               parm = list("sow on a fixed date - maize",7))
#'               
#' ## Illustrating the 'print.path' feature.
#' inspect_apsim("Millet.apsim", src.dir = extd.dir, 
#'                node = "Soil", soil.child = "Water", 
#'                parm = "DUL", print.path = TRUE)
#' ## But the path can also be returned as a string
#' ## Which is useful for later editing
#' pp <-  inspect_apsim("Millet.apsim", src.dir = extd.dir, 
#'                node = "Soil", soil.child = "Water", 
#'                parm = "DUL", print.path = TRUE)
#'                
#' ## Inspecting a factorial
#' ## (or simply a simulation with multiple folders)
#' ## No cover
#' inspect_apsim("maize-factorial.apsim", src.dir = extd.dir, 
#'                root = "IA-CC_Canisteo_No-Cover")
#'                
#' ## Cover
#' inspect_apsim("maize-factorial.apsim", src.dir = extd.dir, 
#'                root = "IA-CC_Canisteo_Cover")
#' }
#' 

inspect_apsim <- function(file = "", src.dir = ".", 
                          node = c("Clock", "Weather", "Soil", "SurfaceOrganicMatter", "Crop", "Manager", "Outputfile", "Other"),
                          soil.child = c("Metadata", "Water", "OrganicMatter", "Nitrogen", "Analysis", "InitialWater", "Sample", "SWIM"),
                          parm = NULL,
                          digits = 3,
                          print.path = FALSE,
                          root){
  
  file.names <- dir(path = src.dir, pattern=".apsim$", ignore.case=TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsim files in the specified directory to inspect.")
  }
  
  node <- match.arg(node)
  soil.child <- match.arg(soil.child)
  
  if(print.path && missing(parm) && node != "Weather") 
    stop("parm should be specified when print.path is TRUE")
  
  ## This matches the specified file from a list of files
  ## Notice that the .apsimx extension will be added here
  file <- match.arg(file, file.names)
  
  ## Read the file
  apsim_xml <- xml2::read_xml(file.path(src.dir, file))
  
  ## This is my attempt at picking the right node in a factorial
  if(!missing(root)){
    apsim_xml0 <- xml2::xml_find_all(apsim_xml, ".//simulation")
    sim.names <- unlist(xml2::xml_attrs(apsim_xml0))
    wsim <- grep(root, sim.names)
    if(length(wsim) == 0)
      stop(c("'root' simulation not found. It should be one of: \n", 
             paste0(sim.names, "\n")),
           call. = FALSE)
    apsim_xml <- apsim_xml0[[wsim]]
    parm.path.root <- xml2::xml_path(apsim_xml)
  }
  
  ## parm.path.0 is the 'root' path
  parm.path <- NULL
  parm.path.0 <- NULL
  parm.path.1 <- NULL

  if(node == "Clock"){
    parm.path.0 <- ".//clock"
    parms <- c("start_date", "end_date")
    for(i in parms){
      clock.node <- xml2::xml_find_first(apsim_xml, paste0(parm.path.0, "/", i))
      cat(i, ":", xml2::xml_text(clock.node), "\n")
    }
    if(!missing(parm)){
      parm <- grep(parm, parms, value = TRUE)
      parm.path.1 <- paste0(parm.path.0, "/", parm)
    }
  }
  
  if(node == "Weather"){
    parm.path.0 <- ".//metfile/filename" 
    weather.filename.node <- xml2::xml_find_first(apsim_xml, parm.path.0)
    cat("Met file:", (xml2::xml_text(weather.filename.node)), "\n")
    if(!missing(parm)){
      if(parm != "filename") warning("parm should be 'filename' when 'node = Weather'")
    }
    parm.path.1 <- parm.path.0
  }
  
  ## Extracting soil Depths
  ## May be I should move this function to 'apsim_internals.R'
  ## t2d is "thickness" to "depth"
  t2d <- function(x){
    x2 <- c(0, x)/10 ## Divide by 10 to go from mm to cm
    ans <- character(length(x))
    csx2 <- cumsum(x2)
    for(i in 2:length(x2)){
        ans[i-1] <- paste0(csx2[i-1], "-", csx2[i]) 
    }
    ans
  }
  
  if(node == "Soil"){
    ## Soil depths for naming columns
    ## It seems that Depth is not explicitly exposed
    ## But Thickness is
    thickness.path <- ".//Thickness"
    soil.thickness <- as.numeric(xml2::xml_text(xml2::xml_children(xml2::xml_find_first(apsim_xml, thickness.path))))
    soil.depths <- t2d(soil.thickness)
    ## Determine the number of soil layers
    number.soil.layers <- length(soil.thickness)
    
    ## Print soil type, latitude and longitude
    cat("Soil Type: ", xml2::xml_text(xml2::xml_find_first(apsim_xml, ".//Soil/SoilType")),"\n")
    cat("Latitude: ", xml2::xml_text(xml2::xml_find_first(apsim_xml, ".//Soil/Latitude")),"\n")
    cat("Longitude: ", xml2::xml_text(xml2::xml_find_first(apsim_xml, ".//Soil/Longitude")),"\n")
    
    if(soil.child == "Metadata"){
      metadata.parms <- c("SoilType", "LocalName", "Site", "NearestTown", "Region", "State",
                          "Country", "ApsoilNumber", "Latitude", "Longitude", "YearOfSampling",
                          "DataSource", "Comments", "NaturalVegetation")
      met.dat <- data.frame(parm = metadata.parms, value = NA)
      j <- 1
      for(i in metadata.parms){
        parm.path.0 <- ".//Soil"
        parm.path.1 <- paste0(parm.path.0, "/", i)
        soil.metadata.node <- xml2::xml_find_first(apsim_xml, parm.path.1)
        if(length(soil.metadata.node) > 0){
          met.dat[j,1] <- i
          met.dat[j,2] <- strtrim(xml2::xml_text(soil.metadata.node), options()$width - 35)
          j <- j + 1
        }
      }
      
      if(missing(parm)){
        print(knitr::kable(stats::na.omit(met.dat)))
      }else{
        parm.path.1 <- paste0(parm.path.0, "/", parm)
        met.dat.nmd <- stats::na.omit(met.dat)
        met.dat.nmd.p <- met.dat.nmd[met.dat.nmd$parm == parm, ]
        print(knitr::kable(met.dat.nmd.p))
      } 
    }
    
    if(soil.child == "Water"){
      ## Crop specific
      crop.parms <- c("LL", "KL", "XF")
      
      ## How many crops are present?
      number.of.crops <- length(xml2::xml_attrs(xml2::xml_find_all(apsim_xml, ".//Soil/Water/SoilCrop")))
      crop.names <- as.character(unlist(xml2::xml_attrs(xml2::xml_find_all(apsim_xml, ".//Soil/Water/SoilCrop"))))
      crop.names.parms <- c(sapply(crop.names, function(x) paste(x, crop.parms)))
      
      val.mat <- matrix(NA, 
                        ncol = c(length(crop.parms) * number.of.crops)+1,
                        nrow = number.soil.layers)
      
      crop.d <- data.frame(val.mat)
      crop.d[,1] <- soil.depths
      names(crop.d) <- c("Depth", crop.names.parms)
      j <- 2
      for(i in crop.names){
        for(k in crop.parms){
          parm.path.0 <- ".//Soil/Water/SoilCrop"
          parm.path.1 <- paste0(parm.path.0, "[@name='", i, "']", "/", k)
          soil.water.crop.node <- xml2::xml_find_first(apsim_xml, parm.path.1)
          crop.d[,j] <- xml2::xml_double(xml2::xml_children(soil.water.crop.node))
          j <- j + 1          
        }
      }
      
      soil.parms <- c("Thickness", "BD", "AirDry", "LL15", "DUL", "SAT", "KS")
      
      val.mat <- matrix(NA, 
                        ncol = length(soil.parms),
                        nrow = number.soil.layers)
      
      soil.d <- data.frame(val.mat)
      names(soil.d) <- soil.parms
      
      j <- 1
      for(i in soil.parms){
        parm.path.0 <- ".//Soil/Water"
        parm.path.1 <- paste0(parm.path.0, "/", i)
        soil.water.node <- xml2::xml_find_first(apsim_xml, parm.path.1)
        soil.d[,j] <- xml2::xml_double(xml2::xml_children(soil.water.node))
        j <- j + 1
      }

      soil.water.parms <- c("SummerCona", "SummerU", "SummerDate",
                            "WinterCona", "WinterU", "WinterDate",
                            "DiffusConst","DiffusSlope", "Salb",
                            "CN2Bare","CNRed","CNCov")
      
      soil.water.xparms <- c("Slope", "Discharge", "CatchmentArea", "MaximumPond")
      
      soil.water.parms.soilwat <- c("Thickness", "SWCON", "MWCON", "KLAT")
      
      soilwat.present <- FALSE
      ## Soil Water, but only when it is present because of SWIM        
      if(length(xml2::xml_find_all(apsim_xml, ".//Soil/SoilWater")) > 0){
        
        soilwat.present <- TRUE
        soil.water.d <- data.frame(soil.water = soil.water.parms,
                                   value = NA)
        j <- 1
        for(i in soil.water.parms){
          parm.path.0 <- ".//Soil/SoilWater"
          parm.path.1 <- paste0(parm.path.0, "/", i)
          soil.water.node <- xml2::xml_find_first(apsim_xml, parm.path.1)
          soil.water.d[j,"value"] <- xml2::xml_text(soil.water.node)
          j <- j + 1
        }

        soil.thickness.sw <- as.numeric(xml2::xml_text(xml2::xml_children(xml2::xml_find_first(apsim_xml, ".//Soil/SoilWater/Thickness"))))
        
        soil.water.soilwat.d <- data.frame(Thickness = rep(NA, length(soil.thickness.sw)), 
                                           SWCON = NA, MWCON = NA, KLAT = NA)
        
        for(i in soil.water.parms.soilwat){
          parm.path.0 <- ".//Soil/SoilWater"
          parm.path.1 <- paste0(parm.path.0, "/", i)
          soil.water.node.soilwat <- xml2::xml_find_first(apsim_xml, parm.path.1)
          if(length(soil.water.node.soilwat) == 0) next
          soil.water.soilwat.d[[i]] <- xml2::xml_double(xml2::xml_children(soil.water.node.soilwat))
        }
      }

      if(missing(parm)){
        print(knitr::kable(cbind(crop.d,soil.d), digits = digits))  
        if(soilwat.present) print(knitr::kable(soil.water.d, digits = digits))
        if(soilwat.present) print(knitr::kable(soil.water.soilwat.d, digits = digits))
      }else{
        found.parm <- FALSE
        ## parm is either a list or a string and it is in crop.parms or crop.names
        if(length(parm) == 1 && (parm %in% crop.parms || parm %in% crop.names)){
          sparm <- grep(parm, crop.names.parms, value = TRUE, ignore.case = TRUE)
          print(knitr::kable(subset(crop.d, select = sparm), digits = digits))
          if(print.path) stop("parameter should be unique if print.path = TRUE")
          found.parm <- TRUE
        }
        if(length(parm) == 2){
          if(is.list(parm)){
            parm.path.0 <- ".//Soil/Water/SoilCrop"
            parm.path.1 <- paste0(parm.path.0, "[@name='", parm[[1]], "']", "/", parm[[2]])
            sparm <- paste(parm[[1]], parm[[2]])
            if(inherits(try(subset(crop.d, select = sparm), silent = TRUE), "try-error")) stop("parm not found")
            print(knitr::kable(subset(crop.d, select = sparm), digits = digits))
            found.parm <- TRUE
          }
          if(is.character(parm)){
            parm.path.0 <- ".//Soil/Water/SoilCrop"
            parm.path.1 <- paste0(parm.path.0, "[@name='", parm[1], "']", "/", parm[2])
            sparm <- paste(parm[1], parm[2])
            if(inherits(try(subset(crop.d, select = sparm), silent = TRUE), "try-error")) stop("parm not found")
            print(knitr::kable(subset(crop.d, select = sparm), digits = digits))
            found.parm <- TRUE
          }
        }
        ## parm is in 'soil.parms'
        if(length(parm) == 1 && parm %in% soil.parms){
          parm.path.0 <- ".//Soil/Water"
          parm.path.1 <- paste0(parm.path.0, "/", parm)
          print(knitr::kable(subset(soil.d, select = parm), digits = digits))  
          found.parm <- TRUE
        }
        ## parm is in 'soil.water.parms'
        if(length(parm) == 1 && parm %in% soil.water.parms){
          parm.path.0 <- ".//Soil/SoilWater"
          parm.path.1 <- paste0(parm.path.0, "/", parm)
          print(knitr::kable(soil.water.d[soil.water.d$soil.water == parm,], digits = digits)) 
          found.parm <- TRUE
        }
        ## parm is in 'soil.water.parms.soilwat'
        if(length(parm) == 1 && parm %in% soil.water.parms.soilwat){
          parm.path.0 <- ".//Soil/SoilWater"
          parm.path.1 <- paste0(parm.path.0, "/", parm)
          print(knitr::kable(subset(soil.water.soilwat.d, select = parm), digits = digits)) 
          found.parm <- TRUE
        }
        if(!found.parm) stop("parm not found")
      }
    }
    
    if(soil.child == "SWIM"){
      parm.path.0 <- ".//Soil/Swim"
      ## This first set are single values
      swim.parms1 <- c("Salb","CN2Bare","CNRed","CNCov","KDul",
                      "PSIDul","VC","DTmin","DTmax","MaxWaterIncrement",
                      "SpaceWeightingFactor","SoluteSpaceWeightingFactor",
                      "Diagnostics")
      
      swim.parms1.d <- data.frame(parm = swim.parms1, value = NA)
      
      j <- 1
      for(i in swim.parms1){
        parm.path.1 <- paste0(parm.path.0, "/", i)
        swim.parm.node <- xml2::xml_find_first(apsim_xml, parm.path.1)
        swim.parms1.d[j,"value"] <- xml2::xml_text(swim.parm.node)
        j <- j + 1
      }
      
      if(missing(parm)) print(knitr::kable(swim.parms1.d))
      
      ## Water table and Subsurface drain
      swim.parms2 <- c("SwimWaterTable/WaterTableDepth",
                       "SwimSubsurfaceDrain/DrainDepth",
                       "SwimSubsurfaceDrain/DrainSpacing",
                       "SwimSubsurfaceDrain/DrainRadius",
                       "SwimSubsurfaceDrain/Klat",
                       "SwimSubsurfaceDrain/ImpermDepth")
      
      swim.parms2.d <- data.frame(parm = gsub("/"," : ",swim.parms2), value = NA)
      
      j <- 1
      for(i in swim.parms2){
        parm.path.1.1 <- paste0(parm.path.0, "/", i)
        swim.parm.node <- xml2::xml_find_first(apsim_xml, parm.path.1.1)
        swim.parms2.d[j,"value"] <- xml2::xml_text(swim.parm.node)
        j <- j + 1
      }
      if(missing(parm)) print(knitr::kable(swim.parms2.d))
      
      if(!missing(parm) && parm %in% swim.parms2){
        if(grepl("WaterTableDepth", parm, fixed=TRUE)){
          parm.path.0 <- paste0(parm.path.0, "/", "SwimWaterTable")
        }else{
          parm.path.0 <- paste0(parm.path.0, "/", "SwimSubsurfaceDrain")
        }
      }
      
      if(!missing(parm)){
        if(parm %in% swim.parms1){
          print(knitr::kable(swim.parms1.d[swim.parms1.d$parm == parm,]))
        }
        if(parm %in% swim.parms2){
          mparm <- gsub("/", " : ", parm)
          print(knitr::kable(swim.parms2.d[swim.parms2.d$parm == mparm,]))
        }
        if(!(parm %in% c(swim.parms1, swim.parms2))) warning("parameter not found in the SWIM module")
      }
    }
    
    if(soil.child == "Nitrogen"){
      stop("not implemented yet")
      nitrogen.parms <- c("fom_type", "fract_carb", "fract_cell", "fract_lign")
      
      val.mat <- matrix(NA, ncol = length(nitrogen.parms),
                        nrow = 6, 
                        dimnames = list(NULL,nitrogen.parms))
      
      nitro.d <- data.frame(val.mat)
      k <- 1
      for(i in nitrogen.parms){
        parm.path <- paste0(".//Soil/SoilNitrogen", "/", i)
        soil.nitrogen.node <- xml2::xml_find_first(apsim_xml, parm.path)
        nitro.d[,k] <- xml2::xml_text(xml2::xml_children(soil.nitrogen.node))
        k <- k + 1
      }
      print(knitr::kable(nitro.d, digits = digits))
    }
    
    if(soil.child == "OrganicMatter"){
      ## State what are organic matter possible parameters
      ## Will keep these ones hard coded
      som.parms1 <- c("RootCN", "RootWt", "SoilCN", "EnrACoeff", "EnrBCoeff")
      
      som.d1 <- data.frame(parm = som.parms1, value = NA)
      
      for(i in som.parms1){
        parm.path.0 <- ".//Soil/SoilOrganicMatter"
        parm.path.1 <- paste0(parm.path.0, "/", i)
        soil.som1.node <- xml2::xml_find_first(apsim_xml, parm.path.1)
        som.d1[som.d1$parm == i,2] <- xml2::xml_text(soil.som1.node)
      }
      
      if(missing(parm)) print(knitr::kable(som.d1, digits = digits))
      
      som.parms2 <- c("Thickness", "OC", "FBiom", "FInert")
      
      val.mat <- matrix(NA, 
                        ncol = (length(som.parms2)+1),
                        nrow = number.soil.layers)
      
      val.mat[,1] <- soil.depths
      som.d2 <- as.data.frame(val.mat)
      names(som.d2) <- c("Depth", som.parms2)
      
      j <- 2
      for(i in som.parms2){
        parm.path.0 <- ".//Soil/SoilOrganicMatter"
        parm.path.1 <- paste0(parm.path.0, "/", i)
        soil.som2.node <- xml2::xml_find_first(apsim_xml, parm.path.1)
        som.d2[,j] <- xml2::xml_text(xml2::xml_children(soil.som2.node))
        j <- j + 1
      }
      if(missing(parm)) print(knitr::kable(som.d2, digits = digits))
      
      if(!missing(parm) && parm %in% c(som.parms1, som.parms2)){
        parm.path.1 <- paste0(".//Soil/SoilOrganicMatter", "/", parm)
        if(parm %in% som.parms1){
          print(knitr::kable(som.d1[som.d1$parm == parm,], digits = digits))
        }
        if(parm %in% som.parms2){
          print(knitr::kable(subset(som.d2, select = parm), digits = digits))  
        }
      }
    }
    
    if(soil.child == "Analysis"){
      ## I will keep this one hard coded because it is simple
      ## There are many potential elements but can't imagine wanting to edit them
      analysis.parms <- c("Thickness", "PH", "EC")
      
      val.mat <- matrix(NA, 
                        ncol = (length(analysis.parms)+1),
                        nrow = number.soil.layers)
      
      analysis.d <- as.data.frame(val.mat)
      analysis.d[,1] <- soil.depths
      names(analysis.d) <- c("Depth", analysis.parms)
      
      j <- 2
      for(i in analysis.parms){
        parm.path.0 <- ".//Soil/Analysis"
        parm.path.1 <- paste0(parm.path.0, "/", i)
        soil.analysis.node <- xml2::xml_find_first(apsim_xml, parm.path.1)
        analysis.d[,j] <- xml2::xml_text(xml2::xml_children(soil.analysis.node))
        j <- j + 1
      }
      if(missing(parm)) print(knitr::kable(analysis.d, digits = digits))
      
      if(!missing(parm) && parm %in% analysis.parms){
        parm.path.1 <- paste0(".//Soil/Analysis", "/", parm)
        print(knitr::kable(subset(analysis.d, select = parm), digits = digits))
      }
    }
    
    if(soil.child == "InitialWater"){
      initialwater.parms <- c("PercentMethod", "FractionFull", "DepthWetSoil")
      
      initial.water.d <- data.frame(parm = initialwater.parms, value = NA)
      
      for(i in initialwater.parms){
        parm.path.0 <- ".//Soil/InitialWater"
        parm.path.1 <- paste0(parm.path.0, "/", i)
        soil.InitialWater.node <- xml2::xml_find_first(apsim_xml, parm.path.1)
        initial.water.d[initial.water.d$parm == i,2] <- xml2::xml_text(soil.InitialWater.node)
      }
      if(missing(parm)) print(knitr::kable(initial.water.d, digits = digits))
      
      if(!missing(parm) && parm %in% initialwater.parms){
        parm.path.1 <- paste0(".//Soil/InitialWater", "/", parm)
        print(knitr::kable(initial.water.d[initial.water.d$parm == parm, ], digits = digits))
      }
    }
    
    if(soil.child == "Sample"){
      sample.parms <- c("Thickness", "NO3", "NH4", "SW", "OC", "EC", "CL", "ESP", "PH")
      
      val.mat <- matrix(NA, 
                        ncol = (length(sample.parms)+1),
                        nrow = number.soil.layers)
      
      sample.d <- data.frame(val.mat)
      sample.d[,1] <- soil.depths
      names(sample.d) <- c("Depth", sample.parms)
      
      j <- 2
      for(i in sample.parms){
        parm.path.0 <- ".//Soil/Sample"
        parm.path.1 <- paste0(parm.path.0, "/", i)
        soil.sample.node <- xml2::xml_find_first(apsim_xml, parm.path.1)
        sample.d[,j] <- xml2::xml_text(xml2::xml_children(soil.sample.node))
        j <- j + 1
      }
      if(missing(parm)) print(knitr::kable(sample.d, digits = digits))
      
      if(!missing(parm) && parm %in% sample.parms){
        parm.path.1 <- paste0(".//Soil/Sample", "/", parm)
        print(knitr::kable(subset(sample.d, select = parm), digits = digits))
      }
    }
  }
  
  if(node == "SurfaceOrganicMatter"){
    pools.path <- ".//surfaceom"
    pools.parms <- xml2::xml_name(xml2::xml_children(xml2::xml_find_first(apsim_xml, pools.path)))
      
    pools.d <- data.frame(parm = pools.parms, value = NA)
      
    for(i in pools.parms){
      parm.path.1 <- paste0(pools.path, "/", i)
      soil.pools.node <- xml2::xml_find_first(apsim_xml, parm.path.1)
      pools.d[pools.d$parm == i, 2] <- xml2::xml_text(soil.pools.node)
    }
    
    if(missing(parm)) print(knitr::kable(pools.d, digits = digits))
    
    if(!missing(parm) && parm %in% pools.parms){
      parm.path.0 <- ".//surfaceom"
      parm.path.1 <- paste0(".//surfaceom", "/", parm)
      print(knitr::kable(pools.d[pools.d$parm == parm,], digits = digits))
    }
  }

  if(node == "Crop" | node == "Manager"){
    ## It seems that for nodes Crop and Manager, the names can be arbitrary which
    ## makes this hard parsing complicated
    cat("Crop Type: ",xml2::xml_text(xml2::xml_find_first(apsim_xml, ".//manager/ui/crop")),"\n")
    ## Make sure sowing rule is present
    xfa.manager <- as.vector(unlist(xml2::xml_attrs(xml2::xml_find_all(apsim_xml, ".//manager"))))
    cat("Management Scripts:", xfa.manager, "\n", sep = "\n")
    
    if(missing(parm)) parm.path.0 <- ".//manager"
    
    if(!missing(parm)){
      parm1 <- parm[[1]]
      position <- parm[[2]]
    
      all.manager.names <- xml2::xml_attrs(xml2::xml_find_all(apsim_xml, ".//manager"))
    
      find.parm <- grep(parm1, all.manager.names, ignore.case = TRUE)
      
      if(length(find.parm) == 0)
        stop(paste(parm1, " not found"))
      if(length(find.parm) > 1) stop("parm selection should be unique")
      
      cat("Selected manager: ", all.manager.names[[find.parm]], "\n")

      crop <- xml2::xml_find_all(apsim_xml, paste0(".//manager"))[find.parm]
      crop2 <- xml2::xml_find_first(crop, "ui")
      
      if(!is.na(position)){
        parm.path.0 <- xml2::xml_path(xml2::xml_children(crop2)[[position]])
      }else{
        parm.path.0 <- ".//manager"
      }
      
      descr <- sapply(xml2::xml_attrs(xml2::xml_children(crop2)), function(x) x[["description"]])
      vals <- xml2::xml_text(xml2::xml_children(crop2))
    
      if(is.na(position)){
        position <- 1:length(vals)
      }
      crop.d <- data.frame(parm = descr, value = vals)[position,]
      print(knitr::kable(crop.d, digits = digits))
    }
  }
  
  if(node == "Outputfile"){
    parm.path.0 <- ".//outputfile"
    outputfile.node <- xml2::xml_find_all(apsim_xml, ".//outputfile")
    outputfile.parms <- c("filename", "title", "variables", "events")
    
    if(missing(parm)){
      parm <- "filename"
    }else{
      parm <- match.arg(parm, outputfile.parms)
    } 
    
    parm.path.1 <- paste0(parm.path.0, "/", parm)
    
    if(parm == "filename" || parm == "title"){
      output.node <- xml2::xml_find_first(outputfile.node, parm)
      output.name <- xml2::xml_text(output.node)
      tmpdat <- data.frame(tmp = output.name)
    }
    
    if(parm == "variables" || parm == "events"){
      variables.node <- xml2::xml_find_all(outputfile.node, parm)
      variables.names <- xml2::xml_text(xml2::xml_children(variables.node))
      tmpdat <- data.frame(tmp = variables.names)
    }
    
    names(tmpdat) <- parm
    print(knitr::kable(tmpdat))
  }
  
  if(node == 'Other'){
    if(missing(parm)){
      stop("parm should be specified when node = 'Other'")
    }
    
    other.d <- NULL
    other <- xml2::xml_find_all(apsim_xml, parm)
    for(i in other){
      ms.attr <- xml2::xml_children(other)
      ms.nm <- xml2::xml_name(ms.attr)
      ms.vl <- xml2::xml_text(ms.attr)
      tmp <- data.frame(parm = ms.nm, value = ms.vl)
      other.d <- rbind(other.d, tmp)
    }
    print(knitr::kable(other.d))
    
    parm.path.0 <- xml_path(other)
    if(length(parm.path.0) > 1) stop("figure out why parm.path is greater than 1 for 'Other'")
  }
  
  if(!node %in% c("Crop","Manager") && !is.null(parm)){
    parm.path <- xml2::xml_path(xml2::xml_find_first(apsim_xml,paste0(parm.path.0,"/",parm)))
  }
    
  if(print.path){
    if(is.null(parm.path.0)) stop("root parm path not found")
    
    if(!missing(root)){
      parm.path.0 <- paste0(parm.path.root, gsub("./", "", parm.path.0, fixed = TRUE))
      parm.path.1 <- paste0(parm.path.root, gsub("./", "", parm.path.1, fixed = TRUE))
    }
    
    parm.path <- parm.path.0
    if(!is.null(parm.path.1)){
      parm.path <- parm.path.1
    }
    cat("Parm path:", parm.path, "\n")
  }
  invisible(parm.path)
}


#'
#' @title Inspect an APSIM Classic auxiliary (XML) file
#' @name inspect_apsim_xml
#' @description inspect an auxiliary XML apsim file. 
#' @param file file ending in .xml to be inspected.
#' @param src.dir directory containing the .xml file to be inspected; defaults to the current working directory
#' @param parm parameter to inspect.
#' @param verbose Whether to print to standard output
#' @param print.path Whether to print the parameter path
#' @note the behavior has changed from previous verions (earlier than 1.977). Before,
#' if more than match was found it would return an error. Now it returns a list with all
#' possible matches. This can be useful when trying to find a parameter.
#' @return absolute parameter path(s)
#' @export
#' @examples  
#' \donttest{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' 
#' inspect_apsim_xml("Maize75.xml", src.dir = extd.dir,
#'                   parm = "leaf_no_rate_change")
#'                   
#' pp <- inspect_apsim_xml("Maize75.xml", src.dir = extd.dir,
#'                         parm = "leaf_no_rate_change",
#'                         verbose = FALSE, 
#'                         print.path = FALSE)
#' }

inspect_apsim_xml <- function(file = "", 
                              src.dir = ".", 
                              parm,
                              verbose = TRUE,
                              print.path = TRUE){
  
  file.names.xml <- dir(path = src.dir, pattern = ".xml$", ignore.case = TRUE)
  file.names.apsim <- dir(path = src.dir, pattern = ".apsim$", ignore.case = TRUE)
  file.names <- c(file.names.xml, file.names.apsim)
  
  if(length(file.names) == 0){
    stop("There are no .xml or .apsim files in the specified directory to inspect.")
  }
  
  file <- match.arg(file, file.names)
  
  ## Read the file
  apsim_xml <- xml2::read_xml(paste0(src.dir, "/", file))
  
  find.all <- xml2::xml_find_all(apsim_xml, paste0(".//", parm))
  
  if(length(find.all) > 1){
    parm.path <- character(length = length(find.all))
    for(i in seq_along(find.all)){
      if(verbose){
        cat("attrs:", xml2::xml_attrs(find.all[[i]], "*"), "\n")
        length.xml.node <- xml2::xml_length(find.all[[i]])
        if(length.xml.node == 1){
          cat("text:", xml2::xml_text(find.all[[i]]), "\n")    
        }else{
          cat("text:", xml2::xml_text(xml2::xml_children(find.all[[i]])), "\n")    
        }        
      }
      parm.path[i] <- xml2::xml_path(find.all[i])
    }
    if(print.path) cat("path:", unlist(parm.path), "\n")
  } 
    
  if(length(find.all) == 1){
    xml_node <- xml2::xml_find_first(apsim_xml, paste0(".//", parm))
    parm.path <- xml2::xml_path(xml_node)
    if(verbose){
      cat("attrs:", xml2::xml_attrs(xml_node, "*"), "\n")
      length.xml.node <- xml2::xml_length(xml_node)
      if(length.xml.node <= 1){
        cat("text:", xml2::xml_text(xml_node), "\n")    
      }else{
        cat("text:", xml2::xml_text(xml2::xml_children(xml_node)), "\n")    
      }
    }
    if(print.path) cat("path:", parm.path, "\n")
  }
  invisible(parm.path)
}

#' view APSIM XML file
#' @title View an APSIM Classic auxiliary (XML) file
#' @name view_apsim_xml
#' @description view an auxilliary XML apsim file. 
#' @param file file ending in .xml to be viewed.
#' @param src.dir directory containing the .xml file to be viewed; defaults to the current working directory
#' @param viewer either \dQuote{json} or \dQuote{react}.
#' @param ... additional arguments passed to either \sQuote{jsonedit} or \sQuote{reactjson}.
#' @return nothing
#' @export
#' @examples  
#' \dontrun{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' view_apsim_xml("Maize75.xml", src.dir = extd.dir)
#' }
#' 
view_apsim_xml <- function(file, src.dir, viewer = c("json","react"), ...){
  
  if(!requireNamespace("listviewer", quietly = TRUE)){
    warning("The listviewer package is required for this function")
    return(NULL)
  }
  
  if(missing(file)) stop("need to specify file name")
  
  .check_apsim_name(file)
  
  ## The might offer suggestions in case there is a typo in 'file'
  file.names <- dir(path = src.dir, pattern = ".xml$", ignore.case = TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .xml files in the specified directory to run.")
  }
  
  file <- match.arg(file, file.names)
  
  if(missing(src.dir)) src.dir <- "."
  
  viewer <- match.arg(viewer)
  
  file.name.path <- file.path(src.dir, file)
  
  apsim_xml <- xml2::read_xml(file.name.path)
  apsim_lst <- jsonlite::toJSON(xml2::as_list(apsim_xml), auto_unbox = TRUE)
  
  names(apsim_lst) <- NULL
  
  if(viewer == "json"){
    ans <- listviewer::jsonedit(listdata = apsim_lst, ...) 
    return(ans)
  }
  
  if(viewer == "react"){
    
    if(!requireNamespace("reactR", quietly = TRUE)){
      warning("The reactR package is required for this function")
      return(NULL)
    }
    
    ans <- listviewer::reactjson(listdata = apsim_lst, ...)  
    return(ans)
  }
}
