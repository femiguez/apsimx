
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
  ## It looks like I need to 'find' the "Models.Core.Simulation" node
  fcsn <- grep("Models.Core.Simulation", apsimx_json$Children, fixed = TRUE)
  parent.node <- apsimx_json$Children[[fcsn]]$Children
  
  ## The previous creates a list
  if(node == "Clock"){
    ## Clock seems to be the first element in the list
    ## parent.node[[1]]
    ## Extract the list which has a component Name == "Clock"
    wlc <- function(x) grepl("Clock", x$Name, ignore.case = TRUE)
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
    cat("Met file:", as.character(sapply(weather.node, gf1)),"\n")
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
    
    ## Extract soil depths
    wsd0 <- grepl("Depth", soil.node[[1]]$Children)
    soil.depth.node0 <- soil.node[[1]]$Children[wsd0]
    soil.depths <- unlist(soil.depth.node0[[1]]$Depth)
    soil.depths <- gsub("-","_to_",soil.depths, fixed = TRUE)
    
    if(soil.child == "Water"){
      
      soil.water.node <- soil.node[[1]]$Children[[1]]
      
      soil.water.d <- data.frame(Thickness = unlist(soil.water.node$Thickness),
                                 BD = unlist(soil.water.node$BD),
                                 AirDry = unlist(soil.water.node$AirDry),
                                 LL15 = unlist(soil.water.node$LL15),
                                 DUL = unlist(soil.water.node$DUL),
                                 SAT = unlist(soil.water.node$SAT),
                                 KS = unlist(soil.water.node$KS))
      
      crop.water.d <- data.frame(LL = unlist(soil.water.node$Children[[1]]$LL),
                                 KL = unlist(soil.water.node$Children[[1]]$KL),
                                 XF = unlist(soil.water.node$Children[[1]]$XF))
      
      crop.soil.water.d <- data.frame(Depth = soil.depths, crop.water.d, soil.water.d)
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
      
      ## I will not print these ones for now
      ## print(kable(soil.water.model.node.d1, digits = digits))
      ## print(kable(soil.water.model.node.d2, digits = digits))
      
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
      
      print(kable(soil.nitrogen.node.d, digits = digits))
    }
    
    if(soil.child == "OrganicMatter"){
      ## Which soil organc matter
      wsomn <- grepl("Models.Soils.SoilOrganicMatter", soil.node[[1]]$Children)
      soil.om.node <- soil.node[[1]]$Children[wsomn][[1]]
      
      tmp <- soil.om.node
      soil.om.d1 <- data.frame(parm = names(tmp)[2:6],
                               value = as.vector(unlist(tmp[2:6])))
      print(kable(soil.om.d1, digits = digits))
      
      soil.om.d2 <- data.frame(Depth = soil.depths,
                               Thickness = unlist(tmp$Thickness),
                               OC = unlist(tmp$OC),
                               FBiom = unlist(tmp$FBiom),
                               FInert = unlist(tmp$FInert))
      
      print(kable(soil.om.d2, digits = digits))
    }
    
    if(soil.child == "Analysis"){
      ## Which soil analysis
      wsan <- grepl("Models.Soils.Analysis", soil.node[[1]]$Children)
      soil.analysis.node <- soil.node[[1]]$Children[wsan][[1]]
      
      tmp <- soil.analysis.node
      soil.analysis.d <- data.frame(Depth = soil.depths,
                                    Thickness = unlist(tmp$Thickness),
                                    PH = unlist(tmp$PH))
      
      print(kable(soil.analysis.d, digits = digits))
    }
    
    if(soil.child == "InitialWater"){
      ## Which soil initialwater
      wsiwn <- grepl("Models.Soils.InitialWater", soil.node[[1]]$Children)
      soil.initialwater.node <- soil.node[[1]]$Children[wsiwn][[1]]
      
      tmp <- soil.initialwater.node
      parms <- c("PercentMethod","FractionFull","DepthWetSoil")
      soil.initialwater.d <- data.frame(parm = parms, value = NA)
      
      soil.initialwater.d[,"value"] <- c(tmp$PercentMethod,
                                         tmp$FractionFull,
                                         tmp$DepthWetSoil)
      
      print(kable(soil.initialwater.d, digits = digits))
    }
    
    if(soil.child == "Sample"){
      ## Which soil sample
      wssn <- grepl("Models.Soils.Sample", soil.node[[1]]$Children)
      soil.sample.node <- soil.node[[1]]$Children[wssn][[1]]
      
      
      tmp <- soil.sample.node
      
      mat <- cbind(soil.depths, unlist(tmp$Thickness), unlist(tmp$NO3),
                   unlist(tmp$NH4), unlist(tmp$SW), unlist(tmp$OC),
                   unlist(tmp$EC), unlist(tmp$CL), unlist(tmp$ESP),
                   unlist(tmp$PH))
      soil.sample.d <- data.frame(mat)
      names(soil.sample.d) <- c("Depth",names(tmp)[2:10])
      
      print(kable(soil.sample.d, digits = digits))
    }
  }
  
  if(node == "SurfaceOrganicMatter"){
    ## Which is 'SurfaceOrganicMatter'
    ## som.child is not relevant at the moment
    wsomn <- grepl("Models.Surface.SurfaceOrganicMatter", core.zone.node)
    som.node <- core.zone.node[wsomn][[1]]
    
    ## The relevant components might be unpredictable
    ## Will need to find a better method in the future
    som.d <- data.frame(parm = names(som.node)[2:8],
                        value = as.vector(unlist(som.node)[2:8]))
    
    print(kable(som.d, digits = digits))
  }
  
  if(node == "MicroClimate"){
    ## Which is 'MicroClimate'
    wmcn <- grepl("Models.MicroClimate", core.zone.node)
    microclimate.node <- core.zone.node[wmcn][[1]]
    
    microclimate.d <- data.frame(parm = names(microclimate.node)[2:9],
                                 value = as.vector(unlist(microclimate.node)[2:9]))
    print(kable(microclimate.d, digits = digits))
  }
  
  if(node == "Crop"){
    ## Which is 'Crop'
    wmmn <- grepl("Models.Manager", core.zone.node)
    manager.node <- core.zone.node[wmmn]
    ## Which element has the crop information?
    wcn <- grepl("CultivarName", manager.node)
    crop.node <- manager.node[wcn][[1]]$Parameters
    
    mat <- matrix(NA, nrow = length(crop.node), ncol = 2,
                  dimnames = list(NULL,c("parm","value")))
    j <- 1
    for(i in 1:length(crop.node)){
      mat[j,1] <- crop.node[[i]]$Key
      mat[j,2] <- crop.node[[i]]$Value
      j <- j + 1
    }
    
    print(kable(as.data.frame(mat), digits = digits))
  }
  
  if(node == "Manager"){
    stop("Not implemented yet")
  }
}





