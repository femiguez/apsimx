#'
#' @title Edit APSIM \sQuote{Classic} file with a replaced soil profile
#' @name edit_apsim_replace_soil_profile
#' @description Edits an APSIM Classic simulation by replacing the soil profile
#' @param file name of the .apsim file to be edited
#' @param src.dir source directory
#' @param wrt.dir writing directory
#' @param soil.profile a soil profile object with class \sQuote{soil_profile}
#' @param swim list with SWIM specific parameters
#' @param soilwat list with SoilWat specific parameters
#' @param initialwater list with InitialWater specific parameters
#' @param edit.tag default edit tag \sQuote{-edited}
#' @param overwrite default FALSE
#' @param verbose default TRUE. Will print messages indicating what was done.
#' @param root supply the node postion in the case of multiple simulations such as factorials.
#' @return writes an APSIM file to disk with the supplied soil profile
#' @details This function is designed to batch replace the whole soil in an APSIM simulation. 
#' @note There is no such thing as a default soil, carefully build the profile for each simulation.
#' This function replaces values and it can grow an XML node, but it cannot edit a property
#' which is not present in the original file.
#' @export
#' @examples 
#' \donttest{
#' sp <- apsimx_soil_profile(nlayers = 20,
#'                           crops = c("Barley", "Chickpea", "Lucerne", 
#'                           "Maize", "Perennial Grass", "Sorghum", 
#'                           "Wheat", "Millet"))
#' 
#' extd.dir <- system.file("extdata", package = "apsimx")
#' 
#' ## Writing to a temp directory
#' tmp.dir <- tempdir()
#' edit_apsim_replace_soil_profile("Millet.apsim", soil.profile = sp, 
#'                                 edit.tag = "-newsoil",
#'                                 src.dir = extd.dir, 
#'                                 wrt.dir = tmp.dir)
#'                                 
#' inspect_apsim("Millet-newsoil.apsim", src.dir = tmp.dir,
#'               node = "Soil", soil.child = "Water")
#'  }
#'

edit_apsim_replace_soil_profile <-  function(file = "", src.dir = ".",
                                             wrt.dir = NULL,
                                             soil.profile = NULL,
                                             swim = NULL,
                                             soilwat = NULL,
                                             initialwater = NULL,
                                             edit.tag = "-edited",
                                             overwrite = FALSE,
                                             verbose = TRUE,
                                             root){
  
  .check_apsim_name(file)
  
  if(missing(wrt.dir)) wrt.dir <- src.dir
  
  file.names <- dir(path = src.dir, pattern=".apsim$", ignore.case=TRUE)
  
  if(length(file.names)==0){
    stop("There are no .apsim files in the specified directory to edit.")
  }
  
  file <- match.arg(file, file.names)
  
  if(missing(soil.profile)) stop("soil profile is missing")
  
  ## I might have to replace things one by one
  soil.water.parms <- c("Thickness","SAT","DUL","KS","LL15","BD","AirDry")
  
  ## This is the first editing that occurs and it it creates a temporary file
  ## with a '-tmp' edit tag
  k <- 1
  for(i in soil.water.parms){
    if(k == 1){
      edit_apsim(file = file, src.dir = src.dir, wrt.dir = ".", 
                 node = "Soil", soil.child = "Water", 
                 edit.tag = "-tmp",
                 parm = i, value = soil.profile$soil[[i]],
                 check.length = FALSE,
                 verbose = verbose,
                 root = root)
    }else{
      new.file.path <- paste0(tools::file_path_sans_ext(file),"-tmp.apsim")

      edit_apsim(file = new.file.path, src.dir = ".", wrt.dir = ".", 
                 node = "Soil", soil.child = "Water", 
                 overwrite = TRUE,
                 parm = i, value = soil.profile$soil[[i]],
                 check.length = FALSE,
                 verbose = verbose,
                 root = root)
    }
    k <- k + 1
  }
  
  ## Edit soil water parameters
  if(!is.null(soilwat) || !all(is.na(soil.profile$soilwat))){
    if(!inherits(soilwat, "soilwat_parms")) stop("object should be of class 'soilwat_parms'")
    
    for(i in seq_along(soilwat)){
      prm.vl <- soilwat[[i]]
      if(is.na(prm.vl)) next
      
      edit_apsim(file = new.file.path, src.dir = ".", wrt.dir = ".",
                 node = "Soil", soil.child = "Water", 
                 overwrite = TRUE, parm = names(soilwat[i]),
                 value = prm.vl,
                 root = root)
    }
  }
  
  if(!is.null(swim) || !all(is.na(soil.profile$swim))){
    if(!inherits(swim, "swim_parms")) stop("object should be of class 'swim_parms'")
    
    for(i in seq_along(swim)){
      prm.vl <- swim[[i]]
      if(is.na(prm.vl)) next
      
      if(grepl("^Swim", names(swim[i]))){
        prm.nm <- strsplit(names(swim[i]),"_")[[1]][2]
      }else{
        prm.nm <- names(swim[i])
      }
      
      edit_apsim(file = new.file.path, src.dir = ".", wrt.dir = ".",
                 node = "Soil", soil.child = "SWIM", 
                 overwrite = TRUE, parm = prm.nm,
                 value = prm.vl,
                 root = root)
    }
  }
  
  ## Edit Initial Water
  if(!is.null(initialwater) || !all(is.na(soil.profile$initialwater))){
    if(!inherits(initialwater, "initialwater_parms")) stop("object should be of class 'initialwater_parms'")
    
    for(i in seq_along(initialwater)){
      prm.vl <- initialwater[[i]]
      if(is.na(prm.vl)) next

      prm.nm <- names(initialwater[i])
      
      edit_apsim(file = new.file.path, src.dir = ".", wrt.dir = ".",
                 node = "Soil", soil.child = "InitialWater", 
                 overwrite = TRUE, parm = prm.nm,
                 value = prm.vl,
                 root = root)
    }
  }
  
  ## Edit metadata
  if(!is.null(soil.profile$metadata)){
    
    for(i in seq_along(soil.profile$metadata)){
      prm.vl <- soil.profile$metadata[[i]]
      if(is.na(prm.vl)) next
      
      prm.nm <- names(soil.profile$metadata[i])
    
      edit_apsim(file = new.file.path, 
                 src.dir = ".", wrt.dir = ".",
                 node = "Soil", soil.child = "Metadata", 
                 overwrite = TRUE, 
                 parm = prm.nm,
                 value = prm.vl,
                 root = root)
    }
  }
  
  ## Edit Carbon (OC), FBiom and FInert
  for(i in c("Thickness","Carbon","FBiom","FInert")){
    if(i == "Carbon"){
      ii <- "OC"
    }else{
      ii <- i
    }
    edit_apsim(file = new.file.path, src.dir = ".", wrt.dir = ".", 
             node = "Soil", soil.child = "OrganicMatter", 
             overwrite = TRUE,
             parm = ii, value = soil.profile$soil[[i]],
             check.length = FALSE,
             verbose = verbose,
             root = root)
  }
  
  ## Edit Analysis PH
  for(i in c("Thickness","PH","EC")){
    if(i == "EC"){
      edit_apsim(file = new.file.path, src.dir = ".", wrt.dir = ".", 
                 node = "Soil", soil.child = "Analysis", 
                 overwrite = TRUE,
                 parm = i, value = rep(0, nrow(soil.profile$soil)),
                 check.length = FALSE,
                 verbose = verbose,
                 root = root)
    }else{
      edit_apsim(file = new.file.path, src.dir = ".", wrt.dir = ".", 
                 node = "Soil", soil.child = "Analysis", 
                 overwrite = TRUE,
                 parm = i, value = soil.profile$soil[[i]],
                 check.length = FALSE,
                 verbose = verbose,
                 root = root)
    }
  }
  
  ## Edit Sample (NO3, NH4)
  for(i in c("Thickness", "NO3N", "NH4N")){
    if(i == "Thickness") ii <- "Thickness"
    if(i == "NO3N") ii <- "NO3"
    if(i == "NH4N") ii <- "NH4"

    edit_apsim(file = new.file.path, src.dir = ".", wrt.dir = ".", 
             node = "Soil", soil.child = "Sample", 
             overwrite = TRUE,
             parm = ii, value = soil.profile$soil[[i]],
             check.length = FALSE,
             verbose = verbose,
             root = root)
  }
  
  ## Parse apsim file (XML), but the -tmp one
  apsim_xml <- xml2::read_xml(new.file.path)
  
  ## Try removing the temp file
  ## Do not clean up at this time
  unlink(new.file.path)
  
  ## Print names of crops present in the original file
  if(missing(root)){
    crop.names <- xml2::xml_attr(xml2::xml_find_all(apsim_xml, ".//Soil/Water/SoilCrop"), "name")  
  }else{
    apsim_xml0 <- xml2::xml_find_all(apsim_xml, ".//simulation")
    sim.names <- unlist(xml2::xml_attrs(apsim_xml0))
    wsim <- grep(root, sim.names)
    root_node_ptr <- apsim_xml0[[wsim]]
    root_node_path <- xml2::xml_path(root_node_ptr)
    root_node <- xml2::xml_find_first(apsim_xml, root_node_path)
    crop.names <- xml2::xml_attr(xml2::xml_find_all(root_node, ".//Soil/Water/SoilCrop"), "name")  
  }
  
  if(verbose) cat("Crops in the original file", crop.names, "\n")

  if(!isTRUE(all.equal(sort(soil.profile$crops), sort(crop.names)))){
      cat("Name of crops in soil profile", soil.profile$crops,"\n")
      cat("Name of crops in APSIM file", crop.names,"\n")
      stop("Names of crops are not the same")
      ##cat("Names of crops are not the same")
  }
  
  soil.crops.node <- xml2::xml_find_all(apsim_xml, ".//Soil/Water/SoilCrop")
  
  for(crop in soil.profile$crops){
    crop.index <- which(crop.names == crop)
    for(j in c("Thickness", "XF", "LL", "KL")){
      
      if(missing(root)){
        crop.specific.node <- xml2::xml_find_first(xml2::xml_find_all(apsim_xml, ".//Soil/Water/SoilCrop")[[crop.index]], paste0("./", j))  
      }else{
        crop.specific.node <- xml2::xml_find_first(xml2::xml_find_all(root_node, ".//Soil/Water/SoilCrop")[[crop.index]], paste0("./", j))  
      }
      
      len.child.crop.specific.node <- length(xml2::xml_children(crop.specific.node))
      rows.soil.profile <- nrow(soil.profile$soil)
      ## If value is larger I grow the children to match lengths
      if(rows.soil.profile > len.child.crop.specific.node){
        for(i in seq_len(rows.soil.profile - len.child.crop.specific.node)){
          xml2::xml_add_child(crop.specific.node, xml2::xml_children(crop.specific.node)[[len.child.crop.specific.node]])
        }
      }
      if(rows.soil.profile < len.child.crop.specific.node){
        cat("Number of rows of soil profile:", rows.soil.profile,"\n")
        cat("Number of layers in apsim file:", len.child.crop.specific.node,"\n")
        cat("length of 'value' is shorter than length of crop specific node. \n At the moment I think I can grow XML nodes but not shrink them.")
        stop("Sorry. Don't really know how to do this yet.")
      }
      
      if(j != "Thickness"){
        jj <- paste0(crop, ".", j)
      }else{
        jj <- j
      }
      # print(jj)
      # print(as.character(soil.profile$soil[[jj]]))
      xml2::xml_set_text(xml2::xml_children(crop.specific.node), as.character(soil.profile$soil[[jj]]))
    }
  }

  if(overwrite == FALSE){
    wr.path <- paste0(wrt.dir,"/",
                      tools::file_path_sans_ext(file),
                      edit.tag,".apsim")
  }else{
    wr.path <- file.path(wrt.dir, file)
  }
  xml2::write_xml(apsim_xml, file = wr.path)
  
  if(verbose){
    cat("Created ", wr.path,"\n")
  }
}


#' 
#' @title Helper function to supply SoilWat parameters
#' @name soilwat_parms
#' @description Creates a list with specific components for the SoilWat model
#' @param SummerCona see APSIM documentation
#' @param SummerU see APSIM documentation
#' @param SummerDate see APSIM documentation
#' @param WinterCona see APSIM documentation
#' @param WinterU see APSIM documentation
#' @param WinterDate see APSIM documentation
#' @param DiffusConst see APSIM documentation
#' @param DiffusSlope see APSIM documentation
#' @param Salb soil albedo (see APSIM documentation)
#' @param CN2Bare see APSIM documentation
#' @param CNRed see APSIM documentation
#' @param CNCov see APSIM documentation
#' @param Slope see APSIM documentation
#' @param DischargeWidth see APSIM documentation
#' @param CatchmentArea see APSIM documentation
#' @param MaxPond see APSIM documentation
#' @param SWCON see APSIM documentation
#' @param Thickness provide the corresponding thickness layer
#' @return a \sQuote{list} with class \sQuote{soilwat_parms}
#' @details current documentation for APSIM 7.10 \url{https://www.apsim.info/documentation/model-documentation/soil-modules-documentation/soilwat/}
#' @export
#' 

soilwat_parms <- function(SummerCona = NA, SummerU = NA, SummerDate = NA,
                          WinterCona = NA, WinterU = NA, WinterDate = NA,
                          DiffusConst = NA, DiffusSlope = NA, Salb = NA,
                          CN2Bare = NA, CNRed = NA, CNCov = NA, Slope = NA,
                          DischargeWidth = NA, CatchmentArea = NA, MaxPond = NA,
                          SWCON = NA, Thickness = NA){
  
  ## Could incorporate error checking in the future
  
  sw.lst <- list(SummerCona = SummerCona, SummerU = SummerU, SummerDate = SummerDate,
              WinterCona = WinterCona, WinterU = WinterU, WinterDate = WinterDate,
              DiffusConst = DiffusConst, DiffusSlope = DiffusSlope, Salb = Salb,
              CN2Bare = CN2Bare, CNRed = CNRed, CNCov = CNCov, Slope = Slope,
              DischargeWidth = DischargeWidth, CatchmentArea = CatchmentArea, MaxPond = MaxPond, 
              SWCON = SWCON, Thickness = Thickness)
  
  if(!is.na(SWCON[1])){
    if(any(SWCON < 0) || any(SWCON > 1)) stop("SWCON should be between 0 and 1", call. = FALSE)  
  }
  
  if(length(SWCON) != length(Thickness))
    stop("length of SWCON should be the same as length of Thickness", call. = FALSE)
  
  if(!is.character(SummerCona) && !is.na(SummerCona)) stop("SummerCona should be a date as a character day-month. Ex: 1-Nov", call. = FALSE)
  if(!is.character(WinterCona) && !is.na(WinterCona)) stop("WinterCona should be a date as a character day-month. Ex: 1-Apr", call. = FALSE)
  
  ans <- structure(sw.lst, class = c("soilwat_parms","list"))
  ans
}

#'
#' @title Helper function to supply SWIM parameters
#' @name swim_parms
#' @description Creates a list with specific components for the SWIM model
#' @param Salb see APSIM documentation
#' @param CN2Bare see APSIM documentation
#' @param CNRed see APSIM documentation
#' @param CNCov see APSIM documentation
#' @param KDul see APSIM documentation
#' @param PSIDul see APSIM documentation
#' @param VC see APSIM documentation
#' @param DTmin see APSIM documentation
#' @param DTmax see APSIM documentation
#' @param MaxWaterIncrement see APSIM documentation
#' @param SpaceWeightingFactor see APSIM documentation
#' @param SoluteSpaceWeightingFactor see APSIM documentation
#' @param Diagnostics see APSIM documentation
#' @param SwimWaterTable_WaterTableDepth see APSIM documentation
#' @param SwimSubsurfaceDrain_DrainDepth see APSIM documentation
#' @param SwimSubsurfaceDrain_DrainSpacing see APSIM documentation
#' @param SwimSubsurfaceDrain_DrainRadius see APSIM documentation
#' @param SwimSubsurfaceDrain_Klat see APSIM documentation
#' @param SwimSubsurfaceDrain_ImpermDepth see APSIM documentation
#' @return a \sQuote{list} with class \sQuote{swim_parms}
#' @details current documentation for APSIM 7.10 \url{https://www.apsim.info/documentation/model-documentation/soil-modules-documentation/swim3/}
#' @export
#' 

swim_parms <- function(Salb = NA, CN2Bare = NA, CNRed = NA,
                       CNCov = NA, KDul = NA, PSIDul = NA,
                       VC = NA, DTmin = NA, DTmax = NA,
                       MaxWaterIncrement = NA, SpaceWeightingFactor = NA, 
                       SoluteSpaceWeightingFactor = NA, Diagnostics = NA,
                       SwimWaterTable_WaterTableDepth = NA, 
                       SwimSubsurfaceDrain_DrainDepth = NA,
                       SwimSubsurfaceDrain_DrainSpacing = NA,
                       SwimSubsurfaceDrain_DrainRadius = NA,
                       SwimSubsurfaceDrain_Klat = NA, 
                       SwimSubsurfaceDrain_ImpermDepth = NA){
  
  ## Could incorporate error checking in the future
  
  swim.lst <- list(Salb = Salb, CN2Bare = CN2Bare, CNRed = CNRed,
                 CNCov = CNCov, KDul = KDul, PSIDul = PSIDul,
                 VC = VC, DTmin = DTmin, DTmax = DTmax,
                 MaxWaterIncrement = MaxWaterIncrement, 
                 SpaceWeightingFactor = SpaceWeightingFactor, 
                 SoluteSpaceWeightingFactor = SoluteSpaceWeightingFactor, 
                 Diagnostics = Diagnostics,
                 SwimWaterTable_WaterTableDepth = SwimWaterTable_WaterTableDepth, 
                 SwimSubsurfaceDrain_DrainDepth = SwimSubsurfaceDrain_DrainDepth,
                 SwimSubsurfaceDrain_DrainSpacing = SwimSubsurfaceDrain_DrainSpacing,
                 SwimSubsurfaceDrain_DrainRadius = SwimSubsurfaceDrain_DrainRadius,
                 SwimSubsurfaceDrain_Klat = SwimSubsurfaceDrain_Klat, 
                 SwimSubsurfaceDrain_ImpermDepth = SwimSubsurfaceDrain_ImpermDepth)
  
  ans <- structure(swim.lst, class = c("swim_parms","list"))
  ans
}

#'
#' @title Helper function to supply additional Soil Organic Matter parameters
#' @name soilorganicmatter_parms
#' @description Creates a list with specific components for the Soil Organic Matter module
#' @param RootCN Root Carbon:Nitrogen ratio (see APSIM documentation)
#' @param RootWt Root weight (see APSIM documentation)
#' @param EnrACoeff (see APSIM documentation)
#' @param EnrBCoeff (see APSIM documentation)
#' @param OCUnits Organic Carbon Units
#' @export

soilorganicmatter_parms <- function(RootCN = NA, RootWt = NA, EnrACoeff = NA, EnrBCoeff = NA, OCUnits = NA){
  
  som.list <- list(RootCN = RootCN, RootWt = RootWt, EnrACoeff = EnrACoeff, EnrBCoeff = EnrBCoeff, OCUnits = OCUnits)
  
  if(!is.na(RootCN) && RootCN <= 0) warning("RootCN should be a value greater than zero") 
  if(!is.na(RootWt) && RootWt <= 0) warning("RootWt should be a value greater than zero")
  if(!is.na(EnrACoeff) && EnrACoeff <= 0) warning("EnrACoeff should be a value greater than zero")
  if(!is.na(EnrBCoeff) && EnrBCoeff <= 0) warning("EnrBCoeff should be a value greater than zero")
  
  ans <- structure(som.list, class = c("soilorganicmatter_parms","list"))
  ans
  
}

#'
#' @title Helper function to supply additional Initial Soil Water parameters
#' @name initialwater_parms
#' @description Creates a list with specific components for the Initial Soil Water module
#' @param Depth depth for soil layers (see APSIM documentation)
#' @param Thickness soil thickness for layers (either enter Depth or Thickness, but not both)
#' @param InitialValues initial values of soil water
#' @param InitialPAWmm Initial Plant Available Water in mm
#' @param PercentFull Percent full (0 - 100)
#' @param RelativeTo usually LL15
#' @param FilledFromTop either true or false
#' @param DepthWetSoil depth of wet soil in mm
#' @export

initialwater_parms <- function(Depth = NA, Thickness = NA, InitialValues = NA, InitialPAWmm = NA, 
                               PercentFull = NA, RelativeTo = NA, FilledFromTop = NA, DepthWetSoil = NA){
  
  initialwater.list <- list(Depth = Depth, Thickness = NA, InitialValues = InitialValues, 
                            InitialPAWmm = InitialPAWmm, PercentFull = PercentFull, 
                            FilledFromTop = FilledFromTop, RelativeTo = RelativeTo, 
                            DepthWetSoil = DepthWetSoil)
  
  if(!is.na(InitialPAWmm) && InitialPAWmm <= 0) warning("InitialPAWmm should be a value greater than zero") 
  if(!is.na(PercentFull) && PercentFull <= 0) warning("PercentFull should be a value greater than zero")
  if(!is.na(PercentFull) && PercentFull > 100) warning("PercentFull should be a value of less than 100")
  if(!is.na(DepthWetSoil) && DepthWetSoil <= 0) warning("DepthWetSoil should be a value greater than zero")
  if(!is.na(Thickness) && any(Thickness < 0)) warning("Thickness values should be greater than zero")
  
  ans <- structure(initialwater.list, class = c("initialwater_parms","list"))
  ans
  
}
