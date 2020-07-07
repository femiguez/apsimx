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
#' @param edit.tag default edit tag \sQuote{-edited}
#' @param overwrite default FALSE
#' @param verbose default TRUE. Will print messages indicating what was done.
#' @return writes a file to disk with the supplied soil profile
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
                                             edit.tag = "-edited",
                                             overwrite = FALSE,
                                             verbose = TRUE){
  
  .check_apsim_name(file)
  
  if(missing(wrt.dir)) wrt.dir <- src.dir
  
  file.names <- dir(path = src.dir, pattern=".apsim$",ignore.case=TRUE)
  
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
                 verbose = verbose)
    }else{
      new.file.path <- paste0(tools::file_path_sans_ext(file),"-tmp.apsim")

      edit_apsim(file = new.file.path, src.dir = ".", wrt.dir = ".", 
                 node = "Soil", soil.child = "Water", 
                 overwrite = TRUE,
                 parm = i, value = soil.profile$soil[[i]],
                 check.length = FALSE,
                 verbose = verbose)
    }
    k <- k + 1
  }
  
  ## Edit soil water parameters
  if(!missing(soilwat) || !is.na(soil.profile$soilwat)){
    if(!inherits(soilwat, "soilwat_parms")) stop("object should be of class 'soilwat_parms'")
    
    for(i in seq_along(soilwat)){
      prm.vl <- soilwat[[i]]
      if(is.na(prm.vl)) next
      
      edit_apsim(file = new.file.path, src.dir = ".", wrt.dir = ".",
                 node = "Soil", soil.child = "Water", 
                 overwrite = TRUE, parm = names(soilwat[i]),
                 value = prm.vl)
    }
  }
  
  if(!missing(swim) || !is.na(soil.profile$swim)){
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
                 value = prm.vl)
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
                 value = prm.vl)
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
             verbose = verbose)
  }
  
  ## Edit Analysis PH
  for(i in c("Thickness","PH","EC")){
    if(i == "EC"){
      edit_apsim(file = new.file.path, src.dir = ".", wrt.dir = ".", 
                 node = "Soil", soil.child = "Analysis", 
                 overwrite = TRUE,
                 parm = i, value = rep(0, nrow(soil.profile$soil)),
                 check.length = FALSE,
                 verbose = verbose)
    }else{
      edit_apsim(file = new.file.path, src.dir = ".", wrt.dir = ".", 
                 node = "Soil", soil.child = "Analysis", 
                 overwrite = TRUE,
                 parm = i, value = soil.profile$soil[[i]],
                 check.length = FALSE,
                 verbose = verbose)
    }
  }
  
  ## Edit Sample (NO3, NH4)
  for(i in c("Thickness","NO3N","NH4N")){
    if(i == "Thickness") ii <- "Thickness"
    if(i == "NO3N") ii <- "NO3"
    if(i == "NH4N") ii <- "NH4"

    edit_apsim(file = new.file.path, src.dir = ".", wrt.dir = ".", 
             node = "Soil", soil.child = "Sample", 
             overwrite = TRUE,
             parm = ii, value = soil.profile$soil[[i]],
             check.length = FALSE,
             verbose = verbose)
  }
  
  ## Parse apsim file (XML), but the -tmp one
  apsim_xml <- xml2::read_xml(new.file.path)
  
  ## Try removing the temp file
  ## Do not clean up at this time
  unlink(new.file.path)
  
  ## Print names of crops present in the original file
  crop.names <- xml2::xml_attr(xml2::xml_find_all(apsim_xml, ".//Soil/Water/SoilCrop"), "name")
  
  if(verbose) cat("Crops in the original file",crop.names,"\n")

  if(!isTRUE(all.equal(sort(soil.profile$crops), sort(crop.names)))){
      cat("Name of crops in soil profile", soil.profile$crops,"\n")
      cat("Name of crops in APSIM file", crop.names,"\n")
      stop("Names of crops are not the same")
  }
  
  soil.crops.node <- xml2::xml_find_all(apsim_xml, ".//Soil/Water/SoilCrop")
  
  for(i in soil.profile$crops){
    crop.index <- which(crop.names == i)
    for(j in c("Thickness","XF","LL","KL")){
      crop.specific.node <- xml2::xml_find_first(xml2::xml_find_all(apsim_xml, ".//Soil/Water/SoilCrop")[[crop.index]],paste0("./",j))
      
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
        jj <- paste0("crop.",j)
      }else{
        jj <- j
      }
      
      xml2::xml_set_text(xml2::xml_children(crop.specific.node), as.character(soil.profile$soil[[jj]]))
    }
  }

  if(overwrite == FALSE){
    wr.path <- paste0(wrt.dir,"/",
                      tools::file_path_sans_ext(file),
                      edit.tag,".apsim")
  }else{
    wr.path <- paste0(wrt.dir,"/",file)
  }
  xml2::write_xml(apsim_xml, file = wr.path)
  
  if(verbose){
    cat("Created ",wr.path,"\n")
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
#' @return a \sQuote{list} with class \sQuote{soilwat_parms}
#' @details current documentation for APSIM 7.10 \url{https://www.apsim.info/documentation/model-documentation/soil-modules-documentation/soilwat/}
#' @export
#' 

soilwat_parms <- function(SummerCona = NA, SummerU = NA, SummerDate = NA,
                          WinterCona = NA, WinterU = NA, WinterDate = NA,
                          DiffusConst = NA, DiffusSlope = NA, Salb = NA,
                          CN2Bare = NA, CNRed = NA, CNCov = NA){
  
  ## Could incorporate error checking in the future
  
  sw.lst <- list(SummerCona = SummerCona, SummerU = SummerU, SummerDate = SummerDate,
              WinterCona = WinterCona, WinterU = WinterU, WinterDate = WinterDate,
              DiffusConst = DiffusConst, DiffusSlope = DiffusSlope, Salb = Salb,
              CN2Bare = CN2Bare, CNRed = CNRed, CNCov = CNCov)
  
  
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
