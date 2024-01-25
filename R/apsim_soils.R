

#' @title Read in a soils (XML) file into a list of \sQuote{soil_profile} objects
#' @name read_apsim_soils
#' @description APSIM soils can be stored as XML files (soils) and reading them in
#' converts them into a list of individual objects of class \sQuote{soil_profile}
#' @param file name of the file (the extension sohuld be .soils)
#' @param src.dir directory containing the .soils file (defaults to the current directory)
#' @param verbose whether to print additional information about the progress of reading 
#' the individual soils in.
#' @export
#' @examples 
#' \donttest{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' 
#' sls <- read_apsim_soils("Clarion.soils", src.dir = extd.dir)
#' 
#' }

read_apsim_soils <- function(file, src.dir = ".", verbose = TRUE){
  
  src.dir <- normalizePath(src.dir)
  
  file.names <- dir(path = src.dir, pattern=".soils$", ignore.case=TRUE)
  
  if(length(file.names)==0){
    stop("There are no .soils files in the specified directory to edit.")
  }
  
  file <- match.arg(file, file.names)
  
  soils <- xml2::read_xml(file.path(src.dir, file))

  # if(!missing(root)){
  #   soils <- xml2::xml_find_first(soils, xpath = root)
  # }
  ## Find all soils
  soils.names <- xml2::xml_find_all(soils, ".//Soil")
  
  soils.nms <- xml2::xml_attr(soils.names, "name")
  
  ans.soils <- vector("list", length = length(soils.names))
  
  ## There are potentially many soils
  for(i in seq_along(soils.names)){

    if(verbose){
      cat("Soil:", i, "\n")
      cat(xml2::xml_attr(soils.names[i], "name"), "\n")
      cat("Progress (%):", i/length(soils.names) * 100, "\n")
    }    
    
    soil.path <- xml2::xml_path(soils.names[i]) ## Path to the current soil
    soil <- xml2::xml_find_first(soils, soil.path) ## Extract current soil
    ## This would need to extract the current soil
    ## Retrieve metadata
    ## Doing this manually because some need to converted to numeric and others don't
    metadata <- list()
    metadata$RecordNumber <- as.numeric(xml2::xml_text(xml2::xml_find_first(soil, ".//RecordNumber")))
    metadata$ASCOrder <- xml2::xml_text(xml2::xml_find_first(soil, ".//ASCOrder"))
    metadata$ASCSubOrder <- xml2::xml_text(xml2::xml_find_first(soil, ".//ASCSubOrder"))
    metadata$SoilType <- xml2::xml_text(xml2::xml_find_first(soil, ".//SoilType"))
    metadata$LocalName <- xml2::xml_text(xml2::xml_find_first(soil, ".//LocalName"))
    metadata$Site <- xml2::xml_text(xml2::xml_find_first(soil, ".//Site"))
    metadata$NearestTown <- xml2::xml_text(xml2::xml_find_first(soil, ".//NearestTown"))
    metadata$Region <- xml2::xml_text(xml2::xml_find_first(soil, ".//Region"))
    metadata$State <- xml2::xml_text(xml2::xml_find_first(soil, ".//State"))
    metadata$Country <- xml2::xml_text(xml2::xml_find_first(soil, ".//Country"))
    metadata$NaturalVegetation <- xml2::xml_text(xml2::xml_find_first(soil, ".//NaturalVegetation"))
    metadata$ApsoilNumber <- as.numeric(xml2::xml_text(xml2::xml_find_first(soil, ".//ApoilNumber")))
    metadata$Latitude <- as.numeric(xml2::xml_text(xml2::xml_find_first(soil, ".//Latitude")))
    metadata$Longitude <- as.numeric(xml2::xml_text(xml2::xml_find_first(soil, ".//Longitude")))
    metadata$LocationAccuracy <- xml2::xml_text(xml2::xml_find_first(soil, ".//LocationAccuracy"))
    metadata$YearOfSampling <- as.numeric(xml2::xml_text(xml2::xml_find_first(soil, ".//YearOfSampling")))
    metadata$DataSource <- xml2::xml_text(xml2::xml_find_first(soil, ".//DataSource"))
    metadata$Comments <- xml2::xml_text(xml2::xml_find_first(soil, ".//Comments"))
    
    ## Get the soil Water
    properties <- c("Thickness", "BD", "AirDry", "LL15", "DUL", "SAT", "BDMetadata", "AirDryMetadata", "LL15Metadata",
                    "DULMetadata", "SATMetadata")
    
    soil.soil.water <- vector("list", length = length(properties)) 
    names(soil.soil.water) <- properties
      
    for(j in seq_along(properties)){
      prprty.node <- xml2::xml_find_first(soil, paste0(".//Water/", properties[j]))
      if(grepl("Metadata", properties[j])){
        soil.soil.water[[j]] <- xml2::xml_text(xml2::xml_children(prprty.node))  
      }else{
        soil.soil.water[[j]] <- xml2::xml_double(xml2::xml_children(prprty.node))    
      }
    }
    
    wssw <- as.vector(which(sapply(soil.soil.water, length) > 0))
    soil.soil.water.d0 <- as.data.frame(soil.soil.water[wssw])
    soil.soil.water.metadata <- soil.soil.water.d0[, grepl("Metadata", names(soil.soil.water.d0))]
    soil.soil.water.d <- soil.soil.water.d0[, !grepl("Metadata", names(soil.soil.water.d0))]
    
    ## Get the soil Crop
    soil.soilcrops <- xml2::xml_attr(xml2::xml_find_all(soil, ".//SoilCrop"), "name")
    
    soil.soilcrops.thickness <- xml2::xml_double(xml2::xml_children(xml2::xml_find_first(soil, ".//SoilCrop/Thickness")))
      
    soilcrop.properties <- c("LL", "KL", "XF", "LLMetadata")
    
    soil.soilcrops.list <- vector("list", length = length(soil.soilcrops))
    names(soil.soilcrops.list) <- soil.soilcrops
    soilcrop.properties.list <- vector("list", length = length(soilcrop.properties))
    names(soilcrop.properties.list) <- soilcrop.properties
      
    for(j in seq_along(soil.soilcrops)){
      for(k in seq_along(soilcrop.properties)){
        soil.prprty.node <- xml2::xml_find_first(soil, paste0(".//SoilCrop[", j, "]/", soilcrop.properties[k]))
        if(grepl("Metadata", soilcrop.properties[k])){  
          soilcrop.properties.list[[k]] <- xml2::xml_text(xml2::xml_children(soil.prprty.node))  
        }else{
          tmp <- xml2::xml_double(xml2::xml_children(soil.prprty.node))  
          if(length(tmp) == 0){
            tmp <- rep(0, length(soilcrop.properties.list[[1]]))
          }
          soilcrop.properties.list[[k]] <- tmp
        }
      }
      soil.soilcrops.list[[j]] <- soilcrop.properties.list
    }
    
    ## Remove metadata if empty
    for(k in seq_along(soil.soilcrops.list)){
      tmp.crop <- soil.soilcrops.list[k]
      not.metadata <- !grepl("Metadata", sapply(tmp.crop, names))
      if(length(tmp.crop[[1]][[which(not.metadata == 0)]]) < 1){
        tmp.list <- vector("list", length = 1)
        names(tmp.list) <- names(tmp.crop)
        tmp.list[[1]] <- tmp.crop[[1]][not.metadata]
        soil.soilcrops.list[k] <- tmp.list         
      }
    }
    soil.soilcrops.d0 <- data.frame(Thickness = soil.soilcrops.thickness, as.data.frame(soil.soilcrops.list))
    soil.soilcrops.metadata <- soil.soilcrops.d0[, grepl("Metadata", names(soil.soilcrops.d0))]
    soil.soilcrops.d <- soil.soilcrops.d0[, !grepl("Metadata", names(soil.soilcrops.d0))]
    
    ## Get SoilWat parameters
    soilwat.node <- xml2::xml_find_all(soil, ".//SoilWater")   
    
    ## Get SWCON first
    swcon <- xml2::xml_double(xml2::xml_children(xml2::xml_find_first(soilwat.node, ".//SWCON")))
    soilwat.thickness <- xml2::xml_double(xml2::xml_children(xml2::xml_find_first(soilwat.node, ".//Thickness")))
    
    if(any(is.na(swcon))){
      swcon[is.na(swcon)] <- mean(swcon, na.rm = TRUE)
    }
    
    if(any(is.nan(swcon))){
      swcon[is.nan(swcon)] <- mean(swcon, na.rm = TRUE)
    }
    
    if(any(swcon > 1)){
      swcon[swcon > 1] <- mean(swcon)
    }
    
    soilwat <- soilwat_parms(Thickness = soilwat.thickness, SWCON = swcon)

    soilwat.var.names <- c("SummerCona", "SummerU", "SummerDate", "WinterCona", "WinterU", "WinterDate",
                           "DiffusConst", "DiffusSlope", "Salb", "CN2Bare", "CNRed", "CNCov", "Slope",
                           "DischargeWidth", "CatchmentArea", "MaxPond")
    
    for(j in seq_along(soilwat.var.names)){
      tmp <- xml2::xml_text(xml2::xml_find_first(soilwat.node, soilwat.var.names[j]))
      if(is.na(tmp)) next
      if(tmp == "NaN") next
      if(soilwat.var.names[j] %in% c("SummerDate", "WinterDate")){
        soilwat[[soilwat.var.names[j]]] <- tmp
      }else{
        soilwat[[soilwat.var.names[j]]] <- as.numeric(tmp)
      }
    }
    
    ## Section SoilOrganicMatter
    som.node <- xml2::xml_find_all(soil, ".//SoilOrganicMatter")   
    
    som.vars1 <- c("Thickness", "OC", "FBiom", "FInert", "SoilCN")
    som.vars2 <- c("RootCN", "RootWt", "EnrACoeff", "EnrBCoeff", "OCUnits")
    
    som.lst1 <- vector("list", length = length(som.vars1)) 
    som.lst2 <- vector("list", length = length(som.vars2)) 
    names(som.lst1) <- som.vars1
    names(som.lst2) <- som.vars2
    
    for(j in seq_along(som.vars1)){
      if(som.vars1[j] != "SoilCN"){
        tmp <- xml2::xml_text(xml2::xml_children(xml2::xml_find_first(som.node, som.vars1[j])))  
      }else{
        tmp <- xml2::xml_text(xml2::xml_find_first(som.node, som.vars1[j]))  
      }
      if(som.vars1[j] == "OCMetadata"){
        som.lst1[[som.vars1[j]]] <- tmp
      }else{
        if(som.vars1[j] == "SoilCN"){
          som.lst1[[som.vars1[j]]] <- as.numeric(rep(tmp, nrow(soil.soil.water.d))) 
        }else{
          if(length(tmp) == 0){
            tmp <- rep(0, length(som.vars1[[1]]))
          }
          som.lst1[[som.vars1[j]]] <- as.numeric(tmp)  
        }
      }
    }
    
    for(j in seq_along(som.vars2)){
      tmp <- xml2::xml_text(xml2::xml_find_first(som.node, som.vars2[j]))
      if(som.vars2[j] == "OCUnits"){
        som.lst2[[som.vars2[[j]]]] <- tmp
      }else{
        som.lst2[[som.vars2[j]]] <- as.numeric(tmp)
      }
    }

    ### Section fixing SoilCN
    if(length(som.lst1$SoilCN) == 0){
      som.lst1$SoilCN <- rep(0, length(som.lst1$Thickness))
    }
    
    if(length(som.lst1$SoilCN) == 1){
      som.lst1$SoilCN <- rep(som.lst1$SoilCN, length(som.lst1$Thickness))
    }
    
    if(length(som.lst1$Thickness) != length(som.lst1$SoilCN)){
      som.lst1$SoilCN <- rep(som.lst1$SoilCN[1], length(som.lst1$Thickness))
    }
    
    ### Section fixing FBiom
    if(length(som.lst1$Fbiom) == 0){
      som.lst1$FBiom <- rep(0, length(som.lst1$Thickness))
    }
    
    if(length(som.lst1$FBiom) == 1){
      som.lst1$FBiom <- rep(som.lst1$FBiom, length(som.lst1$Thickness))
    }
    
    if(length(som.lst1$Thickness) != length(som.lst1$FBiom)){
      som.lst1$FBiom <- rep(som.lst1$FBiom[1], length(som.lst1$Thickness))
    }
    
    som.d1 <- as.data.frame(som.lst1)
    som.d2 <- soilorganicmatter_parms(RootCN = som.lst2$RootCN, RootWt = som.lst2$RootWt, 
                                      EnrACoeff = som.lst2$EnrACoeff, EnrBCoeff = som.lst2$EnrBCoeff,
                                      OCUnits = som.lst2$OCUnits)
    ## Analysis
    analysis.node <- xml2::xml_find_all(soil, ".//Analysis")       

    analysis.vars1 <- c("Thickness", "NO3N", "NH4N", "PH", "ParticleSizeClay", "ParticleSizeSilt", "ParticleSizeSand")
    analysis.vars2 <- c("PHMetadata", "ParticleSizeMetadata")
    analysis.vars3 <- c("PHUnits", "BoronUnits")
    
    analysis.list1 <- vector("list", length = length(analysis.vars1)) 
    names(analysis.list1) <- analysis.vars1
    analysis.list2 <- vector("list", length = length(analysis.vars2)) 
    names(analysis.list2) <- analysis.vars2
    analysis.list3 <- vector("list", length = length(analysis.vars3)) 
    names(analysis.list3) <- analysis.vars3
    
    for(j in seq_along(analysis.vars1)){
      tmp <- xml2::xml_text(xml2::xml_children(xml2::xml_find_first(analysis.node, analysis.vars1[j])))  
      if(j == 1) nlayers <- length(tmp)
      if(length(tmp) == 0){
        analysis.list1[[j]] <- rep(NA, nlayers)
      }else{
        analysis.list1[[j]] <- as.numeric(tmp)    
      }
    }
    
    for(j in seq_along(analysis.vars2)){
      tmp <- xml2::xml_text(xml2::xml_children(xml2::xml_find_first(analysis.node, analysis.vars2[j])))  
      analysis.list2[[j]] <- tmp  
    }
    
    for(j in seq_along(analysis.vars3)){
      tmp <- xml2::xml_text(xml2::xml_children(xml2::xml_find_first(analysis.node, analysis.vars3[j])))  
      analysis.list3[[j]] <- tmp
    }
    
    analysis.d1 <- as.data.frame(analysis.list1)
    analysis.d1 <- analysis.d1[,apply(analysis.d1[1,], 2, function(x) !is.na(x))]
    
    if(length(soil.soilcrops) > 0){
      if(sum(soil.soil.water.d$Thickness - soil.soilcrops.d$Thickness) > 0)
        stop("Thickness of soil.water does not match Thickness of soilcrops")
      which.soilcrops.thickness <- which(names(soil.soilcrops.d) == "Thickness")
      soil0.soil0 <- cbind(soil.soil.water.d, soil.soilcrops.d[,-which.soilcrops.thickness])      
    }else{
      soil0.soil0 <- soil.soil.water.d
    }

    if(length(soil0.soil0$Thickness) != length(som.d1$Thickness)){
      message("Thickness of soil.water/soilcrops does not match Thickness of soilorganicmatter")
      message("Adding/removing rows as necessary")
      nrow.diff <- nrow(soil0.soil0) - nrow(som.d1)
      if(nrow(som.d1) < nrow(soil0.soil0)){
        for(i in 1:nrow.diff){
          som.d1 <- rbind(som.d1, som.d1[nrow(som.d1), , drop = FALSE])  
        }
      }else{
        som.d1 <- som.d1[1:length(soil0.soil0$Thickness),]
      }
    }
      
    which.som.thickness <- which(names(som.d1) == "Thickness")
    soil0.soil1 <- cbind(soil0.soil0, som.d1[,-which.som.thickness])
    
    if(length(soil0.soil1$Thickness) != length(analysis.d1$Thickness)){
      message("Thickness of soil.water/soilcrops does not match Thickness of analysis")
      message("Adding/removing rows as necessary")
      if(nrow(analysis.d1) < nrow(soil0.soil0)){
        nrow.diff <- nrow(soil0.soil0) - nrow(analysis.d1)
        for(i in 1:nrow.diff){
          analysis.d1 <- rbind(analysis.d1, analysis.d1[nrow(analysis.d1), , drop = FALSE])  
        }
      }else{
        analysis.d1 <- analysis.d1[1:length(soil0.soil0$Thickness),]
      }
    }
      
    which.analysis.thickness <- which(names(analysis.d1) == "Thickness")
    soil0.soil <- cbind(soil0.soil1, analysis.d1[,-which.analysis.thickness])
    
    ## Add 'Depth' to soil0.soil
    Depth <- .t2d(soil0.soil$Thickness)
    soil0.soil <- data.frame(Depth = Depth, soil0.soil)
    soil0.soil$Carbon <- soil0.soil$OC
    
    soil0 <- list(soil = soil0.soil, soilwat = soilwat, soilorganicmatter = som.d2, metadata = metadata)
    class(soil0) <- "soil_profile"
    ans.soils[[i]] <- soil0
  }
  
  names(ans.soils) <- soils.nms
  
  return(ans.soils)
}


write_apsim_soils <- function(x, wrt.dir = "."){
  
  if(!is.list(x)){
    stop("object 'x' should be a list", call. = FALSE)
  }
  
  apsimx.version <- packageVersion("apsimx")
  ## Write the header for the soil
  soil.xml.doc <- xml2::read_xml(paste0("<folder version=\"", apsimx.version,"\" creator=\"apsimx r-package\" name=\"Soils\"> </folder>"))
  
  for(i in 1:length(x)){
    if(is.null(names(x)[i])){
      soil.name <- x[[i]]$metadata$SoilType
    }else{
      soil.name <- names(x)[i]
    }
    soil.name.header <- xml2::read_xml(paste0("<Soil name = \"", soil.name, "\"> </Soil>"))
    xml2::xml_add_child(soil.xml.doc, soil.name.header)
    
    ## Add metadata
    for(j in seq_along(x[[i]]$metadata)){
      mp <- names(x[[i]]$metadata[j]) ## metadata property
      mv <- x[[i]]$metadata[[j]] ## metadata value
      metd <- xml2::read_xml(paste0("<", mp, ">", mv, "</", mp, ">"))
      curr.soil <- xml2::xml_child(soil.xml.doc, search = i)
      xml2::xml_add_child(curr.soil, metd)  
    }
    
    double.vars <- c("Thickness", "BD", "AirDry", "LL15", "DUL", "SAT")
    metadata.vars <- c("BDMetadata", "AirDryMetadata", "LL15Metadata", "DULMetadata", "SATMetadata")
    ## Add soil water
    water.node <- xml2::read_xml("<Water></Water>")
    xml2::xml_add_child(curr.soil, water.node)
    for(j in seq_along(names(x[[i]]$soil))){
      scn <- names(x[[i]]$soil)[j] ## soil column name
      if(scn == "Depth") next
      scv <- x[[i]]$soil[[scn]] ## soil column value
      soil.water.node <- xml2::read_xml(paste0("<", scn, "> </", scn, ">"))
      if(scn %in% double.vars){
        for(k in seq_along(scv)){
          dbl.vr <- xml2::read_xml(paste0("<double>", scv[k], "</double>")) ## double variable
          xml2::xml_add_child(soil.water.node, dbl.vr)
        }
      }
      ## If metadata, I don't know what to do
      if(scn %in% metadata.vars){
        for(k in seq_along(scv)){
          mtd.vr <- xml2::read_xml(paste0("<string>", scv[k], "</string>")) ## string variable
          xml2::xml_add_child(soil.water.node, mtd.vr)
        }
      }
      
      ## Add soil crop
      curr.water <- xml2::xml_child(curr.soil, ".//Water")
      xml2::xml_add_child(curr.water, soil.water.node)
    }
    ## Add soil carbon
    
    
  }
  ## Write to file
  ## This is ugly at the moment, but do not know how to fix it
  xml2::write_xml(curr.soil, file = "current_soil_whitespace.xml",
                  options = "format_whitespace")
}