#' 
#' @title Inspect an .apsimx (JSON) file
#' @name inspect_apsimx
#' @description inspect a JSON apsimx file. It does not replace the GUI, but it can save time by quickly checking parameters and values.
#' @param file file ending in .apsimx to be inspected (JSON)
#' @param src.dir directory containing the .apsimx file to be inspected; defaults to the current working directory
#' @param node specific node to be inspected either \sQuote{Clock}, \sQuote{Weather}, 
#' \sQuote{Soil}, \sQuote{SurfaceOrganicMatter}, \sQuote{MicroClimate}, \sQuote{Crop},
#'  \sQuote{Manager} or \sQuote{Other}
#' @param soil.child specific soil component to be inspected. The options vary depending on what is available (see details)
#' @param parm parameter to refine the inspection of the \sQuote{manager} list(\sQuote{parm},\sQuote{position}), use \sQuote{NA} for all the positions. \sQuote{parm} can be a regular expression for partial matching.
#' @param digits number of decimals to print (default 3). Not used now because everything is a character.
#' @param print.path whether to print the path to the specific parameter. Useful to give the later editing. (Also returned as \sQuote{invisible})
#' @param root root node label. In simulation structures such as factorials there will be multiple possible nodes. This can be specified by supplying an appropriate character.
#' @details This is simply a script that prints the relevant parameters which are likely to need editing. It does not print all information from an .apsimx file.
#'          To investigate the available \sQuote{soil.childs} specify \sQuote{Soil} for \sQuote{node} and do not specify the \sQuote{soil.child}.
#' @return prints a table with inspected parameters and values (and \sQuote{parm path} when \sQuote{print.path} = TRUE).
#' @export
#' @examples 
#' \dontrun{
#' ex.dir <- auto_detect_apsimx_examples()
#' inspect_apsimx("Barley", src.dir = ex.dir, node = "Clock") 
#' inspect_apsimx("Barley", src.dir = ex.dir, node = "Weather")
#' inspect_apsimx("Barley", src.dir = ex.dir, node = "Soil", soil.child = "Metadata") 
#' inspect_apsimx("Barley", src.dir = ex.dir, node = "Soil", soil.child = "Water") 
#' inspect_apsimx("Barley", src.dir = ex.dir, node = "Soil", soil.child = "SoilWater") 
#' inspect_apsimx("Barley", src.dir = ex.dir, node = "Soil", soil.child = "Organic")
#' inspect_apsimx("Barley", src.dir = ex.dir, node = "Soil", soil.child = "Chemical")
#' inspect_apsimx("Barley", src.dir = ex.dir, node = "Soil", soil.child = "InitialWater")
#' inspect_apsimx("Barley", src.dir = ex.dir, node = "Soil", soil.child = "InitialN")
#' inspect_apsimx("Barley", src.dir = ex.dir, node = "SurfaceOrganicMatter")
#' inspect_apsimx("Barley", src.dir = ex.dir, node = "MicroClimate")
#' inspect_apsimx("Barley", src.dir = ex.dir, node = "Crop")
#' inspect_apsimx("Barley", src.dir = ex.dir, node = "Manager")
#' }
#'

inspect_apsimx <- function(file = "", src.dir = ".", 
                           node = c("Clock","Weather","Soil","SurfaceOrganicMatter",
                                   "MicroClimate","Crop","Manager","Other"),
                           soil.child = c("Metadata","Water","InitialWater",
                                          "Chemical","Physical","Analysis","SoilWater",
                                          "InitialN", "CERESSoilTemperature","Sample",
                                          "Nutrient","Organic"),
                           parm = NULL,
                           digits = 3,
                           print.path = FALSE,
                           root){
  
  .check_apsim_name(file)
  
  file.names <- dir(path = src.dir, pattern=".apsimx$",ignore.case=TRUE)
  
  if(length(file.names)==0){
    stop("There are no .apsimx files in the specified directory to inspect.")
  }
  
  node <- match.arg(node)
  soil.child <- match.arg(soil.child)
  
  if(soil.child %in% c("Nutrient")) stop("Not implemented yet")
  
  ## This matches the specified file from a list of files
  ## Notice that the .apsimx extension will be added here
  file <- match.arg(file, file.names)
  
  apsimx_json <- jsonlite::read_json(paste0(src.dir,"/",file))
  
  parm.path.0 <- paste0(".",apsimx_json$Name) ## Root
  ## I think that everything I might want to look at 
  ## is under this Children/Children node
  ## It looks like I need to 'find' the "Models.Core.Simulation" node
  fcsn <- grep("Models.Core.Simulation", apsimx_json$Children, fixed = TRUE)
  
  if(length(fcsn) > 1){
    if(missing(root)){
      cat("Simulation structure: \n")
      str_list(apsimx_json)
      stop("more than one simulation found and no root node label has been specified \n select one of the children names above")   
    }else{
      if(length(root) == 1){
        nms <- vapply(apsimx_json$Children, FUN = function(x) x$Name, 
                      FUN.VALUE = "character")
        fcsn <- grep(as.character(root), nms)
        parm.path.1 <- paste0(parm.path.0,".", apsimx_json$Children[[fcsn]]$Name)
        parent.node <- apsimx_json$Children[[fcsn]]$Children
        if(length(fcsn) == 0 || length(fcsn) > 1)
          stop("no root node label found or root is not unique")
      }else{
        nms1 <- vapply(apsimx_json$Children, FUN = function(x) x$Name, 
                      FUN.VALUE = "character")
        fcsn1 <- grep(as.character(root[1]), nms1)
        root.node.0 <- apsimx_json$Children[[fcsn1]]
        root.node.0.child.names <- vapply(root.node.0$Children, function(x) x$Name, 
                                          FUN.VALUE = "character")
        fcsn2 <- grep(as.character(root[2]), root.node.0.child.names)
        parent.node <- apsimx_json$Children[[fcsn1]]$Children[[fcsn2]]$Children
        parm.path.1 <- paste0(parm.path.0,".",apsimx_json$Children[[fcsn1]]$Children[[fcsn2]])
      }
    }
  }else{
    parent.node <- apsimx_json$Children[[fcsn]]$Children  
    parm.path.1 <- paste0(parm.path.0,".",apsimx_json$Children[[fcsn]]$Name)
  }
  
  
  if(node == "Clock"){
    wlc <- function(x) grepl("Clock", x$Name, ignore.case = TRUE)
    wlcl <- sapply(parent.node, FUN = wlc)
    clock.node <- as.list(parent.node[wlcl])[[1]]
    start.name <- grep("start", names(clock.node), ignore.case = TRUE, value = TRUE)
    end.name <- grep("end", names(clock.node), ignore.case = TRUE, value = TRUE)
    cat("Start:", clock.node[[start.name]], "\n")
    cat("End:",  clock.node[[end.name]], "\n")
    ## It is possible for the 'start' and 'end' to be called: 'Start' and 'End'
    ## It is also possible for them to be called 'StartDate' and 'EndDate'
    ## I think APSIM-X is in a state of change and eventually this will
    ## stabilize. At the moment 'Maize' and 'Barley' do not agree.
    ## Final name is 'parm.path', but this is 1.2
    parm.path <- paste0(parm.path.1,".",parent.node[wlcl][[1]]$Name)
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
    parm.path <- paste0(parm.path.1,".",parent.node[wlwl][[1]]$Name)
  }
  
  ## From here on there is an important component that lives inside
  ## 'Models.Core.Zone'
  wcz <- grepl("Models.Core.Zone", parent.node)
  core.zone.node <- parent.node[wcz][[1]]$Children
  
  parm.path.2 <- paste0(parm.path.1,".",parent.node[wcz][[1]]$Name)
  
  if(node == "Soil"){
    ## Which soils node
    wsn <- grepl("Models.Soils.Soil", core.zone.node)
    soil.node <- core.zone.node[wsn]
    
    parm.path.2.1 <- paste0(parm.path.2,".",soil.node[[1]]$Name)
    
    ## Print some basic soil information
    cat("Soil Type: ", soil.node[[1]]$SoilType,"\n")
    cat("Latitude: ", soil.node[[1]]$Latitude,"\n")
    cat("Longitude: ", soil.node[[1]]$Longitude,"\n")
    
    if(length(soil.node) != 1) stop("soil.node not equal to one")
    
    soil.children.names <- sapply(soil.node[[1]]$Children, function(x) x$Name)
    
    cat("Soil children:", soil.children.names,"\n")
    
    if(soil.child == "Metadata"){
      parm.path <- parm.path.2.1
      metadata <- NULL
      for(i in names(soil.node[[1]])){
        if(i %in% c("Name","Children","IncludeInDocumentation","Enabled","ReadOnly")) next
        val <- as.character(ifelse(is.null(soil.node[[1]][[i]]),NA,soil.node[[1]][[i]]))
        if(!is.na(val) && nchar(val) > options()$width-30) val <- paste(strtrim(val, options()$width-30),"...")
        metadata <- rbind(metadata, data.frame(parm = i, value = val))
      }
      print(knitr::kable(metadata, longtable = FALSE))
    }else{
      ## Pick which soil component we want to look at
      ## Which is not 'Metadata"
      wsc <- grep(soil.child, soil.children.names)
      if(length(wsc) == 0) stop("soil.child likely not present")
    
      selected.soil.node.child <- soil.node[[1]]$Children[wsc]
    }
    
      ## For some variables now it is the time to print
      ## The code below is not strictly needed but it is here
      ## in case I need a second level of soil in the future
    first.level.soil <- c("Water","Physical",
                          "Chemical","Analysis","InitialWater",
                          "InitialN","SoilWater","Analysis",
                          "CERESSoilTemperature","Organic")
    if(soil.child %in% first.level.soil){
      ## Assuming there is only one 'relevant' level here
      ## This parameter level would be 2.1.1
      parm.path <- paste0(parm.path.2.1,".",selected.soil.node.child[[1]]$Name) 
      enms <- c("IncludeInDocumentation","Enabled","ReadOnly","Children","Name")
      cnms <- setdiff(names(selected.soil.node.child[[1]]),enms)
      soil.d1 <- NULL
      soil.d2 <- NULL
      col.nms <- NULL
      for(ii in cnms){
        tmp <- selected.soil.node.child[[1]][ii][[1]]
        if(length(tmp) == 0) next
        if(length(tmp) == 1){
          soil.d1 <- rbind(soil.d1, 
                           data.frame(parm = ii, value = as.character(tmp)))
        }
        if(length(tmp) > 1){
          col.nms <- c(col.nms, ii)
          vals <- as.vector(unlist(tmp))
          soil.d2 <- cbind(soil.d2, vals)
        }
      }
      ## Print first set of soil parameters
      if(!is.null(soil.d1)) print(kable(soil.d1, digits = digits))
      ## Print second set of soil parameters
      if(!is.null(soil.d2)){ 
        soil.d2 <- as.data.frame(soil.d2)
        names(soil.d2) <- col.nms
        print(kable(soil.d2, digits = digits))
      }
    }
  }
  
  if(node == "SurfaceOrganicMatter"){
    ## Which is 'SurfaceOrganicMatter'
    wsomn <- grepl("Models.Surface.SurfaceOrganicMatter", core.zone.node)
    som.node <- core.zone.node[wsomn][[1]]
    
    parm.path <- paste0(parm.path.2,".",som.node$Name)
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
    
    parm.path <- paste0(parm.path.2,".",microclimate.node$Name)

    microclimate.d <- data.frame(parm = names(microclimate.node)[2:9],
                                 value = as.vector(unlist(microclimate.node)[2:9]))
    print(kable(microclimate.d, digits = digits))
  }
  
  if(node == "Crop"){
    ## Which is 'Crop'
    wmmn <- grepl("Models.Manager", core.zone.node)
    manager.node <- core.zone.node[wmmn]
    
    #cat("Manager Name:",names(manager.node[[1]]),"\n")
    ## Which element has the crop information?
    wcn <- grepl("CultivarName", manager.node)
    crop.node <- manager.node[wcn][[1]]$Parameters
    
    ## This would be 2.1.1
    parm.path <- paste0(parm.path.2,".",manager.node[wcn][[1]]$Name)
      
    mat <- matrix(NA, nrow = length(crop.node), ncol = 2,
                  dimnames = list(NULL,c("parm","value")))
    j <- 1
    for(i in 1:length(crop.node)){
      mat[j,1] <- crop.node[[i]]$Key
      mat[j,2] <- crop.node[[i]]$Value
      j <- j + 1
    }
    
    print(knitr::kable(as.data.frame(mat), digits = digits))
  }
  
  if(node == "Manager"){
    wmmn <- grepl("Models.Manager", core.zone.node)
    manager.node <- core.zone.node[wmmn]
    parm.path <- parm.path.2
    ## Print available Manager components
    manager.node.names <- sapply(manager.node, FUN = function(x) x$Name)
    cat("Management Scripts: ", manager.node.names,"\n\n")
    
    if(!is.null(parm)){
      parm1 <- parm[[1]]
      position <- parm[[2]]
      find.manager <- grep(parm1, manager.node.names, ignore.case = TRUE)
      selected.manager.node <- manager.node.names[find.manager]
      parm.path <- paste0(parm.path.2,".",selected.manager.node)
      
      if(is.na(position)){
        ms.params <- manager.node[[find.manager]]$Parameters
        if(length(ms.params) == 0) warning("parameter not found")
        
        mat <- matrix(NA, ncol=2, nrow = length(ms.params),
                      dimnames = list(NULL,c("parm","value")))
        
        if(length(ms.params) > 0){
          for(j in 1:length(ms.params)){
            mat[j,1] <- ms.params[[j]]$Key
            mat[j,2] <- ms.params[[j]]$Value
          }
        }
        cat("Name: ", selected.manager.node,"\n")
        print(knitr::kable(as.data.frame(mat), digits = digits))
        cat("\n") 
      }
      
      if(!is.na(position)){
        ms.params <- manager.node[[find.manager]]$Parameters
        if(length(ms.params) == 0) warning("no parameters found")
        mat <- matrix(NA, ncol=2, nrow = length(position),
                      dimnames = list(NULL,c("parm","value")))
        
        k <- 1
        for(j in 1:length(ms.params)){
          if(j == position){
            mat[k,1] <- ms.params[[j]]$Key
            mat[k,2] <- ms.params[[j]]$Value
            k <- k + 1
          }
        }
        cat("Name: ", selected.manager.node,"\n")
        parm2 <- ms.params[[position]]$Key
        cat("Key:", ms.params[[position]]$Key, "\n")
        print(kable(as.data.frame(mat), digits = digits))
        cat("\n")
      }
    }
  }
  
  if(print.path){
    if(!missing(parm)){
      if(length(parm) == 1){
        parm.path <- paste0(parm.path,".",parm)
      }else{
        if(!is.na(position)){
          parm.path <- paste0(parm.path,".",parm2)
        }
      }
    }
    cat("Parm path:", parm.path,"\n")
  }
  invisible(parm.path)
}





