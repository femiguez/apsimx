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
#' \donttest{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Clock") 
#' inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Weather")
#' inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", soil.child = "Metadata") 
#' inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", soil.child = "Physical") 
#' inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", soil.child = "SoilWater") 
#' inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", soil.child = "Organic")
#' inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", soil.child = "Chemical")
#' inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", soil.child = "InitialWater")
#' inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", soil.child = "InitialN")
#' inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "SurfaceOrganicMatter")
#' inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "MicroClimate")
#' inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Crop")
#' inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Manager")
#' inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Report")
#' 
#' ## Manager folder present
#' extd.dir <- system.file("extdata", package = "apsimx")
#' inspect_apsimx("maize-manager-folder.apsimx", node = "Other", src.dir = extd.dir,
#'                parm = list("Manager", "Fertiliser", "Amount"))
#'                
#' }
#'

inspect_apsimx <- function(file = "", src.dir = ".", 
                           node = c("Clock", "Weather", "Soil", "SurfaceOrganicMatter", "MicroClimate", "Crop", "Manager","Report", "Other"),
                           soil.child = c("Metadata", "Water", "InitialWater",
                                          "Chemical", "Physical", "Analysis", "SoilWater",
                                          "InitialN", "CERESSoilTemperature", "Sample",
                                          "Nutrient", "Organic"),
                           parm = NULL,
                           digits = 3,
                           print.path = FALSE,
                           root){
  
  .check_apsim_name(file)
  
  file.names <- dir(path = src.dir, pattern=".apsimx$", ignore.case=TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsimx files in the specified directory to inspect.")
  }
  
  node <- match.arg(node)
  soil.child <- match.arg(soil.child)
  
  if(soil.child %in% c("Nutrient")) stop("Not implemented yet")
  
  ## This matches the specified file from a list of files
  ## Notice that the .apsimx extension will be added here
  file <- match.arg(file, file.names)
  
  apsimx_json <- jsonlite::read_json(file.path(src.dir, file))
  
  parm.path.0 <- paste0(".", apsimx_json$Name) ## Root
  ## I think that everything I might want to look at 
  ## is under this Children/Children node
  ## It looks like I need to 'find' the "Models.Core.Simulation" node
  fcsn <- grep("Models.Core.Simulation", apsimx_json$Children, fixed = TRUE)
  
  if(length(fcsn) > 1 || !missing(root)){
    if(missing(root)){
      cat("Simulation structure: \n")
      str_list(apsimx_json)
      stop("more than one simulation found and no root node label has been specified \n select one of the children names above")   
    }else{
      if(length(root) == 1){
        nms <- vapply(apsimx_json$Children, FUN = function(x) x$Name, 
                      FUN.VALUE = "character")
        fcsn <- grep(as.character(root), nms)
        parm.path.1 <- paste0(parm.path.0, ".", apsimx_json$Children[[fcsn]]$Name)
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
    parm.path.1 <- paste0(parm.path.0, ".", apsimx_json$Children[[fcsn]]$Name)
  }
  
  if(node == "Clock"){
    wlc <- function(x) grepl("Clock", x$Name, ignore.case = TRUE)
    wlcl <- sapply(parent.node, FUN = wlc)
    if(all(wlcl == FALSE)){
      stop("Clock not found")
    } 
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
    if(all(wlwl == FALSE)){
      stop("Weather not found")
    }
    weather.node <- parent.node[wlwl]
    ## Select the string which has a met file
    gf1 <- function(x) grep(".met$", x, value = TRUE)
    cat("Met file:", as.character(sapply(weather.node, gf1)), "\n")
    parm.path <- paste0(parm.path.1, ".", parent.node[wlwl][[1]]$Name)
  }
  
  ## From here on there is an important component that lives inside
  ## 'Models.Core.Zone'
  wcz <- grepl("Models.Core.Zone", parent.node)
  core.zone.node <- parent.node[wcz][[1]]$Children
  
  parm.path.2 <- paste0(parm.path.1, ".", parent.node[wcz][[1]]$Name)
  
  if(node == "Soil"){
    ## Which soils node
    wsn <- grepl("Models.Soils.Soil", core.zone.node)
    if(all(wsn == FALSE)){
      stop("Soil not found")
    }
    soil.node <- core.zone.node[wsn]
    
    parm.path.2.1 <- paste0(parm.path.2, ".", soil.node[[1]]$Name)
    
    ## Print some basic soil information
    cat("Soil Type: ", soil.node[[1]]$SoilType, "\n")
    cat("Latitude: ", soil.node[[1]]$Latitude, "\n")
    cat("Longitude: ", soil.node[[1]]$Longitude, "\n")
    
    if(length(soil.node) != 1) stop("soil.node not equal to one")
    
    soil.children.names <- sapply(soil.node[[1]]$Children, function(x) x$Name)
    
    cat("Soil children:", soil.children.names, "\n")
    
    if(soil.child == "Metadata"){
      parm.path <- parm.path.2.1
      metadata <- NULL
      for(i in names(soil.node[[1]])){
        if(i %in% c("Name","Children","IncludeInDocumentation","Enabled","ReadOnly")) next
        val <- as.character(ifelse(is.null(soil.node[[1]][[i]]),NA,soil.node[[1]][[i]]))
        if(!is.na(val) && nchar(val) > options()$width-30) val <- paste(strtrim(val, options()$width-30),"...")
        metadata <- rbind(metadata, data.frame(parm = i, value = val))
      }
      
      if(missing(parm)){
        print(knitr::kable(metadata, longtable = FALSE))  
      }else{
        if(!(parm %in% metadata[["parm"]])) stop("parm does not match a parameter in metadata")
        print(knitr::kable(metadata[metadata$parm == parm,]))  
      }
      
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
    first.level.soil <- c("Water", "Physical",
                          "Chemical", "Analysis", "InitialWater",
                          "InitialN", "SoilWater", "Analysis",
                          "CERESSoilTemperature", "Organic")
    if(soil.child %in% first.level.soil){
      ## Assuming there is only one 'relevant' level here
      ## This parameter level would be 2.1.1
      parm.path <- paste0(parm.path.2.1,".",selected.soil.node.child[[1]]$Name) 
      enms <- c("IncludeInDocumentation", "Enabled", "ReadOnly", "Children", "Name")
      cnms <- setdiff(names(selected.soil.node.child[[1]]), enms)
      
      if(soil.child == "Physical" || soil.child == "Water")
        cnms <- c(cnms, "Crop LL", "Crop KL", "Crop XF")
        
      soil.d1 <- NULL
      soil.d2 <- NULL
      soil.d3 <- NULL
      col.nms <- NULL
      d3.col.nms <- NULL
      
      for(ii in cnms){
        
        tmp <- selected.soil.node.child[[1]][ii][[1]]
        
        if(ii %in% c("Crop LL", "Crop KL", "Crop XF")){
          for(j in seq_along(selected.soil.node.child[[1]]$Children)){
            crop.name <- gsub("Soil", "", selected.soil.node.child[[1]]$Children[[j]]$Name) 
            if(ii == "Crop LL") tmp <- selected.soil.node.child[[1]]$Children[[j]]$LL
            if(ii == "Crop KL") tmp <- selected.soil.node.child[[1]]$Children[[j]]$KL
            if(ii == "Crop XF") tmp <- selected.soil.node.child[[1]]$Children[[j]]$XF
            d3.col.nms <- c(d3.col.nms, gsub("Crop", crop.name, ii))
            vals <- as.vector(unlist(tmp))
            soil.d3 <- cbind(soil.d3, vals) 
          }
        }        
        
        if(length(tmp) == 0) next
        if(length(tmp) == 1){
          soil.d1 <- rbind(soil.d1, 
                           data.frame(parm = ii, value = as.character(tmp)))
        }
        if(length(tmp) > 1){
          if(!ii %in% c("Crop LL", "Crop KL", "Crop XF")){
            col.nms <- c(col.nms, ii)
            vals <- as.vector(unlist(tmp))
            soil.d2 <- cbind(soil.d2, vals)                        
          }
        }
      }
      
      if(missing(parm)){
        ## Print first set of soil parameters
        if(!is.null(soil.d1)) print(knitr::kable(soil.d1, digits = digits))  
        ## Print second set of soil parameters
        if(!is.null(soil.d2)){ 
          soil.d2 <- as.data.frame(soil.d2)
          names(soil.d2) <- col.nms
          print(knitr::kable(soil.d2, digits = digits))
        }
        ## Print third set of crop-soil parameters
        if(!is.null(soil.d3)){ 
          soil.d3 <- as.data.frame(soil.d3)
          names(soil.d3) <- d3.col.nms
          soil.d3 <- subset(soil.d3, select = sort(names(soil.d3)))
          print(knitr::kable(soil.d3, digits = digits))
        }
      }else{
        ## Print first set of soil parameters
        if(!is.null(soil.d1)) print(knitr::kable(soil.d1[soil.d1$parm == parm,], digits = digits))  
        ## Print second set of soil parameters
        if(!is.null(soil.d2)){ 
          soil.d2 <- as.data.frame(soil.d2)
          names(soil.d2) <- col.nms
          print(knitr::kable(soil.d2[soil.d2$parm == parm,], digits = digits))
        }
        ## Print third set of crop-soil parameters
        if(!is.null(soil.d3)){ 
          soil.d3 <- as.data.frame(soil.d3)
          names(soil.d3) <- d3.col.nms
          soil.d3 <- subset(soil.d3, select = sort(names(soil.d3)))
          print(knitr::kable(soil.d3[soil.d3$parm == parm,], digits = digits))
        }
      }
    }
  }
  
  if(node == "SurfaceOrganicMatter"){
    ## Which is 'SurfaceOrganicMatter'
    wsomn <- grepl("Models.Surface.SurfaceOrganicMatter", core.zone.node)
    if(all(wsomn == FALSE)){
      stop("SurfaceOrganicMatter not found")
    }
    som.node <- core.zone.node[wsomn][[1]]
    
    parm.path <- paste0(parm.path.2,".",som.node$Name)
    ## The relevant components might be unpredictable
    ## Will need to find a better method in the future
    som.d <- data.frame(parm = names(som.node)[2:8],
                        value = as.vector(unlist(som.node)[2:8]))
    
    print(knitr::kable(som.d, digits = digits))
  }
  
  if(node == "MicroClimate"){
    ## Which is 'MicroClimate'
    wmcn <- grepl("Models.MicroClimate", core.zone.node)
    if(all(wmcn == FALSE)){
      stop("MicroClimate not found")
    }
    microclimate.node <- core.zone.node[wmcn][[1]]
    
    parm.path <- paste0(parm.path.2,".",microclimate.node$Name)

    microclimate.d <- data.frame(parm = names(microclimate.node)[2:9],
                                 value = as.vector(unlist(microclimate.node)[2:9]))
    print(knitr::kable(microclimate.d, digits = digits))
  }
  
  if(node == "Crop"){
    ## Which is 'Crop'
    wmmn <- grepl("Models.Manager", core.zone.node)
    if(all(wmmn == FALSE)){
      stop("Crop not found")
    }
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
    if(all(wmmn == FALSE)){
      stop("Manager not found")
    }
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
      parm.path <- paste0(parm.path.2, ".", selected.manager.node)
      
      if(is.na(position)){
        ms.params <- manager.node[[find.manager]]$Parameters
        if(length(ms.params) == 0) warning("parameter not found")
        
        mat <- matrix(NA, ncol=2, nrow = length(ms.params),
                      dimnames = list(NULL, c("parm", "value")))
        
        if(length(ms.params) > 0){
          for(j in 1:length(ms.params)){
            mat[j,1] <- ms.params[[j]]$Key
            mat[j,2] <- ms.params[[j]]$Value
          }
        }
        cat("Name: ", selected.manager.node, "\n")
        print(knitr::kable(as.data.frame(mat), digits = digits))
        cat("\n") 
      }
      
      if(!is.na(position)){
        ms.params <- manager.node[[find.manager]]$Parameters
        if(length(ms.params) == 0) warning("no parameters found")
        mat <- matrix(NA, ncol=2, nrow = length(position),
                      dimnames = list(NULL, c("parm", "value")))
        
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
        print(knitr::kable(as.data.frame(mat), digits = digits))
        cat("\n")
      }
    }
  }
  
  if(node == "Report"){
    wrn <- grepl("Models.Report", core.zone.node)
    if(all(wrn == FALSE)){
      stop("Report not found")
    }
    report.node <- core.zone.node[wrn]
    parm.path <- parm.path.2
    ## Print available Manager components
    report.node.names <- sapply(report.node, FUN = function(x) x$Name)

    tmp <- vector("list", length = length(report.node.names))
    for(i in 1:length(report.node)){
      ## Variable Names
      vn <- as.data.frame(unlist(report.node[[i]]$VariableNames))
      names(vn) <- "VariableNames"
      en <- as.data.frame(unlist(report.node[[i]]$EventNames))
      names(en) <- "EventNames"
      tmp[[i]] <- list(vn = vn, en = en)
    }
    
    if(missing(parm)){
      for(i in 1:length(tmp)){
        cat("Report name:", report.node[[i]]$Name, "\n")
        print(knitr::kable(tmp[[i]]$vn))
        print(knitr::kable(tmp[[i]]$en))   
        cat("\n")
      }
      parm.path <- paste0(parm.path.2,".", report.node.names)
    }else{
        if(!is.list(parm)){
          if(length(report.node.names) > 1)
            stop("More than one Report is present. Use a list to choose one.")
          if(!grepl(parm, "VariableNames") && !grepl(parm, "EventNames"))
            stop("parm should contain either VariableNames or EventNames")
          if(parm == "VariableNames") print(knitr::kable(tmp[[i]]$vn))
          if(parm == "EventNames") print(knitr::kable(tmp[[i]]$en))
          parm.path <- paste0(parm.path.2,".", report.node.names)
       }else{
          if(!any(grepl(parm[[1]], report.node.names)))
            stop("the first element of parm should match a report name")
          wr2p <- grep(parm[[1]], report.node.names)
          if(is.na(parm[[2]])){
            print(knitr::kable(tmp[[wr2p]]$vn))
            print(knitr::kable(tmp[[wr2p]]$en))
            parm.path <- paste0(parm.path.2,".", report.node.names[wr2p])
            position <- NA
          }else{
            if(grepl(parm[[2]],"VariableNames")) print(knitr::kable(tmp[[wr2p]]$vn))
            if(grepl(parm[[2]], "EventNames")) print(knitr::kable(tmp[[wr2p]]$en)) 
            parm.path <- paste0(parm.path.2,".", report.node.names[wr2p])
            parm1 <- grep(parm[[1]], report.node.names, value = TRUE)
            parm2 <- grep(parm[[2]], c("VariableNames", "EventNames"), value = TRUE)
            position <- wr2p
          }
       }
    }
  }
  
  if(node == "Other"){
    
    tmp <- core.zone.node
    parm.path.2.1 <- parm.path.2
    ## Check for parm
    if(is.null(parm)) stop("'parm' should be provided when node = 'Other'")
    if(length(parm) == 1L) stop("'parm' should be a list of length 2 or more")
    ## This extracts a node
    for(i in 1:(length(parm) - 1)){
      nms <- sapply(tmp, function(x) x$Name)
      wcp <- grep(parm[[i]], nms)
      if(length(wcp) == 0){
        cat("Names: ", nms, "\n")
        cat("parm[[i]]", parm[[i]], "\n")
        stop("Parameter not found")
      }
      tmp <- tmp[[wcp]]
      if(!is.null(tmp$Children)) tmp <- tmp$Children
      ## Build the parm.path
      parm.path.2.1 <- paste0(parm.path.2.1, ".", nms[wcp])
    }
    
    if(!is.null(tmp$Parameters)){
      wp <- grep(parm[[length(parm)]], tmp$Parameters)
      tmp2 <- tmp$Parameters[[wp]]
      ## Process parameter path
      parm.path <- paste0(parm.path.2.1, ".", tmp2$Key)
      print(knitr::kable(as.data.frame(tmp2)))
    }else{
      parm.path <- parm.path.2.1
      unpack_node(tmp)
    }
  }
  
  if(print.path && node != "Other"){
    if(!missing(parm)){
      if(length(parm) == 1){
        parm.path <- paste0(parm.path, ".", parm)
      }else{
        if(!is.na(position)){
          parm.path <- paste0(parm.path,".",parm2)
        }
      }
    }
    cat("Parm path:", parm.path,"\n")
  }else{
    if(print.path) cat("Parm path:", parm.path,"\n")
  }
  
  invisible(parm.path)
}

#' 
#'  This function is a work in progress. There are many instances for which it will not work.
#'  
#'  It will probably only find the first instance that matches.
#'  
#'  A future feature will be to search for a jspath instead of simply a regular expression
#'  
#' @title Inspect an .apsimx or .json (JSON) file
#' @name inspect_apsimx_json
#' @description inspect an .apsimx or .json (JSON) file. It does not replace the GUI, but it can save time by quickly checking parameters and values.
#' @param file file ending in .apsimx or .json to be inspected (JSON)
#' @param src.dir directory containing the .apsimx or .json file to be inspected; defaults to the current working directory
#' @param parm string or regular expression for partial matching.
#' @param search.depth default is 15. How deep should the algorithm explore the structure of the list.
#' @param print.path whether to print the parameter path (default is FALSE)
#' @param verbose whether to print additional information (mostly used for debugging)
#' @return prints a table with inspected parameters and values (and the path when \sQuote{print.path} = TRUE).
#' @export
#' @examples 
#' \donttest{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' ## It seems to work for simple search
#' inspect_apsimx_json("Wheat.apsimx", src.dir = extd.dir, parm = "Version")
#' inspect_apsimx_json("Wheat.apsimx", src.dir = extd.dir, parm = "Simulations")
#' inspect_apsimx_json("Wheat.apsimx", src.dir = extd.dir, parm = "Clock")
#' inspect_apsimx_json("Wheat.apsimx", src.dir = extd.dir, parm = "Weather")
#' ## Does return soil components
#' inspect_apsimx_json("Wheat.apsimx", src.dir = extd.dir, parm = "DUL")
#' ## Or cultivar
#' inspect_apsimx_json("Wheat.apsimx", src.dir = extd.dir, parm = "Hartog")
#' 
#' }

inspect_apsimx_json <- function(file = "", src.dir = ".", parm,
                                search.depth = 15,
                                print.path = FALSE,
                                verbose = FALSE){
  
  .check_apsim_name(file)
  .check_apsim_name(src.dir)
  
  if(missing(parm))
    stop("You need to specify the parm argument")
  
  file.names.apsimx <- dir(path = src.dir, pattern = ".apsimx$", ignore.case = TRUE)
  file.names.json <- dir(path = src.dir, pattern = ".json$", ignore.case = TRUE)
  
  if(length(file.names.apsimx) == 0 && length(file.names.json) == 0){
    stop("There are no .json or .apsimx files in the specified directory to inspect.")
  }
  
  apsimx_json <- jsonlite::read_json(file.path(src.dir, file)) ### This is a list
  
  jsonpath <- "$"
  
  x <- apsimx_json
  
  ## This handles parameters at the first level
  if(any(grepl(parm, names(x)))){
    wpi <- grep(parm, names(x))
    jsonpath <- paste0(jsonpath, ".", names(x)[wpi])
    print(knitr::kable(as.data.frame(x[wpi])))
    if(print.path) print(jsonpath)
    if(verbose) cat("Level: 0 \n")
    return(invisible(jsonpath))
  }

  jsonpath <- paste0(jsonpath, ".", x$Name)
  
  for(i in 1:search.depth){
    
    ## First try to see if parameter is in names
    nms <- names(x)
    
    if(!is.null(nms) && any(grepl(parm, nms))){
      wpi <- grep(parm, nms)
      jsonpath <- paste0(jsonpath, ".", names(x)[wpi])
      xd0 <- x[wpi]
      xd1 <- jsonlist2dataframe(xd0)
      
      print(knitr::kable(xd1))
      if(print.path) print(jsonpath)
      if(verbose) cat("Level: 1 \n")
      return(invisible(jsonpath))
    }

    ## It is possible for parm to be in Name
    if(!is.atomic(x)) nm <- x$Name
      
    if(!is.null(nm)){
      if(grepl(parm, nm)){
        wpn <- grep(parm, nm)  
        jsonpath <- paste0(jsonpath, ".", nm)
        xd0 <- x[wpn]
        xd1 <- jsonlist2dataframe(xd0)
        names(xd1) <- nm
        print(knitr::kable(xd1))
        if(print.path) print(jsonpath)
        return(invisible(jsonpath))
      }        
    }

    if(any(grepl(parm, x))){
      
      wgpx <- grep(parm, x)
      
      if(length(wgpx) > 1){
        cat("Parameter found in more than one postion\n")
        for(i in wgpx){
          cat("Position:", i, "\n")
          print(names(x)[i])
        }
        stop("Parameter is not unique", call. = FALSE)
      }
        
      
      nms <- try(sapply(x[[wgpx]], function(x) x$Name), silent = TRUE)
      
      if(verbose){
        print(i)
        print(wgpx)
        print(jsonpath)
        print(nms)
      }
      
      if(inherits(nms, "try-error")) nms <- NULL
      
      if(any(sapply(nms, is.null))) nms <- NULL
      
      if(!is.null(nms)){
        if(any(grepl(parm, nms))){
          wpn <- grep(parm, nms)  
          jsonpath <- paste0(jsonpath, ".", nms[wpn])
          xd0 <- x[[wgpx]][wpn]
          xd1 <- jsonlist2dataframe(xd0)
          print(knitr::kable(xd1))
          if(print.path) print(jsonpath)
          return(invisible(jsonpath))
        }        
      }
      
      wgpx2 <- grep(parm, x[[wgpx]])
      
      if(length(wgpx2) > 1){
        cat("Parameter found in more than one postion\n")
        for(i in wgpx2){
          cat("Position:", i, "\n")
          print(names(x)[i])
        }
        stop("Parameter is not unique", call. = FALSE)
      }
      
      if(!is.null(nms)){
        
        if(!is.null(nms[wgpx2]))
          jsonpath <- paste0(jsonpath, ".", nms[wgpx2])

        if(wgpx2 <= length(x[[wgpx]])){
          x <- x[[wgpx]][[wgpx2]]
        }else{
          if(verbose){
            cat("x length:", length(x), "\n")
            cat("wgpx2", wgpx2, "\n")
            cat("length x[[wgpx]]", length(x[[wgpx]]), "\n")
          }
        }
        
      }else{

        gjl <- grep_json_list(parm, x)
        gjlm <- strsplit(gjl$positions, ".", fixed = TRUE)[[1]]
        gjlms <- as.numeric(gjlm[2:(length(gjlm) - 1)])

        for(i in gjlms) x <- x[[i]]
        
        ## Need to bring back the code I wrote before to extract the right stuff
        if(!is.null(x$Key))
          jsonpath <- paste0(jsonpath, ".", x$Key)
          
        print(knitr::kable(as.data.frame(x)))
        if(print.path) print(jsonpath)
        return(invisible(jsonpath))
      }
    }
  }
  
  if(i == search.depth) stop("Parameter not found")

  invisible(jsonpath)
}

## Convert a json list to a data.frame
## This needs to handle a variety of cases
## 1. Simple approach (unlist and data.frame return no error)
##    In this case all the list elements have equal length
## 2. 
jsonlist2dataframe <- function(x){
  
  nms0 <- names(x)
  if(is.list(x) && length(x) == 1) x <- x[[1]]
  
  if(length(unique(sapply(x, length))) == 1){
    dt0 <- try(as.data.frame(unlist(x)), silent = TRUE)
    if(!inherits(dt0, "try-error")){
      if(!is.null(nms0)) names(dt0) <- nms0
      return(dt0)    
    }else{
      print(str_list(x))
      stop("Can't convert list to data.frame")
    } 
  }

  ## The problem is that some elements might be null
  winn <- !sapply(x, function(x) is.null(x)) ## which is not null
  x0 <- x[winn]
  tmp <- NULL
  for(i in seq_along(x0)){
    nms.x0 <- names(x0)[i]
    if(!names(x0)[i] %in% c("Children", "Code", "Parameters")){
      tmp <- rbind(tmp, as.data.frame(unlist(x0[i])))
    }
    
    if(names(x0)[i] == "Children"){
      lchildren <- length(x0$Children)
      tmp <- rbind(tmp, paste0("List of length: ", lchildren))
      rnms <- row.names(tmp)
      rnms[i] <- "Children"
      row.names(tmp) <- rnms
    }
    
    if(names(x0)[i] == "Code"){
      tmp <- rbind(tmp, paste0("C# code..."))
      rnms <- row.names(tmp)
      rnms[i] <- "C# code"
      row.names(tmp) <- rnms
    }
    
    if(names(x0)[i] == "Parameters"){
      tmp <- rbind(tmp, paste0("Parameters..."))
      rnms <- row.names(tmp)
      rnms[i] <- "Parameters"
      row.names(tmp) <- rnms
    }
  }
  
  if(!is.null(x0$Name)){
    names(tmp) <- x$Name
  }else{
    if(!is.null(names(x)[winn])) names(tmp) <- names(x)[winn]
  } 
  tmp
}


## The idea of this function is that it will return the position where the 
## pattern is found and also the node
## The trick is that the previous function does not quite give me what I want
#' @title grep but for json list
#' @name grep_json_list
#' @description recursive grep adapted for a json list
#' @param pattern as in grep
#' @param x object (a list)
#' @param ignore.case as in grep
#' @param search.depth search depth for the list (to prevent endless search)
#' @return It returns a list with the found object, the json path and the positions in the list.
#' @export
grep_json_list <- function(pattern, x, ignore.case = FALSE, search.depth = 10){
  
  ## Check first
  rar <- rapply(x, function(x) grep(pattern, x, ignore.case = ignore.case))
  
  if(length(rar) == 0L)
    stop("pattern not found")
  
  positions <- ""
  jsonpath <- ""
  
  for(i in 1:search.depth){
    
    if(is.list(x) && length(x) == 1) x <- x[[1]]
    wgnp <- grep(pattern, x, ignore.case = ignore.case)
    
    if(length(wgnp) < 0.5) break
    
    if(!is.atomic(x) && !is.null(x$Name))
      jsonpath <- paste0(jsonpath, ".", x$Name)
    
    if(!is.atomic(x) && length(wgnp) > 0)
      positions <- paste0(positions, ".", wgnp)
    
    if(length(wgnp) == 1){
      x <- x[[wgnp]]  
    }else{
      xn <- list()
      for(i in seq_along(wgnp)){
        xn[[i]] <- x[[i]]
      }
      x <- xn
    }
  }
  
  return(list(x = x, jsonpath = jsonpath, positions = positions))
}

## This version of grep is not exposed at the moment
grep_json_list1 <- function(pattern, x, ...){
  
  tmp <- rapply(x, function(x,...) grep(pattern = pattern, x = x, value = TRUE, ...), ...)
  
  ans <- vector("list", length = length(tmp))
  
  for(i in seq_along(tmp)){
    ans[[i]] <- tmp[[i]]
  }
  
  return(ans)
}
