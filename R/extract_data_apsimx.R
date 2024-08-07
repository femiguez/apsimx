#'
#' This function does not print anything (compared to inspect_apsimx). The purpose is to return data
#' contained in the APSIM simulation as a data.frame. It will return a \sQuote{list} when a data frame
#' does not naturally accommodate the result. For example, the complete manager node does not naturally
#' fit into a data frame structure. In some cases, multiple data frames are returned as part of lists.
#'
#' @title Extract data from an .apsimx (JSON) file
#' @name extract_data_apsimx
#' @description Extract data from a JSON apsimx file. 
#' @param file file ending in .apsimx to be inspected (JSON)
#' @param src.dir directory containing the .apsimx file to be inspected; defaults to the current working directory
#' @param node specific node to be used either \sQuote{Clock}, \sQuote{Weather}, 
#' \sQuote{Soil}, \sQuote{SurfaceOrganicMatter}, \sQuote{MicroClimate}, \sQuote{Crop},
#'  \sQuote{Manager}, \sQuote{Operations} or \sQuote{Other}
#' @param soil.child specific soil component to be inspected. The options vary depending on what is available (see \link{inspect_apsimx})
#' @param parm parameter to refine the extraction of the \sQuote{manager} list(\sQuote{parm},\sQuote{position}), use \sQuote{NA} for all the positions. \sQuote{parm} can be a regular expression for partial matching.
#' @param digits number of decimals to print (default 3). Not used now because everything is a character.
#' @param root root node label. In simulation structures such as factorials there will be multiple possible nodes. This can be specified by supplying an appropriate character.
#' @details Have not written this section yet
#' @return a \link{data.frame} or a \link{list}. It does not return a path.
#' @export
#' @examples 
#' \donttest{
#' extd.dir <- system.file("extdata", package = "apsimx")
#' (edf <- extract_data_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Clock")) 
#' (edf <- extract_data_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Weather"))
#' (edf <- extract_data_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", 
#' soil.child = "Metadata")) 
#' (edf <- extract_data_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", 
#' soil.child = "Physical")) 
#' (edf <- extract_data_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", 
#' soil.child = "SoilWater")) 
#' (edf <- extract_data_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", 
#' soil.child = "Organic"))
#' (edf <- extract_data_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", 
#' soil.child = "Chemical"))
#' (edf <- extract_data_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", 
#' soil.child = "InitialWater"))
#' (edf <- extract_data_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", 
#' soil.child = "InitialN"))
#' (edf <- extract_data_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "SurfaceOrganicMatter"))
#' (edf <- extract_data_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "MicroClimate"))
#' (edf <- extract_data_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Crop")) 
#' (edf <- extract_data_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Manager")) 
#' (edf <- extract_data_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Report")) 
#' }
#'

extract_data_apsimx <- function(file = "", src.dir = ".", 
                                node = c("Clock", "Weather", "Soil", "SurfaceOrganicMatter", "MicroClimate", "Crop", "Manager","Report", "Operations", "Other"),
                                soil.child = c("Metadata", "Water", "InitialWater",
                                          "Chemical", "Physical", "Analysis", "SoilWater",
                                          "InitialN", "CERESSoilTemperature", "Sample",
                                          "Solute", "NO3", "NH4", "Urea",
                                          "Nutrient", "Organic", "Swim3"),
                                parm = NULL,
                                digits = 3,
                                root = NULL){
  #### Beginning of function ----
  if(isFALSE(apsimx::apsimx.options$allow.path.spaces)){
    .check_apsim_name(file)
    .check_apsim_name(normalizePath(src.dir))
  }
  
  file.names <- dir(path = src.dir, pattern=".apsimx$", ignore.case=TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsimx files in the specified directory to inspect.")
  }
  
  node <- match.arg(node)
  soil.child <- match.arg(soil.child)
  
  if(soil.child %in% c("Nutrient")) stop("Not implemented yet", call. = FALSE)
  
  ## This matches the specified file from a list of files
  ## Notice that the .apsimx extension will be added here
  file <- match.arg(file, file.names)
  
  apsimx_json <- jsonlite::read_json(file.path(src.dir, file))
  
  find.root <- TRUE
  other.parm.flag <- 1
  if(node == "Other" && is.numeric(parm)) parm <- as.integer(parm)
  parm.path.0 <- paste0(".", apsimx_json$Name) ## Root
  ## I think that everything I might want to look at 
  ## is under this Children/Children node
  ## It looks like I need to 'find' the "Models.Core.Simulation" node
  fcsn <- grep("Models.Core.Simulation", apsimx_json$Children, fixed = TRUE)
  
  ret_df <- NULL ## This is the returned data frame
  ret_lst <- NULL
  ## When node == "Other" root should always be missing (not sure)
  if((node == "Other" && length(fcsn) > 1) || (node == "Other" && (is.null(parm) || is.integer(parm)))){
    parm.path <- parm.path.0
    other.parm.flag <- -1
    find.root <- FALSE
    ## Process 'parm'
    if(is.null(parm) || is.integer(parm)){
      if(is.null(parm)) parm <- 1L
      if(length(parm) > 1)
        stop("'parm' should be of length = 1", call. = FALSE)
      if(parm == 0){
        ret_df <- data.frame(Level_0 = gsub(".", "", parm.path.0, fixed = TRUE))
      }
      ## If parm is null there are different levels of display
      if(parm == 1){
        first.column.name <- gsub(".", "", parm.path.0, fixed = TRUE)
        root.names.level.1 <- vapply(apsimx_json$Children, FUN = function(x) x$Name, 
                                     FUN.VALUE = "character")
        pdat <- data.frame(first_level = c(first.column.name, rep(".", length(root.names.level.1) - 1)),
                           second_level = root.names.level.1)
        ret_df <- pdat
      }
      ## If parm == 2
      if(parm == 2){
        first.column.name <- gsub(".", "", parm.path.0, fixed = TRUE)
        root.names.level.1 <- vapply(apsimx_json$Children, FUN = function(x) x$Name, 
                                     FUN.VALUE = "character")
        second.level.names <- NULL
        third.level.names <- NULL
        for(i in seq_along(root.names.level.1)){
          chld.len <- length(apsimx_json$Children[[i]]$Children)
          rep.times <- ifelse(chld.len == 0, 1, chld.len)
          second.level.names <- c(second.level.names, c(root.names.level.1[i], rep(".", times = rep.times - 1)))
          ## Need to generate third level names
          third.level.names.0 <- sapply(apsimx_json$Children[[i]]$Children, FUN = function(x) x$Name)
          if(length(third.level.names.0) == 0) third.level.names.0 <- "."
          third.level.names.1 <- c(third.level.names, third.level.names.0)
          third.level.names <- unlist(third.level.names.1)
        }
        ### Is this table good enough?
        pdat <- data.frame(first_level = c(first.column.name, rep(".", length(second.level.names) - 1)),
                           second_level = second.level.names,
                           third_level = third.level.names)
        ret_df <- pdat
      }
      ## If parm == 3
      if(parm == 3){
        first.column.name <- gsub(".", "", parm.path.0, fixed = TRUE)
        root.names.level.1 <- vapply(apsimx_json$Children, FUN = function(x) x$Name, 
                                     FUN.VALUE = "character")
        second.level.names <- NULL
        third.level.names <- NULL
        fourth.level.names <- NULL
        k <- 1
        for(i in seq_along(root.names.level.1)){
          chld.len <- length(apsimx_json$Children[[i]]$Children)
          rep.times <- ifelse(chld.len == 0, 1, chld.len)
          second.level.names <- c(second.level.names, c(root.names.level.1[i], rep(".", times = rep.times - 1)))
          ## Need to generate third level names
          third.level.names.0 <- sapply(apsimx_json$Children[[i]]$Children, FUN = function(x) x$Name)
          if(length(third.level.names.0) == 0) third.level.names.0 <- "."
          for(j in seq_along(third.level.names.0)){
            if(length(third.level.names.0) == 1){
              if(third.level.names.0 == "."){
                third.level.names <- c(third.level.names, ".")
                fourth.level.names <- c(fourth.level.names, ".")
                next 
              } 
            }
            fourth.level <- apsimx_json$Children[[i]]$Children
            if(length(fourth.level[[j]]) == 0){
              third.level.names <- c(third.level.names, ".")
              fourth.level.names <- c(fourth.level.names, ".")
              next
            } 
            if(length(fourth.level[[j]]$Children) == 0){
              second.level.names <- c(second.level.names, rep(".", times = length(names(fourth.level[[j]])) - 1))
              third.level.names <- c(third.level.names, c(third.level.names.0[j], rep(".", times = length(names(fourth.level[[j]])) - 1)))
              fourth.level.names <- c(fourth.level.names, names(fourth.level[[j]]))
            }else{
              second.level.names <- c(second.level.names, rep(".", times = length(names(fourth.level[[j]])) - 1))
              fourth.level.names <- c(fourth.level.names, names(fourth.level[[j]]))
              third.level.names <- c(third.level.names, third.level.names.0[j], 
                                     rep(".", times = length(names(fourth.level[[j]])) - 1))
            }
          }
        }
        ### Is this table good enough?
        pdat <- data.frame(first_level = c(first.column.name, rep(".", length(fourth.level.names) - 1)),
                           second_level = second.level.names,
                           third_level = third.level.names,
                           fourth_level = fourth.level.names)
        ret_df <- pdat
      }      
      if(parm == 4) stop("Not implemented yet")
    }else{
      ## This gets triggered if there are multiple simulations
      ## and parm is a character. If it is a list it will be 
      ## handled by the code in the section at the bottom
      if(!is.list(parm)){
        if(!is.character(parm))
          stop("parm should be a character string when node = 'Other' and is not a number", call. = FALSE)
        if(!grepl(".", parm, fixed = TRUE))
          stop("'parm' needs to be a json path")
        if(parm == ".")
          stop("'parm' needs to be a proper json path")
        pparm <- strsplit(parm, split = ".", fixed = TRUE)[[1]]
        root.name.level.0 <- gsub(".", "", parm.path.0, fixed = TRUE)
        if(pparm[2] != root.name.level.0)
          stop(paste("First parm element does not match:", 
                     paste(root.name.level.0, collapse = " ")), call. = FALSE)
        root1 <- pparm[3]
        ## Guess if 'root' is contained in the first level of names
        root.names.level.1 <- vapply(apsimx_json$Children, FUN = function(x) x$Name, 
                                     FUN.VALUE = "character")
        wroot1 <- grep(as.character(root1), root.names.level.1)    
        if(length(wroot1) == 0 || all(is.na(wroot1)))
          stop(paste("Second element of parm did not match:", 
                     paste(root.names.level.1, collapse = " ")), call. = FALSE)
        ## Need to test if the fourth element of pparm is a node
        nodes <- c("Clock", "Weather", "Soil", "SurfaceOrganicMatter", "MicroClimate", "Crop", "Manager","Report", "Operations", "Other")
        if(pparm[4] %in% nodes){
          ## This amounts to guessing that root should be of length 1
          root <- list(pparm[3])
        }else{
          ## This amounts to guessing that pparm[4] should be the second element in 
          root.names.level.2 <- vapply(apsimx_json$Children[[wroot1]]$Children, 
                                       FUN = function(x) x$Name, 
                                       FUN.VALUE = "character")
          root2 <- pparm[4]
          wroot2 <- grep(as.character(root2), root.names.level.2)   
          if(length(wroot2) == 0 || all(is.na(wroot2)))
            stop(paste("Third element of parm did not match:", 
                       paste(root.names.level.2, collapse = " ")), call. = FALSE)
          if(pparm[5] %in% nodes){
            root <- list(pparm[3], pparm[4])
          }else{
            root.names.level.3 <- vapply(apsimx_json$Children[[wroot1]]$Children[[wroot2]]$Children, 
                                         FUN = function(x) x$Name, 
                                         FUN.VALUE = "character")
            root3 <- pparm[5]
            wroot3 <- grep(as.character(root3), root.names.level.3)    
            if(length(wroot3) == 0)
              stop(paste("Fourth element of parm did not match:", 
                         paste(root.names.level.3, collapse = " ")), call. = FALSE)
          }
        }
        parm.path <- parm
      }else{
        other.parm.flag <- 1
      }
    }
  }

  if((length(fcsn) > 1 || !is.null(root)) && find.root){
    if(is.null(root)){
      cat("Simulation structure: \n")
      str_list(apsimx_json)
      stop("more than one simulation found and no root node label has been specified \n select one of the children names above", call. = FALSE)   
    }else{
      ## Parse root
      root <- parse_root(root)
      if(length(root) > 3)
        stop("At the moment 3 is the maximum length for root for this function", call. = FALSE)
      if(length(root) == 1){
        nms <- vapply(apsimx_json$Children, FUN = function(x) x$Name, 
                      FUN.VALUE = "character")
        fcsn <- grep(as.character(root), nms)
        if(length(fcsn) == 0 || length(fcsn) > 1){
          cat("Children:", nms, "\n")
          stop("'root' not found. Choose one of the options above", call. = FALSE)
        }
        parm.path.1 <- paste0(parm.path.0, ".", apsimx_json$Children[[fcsn]]$Name)
        parent.node <- apsimx_json$Children[[fcsn]]$Children
      }else{
        if(length(root) == 2){
          root.node.0.names <- sapply(apsimx_json$Children, function(x) x$Name)
          wcore1 <- grep(as.character(root[1]), root.node.0.names)
          if(length(wcore1) == 0 || length(wcore1) > 1){
            cat("Children:", root.node.0.names, "\n")  
            stop("no root node label in position 1 found or root is not unique", call. = FALSE)
          }
          root.node.0 <- apsimx_json$Children[[wcore1]]
          root.node.0.child.names <- sapply(root.node.0$Children, function(x) x$Name)  
          wcore2 <- grep(as.character(root[2]), root.node.0.child.names)
          if(length(wcore2) == 0 || length(wcore2) > 1)
            stop("no root node label in position 2 found or root is not unique")
          parent.node <- apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Children        
          parm.path.1 <- paste0(parm.path.0,".",apsimx_json$Children[[wcore1]]$Children[[wcore2]])
          ## Is this only a problem for factorials?
          if(length(parm.path.1) > 1){
            parm.path.1 <- paste(parm.path.0, apsimx_json$Children[[wcore1]]$Name, apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Name, sep = ".")
          }
        }
        if(length(root) == 3){
          root.node.0.names <- sapply(apsimx_json$Children, function(x) x$Name)
          wcore1 <- grep(as.character(root[1]), root.node.0.names)
          if(length(wcore1) == 0 || length(wcore1) > 1)
            stop("no root node label in position 1 found or root is not unique")
          root.node.0 <- apsimx_json$Children[[wcore1]]
          root.node.0.child.names <- sapply(root.node.0$Children, function(x) x$Name)
          wcore2 <- grep(as.character(root[2]), root.node.0.child.names)
          if(length(wcore2) == 0 || length(wcore2) > 1)
            stop("no root node label in position 2 found or root is not unique")
          root.node.1 <- apsimx_json$Children[[wcore1]]$Children[[wcore2]]
          root.node.1.child.names <- sapply(root.node.1$Children, function(x) x$Name)  
          wcore3 <- grep(as.character(root[3]), root.node.1.child.names)
          if(length(wcore3) == 0 || length(wcore3) > 1)
            stop("no root node label in position 3 found or root is not unique")
          parent.node <- apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Children[[wcore3]]$Children
          parm.path.1 <- paste0(parm.path.0,".",apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Children[[wcore3]])
          ### Is this a problem for factorials only?
          if(length(parm.path.1) > 1){
            parm.path.1 <- paste(parm.path.0, apsimx_json$Children[[wcore1]]$Name, apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Name, 
                                 apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Children[[wcore3]]$Name, sep = ".")
          }
        }
      }
    }
  }else{
    if(find.root){
      parent.node <- apsimx_json$Children[[fcsn]]$Children  
      parm.path.1 <- paste0(parm.path.0, ".", apsimx_json$Children[[fcsn]]$Name)      
    }
  }

  if(node == "Clock"){
    wlc <- function(x) grepl("Models.Clock", x$`$type`, ignore.case = TRUE)
    wlcl <- sapply(parent.node, FUN = wlc)
    if(all(wlcl == FALSE)){
      stop("Clock not found")
    } 
    clock.node <- as.list(parent.node[wlcl])[[1]]
    start.name <- grep("start", names(clock.node), ignore.case = TRUE, value = TRUE)
    end.name <- grep("end", names(clock.node), ignore.case = TRUE, value = TRUE)
    ret_df <- data.frame(Start = clock.node[[start.name]],
                      End = clock.node[[end.name]])
    ## It is possible for the 'start' and 'end' to be called: 'Start' and 'End'
    ## It is also possible for them to be called 'StartDate' and 'EndDate'
    ## I think APSIM-X is in a state of change and eventually this will
    ## stabilize. At the moment 'Maize' and 'Barley' do not agree.
    ## Final name is 'parm.path', but this is 1.2
    ## parm.path <- paste0(parm.path.1,".",parent.node[wlcl][[1]]$Name)
  }
  
  ## The previous creates a list
  if(node == "Weather"){
    ## Extract the list which has a component Name == "Weather"
    wlw <- function(x) grepl("Models.Climate.Weather|Models.Weather", x$`$type`)
    wlwl <- sapply(parent.node, FUN = wlw)
    if(all(wlwl == FALSE)){
      stop("Weather not found")
    }
    weather.node <- parent.node[wlwl]
    ## Select the string which has a met file
    gf1 <- function(x) grep(".met$", x, value = TRUE)
    ret_df <- data.frame(Met_file = as.character(sapply(weather.node, gf1)))
    ## parm.path <- paste0(parm.path.1, ".", parent.node[wlwl][[1]]$Name)
  }
  
  ## From here on there is an important component that lives inside
  ## 'Models.Core.Zone'
  if(find.root){
    wcz <- grepl("Models.Core.Zone", parent.node)
    if(sum(wcz) < 0.5)
      stop("Core Zone Simulation not found", call. = FALSE)
    core.zone.node <- parent.node[wcz][[1]]$Children
    
    parm.path.2 <- paste0(parm.path.1, ".", parent.node[wcz][[1]]$Name)    
  }
  
  if(node == "Soil"){
    #### Soil node ----
    wsn <- grepl("Models.Soils.Soil", core.zone.node)
    if(all(wsn == FALSE)){
      stop("Soil not found")
    }
    soil.node <- core.zone.node[wsn]
    
   parm.path.2.1 <- paste0(parm.path.2, ".", soil.node[[1]]$Name)
    
   ## Print some basic soil information
   ret_df1 <- data.frame(Soil_Type = soil.node[[1]]$SoilType,
                      Latitude = soil.node[[1]]$Latitude,
                      Longitude = soil.node[[1]]$Longitude)
    
   if(length(soil.node) != 1) stop("soil.node not equal to one")
    
   soil.children.names <- sapply(soil.node[[1]]$Children, function(x) x$Name)
    
   ## cat("Soil children:", soil.children.names, "\n")
    
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
        ret_lst <- list(soil = ret_df1, metadata = metadata)
      }else{
        if(!(parm %in% metadata[["parm"]])) stop("parm does not match a parameter in metadata")
        ret_df <- metadata[metadata$parm == parm,]
      }
      
    }else{
      ## Pick which soil component we want to look at
      ## Which is not 'Metadata"
      ## If "InitialWater" is present then use it
      wsiw <- "InitialWater" %in% soil.children.names
      if(soil.child %in% c("Water", "InitialWater")){
        if(soil.child == "InitialWater" && isFALSE(wsiw)){
          wsiw2 <- grep("initial water", soil.children.names, ignore.case = TRUE)
          if(length(wsiw2) > 0){
            soil.child <-  grep("initial water", soil.children.names, ignore.case = TRUE, value = TRUE)
          }else{
            wsiw3 <- grep("^Water", soil.children.names)
            if(length(wsiw3) > 0){
              soil.child <- "Water"
            }else{
              soil.child <- "InitialWater"                          
            }
          }
        }        
      }
 
      if(soil.child != "Solute"){
        wsc <- which(soil.child == soil.children.names)
      }else{
        wsc <- which(soil.children.names %in% c("NO3", "NH4", "Urea"))
      }

      if(soil.child == "Water" && length(wsc) == 2) wsc <- wsc[2]
      
      if(length(wsc) == 0) stop("soil.child likely not present")
      
      selected.soil.node.child <- soil.node[[1]]$Children[wsc]
    }
    ## if(soil.child == "InitialWater") browser()
    ## For some variables now it is the time to print
    ## The code below is not strictly needed but it is here
    ## in case I need a second level of soil in the future
    first.level.soil <- c("Water", "Physical",
                          "Chemical", "Analysis", "InitialWater", "Initial water", "initial water",
                          "InitialN", "SoilWater", "Analysis",
                          "CERESSoilTemperature", "Organic", "Swim3")
    if(soil.child %in% first.level.soil){
      ## Assuming there is only one 'relevant' level here
      ## This parameter level would be 2.1.1
      parm.path <- paste0(parm.path.2.1, ".", selected.soil.node.child[[1]]$Name) 
      enms <- c("IncludeInDocumentation", "Enabled", "ReadOnly", "Children", "Name", "$type")
      cnms <- setdiff(names(selected.soil.node.child[[1]]), enms)
      ## print(names(selected.soil.node.child[[1]]))
      
      if(soil.child == "Physical")
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
        if(!is.null(soil.d1)) ret_df <- soil.d1
        ## Print second set of soil parameters
        if(!is.null(soil.d2)){ 
          soil.d2 <- as.data.frame(soil.d2)
          names(soil.d2) <- col.nms
          if(is.null(soil.d1)){
            ret_df <- soil.d2
          }else{
            ret_lst <- list(first = soil.d1, second = soil.d2)
          } 
        }
        ## Print third set of crop-soil parameters
        if(!is.null(soil.d3)){ 
          soil.d3 <- as.data.frame(soil.d3)
          names(soil.d3) <- d3.col.nms
          soil.d3 <- subset(soil.d3, select = sort(names(soil.d3)))
          if(!is.null(soil.d1) && !is.null(soil.d2)){
           ret_lst <- list(first = soil.d1, soil.layers = soil.d2, crop = soil.d3) 
          }else{
            if(is.null(soil.d1) && !is.null(soil.d2)){
              ret_df <- list(soil.layers = soil.d2, crop = soil.d3) 
            }else{
              ret_df <- soil.d3              
            }
          }
        }
      }else{
        parm.physical.found <- FALSE
        ## Print first set of soil parameters
        if(!is.null(soil.d1)){
          if(parm %in% names(soil.d1)){
            ##print(knitr::kable(soil.d1[soil.d1$parm == parm,], digits = digits))    
            ret_df <- soil.d1[, parm, drop = FALSE]
            parm.physical.found <- TRUE
          }
          if(parm %in% soil.d1$parm){
            ret_df <- soil.d1[soil.d1$parm == parm,]
            parm.physical.found <- TRUE
          }
        } 
        ## Print second set of soil parameters
        if(!is.null(soil.d2)){ 
          soil.d2 <- as.data.frame(soil.d2)
          names(soil.d2) <- col.nms
          ## print(knitr::kable(soil.d2[soil.d2$parm == parm,], digits = digits))
          if(parm %in% names(soil.d2)){
            ret_df <- soil.d2[, parm, drop = FALSE]
            parm.physical.found <- TRUE
          }
          if(parm %in% soil.d2$parm){
            ret_df <- soil.d2[soil.d2$parm == parm,]
            parm.physical.found <- TRUE
          }
        }
        ## Print third set of crop-soil parameters
        if(!is.null(soil.d3)){ 
          soil.d3 <- as.data.frame(soil.d3)
          names(soil.d3) <- d3.col.nms
          soil.d3 <- subset(soil.d3, select = sort(names(soil.d3)))
          ##print(knitr::kable(soil.d3[soil.d3$parm == parm,], digits = digits))
          if(parm %in% names(soil.d3)){
            ret_df <- soil.d3[, parm, drop = FALSE]
            parm.physical.found <- TRUE
          }
          if(parm %in% soil.d3$parm){
            ret_df <- soil.d3[soil.d3$parm == parm,]
            parm.physical.found <- TRUE
          }
        }
        if(!parm.physical.found)
          stop("Soil physical parameter not found", call. = FALSE)
      }
    }
  }
  
  second.level.soil <- c("Solute", "NO3", "NH4", "Urea")
  
  if(soil.child %in% second.level.soil){
    
    soil.d2 <- NULL
    soil.d3 <- NULL
    enms <- c("IncludeInDocumentation", "Enabled", "ReadOnly", "Children", "Name", "$type")
    ssnc <- selected.soil.node.child
    
    if(soil.child == "Solute"){
      
      solutes <- sapply(ssnc, function(x) x$Name)
      
      parm.path <- paste0(parm.path.2.1, ".", "Solute")
      
      if(missing(parm)){
        ## cat("Solutes:", solutes, "\n")
        ## Do I need to do anything here?
      }else{
        ret_lst <- vector('list', length = length(solutes))
        names(ret_lst) <- solutes
        sel.parm <- grep(parm[[1]], solutes)
        if(length(sel.parm) == 0)
          stop("'parm' should be one of: ", paste(solutes, collapse = ", "), call. = FALSE)
        solutes <- parm[[1]]
      }
      
      ret_lst <- vector('list', length = length(solutes))
      names(ret_lst) <- solutes
      
      for(j in seq_along(solutes)){
        ## cat("\nSolute:", solutes[j], "\n")
        ssnc.solute <- ssnc[[j]]
        if(length(unlist(ssnc.solute$Thickness)) != length(unlist(ssnc.solute$InitialValues))){
          cat("Length of Thickness:", length(unlist(ssnc.solute$Thickness)), "\n")
          cat("Length of InitialValues:", length(unlist(ssnc.solute$InitialValues)), "\n")
          stop("Length of 'Thickness' does not match length of 'InitialValues'", call. = FALSE)
        }
        soil.d1 <- data.frame(Thickness = unlist(ssnc.solute$Thickness),
                              InitialValues = unlist(ssnc.solute$InitialValues))
        if(!is.null(soil.d1)){
          if(is.null(parm)){
            ret_lst[[j]][["first"]] <- soil.d1
          }else{
            if(length(parm) == 1){
              if(parm %in% c("Thickness", "InitialValues")){
                ret_lst[[j]][["first"]] <- soil.d1[, parm, drop = FALSE]
              }else{
                ret_lst[[j]][["first"]] <- soil.d1
              }
            }
          }
        } 
        
        cnms <- setdiff(names(ssnc.solute), enms)        
        for(k in cnms){
          if(k %in% c("Thickness", "InitialValues")) next
          tmp <- ssnc.solute[k][[1]]
          if(is.null(tmp)) tmp <- ""
          soil.d2 <- rbind(soil.d2,
                           data.frame(parm = k, value = as.character(tmp)))
        }
        if(missing(parm) || length(parm) == 1){
          if(!is.null(soil.d2)){
            ret_lst[[j]][["second"]] <- soil.d2
          } 
        }else{
          if(is.list(parm) && is.numeric(parm[[2]])){
            soil.d2.s <- soil.d2[parm[[2]], , drop = FALSE]
            if(!is.null(soil.d2)){
              ret_lst[[j]][["second"]] <- soil.d2.s
            }            
          }
        }
        soil.d2 <- NULL
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
    
    ## parm.path <- paste0(parm.path.2,".",som.node$Name)
    ## The relevant components might be unpredictable
    ## Will need to find a better method in the future
    names.som.node0 <- names(som.node)
    som.names.table <- c("InitialResidueName", "InitialResidueType", "InitialResidueMass",
                         "InitialStandingFraction", "InitialCPR", "InitialCNR")
    wsomnn <- names.som.node0 %in% som.names.table
    values.som.node0 <- unlist(som.node)
    wsomvn <- names(values.som.node0) %in% som.names.table
    som.d <- data.frame(parm = names(som.node)[wsomnn],
                        value = as.vector(values.som.node0[wsomvn]))
    
    if(missing(parm)){
      ret_df <- som.d
    }else{
      if(parm %in% som.d$parm){
        ret_df <- som.d[som.d$parm == parm, ]
      }else{
        stop("Surface OM parameter not found", call. = FALSE)
      }
    }
  }
  
  if(node == "MicroClimate"){
    ## Which is 'MicroClimate'
    ## This only works if it is under Field (old versions, before 2023-12-10)
    wmcn <- grepl("Models.MicroClimate", core.zone.node)
    mcincz <- TRUE ## MicroClimate in core.zone
    if(all(wmcn == FALSE)){
      wmcnf <- function(x) grepl("Models.MicroClimate", x$`$type`)
      wmcn <- sapply(parent.node, FUN = wmcnf)
      mcincz <- FALSE
      if(all(wmcn == FALSE))
        stop("MicroClimate not found")
    }
    if(mcincz){
      microclimate.node <- core.zone.node[wmcn][[1]]  
    }else{
      microclimate.node <- parent.node[wmcn][[1]]  
    }
    
    parm.path <- paste0(parm.path.2,".", microclimate.node$Name)
    
    microclimate.d <- data.frame(parm = names(microclimate.node)[2:9],
                                 value = as.vector(unlist(microclimate.node)[2:9]))
    
    if(missing(parm)){
      ret_df <- microclimate.d
    }else{
      if(parm %in% microclimate.d$parm){
        ret_df <- microclimate.d[microclimate.d$parm == parm, ]
      }else{
        stop("MicroClimate parameter not found", call. = FALSE)
      }
    }
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
    
    ret_df <- as.data.frame(mat)
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
    ## cat("Management Scripts: ", manager.node.names,"\n")
    
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
        ## cat("Name: ", selected.manager.node, "\n")
        ret_df <- as.data.frame(mat)
        ## cat("\n") 
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
        ## cat("Name: ", selected.manager.node,"\n")
        parm2 <- ms.params[[position]]$Key
        ## cat("Key:", ms.params[[position]]$Key, "\n")
        ret_df <- as.data.frame(mat)
        ## cat("\n")
      }
    }else{
      ret_lst <- vector("list", length = length(manager.node))
      for(i in seq_along(manager.node)){
        ret_lst[[i]] <- manager.node[[i]]
        names(ret_lst) <- manager.node.names
      }
    }
  }
  
  if(node == "Operations"){
    won <- grepl("Models.Operations", core.zone.node)
    if(length(won) == 0)
      stop("Operations node not found", call. = FALSE)
    operations.node <- core.zone.node[won]
    
    if(length(operations.node) > 1)
      stop("Not ready to handle multiple 'Operations'", call. = FALSE)
    
    if(is.null(operations.node[[1]]$Operation))
      stop("'Operation' child node not found", call. = FALSE)
    
    len.op <- length(operations.node[[1]]$Operation)
    op.mat <- matrix(nrow = len.op, ncol = 3,
                     dimnames = list(NULL, c("Date", "Action", "Line")))
    
    for(i in seq_len(len.op)){
      op.mat[i, ] <- c(operations.node[[1]]$Operation[[i]]$Date,
                       operations.node[[1]]$Operation[[i]]$Action,
                       operations.node[[1]]$Operation[[i]]$Line)
    }
    
    if(is.null(parm)){
      parm.path <- paste0(parm.path.2, ".Operations")
      ret_df <- as.data.frame(op.mat)
    }else{
      parm.path.3 <- paste0(parm.path.2, ".Operations")
      parm.path <- paste(parm.path.3, parm[[1]], parm[[2]], sep = ".")
      op.mat.dat <- as.data.frame(op.mat) 
      op.mat.dat.parm <- op.mat.dat[parm[[1]], parm[[2]], drop = FALSE] 
      ret_df <- op.mat.dat.parm
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
      ret_lst <- vector("list", length = length(tmp))
      for(i in 1:length(tmp)){
        cat("Report name:", report.node[[i]]$Name, "\n")
        ret_lst[[i]] <- list(VariableNames = tmp[[i]]$vn, EventNames = tmp[[i]]$en)
        cat("\n")
      }
      parm.path <- paste0(parm.path.2,".", report.node.names)
    }else{
      if(!is.list(parm)){
        if(length(report.node.names) > 1)
          stop("More than one Report is present. Use a list to choose one.")
        if(!grepl(parm, "VariableNames") && !grepl(parm, "EventNames"))
          stop("parm should contain either VariableNames or EventNames")
        if(parm == "VariableNames") ret_df <- as.data.frame(tmp[[i]]$vn)
        if(parm == "EventNames") ret_df <- as.data.frame(tmp[[i]]$en)
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
  
  #### Option for 'Other' character or 'list' ----
  if(node == "Other" && other.parm.flag > 0){
    ## I will develop this option assuming parm is a list or a character
    if(is.null(parm))
      stop("'parm' is missing", call. = FALSE)
    
    if(is.character(parm)){
      if(!grepl(".", parm, fixed = TRUE))
        stop("'parm' needs to be a proper json path")
      parm.path <- parm
      if(parm == ".")
        stop("'parm' path is empty")
      pparm <- strsplit(parm, split = ".", fixed = TRUE)[[1]]
      root.name.level.0 <- gsub(".", "", parm.path.0, fixed = TRUE)
      if(pparm[2] != root.name.level.0)
        stop(paste("First 'parm' element does not match:", 
                   paste(root.name.level.0, collapse = " ")), call. = FALSE)
      children.names <- sapply(apsimx_json$Children, FUN = function(x) x$Name)
      if(length(pparm) == 2){
        cat("Simulation children names:")
        print(children.names)
        cat("\n")
      }
      if(length(pparm) == 3){
        root1 <- pparm[3]
        ## Guess if 'root' is contained in the first level of names
        root.names.level.1 <- vapply(apsimx_json$Children, FUN = function(x) x$Name, 
                                     FUN.VALUE = "character")
        wroot1 <- grep(as.character(root1), root.names.level.1)    
        if(length(wroot1) == 0)
          stop(paste("Second 'parm' element did not match:", 
                     paste(root.names.level.1, collapse = " ")), call. = FALSE)
        root.names.level.2 <- vapply(apsimx_json$Children[[wroot1]]$Children, 
                                     FUN = function(x) x$Name, 
                                     FUN.VALUE = "character")
        cat(paste(root1, "Names level 2:"))
        print(root.names.level.2)
        cat("\n")
      }
      if(length(pparm) == 4){
        root1 <- pparm[3]
        ## Guess if 'root' is contained in the first level of names
        root.names.level.1 <- vapply(apsimx_json$Children, FUN = function(x) x$Name, 
                                     FUN.VALUE = "character")
        wroot1 <- grep(as.character(root1), root.names.level.1)    
        if(length(wroot1) == 0)
          stop(paste("Second 'parm' element did not match:", 
                     paste(root.names.level.1, collapse = " ")), call. = FALSE)
        root.names.level.2 <- vapply(apsimx_json$Children[[wroot1]]$Children, 
                                     FUN = function(x) x$Name, 
                                     FUN.VALUE = "character")
        root2 <- pparm[4]
        wroot2 <- grep(as.character(root2), root.names.level.2)    
        if(length(wroot2) == 0)
          stop(paste("Third 'parm' element did not match:", 
                     paste(root.names.level.2, collapse = " ")), call. = FALSE)
        root.names.level.3 <- vapply(apsimx_json$Children[[wroot1]]$Children[[wroot2]]$Children, 
                                     FUN = function(x) x$Name, 
                                     FUN.VALUE = "character")
        cat(paste(root1, root2, "Names level 3:"))
        print(root.names.level.3)
        cat("\n")
        
      }
      if(length(pparm) > 4) stop("Not implemented yet", call. = FALSE)
    }
    
    if(is.list(parm)){
      if(length(parm) == 0)
        stop("Length of 'parm' list should be greater than zero", call. = FALSE)
      if(is.numeric(parm[[1]])){
        ### In this case the list will be used to create a parameter path
        if(parm[[1]] != 1)
          stop("First element of list should be equal to 1", call. = FALSE)
        parm.path.0 <- paste0(".", apsimx_json$Name) ## Root
        if(length(parm) >= 2){
          if(!is.numeric(parm[[2]]))
            stop("Second element of 'parm' should be numeric")
          root.names.level.1 <- vapply(apsimx_json$Children, FUN = function(x) x$Name, 
                                       FUN.VALUE = "character")
          if(length(root.names.level.1) == 0)
            stop("'parm' index parm[[2]] not found at this level")
          if(length(parm[[2]]) == 1){
            if(parm[[2]] > 0){
              wlevel1 <- apsimx_json$Children[[parm[[2]]]]
              parm.path <- paste0(parm.path.0, ".", root.names.level.1[parm[[2]]])
            }else{
              parm.path <- parm.path.0
              pdat <- data.frame(second_level = root.names.level.1)
              ret_df <- pdat
            } 
          }else{
            ### Here I assume that parm[[2]] is a vector
            if(length(parm[[2]]) > length(root.names.level.1))
              stop("Length of parm[[2]] should be less than number of elements in the second level")
            selected.levels <- root.names.level.1[parm[[2]]]
            parm.path <- paste0(parm.path, ".", selected.levels)
            pdat <- data.frame(second_level = root.names.level.1)
            ret_df <- pdat[parm[[2]],, drop = FALSE]
          }
        }
        if(length(parm) >= 3){
          root.names.level.2 <- vapply(wlevel1$Children, FUN = function(x) x$Name, 
                                       FUN.VALUE = "character")
          if(length(root.names.level.2) == 0){
            ## This means that Children are empty
            ## Need to pass this to the next step
            root.names.level.2 <- names(wlevel1)
          }
          if(length(parm[[3]]) == 1){
            if(parm[[3]] > 0){
              wlevel2 <- apsimx_json$Children[[parm[[2]]]]$Children[[parm[[3]]]]
              parm.path <- paste0(parm.path, ".", root.names.level.2[parm[[3]]]) 
            }else{
              pdat <- data.frame(third_level = root.names.level.2)
              ret_df <- pdat
            }            
          }else{
            ### Here I assume that parm[[3]] is a vector
            if(length(parm[[3]]) > length(root.names.level.2))
              stop("Length of parm[[3]] should be less than number of elements in the third level")
            selected.levels <- root.names.level.2[parm[[3]]]
            parm.path <- paste0(parm.path.0, ".", selected.levels)
            pdat <- data.frame(third_level = root.names.level.2)
            ret_df <- pdat[parm[[3]],, drop = FALSE]
          }
        }
        if(length(parm) >= 4){
          root.names.level.3 <- vapply(wlevel2$Children, FUN = function(x) x$Name, 
                                       FUN.VALUE = "character")
          if(length(root.names.level.3) == 0)
            root.names.level.3 <- names(wlevel2)
          if(length(parm[[4]]) == 1){
            if(parm[[4]] > 0){
              wlevel3 <- apsimx_json$Children[[parm[[2]]]]$Children[[parm[[3]]]]$Children[[parm[[4]]]]
              parm.path <- paste0(parm.path, ".", root.names.level.3[parm[[4]]])            
            }else{
              pdat <- data.frame(fourth_level = root.names.level.3)
              ret_df <- pdat
            }            
          }else{
            ### Here I assume that parm[[4]] is a vector
            if(length(parm[[4]]) > length(root.names.level.3))
              stop("Length of parm[[4]] should be less than number of elements in the fourth level")
            selected.levels <- root.names.level.3[parm[[4]]]
            parm.path <- paste0(parm.path.0, ".", selected.levels)
            pdat <- data.frame(fourth_level = root.names.level.3)
            ret_df <- pdat[parm[[4]],, drop = FALSE]
          }
        }
        if(length(parm) >= 5){
          root.names.level.4 <- vapply(wlevel3$Children, FUN = function(x) x$Name, 
                                       FUN.VALUE = "character")
          if(length(root.names.level.4) == 0)
            root.names.level.4 <- names(wlevel3)
          if(length(parm[[5]]) > 1)
            stop("Have not implemented this yet")
          if(parm[[5]] > 0){
            wlevel4 <- apsimx_json$Children[[parm[[2]]]]$Children[[parm[[3]]]]$Children[[parm[[4]]]]$Children[[parm[[5]]]]
            parm.path <- paste0(parm.path, ".", root.names.level.4[parm[[5]]])            
          }else{
            pdat <- data.frame(fifth_level = root.names.level.4)
            ret_df <- pdat
          }
        }
        if(length(parm) >= 6){
          root.names.level.5 <- vapply(wlevel4$Children, FUN = function(x) x$Name, 
                                       FUN.VALUE = "character")
          if(length(parm[[6]]) > 1)
            stop("Have not implemented this yet")
          if(parm[[6]] > 0){
            wlevel5 <- apsimx_json$Children[[parm[[2]]]]$Children[[parm[[3]]]]$Children[[parm[[4]]]]$Children[[parm[[5]]]]$Children[[parm[[6]]]]
            parm.path <- paste0(parm.path, ".", root.names.level.5[parm[[6]]])            
          }else{
            pdat <- data.frame(sixth_level = root.names.level.5)
            ret_df <- pdat
          }
        }
        if(length(parm) >= 7)  
          stop("Have not developed this yet", call. = FALSE)
      }else{
        # if(length(parm) == 1){
        #   tmpr <- grep_json_list(parm, apsimx_json$Children)
        #   print(tmpr)
        # }
        warning("This is a work in progress")
        #### node = 'Other' and list with characters ----
        if(length(fcsn) > 1){
          if(missing(root)){
            ## In this case the first element of the list should be the root
            if(length(parm) == 1)
              stop("'parm' should be at least length equal to 2", call. = FALSE)
            ## select one of the simulations
            simulation.names <- sapply(apsimx_json$Children, FUN = function(x) x$Name)
            wsim <- grep(parm[[1]], simulation.names)
            if(length(wsim) == 0){
              cat("Simulation names:", paste(simulation.names, collpse = " "), "\n")
              stop("First element of 'parm' list does not match Simulation names", call. = FALSE)
            }else{
              fcsn <- wsim
              parm <- parm[2:length(parm)]
              if(parm[[1]] == "")
                stop("'parm' is empty")
            }
            parent.node <- apsimx_json$Children[[fcsn]]$Children   
          }else{
            ### root is present
            if(length(root) == 1){
              nms <- vapply(apsimx_json$Children, FUN = function(x) x$Name, 
                            FUN.VALUE = "character")
              fcsn <- grep(as.character(root), nms)
              if(length(fcsn) == 0 || length(fcsn) > 1){
                cat("Children:", nms, "\n")
                stop("'root' not found. Choose one of the options above", call. = FALSE)
              }
              parm.path.1 <- paste0(parm.path.0, ".", apsimx_json$Children[[fcsn]]$Name)
              parent.node <- apsimx_json$Children[[fcsn]]$Children
            }else{
              stop("Have not developed this yet")
            }
          }
        }else{
          parent.node <- apsimx_json$Children[[fcsn]]$Children            
        }
        wcz <- grepl("Models.Core.Zone", parent.node)
        if(sum(wcz) < 0.5)
          stop("Core Simulation not found", call. = FALSE)
        core.zone.node <- parent.node[wcz][[1]]$Children
        parm.path.1 <- paste0(parm.path.0, ".", apsimx_json$Children[[fcsn]]$Name) 
        parm.path.2 <- paste0(parm.path.1, ".", parent.node[wcz][[1]]$Name)  
        tmp <- core.zone.node
        parm.path.2.1 <- parm.path.2
        
        if(length(parm) > 1){
          for(i in 1:(length(parm) - 1)){
            nms <- sapply(tmp, function(x) x$Name)
            print(nms)
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
        }else{
          ## How do I find parm?
          tmp.names <- sapply(tmp, FUN = function(x) x$Name)
          wparm <- grep(parm[[1]], tmp.names)
          if(length(wparm) == 1){
            tmp <- tmp[[wparm]]
          }else{
            #### Work in progress ----
            for(j in seq_along(tmp)){
              tmp.nms <- names(tmp[[j]])
              for(jj in seq_along(parm)){
                wtn <- grep(parm[[jj]], tmp.nms)
                if(length(wtn) > 0){
                  print(tmp[[j]][wtn])
                }
              }
            }
          }
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
    }
  }
  
  # if(print.path && node != "Other" && node != "Operations"){
  #   if(!missing(parm)){
  #     if(length(parm) == 1){
  #       parm.path <- paste0(parm.path, ".", parm)
  #     }else{
  #       if(!is.na(position)){
  #         parm.path <- paste0(parm.path, ".", parm2)
  #       }
  #     }
  #   }
  #   cat("Parm path:", parm.path,"\n")
  # }else{
  #   if(print.path) cat("Parm path:", parm.path,"\n")  
  # }
  #### Return ----
  if(!is.null(ret_lst)){
    ret <- ret_lst
  }else{
    ret <- ret_df
  }
  return(ret)
}

#'
#' Extract initial values from a parameter path
#' @title Extract values from a parameter path
#' @name extract_values_apsimx
#' @param file file name to be run (the extension .apsimx is optional)
#' @param src.dir directory containing the .apsimx file to be run (defaults to the current directory)
#' @param parm.path parameter path either use inspect_apsimx or see example below
#' @return a vector with extracted parameter values from an APSIM file.
#' @export
#' @examples 
#' \donttest{
#' ## Find examples
#' extd.dir <- system.file("extdata", package = "apsimx")
#' ## Extract parameter path
#' pp <- inspect_apsimx("Maize.apsimx", src.dir = extd.dir,
#'                      node = "Manager", parm = list("Fert", 1))
#' ppa <- paste0(pp, ".Amount")
#' ## Extract value
#' extract_values_apsimx("Maize.apsimx", src.dir = extd.dir, parm.path = ppa)
#' }
extract_values_apsimx <- function(file, src.dir, parm.path){
  
  .check_apsim_name(file)
  
  file.names <- dir(path = src.dir, pattern=".apsimx$", ignore.case=TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsimx files in the specified directory to inspect.")
  }
  
  file <- match.arg(file, file.names)
  
  apsimx_json <- jsonlite::read_json(paste0(src.dir, "/", file))
  
  upp <- strsplit(parm.path, ".", fixed = TRUE)[[1]]
  upp.lngth <- length(upp)
  if(upp.lngth < 5) stop("Parameter path too short?")
  if(upp.lngth > 10) stop("Cannot handle this yet")
  ## upp[2] is typically "Simulations"
  if(apsimx_json$Name != upp[2])
    stop("Simulation root name does not match")
  wl3 <- which(upp[3] == sapply(apsimx_json$Children, function(x) x$Name))
  ## At this level I select among simulation children
  ## upp[3] is typically "Simulation"
  n3 <- apsimx_json$Children[[wl3]]
  ## Look for the first reasonable parameter
  wl4 <- which(upp[4] == sapply(n3$Children, function(x) x$Name))
  ## This is super dumb but I do not know how to do it otherwise
  ## Length is equal to 5
  if(upp.lngth == 5){
    if(upp[5] %in% names(n3$Children[[wl4]])){
      value <- n3$Children[[wl4]][[upp[5]]]
    }else{
      wl5 <- which(upp[5] == sapply(n3$Children[[wl4]]$Children, function(x) x$Name))
      if(length(wl5) == 0) stop("Parameter not found at level 5")
      value <- n3$Children[[wl4]]$Children[[wl5]][[upp[5]]]
    }
  }
  ## Length is equal to 6
  if(upp.lngth == 6){
    n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
    wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
    if(upp[6] %in% names(n4$Children[[wl5]])){
      value <- n4$Children[[wl5]][[upp[6]]]
    }else{
      if("Parameters" %in% names(n4$Children[[wl5]])){
        wp <- grep(upp[6], n4$Children[[wl5]]$Parameters)
        if(length(wp) == 0) stop("Could not find parameter")
        value <- n4$Children[[wl5]]$Parameters[[wp]]$Value
      }else{
        wl6 <- which(upp[6] == sapply(n4$Children[[wl5]]$Children, function(x) x$Name))
        if(length(wl6) == 0) stop("Could not find parameter")
        value <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]][[upp[6]]]          
      }
    }
  }
  if(upp.lngth == 7){
    n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
    wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
    n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
    wl6 <- which(upp[6] == sapply(n5$Children, function(x) x$Name))
    value <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]][[upp[7]]]
    if(is.null(value)){
      n6 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]
      if("Command" %in% names(n6)){
        gpv <- grep(upp[7], n6$Command, value = TRUE)
        value <- as.numeric(strsplit(gpv, "=")[[1]][2])
      }
    }
  }
  if(upp.lngth == 8){
    n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
    wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
    n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
    wl6 <- which(upp[6] == sapply(n5$Children, function(x) x$Name))
    n6 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]
    wl7 <- which(upp[7] == sapply(n6$Children, function(x) x$Name))
    value <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]][[upp[8]]]
    if(is.null(value)){
      n7 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]
      if("Command" %in% names(n7)){
        gpv <- grep(upp[8], n7$Command, value = TRUE)
        value <- as.numeric(strsplit(gpv, "=")[[1]][2])
      }
    }
  }
  if(upp.lngth == 9){
    n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
    wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
    n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
    wl6 <- which(upp[6] == sapply(n5$Children, function(x) x$Name))
    n6 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]
    wl7 <- which(upp[7] == sapply(n6$Children, function(x) x$Name))
    n7 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]
    wl8 <- which(upp[8] == sapply(n7$Children, function(x) x$Name))
    value <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]$Children[[wl8]][[upp[9]]] 
    if(is.null(value)){
      n8 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]$Children[[wl8]]
      if("Command" %in% names(n8)){
        gpv <- grep(upp[9], n8$Command, value = TRUE)
        value <- as.numeric(strsplit(gpv, "=")[[1]][2])
      }
    }
  }
  if(upp.lngth == 10){
    n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
    wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
    n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
    wl6 <- which(upp[6] == sapply(n5$Children, function(x) x$Name))
    n6 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]
    wl7 <- which(upp[7] == sapply(n6$Children, function(x) x$Name))
    n7 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]
    wl8 <- which(upp[8] == sapply(n7$Children, function(x) x$Name))
    n8 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]$Children[[wl8]]
    wl9 <- which(upp[9] == sapply(n8$Children, function(x) x$Name))
    value <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]$Children[[wl8]]$Children[[wl9]][[upp[10]]]
    if(is.null(value)){
      n9 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]$Children[[wl8]]$Children[[wl9]]
      if("Command" %in% names(n9)){
        gpv <- grep(upp[10], n9$Command, value = TRUE)
        value <- as.numeric(strsplit(gpv, "=")[[1]][2])
      }
    }
  }
  return(value)
}


