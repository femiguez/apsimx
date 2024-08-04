#'
#' In general, this function is used to inspect one parameter at a time. There are some exceptions. \cr
#'
#' When node equals \sQuote{Other} there are several options. If \sQuote{parm} is not specified the structure
#' of the simulation file will be returned. In this case, the parameter to print is typically just \sQuote{Simulations}.
#' This option is useful when the intention is to show the simulation structure to pick a root presumably. \sQuote{parm}
#' can be set as 0, 1, 2 or 3 for different levels.
#' \sQuote{parm} can also be a list with integers, such as \sQuote{list(1, 2, 3)}. If zero is included, available elements
#  will be displayed.
#' If a parameter is specified the function will try to \sQuote{guess} the root elements from the parameter path supplied.
#'
#' @title Inspect an .apsimx (JSON) file
#' @name inspect_apsimx
#' @description inspect a JSON apsimx file. It does not replace the GUI, but it can save time by quickly checking parameters and values.
#' @param file file ending in .apsimx to be inspected (JSON)
#' @param src.dir directory containing the .apsimx file to be inspected; defaults to the current working directory
#' @param node specific node to be inspected either \sQuote{Clock}, \sQuote{Weather},
#' \sQuote{Soil}, \sQuote{SurfaceOrganicMatter}, \sQuote{MicroClimate}, \sQuote{Crop},
#'  \sQuote{Manager}, \sQuote{Operations} or \sQuote{Other}
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
#' ## Examples of using node = "Other"
#' extd.dir <- system.file("extdata", package = "apsimx")
#'
#' ## When parm is not provided
#' inspect_apsimx("maize-manager-folder.apsimx", src.dir = extd.dir, node = "Other")
#' ## When parm = 2
#' inspect_apsimx("maize-manager-folder.apsimx", src.dir = extd.dir,
#'                node = "Other", parm = 2)
#' ## When parm = 3
#' inspect_apsimx("maize-manager-folder.apsimx", src.dir = extd.dir,
#'                node = "Other", parm = 3)
#' ## When parm is a path
#' inspect_apsimx("maize-manager-folder.apsimx", src.dir = extd.dir,
#'                node = "Other", parm = ".Simulations.Simulation")
#' ## When parm is a list with numbers (integers)
#' pp <- inspect_apsimx("maize-manager-folder.apsimx", src.dir = extd.dir,
#'                      node = "Other", parm = list(1, 1, 5),
#'                      print.path = TRUE)
#' ## Same as above, but with zero prints possible options
#' inspect_apsimx("maize-manager-folder.apsimx", src.dir = extd.dir,
#'                 node = "Other", parm = list(1, 1, 5, 0))
#'
#' ## It is possible to look into folders using this method
#' inspect_apsimx("maize-manager-folder.apsimx", node = "Other", src.dir = extd.dir,
#'                parm = list("Manager", "Fertiliser", "Amount"))
#'
#'
#'
#' }
#'

inspect_apsimx <- function(file = "", src.dir = ".",
                           node = c("Clock", "Weather", "Soil", "SurfaceOrganicMatter", "MicroClimate", "Crop", "Manager","Report", "Operations", "Other"),
                           soil.child = c("Metadata", "Water", "InitialWater",
                                          "Chemical", "Physical", "Analysis", "SoilWater",
                                          "InitialN", "CERESSoilTemperature", "SoilTemperature",
                                          "Sample", "Solute", "NO3", "NH4", "Urea",
                                          "Nutrient", "Organic", "Swim3"),
                           parm = NULL,
                           digits = 3,
                           print.path = FALSE,
                           root){
  #### Beginning of function ----
  ##.check_apsim_name(file)

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

  ## The code below is confusing. There are some reasons for it.
  ## It is activated if node == "Other" and there is more than on simulation
  ## Or if node == "Other" and parm is either NULL or an integer
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
        cat("Level 0:", gsub(".", "", parm.path.0, fixed = TRUE), "\n")
      }
      ## If parm is null there are different levels of display
      if(parm == 1){
        first.column.name <- gsub(".", "", parm.path.0, fixed = TRUE)
        root.names.level.1 <- vapply(apsimx_json$Children, FUN = function(x) x$Name,
                                     FUN.VALUE = "character")
        pdat <- data.frame(first_level = c(first.column.name, rep(".", length(root.names.level.1) - 1)),
                           second_level = root.names.level.1)
        print(knitr::kable(pdat, row.names = TRUE))
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
        print(knitr::kable(pdat, row.names = TRUE))
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
        print(knitr::kable(pdat, row.names = TRUE))
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
        ### This flag is used to activate the code at the bottom of the function
        ### This is not great code, but I'm trying to get something that works for now
        other.parm.flag <- 1
      }
    }
  }

  if((length(fcsn) > 1 || !missing(root)) && find.root){
    if(missing(root)){
      cat("Simulation structure: \n")
      str_list(apsimx_json)
      stop("more than one simulation found and no root node label has been specified \n select one of the children names above", call. = FALSE)
    }else{
      ### Parse root
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
    wlw <- function(x) grepl("Models.Climate.Weather|Models.Weather", x$`$type`)
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
  if(find.root){
    wcz <- grepl("Models.Core.Zone", parent.node)
    if(sum(wcz) < 0.5)
      stop("Core Zone Simulation not found", call. = FALSE)
    core.zone.node <- parent.node[wcz][[1]]$Children

    parm.path.2 <- paste0(parm.path.1, ".", parent.node[wcz][[1]]$Name)
  }

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

      ### This is a bad idea probably
      if(soil.child == "Water" && length(wsc) == 2) wsc <- wsc[2]

      if(length(wsc) == 0) stop("soil.child likely not present")

      selected.soil.node.child <- soil.node[[1]]$Children[wsc]
    }

      ## For some variables now it is the time to print
      ## The code below is not strictly needed but it is here
      ## in case I need a second level of soil in the future
    first.level.soil <- c("Water", "Physical",
                          "Chemical", "Analysis", "InitialWater", "Initial water", "initial water",
                          "InitialN", "SoilWater", "SoilTemperature",
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
            if(is.null(vals)) vals <- rep(".", length(tmp))
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
        parm.physical.found <- FALSE
        ## Print first set of soil parameters
        if(!is.null(soil.d1)){
          if(parm %in% names(soil.d1)){
            ##print(knitr::kable(soil.d1[soil.d1$parm == parm,], digits = digits))
            print(knitr::kable(soil.d1[, parm, drop = FALSE], digits = digits))
            parm.physical.found <- TRUE
          }
          if(parm %in% soil.d1$parm){
            print(knitr::kable(soil.d1[soil.d1$parm == parm,], digits = digits))
            parm.physical.found <- TRUE
          }
        }
        ## Print second set of soil parameters
        if(!is.null(soil.d2)){
          soil.d2 <- as.data.frame(soil.d2)
          names(soil.d2) <- col.nms
          ## print(knitr::kable(soil.d2[soil.d2$parm == parm,], digits = digits))
          if(parm %in% names(soil.d2)){
            print(knitr::kable(soil.d2[, parm, drop = FALSE], digits = digits))
            parm.physical.found <- TRUE
          }
          if(parm %in% soil.d2$parm){
            print(knitr::kable(soil.d2[soil.d2$parm == parm,], digits = digits))
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
            print(knitr::kable(soil.d3[, parm, drop = FALSE], digits = digits))
            parm.physical.found <- TRUE
          }
          if(parm %in% soil.d3$parm){
            print(knitr::kable(soil.d3[soil.d3$parm == parm,], digits = digits))
            parm.physical.found <- TRUE
          }
        }
        if(!parm.physical.found)
          stop("Soil physical parameter not found", call. = FALSE)
      }
    }
  }
  #### Solute ----
  second.level.soil <- c("Solute", "NO3", "NH4", "Urea")

  if(soil.child %in% second.level.soil){

    soil.d2 <- NULL
    soil.d3 <- NULL
    enms <- c("IncludeInDocumentation", "Enabled", "ReadOnly", "Children", "Name", "$type")
    ssnc <- selected.soil.node.child

    if(soil.child %in% c("Solute", "NO3", "NH4", "Urea")){
      
      solutes <- sapply(ssnc, function(x) x$Name)
      parm.path <- paste0(parm.path.2.1, ".", soil.child)
      
      if(missing(parm)){
        cat("Solutes:", solutes, "\n")
      }else{
        if(soil.child == "Solute"){
          sel.parm <- grep(parm[[1]], solutes)
          if(length(sel.parm) == 0)
            stop("'parm' should be one of: ", paste(solutes, collapse = ", "), call. = FALSE)
          solutes <- parm[[1]]          
        }
        if(soil.child %in% c("NO3", "NH4", "Urea")){
          ## In this case 'parm' could be either Thickness, InitialValues
          ## or some of the other ones. 
          parm.path <- paste0(parm.path.2.1, ".", soil.child)
        }
      }

      for(j in seq_along(solutes)){
        cat("\nSolute:", solutes[j], "\n")
        ssnc.solute <- ssnc[[j]]
        if(length(unlist(ssnc.solute$Thickness)) != length(unlist(ssnc.solute$InitialValues))){
          cat("Length of Thickness:", length(unlist(ssnc.solute$Thickness)), "\n")
          cat("Length of InitialValues:", length(unlist(ssnc.solute$InitialValues)), "\n")
          stop("Length of 'Thickness' does not match length of 'InitialValues'", call. = FALSE)
        }
        soil.d1 <- data.frame(Thickness = unlist(ssnc.solute$Thickness),
                              InitialValues = unlist(ssnc.solute$InitialValues))
        if(missing(parm)){
          if(!is.null(soil.d1)) print(knitr::kable(soil.d1, digits = digits))  
        }else{
          if(length(parm) == 1){
            if(parm %in% c("Thickness", "InitialValues")){
              soil.d1.s <- soil.d1[, parm, drop = FALSE]
              if(!is.null(soil.d1)) print(knitr::kable(soil.d1.s, digits = digits))  
            }else{
              ### Parm is of length equal to 1 and is not "Thickness" or "InitialValues"
              if(parm %in% c("NO3", "NH4", "Urea")){
                if(!is.null(soil.d1)) print(knitr::kable(soil.d1, digits = digits))  
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
        if(missing(parm)){
          if(!is.null(soil.d2)) print(knitr::kable(soil.d2, digits = digits))
        }else{
          if(length(parm) == 1){
            if(soil.child == "Solute"){
              if(!parm %in% solutes)
                stop("'parm' should be one of:", solutes, call. = FALSE)
              if(!is.null(soil.d2)) print(knitr::kable(soil.d2, digits = digits))                          
            }else{
              if(!parm %in% c("Thickness", "InitialValues")){
                if(!parm %in% soil.d2$parm)
                  stop("'parm' should be one of:", paste(soil.d2$parm, collapse = " "), call. = FALSE)
                soil.d2.s <- soil.d2[soil.d2$parm == parm, , drop = FALSE] 
                if(!is.null(soil.d2)) print(knitr::kable(soil.d2.s, digits = digits)) 
              }
            }
          }
          if(is.list(parm)){
            if(length(parm) != 2)
              stop("'parm' should be a list of length 2", call. = FALSE)
            if(is.numeric(parm[[2]])){
              if(parm[[2]] < 1 || parm[[2]] > nrow(soil.d2))
                stop("The second elment of 'parm' should be a number between 1 and ", nrow(soil.d2), call. = FALSE)
              soil.d2.s <- soil.d2[parm[[2]], , drop = FALSE]  
            }
            if(is.character(parm[[2]])){
              if(!parm[[2]] %in% soil.d2$parm)
                stop("The second element of 'parm' should be one of:", soil.d2$parm, call. = FALSE)
              soil.d2.s <- soil.d2[soil.d2$parm == parm[[2]], , drop = FALSE]  
            }
            if(!is.null(soil.d2)) print(knitr::kable(soil.d2.s, digits = digits))            
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

    parm.path <- paste0(parm.path.2,".",som.node$Name)
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
      print(knitr::kable(som.d, digits = digits))
    }else{
      if(parm %in% som.d$parm){
        print(knitr::kable(som.d[som.d$parm == parm, ], digits = digits))
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
      print(knitr::kable(microclimate.d, digits = digits))
    }else{
      if(parm %in% microclimate.d$parm){
        print(knitr::kable(microclimate.d[microclimate.d$parm == parm, ], digits = digits))
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
      print(knitr::kable(as.data.frame(op.mat), digits = digits))
    }else{
      parm.path.3 <- paste0(parm.path.2, ".Operations")
      parm.path <- paste(parm.path.3, parm[[1]], parm[[2]], sep = ".")
      op.mat.dat <- as.data.frame(op.mat)
      op.mat.dat.parm <- op.mat.dat[parm[[1]], parm[[2]], drop = FALSE]
      print(knitr::kable(op.mat.dat.parm, digits = digits))
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

  #### Option for 'Other' character or 'list' ----
  #### I might have to re-factor this code someday.
  #### The list with numbers seems to be quite useful
  #### now for creating paths. Initially, I thought I would
  #### just use it to form 'root' paths, but now it has enough
  #### functionality to inspect and create paths
  #### 'parm' can be:
  ####               1. A character. This can be used to create a json path
  ####.              2. A list: with integers as elements. Easy and simple way of creating paths
  ####               3. A list: with characters. Still developing this...
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
        if(length(parm) == 1)
          stop("Length of 'parm' should be greated than 1", call. = FALSE)
        if(length(parm) >= 2){
          if(!is.numeric(parm[[2]]))
            stop("Second element of 'parm' should be numeric")
          root.names.level.1 <- vapply(apsimx_json$Children, FUN = function(x) x$Name,
                                       FUN.VALUE = "character")
          if(length(root.names.level.1) == 0)
            stop("'parm' index parm[[2]] not found at this level")
          if(length(parm[[2]]) == 1){
            if(parm[[2]] > 0){
              wlevel1 <- try(apsimx_json$Children[[parm[[2]]]], silent = TRUE)
              if(inherits(wlevel1, "try-error"))
                stop("parm[[2]] did not match an available node", call. = FALSE)
              parm.path <- paste0(parm.path.0, ".", root.names.level.1[parm[[2]]])
            }else{
              parm.path <- parm.path.0
              pdat <- data.frame(second_level = root.names.level.1)
              print(knitr::kable(pdat, row.names = TRUE))
            }
          }else{
            ### Here I assume that parm[[2]] is a vector
            if(length(parm[[2]]) > length(root.names.level.1))
              stop("Length of parm[[2]] should be less than number of elements in the second level")
            selected.levels <- root.names.level.1[parm[[2]]]
            parm.path <- paste0(parm.path, ".", selected.levels)
            pdat <- data.frame(second_level = root.names.level.1)
            print(knitr::kable(pdat[parm[[2]],, drop = FALSE], row.names = TRUE))
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
              wlevel2 <- try(apsimx_json$Children[[parm[[2]]]]$Children[[parm[[3]]]], silent = TRUE)
              if(inherits(wlevel2, "try-error"))
                stop("parm[[3]] did not match an available node", call. = FALSE)
              parm.path <- paste0(parm.path, ".", root.names.level.2[parm[[3]]])
            }else{
              pdat <- data.frame(third_level = root.names.level.2)
              print(knitr::kable(pdat, row.names = TRUE))
            }
          }else{
            ### Here I assume that parm[[3]] is a vector
            if(length(parm[[3]]) > length(root.names.level.2))
              stop("Length of parm[[3]] should be less than number of elements in the third level")
            selected.levels <- root.names.level.2[parm[[3]]]
            parm.path <- paste0(parm.path, ".", selected.levels)
            pdat <- data.frame(third_level = root.names.level.2)
            print(knitr::kable(pdat[parm[[3]],, drop = FALSE], row.names = TRUE))
          }
        }
        if(length(parm) >= 4){
          names.level.3 <- FALSE
          root.names.level.3 <- vapply(wlevel2$Children, FUN = function(x) x$Name,
                                       FUN.VALUE = "character")
          if(length(root.names.level.3) == 0){
            root.names.level.3 <- names(wlevel2)
            names.level.3 <- TRUE
          }
          if(length(parm[[4]]) == 1){
            if(parm[[4]] > 0){
              if(names.level.3){
                selected.names <- root.names.level.3[parm[[4]]]
                ##pdat <- data.frame(fourth_level = selected.names)
                wlevel3 <- NULL
                ##print(knitr::kable(pdat, row.names = TRUE))
                parm.path <- paste0(parm.path, ".", selected.names)
              }else{
                wlevel3 <- try(apsimx_json$Children[[parm[[2]]]]$Children[[parm[[3]]]]$Children[[parm[[4]]]], silent = TRUE)
                if(inherits(wlevel3, "try-error"))
                  stop("parm[[4]] did not match an available node", call. = FALSE)
                parm.path <- paste0(parm.path, ".", root.names.level.3[parm[[4]]])
              }
            }else{
              pdat <- data.frame(fourth_level = root.names.level.3)
              print(knitr::kable(pdat, row.names = TRUE))
            }
          }else{
            ### Here I assume that parm[[4]] is a vector
            if(length(parm[[4]]) > length(root.names.level.3))
              stop("Length of parm[[4]] should be less than number of elements in the fourth level")
            selected.levels <- root.names.level.3[parm[[4]]]
            parm.path <- paste0(parm.path, ".", selected.levels)
            pdat <- data.frame(fourth_level = root.names.level.3)
            print(knitr::kable(pdat[parm[[4]],, drop = FALSE], row.names = TRUE))
          }
        }
        if(length(parm) >= 5){
          names.level.4 <- FALSE
          ##browser()
          root.names.level.4 <- vapply(wlevel3$Children, FUN = function(x) x$Name,
                                       FUN.VALUE = "character")
          if(length(root.names.level.4) == 0){
            root.names.level.4 <- names(wlevel3)
            names.level.4 <- TRUE
          }
          if(length(parm[[5]]) == 1){
            if(parm[[5]] > 0){
              if(names.level.4){
                selected.names <- root.names.level.4[parm[[5]]]
                ##pdat <- data.frame(fifth_level = selected.names)
                ##print(knitr::kable(pdat, row.names = TRUE))
                parm.path <- paste0(parm.path, ".", selected.names)
              }else{
                wlevel4 <- try(apsimx_json$Children[[parm[[2]]]]$Children[[parm[[3]]]]$Children[[parm[[4]]]]$Children[[parm[[5]]]], silent = TRUE)
                if(inherits(wlevel4, "try-error"))
                  stop("parm[[5]] did not match an available node", call. = FALSE)
                parm.path <- paste0(parm.path, ".", root.names.level.4[parm[[5]]])
              }
            }else{
              pdat <- data.frame(fifth_level = root.names.level.4)
              print(knitr::kable(pdat, row.names = TRUE))
            }
          }else{
            if(length(parm[[5]]) > length(root.names.level.4))
              stop("Length of parm[[5]] should be less than number of elements in the fifth level")
            selected.levels <- root.names.level.4[parm[[5]]]
            parm.path <- paste0(parm.path, ".", selected.levels)
            pdat <- data.frame(fifth_level = root.names.level.4)
            print(knitr::kable(pdat[parm[[5]],, drop = FALSE], row.names = TRUE))
          }
        }
        if(length(parm) >= 6){
          names.level.5 <- FALSE
          root.names.level.5 <- vapply(wlevel4$Children, FUN = function(x) x$Name,
                                       FUN.VALUE = "character")
          if(length(root.names.level.5) == 0){
            root.names.level.5 <- names(wlevel4)
            names.level.5 <- TRUE
          }
          if(length(parm[[6]]) == 1){
            if(parm[[6]] > 0){
              if(names.level.5){
                selected.names <- root.names.level.5[parm[[6]]]
                ##pdat <- data.frame(sixth_level = selected.names)
                ##print(knitr::kable(pdat, row.names = TRUE))
                parm.path <- paste0(parm.path, ".", selected.names)
              }else{
                wlevel5 <- try(apsimx_json$Children[[parm[[2]]]]$Children[[parm[[3]]]]$Children[[parm[[4]]]]$Children[[parm[[5]]]]$Children[[parm[[6]]]], silent = TRUE)
                if(inherits(wlevel5, "try-error"))
                  stop("parm[[6]] did not match an available node", call. = FALSE)
                parm.path <- paste0(parm.path, ".", root.names.level.5[parm[[6]]])
              }
            }else{
              pdat <- data.frame(sixth_level = root.names.level.5)
              print(knitr::kable(pdat, row.names = TRUE))
            }
          }else{
            ### Here I assume that parm[[6]] is a vector
            if(length(parm[[6]]) > length(root.names.level.5))
              stop("Length of parm[[6]] should be less than number of elements in the sixth level")
            selected.levels <- root.names.level.5[parm[[6]]]
            parm.path <- paste0(parm.path, ".", selected.levels)
            pdat <- data.frame(sixth_level = root.names.level.5)
            print(knitr::kable(pdat[parm[[6]],, drop = FALSE], row.names = TRUE))
          }
        }
        if(length(parm) >= 7)
          stop("Have not developed this yet", call. = FALSE)
      }else{
        ## This is if the parm is just one term
        parm.found <- FALSE
        if(length(parm) == 1){
          tparm1 <- try(inspect_apsimx_json(file = file, src.dir = src.dir, parm = parm[[1]]), silent = TRUE)
          if(!inherits(tparm1, 'try-error')){
            parm.path <- tparm1
            parm.found <- TRUE
          }else{
            stop("Parameter not found")
          }
        }
        #### node = 'Other' and list with characters ----
        if(!parm.found){
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
        }

        if(!parm.found){
          wcz <- grepl("Models.Core.Zone", parent.node)
          if(sum(wcz) < 0.5)
            stop("Core Simulation not found", call. = FALSE)
          core.zone.node <- parent.node[wcz][[1]]$Children
          parm.path.1 <- paste0(parm.path.0, ".", apsimx_json$Children[[fcsn]]$Name)
          parm.path.2 <- paste0(parm.path.1, ".", parent.node[wcz][[1]]$Name)
          tmp <- core.zone.node
          parm.path.2.1 <- parm.path.2
        }

        if(!parm.found){
          if(length(parm) > 1){
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
  }

  if(print.path && node != "Other" && node != "Operations"){
    if(!missing(parm)){
      if(length(parm) == 1){
        parm.path <- paste0(parm.path, ".", parm)
      }else{
        if(!is.na(position)){
          parm.path <- paste0(parm.path, ".", parm2)
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
#' @title Inspect an .apsimx or .json (JSON) file
#' @name inspect_apsimx_json
#' @description inspect an .apsimx or .json (JSON) file. It does not replace the GUI, but it can save time by quickly checking parameters and values.
#' @param file file ending in .apsimx or .json to be inspected (JSON)
#' @param src.dir directory containing the .apsimx or .json file to be inspected; defaults to the current working directory
#' @param parm string or regular expression for partial matching. It can be two strings separated by a period to search within a node (child).
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
    stop("You need to specify the parm argument", call. = FALSE)

  file.names.apsimx <- dir(path = src.dir, pattern = ".apsimx$", ignore.case = TRUE)
  file.names.json <- dir(path = src.dir, pattern = ".json$", ignore.case = TRUE)

  if(length(file.names.apsimx) == 0 && length(file.names.json) == 0){
    stop("There are no .json or .apsimx files in the specified directory to inspect.")
  }

  apsimx_json <- jsonlite::read_json(file.path(src.dir, file)) ### This is a list

  jsonpath <- "$"

  x <- apsimx_json

  ### This means that parm could be used as a jspath
  if(grepl(".", parm, fixed = TRUE)){
    pparm <- strsplit(parm, ".", fixed = TRUE)[[1]]
    if(pparm[1] == "") pparm <- pparm[2:length(pparm)]
    if(length(pparm) == 2){
      ## Find the 'root' to extract new x
      first.level.names <- sapply(X = x$Children, FUN = function(xlist) xlist$Name)
      wfirst <- grep(pparm[1], first.level.names)
      if(length(wfirst) > 1){
        stop(paste("More than one node found", paste(first.level.names, collapse = " ")), call. = FALSE)
      }else{
        jsonpath <- paste0(jsonpath, ".", x$Name)
        x <- x$Children[[wfirst]]
        parm <- pparm[2]
      }
    }
  }

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
#' @param how argument passed to \link{rapply}
#' @return It returns a list with the found object, the json path and the positions in the list.
#' @export
grep_json_list <- function(pattern, x, ignore.case = FALSE, search.depth = 10,
                           how = c("unlist", "replace", "list")){

  ## Check first
  hw <- match.arg(how)
  rar <- rapply(x, function(x) grep(pattern, x, ignore.case = ignore.case),
                how = hw)
  # rar2 <- rapply(x, function(x) grep(pattern, names(x), ignore.case = ignore.case))

  # if(length(rar) == 0L && length(rar2) == 0)
  #   stop("pattern not found")
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

#### Parse 'root' ----
parse_root <- function(root){
  ## Add flexibility to what can be passed as 'root'
  ## If root is a json path
  if(!all(sapply(root, is.character)))
    stop("'root' should be a character", call. = FALSE)
  if(length(root) == 1){
    if(grepl(".", root, fixed = TRUE)){
      ## Split the root
      proot <- strsplit(root, ".", fixed = TRUE)[[1]]
      wec <- which(proot == "") ## which empty character
      if(length(wec) > 0){
        root <- proot[-wec]
      }else{
        root <- proot
      }
    }
  }
  root
}
