
#' @title Obtain a synthetic APSIM soil profile from the World Modeler database
#' @description Retrieves soil data from the World Modeler global database and (optionally) saves it to a soils file
#' @name get_worldmodeler_soil_profile
#' @param lonlat Longitude and latitude vector (e.g. c(-93, 42)) or matrix.
#' @param soil.name optional soil name
#' @param wrt.dir optional directory where to save a file with \sQuote{soils} extension.
#' If missing it will be written to a temporary directory.
#' @param filename optional name to be used when saving the file
#' @param verbose verbose argument passed to \sQuote{read_apsim_soils}
#' @return it returns a list with objects of class \sQuote{soil_profile}. If
#' \sQuote{filename} is specified it also creates a file with extension \sQuote{soils}, 
#' which can be read using function \code{\link{read_apsim_soils}}.
#' @author Brian Collins (University of Southern Queensland) and Fernando Miguez
#' @export
#' @examples 
#' \dontrun{
#' ## Get soil profile properties for a single point
#'   sp1 <- get_worldmodeler_soil_profile(lonlat = c(-93, 42))
#'   
#'   if(inherits(sp1[[1]], 'soil_profile')){
#'     plot(sp1[[1]], property = "Carbon")
#'   }
#' }

get_worldmodeler_soil_profile <- function(lonlat, soil.name, wrt.dir, filename, verbose = FALSE){
  
  ## Create temporary directory 
  if(missing(wrt.dir)){
    wrt.dir <- tempdir()    
  }else{
    wrt.dir <- normalizePath(wrt.dir)
  }
  
  cleanup <- FALSE
  
  if(missing(filename)){
    filename <- "wm_temp_soils.soils"
    cleanup <- TRUE
  }else{
    if(tools::file_ext(filename) != "soils")
      stop("'filename' should have a '.soils' extention", call. = FALSE)
  }

  if(inherits(lonlat, "numeric")){
    lonlat <- matrix(lonlat, nrow = 1)
  }
  
  if(missing(soil.name)){
    soil.name <- paste("SoilName", seq_len(nrow(lonlat)), sep = "_")
  }
  
  ## Lonlat will be a matrix from now on
  soil.node <- xml2::read_xml('<soils version="37"></soils>')
  
  for(i in seq_len(nrow(lonlat))){
    x <- try(xml2::read_xml(paste0('https://worldmodel.csiro.au/apsimsoil?lon=', lonlat[i, 1], '&lat=',lonlat[i, 2])), silent = TRUE)
    if(!inherits(x, 'try-error')){
      xs <- xml2::xml_find_all(x, 'Soil')[[1]]
      xml2::xml_set_attr(xs, 'name', soil.name[i])
      xml2::xml_add_child(soil.node, xs, .copy = TRUE)      
    }else{
      warning(paste("Could not get a soil for row", i), immediate. = TRUE)
    }
  }
  
  xml2::write_html(soil.node, file = file.path(wrt.dir, filename), options = c("format", "no_declaration")) 
  res <- read_apsim_soils(filename, src.dir = wrt.dir, verbose = verbose)
  
  if(cleanup) file.remove(file.path(wrt.dir, filename)) 
  
  return(res)
}

#### Get WM APSIM met ----
#' @title Obtain a weather APSIM met from the World Modeler database
#' @description Retrieves met data from the World Modeler global database and (optionally) saves it to a file
#' @name get_worldmodeler_apsim_met
#' @param lonlat Longitude and latitude vector (e.g. c(-93, 42)) or matrix.
#' @param dates date range (see example for format)
#' @param wrt.dir optional directory where to save a file with \sQuote{met} extension.
#' If missing it will be written to a temporary directory.
#' @param filenames optional name(s) to be used when saving the file. It should be equal to the
#' number of rows of the input matrix.
#' @param check whether to check the met file
#' @param verbose argument passed to read_apsim_met
#' @return it creates a list with objects of class \sQuote{met}. 
#' If it fails, then the objects will be of class \sQuote{try-error}.
#' @export
#' @examples 
#' \dontrun{
#' ## Get soil profile properties for a single point
#'   am1 <- get_worldmodeler_apsim_met(lonlat = c(-93, 42), 
#'                                     dates = c("2010-01-01", "2013-12-31"))
#'   if(inherits(am1, 'met')){
#'      plot(am1[[1]], met.var = "rain", cumulative = TRUE)
#'   }
#'  }
#' 

get_worldmodeler_apsim_met <- function(lonlat, dates, wrt.dir, filenames, check = FALSE, verbose = FALSE){
  
  ## Create temporary directory 
  if(missing(wrt.dir)){
    wrt.dir <- tempdir()    
  }else{
    wrt.dir <- normalizePath(wrt.dir)
  }
  
  if(inherits(lonlat, "numeric")){
    lonlat <- matrix(lonlat, nrow = 1)
  }

  cleanup <- FALSE
  
  if(missing(filenames)){
    filenames <- paste0("wm_apsim_met", seq_len(nrow(lonlat)), ".met")
    cleanup <- TRUE
  }else{
    for(i in seq_along(filenames)){
      if(tools::file_ext(filenames[i]) != "met")
        stop("'filenames' should have a '.met' extention", call. = FALSE)      
    }
  } 
 
  if(length(filenames) != nrow(lonlat))
    stop("'filenames' length should be equal to the number of rows in the input matrix", call. = FALSE)
  
  res <- vector('list', length = nrow(lonlat))
  for(i in seq_len(nrow(lonlat))){
    url1 <- 'https://worldmodel.csiro.au/gclimate?lat='
    url2 <- paste0(lonlat[i, 2], '&lon=', lonlat[i, 1], '&')
    url3 <- 'format=apsim&start='
    url4 <- gsub("-", "", dates[1], fixed = TRUE)
    url5 <- '&stop='
    url6 <- gsub("-", "", dates[2], fixed = TRUE)
    x <- try(readLines(paste0(url1, url2, url3, url4, url5, url6)), silent = TRUE)
    if(!inherits(x, 'try-error')){
      writeLines(x, file.path(wrt.dir, filenames[i]))
      res[[i]] <- read_apsim_met(filenames[i], src.dir = wrt.dir, verbose = verbose)
      if(check){
        cat("Checking lonlat", lonlat[i, ], "\n")
        check_apsim_met(res[[i]])
      }      
    }else{
      if(verbose)
        warning(paste("Could not get met file for row", i), immediate. = TRUE)
     res[[i]] <- x 
    }
  }  
  
  ## If filenames is missing I will cleanup
  if(cleanup){
    for(i in seq_len(nrow(lonlat))){
      file.remove(file.path(wrt.dir, filenames[i]))  
    }
  } 
  
  return(res)
}
