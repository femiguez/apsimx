
#' @title Obtain a synthetic APSIM soil profile from the World Modeler database
#' @description Retrieves soil data from the World Modeler global database and (optionally) saves it to a soils file
#' @name get_worldmodeler_soil_profile
#' @param lonlat Longitude and latitude vector (e.g. c(-93, 42)) or matrix.
#' @param fix whether to fix the soil profiles
#' @param check whether to check the soil profiles
#' @param soil.name optional soil name
#' @param wrt.dir optional directory where to save a file with \sQuote{soils} extension.
#' If missing it will be written to a temporary directory.
#' @param filename optional name to be used when saving the file
#' @param verbose verbose argument passed to read_apsim_soils
#' @param cleanup argument used to delete the file after download
#' @return it creates a file with extension \sQuote{soils}
#' @author Brian Collins (University of Southern Queensland) and Fernando Miguez
#' @export
#' @examples 
#' \dontrun{
#' ## Get soil profile properties for a single point
#' if(FALSE){
#'   sp1 <- get_worldmodeler_soil_profile(lonlat = c(-93, 42))
#'   plot(sp1[[1]], property = "Carbon")
#' }
#' }

get_worldmodeler_soil_profile <- function(lonlat, fix = FALSE, check = FALSE, 
                                          soil.name, wrt.dir, filename,
                                          verbose = FALSE, cleanup = FALSE){
  
  ## Create temporary directory 
  if(missing(wrt.dir)){
    wrt.dir <- tempdir()    
  }else{
    wrt.dir <- normalizePath(wrt.dir)
  }
  
  if(missing(filename)){
    filename <- "wm_temp_soils.soils"
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
  
  for(i in 1:nrow(lonlat)){
    x <- xml2::read_xml(paste0('https://worldmodel.csiro.au/apsimsoil?lon=', lonlat[i, 1], '&lat=',lonlat[i, 2]))
    xs <- xml2::xml_find_all(x, 'Soil')[[1]]
    xml2::xml_set_attr(xs, 'name', soil.name[i])
    xml2::xml_add_child(soil.node, xs, .copy = TRUE)
  } 

  ## Temporary file with soil 
  xml2::write_html(soil.node, file = file.path(wrt.dir, filename), options = c("format", "no_declaration")) 
  soil_profile <- read_apsim_soils(filename, src.dir = wrt.dir, verbose = verbose)
   
  if(fix){
    for(j in seq_len(nrow(lonlat))){
      soil_profile[[j]] <- fix_apsimx_soil_profile(soil_profile[[j]], verbose = verbose)
    }
  }

  if(check){
    for(j in seq_len(nrow(lonlat))){
      if(check) check_apsimx_soil_profile(soil_profile[[j]])
    }
  }

  if(cleanup) file.remove(file.path(wrt.dir, filename))

  return(soil_profile)
}