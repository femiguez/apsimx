
#' @title Generate a synthetic APSIM soil profile from the World Modeler database
#' @description Retrieves soil data from the World Modeler global database and converts it to an apsimx soil_profile object
#' @name get_worldmodeler_soil_profile
#' @param lonlat Longitude and latitude vector (e.g. c(-93, 42)) or matrix.
#' @param fix whether to attempt to fix issues with the profile
#' @param check whether to check the soil profile
#' @param soil.name optional soil name
#' @param verbose default is FALSE
#' @param cleanup whether to delete temporary files
#' @return it generates an object of class \sQuote{soil_profile}.
#' @author Brian Collins (University of Southern Queensland) and Fernando Miguez
#' @examples 
#' \dontrun{
#' ## Get soil profile properties for a single point
#' ## This causes a fatal error at the moment
#' ## This is why I don't run it
#' if(FALSE){
#'   sp1 <- get_worldmodeler_soil_profile(lonlat = c(-93, 42), fix = TRUE, verbose = FALSE)
#' 
#'   ## Visualize
#'   plot(sp1)
#'   plot(sp1, property = "water")
#' }
#' }

get_worldmodeler_soil_profile <- function(lonlat, fix = FALSE, check = FALSE, 
                                          soil.name, 
                                          verbose = FALSE, cleanup = FALSE){
  
  ## Create temporary directory 
  tmpdir <- tempdir()
  
  if(inherits(lonlat, "numeric")){
    lonlat <- matrix(lonlat, nrow = 1)
  }
  
  if(missing(soil.name)){
    soil.name <- paste("SoilName", seq_len(nrow(lonlat)), sep = "_")
  }
  
  ## Lonlat will be a matrix from now on
  soil.node <- xml2::read_xml('<folder version="37" name = "Soils"></folder>')
  
  for(i in 1:nrow(lonlat)){
    x <- xml2::read_xml(paste0('https://worldmodel.csiro.au/apsimsoil?lon=', lonlat[i, 1], '&lat=',lonlat[i, 2]))
    x <- xml2::xml_find_all(x, 'Soil')[[1]]
    xml2::xml_set_attr(x, 'name', soil.name[i])
    xml2::xml_add_child(soil.node, x, .copy=F)
  } 

  ## Temporary file with soil 
  xml2::write_html(soil.node, file = file.path(tmpdir, 'temps.soils')) 
  soil_profile <- read_apsim_soils("temps.soils", src.dir = tmpdir, verbose = verbose)
  
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
  
  if(cleanup) unlink(tmpdir)
  
  return(soil_profile)
}