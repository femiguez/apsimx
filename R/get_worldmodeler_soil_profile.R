
#' @title Generate a synthetic APSIM soil profile from the World Modeler database
#' @description Retrieves soil data from the World Modeler global database and converts it to an apsimx soil_profile object
#' @name get_worldmodeler_soil_profile
#' @param lonlat Longitude and latitude vector (e.g. c(-93, 42)) or matrix.
#' @param soil.name optional soil name
#' @param verbose default is FALSE
#' @param cleanup whether to delete temporary files
#' @return it generates an object of class \sQuote{soil_profile}.
#' @author Brian Collins (University of Southern Queensland) and Fernando Miguez
#' @export
#' @examples 
#' \dontrun{
#' ## Get soil profile properties for a single point
#' sp1 <- get_worldmodel_soil_profile(lonlat = c(-93, 42), fix = TRUE, verbose = FALSE)
#' 
#' ## Visualize
#' plot(sp1)
#' plot(sp1, property = "water")
#' 
#' }

get_worldmodeler_soil_profile <- function(lonlat, fix = TRUE, check = TRUE, 
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
    x <- xml2::read_xml(sprintf('https://worldmodel.csiro.au/apsimsoil?lon=%s&lat=%s', lonlat[i, 1], lonlat[i, 2]))
    x <- xml2::xml_find_all(x, 'Soil')[[1]]
    xml2::xml_set_attr(x, 'name', soil.name[i])
    xml2::xml_add_child(soil.node, x, .copy=F)
  } 

  ## Temporary file with soil 
  xml2::write_html(soil.node, file = file.path(tmpdir, 'temps.soils')) 
  soil_profile <- read_apsim_soils("temps.soils", src.dir = tmpdir, verbose = verbose)
  
  if(fix) soil_profile <- fix_apsimx_soil_profile(soil_profile, verbose = verbose)
  
  if(check) check_apsimx_soil_profile(soil_profile)
  
  if(cleanup) unlink(tmpdir)
  
  return(soil_profile)
}