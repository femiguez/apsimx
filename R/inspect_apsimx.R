
#' Similar structure to the 'edit_apsimx' file but aimed at a simple way
#' of inspecting a file
#' 
#' @title Inspect an .apsimx file (XML or JSON)
#' @name inspect_apsimx
#' @description inspect either an XML or JSON apsimx file. It does not replace the GUI, but it can save time by quickly checking parameters and values.
#' @param file file ending in .apsimx to be inspected
#' @param src.dir directory containing the .apsimx file to be inspected; defaults to the current working directory
#' @param node either 'Clock', 'Weather', 'Soil', 'SurfaceOrganicMatter', 'MicroClimate', 'Crop', 'Manager' or 'Other'
#' @param soil.child specific soil component to be inspected. For example, 'Water' or 'Nitrogen'.
#' @param som.child specific soil organic matter component to be inspected ('Pools' or 'Other')
#' @param parm parameter to inspect when node = 'Other' (XML only)
#' @param digits number of decimals to print
#' @details The inspection has many elements that are hard coded, it is not a literal inspection tool for all aspects of the file. For this use a text editor.
#' @return table with inspected parameters and values
#' @export
#' @note node 'Manager' can be complicated and it is not guranteed to work. First version of a file which supports
#' both XML and JSON files. This is simply a script that prints the relevant parameters which are likely to need editing. It does not print all information from an .apsimx file.
#' @examples 
#' \dontrun{
#' ex.dir <- auto_detect_apsimx_examples()
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Clock")        
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Weather")        
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Soil", soil.child = "Water")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Soil", soil.child = "Nitrogen")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Soil", soil.child = "OrganicMatter")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Soil", soil.child = "Analysis")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Soil", soil.child = "InitialWater")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Soil", soil.child = "Sample")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "SurfaceOrganicMatter", som.child = "Pools")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "SurfaceOrganicMatter", som.child = "Other")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "MicroClimate")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Crop")
#' inspect_apsimx("Maize", src.dir = ex.dir, node = "Manager")
#' }
#' 

inspect_apsimx <- function(file = "", src.dir = ".", 
                           node = c("Clock","Weather","Soil","SurfaceOrganicMatter",
                                    "MicroClimate","Crop","Manager"),
                           soil.child = c("Water","Nitrogen","OrganicMatter",
                                          "Analysis","InitialWater","Sample"),
                           som.child = c("Pools","Other"),
                           parm = NULL,
                           digits = 3){
  
  fileNames <- dir(path = src.dir, pattern=".apsimx$",ignore.case=TRUE)
  
  if(length(fileNames)==0){
    stop("There are no .apsimx files in the specified directory to inspect.")
  }
  
  node <- match.arg(node)
  soil.child <- match.arg(soil.child)
  som.child <- match.arg(som.child)
  
  ## This matches the specified file from a list of files
  ## Notice that the .apsimx extension will be added here
  file <- match.arg(file, fileNames, several.ok=FALSE)
  
  apsimx_ft <- apsimx_filetype(file = file, src.dir = src.dir)
  ## Inspect XML file type
  if(apsimx_ft == "xml"){
    inspect_apsimx_xml(file = file, src.dir = src.dir, node = node,
                       soil.child = soil.child, som.child = som.child, 
                       parm = parm, digits = digits)
  }
  
  ## Inspect JSON file type
  if(apsimx_ft == "json"){
    inspect_apsimx_json(file = file, src.dir = src.dir, node = node,
                        soil.child = soil.child, som.child = som.child, 
                        digits = digits) 
  }
  
  if(apsimx_ft == "unknown"){
    stop("file type unknown")
  }
} 

