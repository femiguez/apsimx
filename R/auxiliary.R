## Auxiliary undocumented functions

exclude <- function(x, names){
  tmp <- which(x %in% names)
  ans <- x[-tmp]
  ans
}

## Find output file names
find_output_names <- function(file, src.dir = ""){
  
  apsim_xml <- read_xml(paste0(src.dir,"/",file))
  
  find.output <- xml_find_all(apsim_xml, ".//outputfile/filename")
  
  ans <- xml_text(find.output)
  
  return(ans)
}