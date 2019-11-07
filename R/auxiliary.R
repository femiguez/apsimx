## Auxiliary undocumented functions

## Find output file names
find_output_names <- function(file, src.dir = "."){
  
  apsim_xml <- read_xml(paste0(src.dir,"/",file))
  
  find.output <- xml_find_all(apsim_xml, ".//outputfile/filename")
  
  ans <- xml_text(find.output)
  
  return(ans)
}

## Check the name 

check_apsim_name <- function(file){
  
  ## No spaces are allowed, provide informative error
  if(grepl("\\s", file))
    stop("White spaces are not allowed in file names")
  
}
