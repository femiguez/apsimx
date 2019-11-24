## Auxiliary undocumented functions

## Find output file names
find_output_names <- function(file, src.dir = "."){
  
  apsim_xml <- read_xml(paste0(src.dir,"/",file))
  
  find.output <- xml_find_all(apsim_xml, ".//outputfile/filename")
  
  ans <- xml_text(find.output)
  
  return(ans)
}

## Check the name and disallow white spaces 
check_apsim_name <- function(file){
  
  ## No spaces are allowed, provide informative error
  if(grepl("\\s", file))
    stop("White spaces are not allowed in file names")
  
}

## This is for when a list has only one element
## But inside that element there is a children with multiple
## elements, or at least is not null
extract_bad_children <- function(x){
  
  if(!is.list(x)) stop("x should be a list")
  
  lnms <- names(x)
  nco <- is.null(x$Children)
  
  if(length(x) == 1 && lnms == 0 && !nco){
    x <- x$Children[[1]]
  }
  return(x)
}
