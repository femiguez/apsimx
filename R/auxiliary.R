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

## Print APSIM-X version currently in use
apsimx_version <- function(){
  ## Just return the version number
  
  fev <- function(x) as.numeric(strsplit(x, ".", fixed = TRUE)[[1]][4])
  
  if(!is.na(apsimx::apsimx.options$exe.path)){
    ## Attempt to recover the number
    apsimx.version <- fev(apsimx::apsimx.options$exe.path)
    ## If this fails the full exe path is returned
    if(is.na(apsimx.version)){
      apsimx.version <- apsimx::apsimx.options$exe.path
    }
  }
  
  if(.Platform$OS.type == "unix"){
    
    if(grepl("Darwin", Sys.info()[["sysname"]])){
      laf <- list.files("/Applications/")
      find.apsim <- grep("APSIM",laf)
      if(length(find.apsim) == 0) stop("APSIM-X not found")
      apsimx.versions <- sapply(laf[find.apsim],fev)
      apsimx.version <- sort(apsimx.versions, decreasing = TRUE)[1]
    }
    
    if(grepl("Linux", Sys.info()[["sysname"]])){
      
      find.apsim <- grep("apsim", list.files("/usr/local/lib"))
      if(length(find.apsim) == 0) stop("APSIM-X not found")
      
      apsimx.versions <- list.files("/usr/local/lib/apsim")
      versions <- sapply(apsimx.versions, fev)
      apsimx.version <- sort(versions, decreasing = TRUE)[1]
    }
  }
  
  if(.Platform$OS.type == "windows"){
    st1 <- "C:/PROGRA~1/"
    laf <- list.files(st1)
    find.apsim <- grep("APSIM",laf)
    if(length(find.apsim) == 0) stop("APSIM-X not found")
    apsimx.versions <- laf[find.apsim]
    versions <- sapply(apsimx.versions, fev)
    apsimx.version <- sort(versions, decreasing = TRUE)[1]
  }
  return(as.vector(apsimx.version))
}