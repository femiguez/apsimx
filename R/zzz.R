## Function to check whether mono and APSIM-X are installed

.onLoad <- function(libname, pkgname){
  
  ## For unix
  if(.Platform$OS.type == "unix"){
    ## Check for mono
    mono <- system("which mono", intern = TRUE)
    if(length(mono) == 0){
      warning("Mono framework not found")
      ## In Ubuntu mono seems to be in /usr/bin/mono
      ## In Mac mono seems to be installed in:
      ## "/Library/Frameworks/Mono.framework/Versions/Current/Commands/mono"
      ## In both of them the command 'mono' seems to work
    }
    
    ## Check whether we find APSIM-X in Mac
    if(grepl("Darwin", Sys.info()[["sysname"]])){
      laf <- list.files("/Applications/")
      apsimx <- laf[grepl("APSIM", laf)]
      if(length(apsimx) == 0){
        warning("APSIM-X not found")
      }
    }
    ## Check whether we find APSIM-X in Linux (Debian)
    if(grepl("Linux", Sys.info()[["sysname"]])){
      local.lib <- "/usr/local/lib"
      laf <- list.files(local.lib)
      if(!grepl("apsim", laf)){
        warning("APSIM-X not found")
      }
    }
  }
    
  if(grepl("Windows", Sys.info()[["sysname"]])){
      local.lib <- "c:/Program Files"
      laf <- list.files(local.lib)
      if(!grepl("apsim", laf, ignore.case = TRUE)){
        warning("APSIM-X not found")
    }
  }
}