## Function to check whether mono (unix only) and APSIM-X are installed

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
    
    ## List of application files in Darwin (Mac)
    if(grepl("Darwin", Sys.info()[["sysname"]])){
      laf <- list.files("/Applications/")
    }
    ## Linux (Debian)
    if(grepl("Linux", Sys.info()[["sysname"]])){
      laf <- list.files("/usr/local/lib")
    }
  }else{
        if(grepl("Windows", Sys.info()[["sysname"]])){
          laf <- list.files("c:/PROGRA~1")
      }
  }
  ## Check whether APSIM-X is detected
  find.apsimx <- laf[grepl("APSIM", laf, ignore.case = TRUE)]
  if(length(find.apsimx) > 0){
    cat("Found: ", find.apsimx,"\n")
  }else{
    warning("APSIM-X not found. \n 
             If APSIM-X is installed in an alternative location, \n
            set paths manually using 'apsimx_options'")
  }
}
