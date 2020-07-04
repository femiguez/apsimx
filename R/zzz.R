## Function to check whether mono (unix only) and APSIM-X are installed

.onAttach <- function(libname, pkgname){
  
  find.mono <- NULL
  
  find.apsim <- NULL
  ## For unix
  if(.Platform$OS.type == "unix"){
    ## Check for mono
    mono <- system("which mono", intern = TRUE)
    if(length(mono) == 0){
      find.mono <- "Mono framework not found \n"
      ## In Ubuntu mono seems to be in /usr/bin/mono
      ## In Mac mono seems to be installed in:
      ## "/Library/Frameworks/Mono.framework/Versions/Current/Commands/mono"
      ## In both of them the command 'mono' seems to work
    }
    
    ## This is for solaris or any other flavor of unix
    laf <- character(0)
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
          laf <- list.files("C:/PROGRA~1")
          laf2 <- list.files("C:/PROGRA~2")
      }
  }
  ## Check whether APSIM-X is detected
  if(!grepl("Windows", Sys.info()[["sysname"]])){
    find.apsimx <- laf[grepl("APSIM", laf, ignore.case = TRUE)]
  }else{
    ## This is the location for APSIM-X
    find.apsimx <- laf[grepl("APSIM", laf, ignore.case = TRUE)]
    ## This is the location for APSIM "Classic"
    find.apsim <- laf2[grepl("APSIM", laf2, ignore.case = TRUE)]
  }
  
  if(length(find.apsimx) > 0 || length(find.apsim) > 0){
    
    fax <- paste0("Found APSIM or APSIM-X")
    ## Won't print ApsimX at the moment but might change it in the future
    ## packageStartupMessage(fax)
    
  }else{
    
    apsim.not.found <- "APSIM(X) not found. \n 
             If APSIM(X) is installed in an alternative location, \n
            set paths manually using 'apsimx_options' or 'apsim_options'"
    
    if(!is.null(find.mono)) apsim.not.found <- c(find.mono, apsim.not.found)
    
    packageStartupMessage(apsim.not.found)
  }
}
