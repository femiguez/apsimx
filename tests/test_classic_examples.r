## Write a test for running APSIM only under Windows
require(apsimx)
apsim_options(warn.versions = FALSE)

## This needs windows and APSIM Classic
run.classic.examples <- grepl("windows", Sys.info()[["sysname"]], ignore.case = TRUE) && get(".run.local.tests", envir = apsimx.options)

tmp.dir <- tempdir()
setwd(tmp.dir)

if(run.classic.examples){
  
  ade <- auto_detect_apsim_examples()
  
  ex <- list.files(path = ade, pattern = ".apsim$")
  ## Will only run a few
  ex.to.run <- c("Canopy","Centro","Millet","Potato","Sugar")
  
  for(i in ex.to.run){
    tmp <- apsim_example(i)
    cat("Ran (apsim_example):",i,"\n")
  }
  
  ## Test examples individually
  ## Note: this will not work unless tmp.dir is the current directory
  ## because of a bug in APSIM
  for(i in ex.to.run){
    file.copy(paste0(ade,"/",i,".apsim"), tmp.dir)
    tmp <- apsim(paste0(i,".apsim"), cleanup = TRUE)
    file.remove(paste0(tmp.dir,"/",i,".apsim"))
    cat("Ran (apsim):",i,"\n")
  }
}
