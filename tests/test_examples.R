## Run a few tests for the examples
require(apsimx)
apsimx_options(warn.versions = FALSE)

run.example.tests <- get(".run.local.tests", envir = apsimx.options)

examples <- c("Barley","ControlledEnvironment","Eucalyptus",
              "EucalyptusRotation",
              "Maize","Oats","Sugarcane","Wheat")

tmp.dir <- tempdir()

setwd(tmp.dir)

if(run.example.tests){
  
  ex.dir <- auto_detect_apsimx_examples()
  
  start <- Sys.time()
  for(i in examples){
    ex.tst <- apsimx_example(i)
    cat("Ran: ", i, "\n")
  }
  end <- Sys.time()
  cat("Total time:", end - start, "\n")
    
  if(.Platform$OS.type == "unix"){
    cat("APSIM-X version:", apsim_version(which = "inuse"),"\n")
  }else{
    cat("APSIM-X version:", as.vector(apsim_version(which = "inuse")[[2]]),"\n")
  }

}
