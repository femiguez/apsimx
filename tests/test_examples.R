## Run a few tests for the examples
require(apsimx)
apsimx_options(warn.versions = FALSE)

run.example.tests <- TRUE

ex.dir <- auto_detect_apsimx_examples()

examples <- c("Barley","ControlledEnvironment","Eucalyptus",
              "EucalyptusRotation",
              "Maize","Oats","Sugarcane","Wheat")

if(run.example.tests){
  start <- Sys.time()
  for(i in examples){
    ex.tst <- apsimx_example(i)
    cat("Ran: ", i, "\n")
  }
  end <- Sys.time()
  cat("Total time:", end - start, "\n")
    
  cat("APSIM-X version:", as.vector(apsim_version(which = "inuse")[[2]]),"\n")

}
