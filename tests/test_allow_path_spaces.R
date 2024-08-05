
library(apsimx)
packageVersion("apsimx")
apsimx_options(warn.versions = FALSE, allow.path.spaces = TRUE, warn.find.apsimx = FALSE)

run.apsimx.path.spaces <- get(".run.local.tests", envir = apsimx.options)

if(run.apsimx.path.spaces){
  
  #### Allow all examples to be run with spaces
  tmp.dir <- tempdir()
  ex.dir <- auto_detect_apsimx_examples()
  dir.create(file.path(tmp.dir, "examples from apsim"))
  
  examples <- c("AgPasture", "Barley", "Canola", "Chickpea", 
                "ControlledEnvironment", "Eucalyptus",
                "EucalyptusRotation",
                "Maize", "Mungbean",
                "Oats", "OilPalm", "Peanut", "Pinus", "Potato",
                "RedClover",
                "Rotation", "Sorghum",
                "Soybean", "Sugarcane", "Wheat", "WhiteClover")
  
  for(i in examples){
    if(!file.exists(file.path(tmp.dir, "examples from apsim", paste0(i, ".apsimx"))))
      file.copy(from = file.path(ex.dir, paste0(i, ".apsimx")), 
                to = file.path(tmp.dir, "examples from apsim", paste0(i, ".apsimx")))
  }
  ## dir(include.dirs = TRUE, recursive = TRUE)
  for(i in examples){
    file.to.run <- paste0(i, ".apsimx")
    src.dir <- file.path(tmp.dir, "examples from apsim")
    cat("Running:", file.to.run)
    atmp <- apsimx(file = file.to.run, src.dir = src.dir, simplify = FALSE)
    cat("... Done ... \n")
  }
}