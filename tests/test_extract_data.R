#### Testing extract data function
require(apsimx)
packageVersion("apsimx")
apsimx_options(warn.versions = FALSE)

run.extract.tests <- get(".run.local.tests", envir = apsimx.options)

if(run.extract.tests){
  
  tmp.dir <- tempdir()
  dir(tmp.dir)
  ex.dir <- auto_detect_apsimx_examples()
  
  ### Test with all examples
  ex.dir.list <- dir(ex.dir, recursive = FALSE, pattern = "apsimx$")
  
  ## ex.dir.list2 <- ex.dir.list[c(2,4,5,7:11,13,16:20,22:24,28:30,32:35)]
  ## Excluded: AgPasture, BiomassRemovalFromPlant, Chickpea, Factorial, Grapevine
  ##           Graph, Pinus, Rotation, SCRUM, SimpleGrazing, Stock  
  ## Trying node = "Clock" - default
  
  ex.dir.list2 <- c("Barley.apsimx", "Canola.apsimx", "CanolaGrazing.apsimx",
                    "Chicory.apsimx", "ControlledEnvironment.apsimx", "CsvWeather.apsimx",
                    "Eucalyptus.apsimx", "EucalyptusRotation.apsimx", "FodderBeet.apsimx",
                    "Maize.apsimx", "Mungbean.apsimx", "Oats.apsimx",  "OilPalm.apsimx",
                    "Peanut.apsimx", "PlantainForage.apsimx", "Potato.apsimx", "RedClover.apsimx",
                    "Slurp.apsimx", "Sorghum.apsimx", "Soybean.apsimx", "Sugarcane.apsimx",
                    "SWIM.apsimx",  "Wheat.apsimx", "WhiteClover.apsimx") 
  
  for(i in ex.dir.list2){
    if(!file.exists(file.path(tmp.dir, i))) file.copy(from = file.path(ex.dir, i), to = tmp.dir)  
    cat("Simulation:", i)
    (edf <- extract_data_apsimx(i, src.dir = tmp.dir))  
    cat("\n")
  }
  
  ## ex.dir.list3 <- ex.dir.list[c(1, 3, 6, 12, 14, 15, 21, 25, 26, 27, 31)]
  ## Number 27: SimpleGrazing is a mess
  ## ex.dir.list3 <- ex.dir.list[c(1, 3, 6, 12, 14, 15, 21, 25, 26, 31)]
  
  ex.dir.list3 <- c("AgPasture.apsimx", "BiomassRemovalFromPlant.apsimx",
                    "Chickpea.apsimx", "Factorial.apsimx",   
                    "Pinus.apsimx", "Rotation.apsimx",  
                    "SCRUM.apsimx")   
  
  for(i in ex.dir.list3){
    if(!file.exists(file.path(tmp.dir, i))) file.copy(from = file.path(ex.dir, i), to = tmp.dir)  
    cat("Simulation:", i, "\n")
    next
    ## inspect_apsimx(i, src.dir = tmp.dir, node = "Other")
    if(i == "AgPasture.apsimx")  (edf.agp <- extract_data_apsimx(i, src.dir = tmp.dir, root = "AgPastureExample"))  
    if(i == "BiomassRemovalFromPlant.apsimx") (edf.brfp <- extract_data_apsimx(i, src.dir = tmp.dir, root = "SendingDatesFromOpperations"))  
    if(i == "Chickpea.apsimx") (edf.chp <- extract_data_apsimx(i, src.dir = tmp.dir, root = list("Continuous_TOS", "Cont_TOS")))  
    if(i == "Factorial.apsimx") (edf.fct <- extract_data_apsimx(i, src.dir = tmp.dir, root = list("RangeExperiment", "Base1")))  
    if(i == "Pinus.apsimx") (edf.pns <- extract_data_apsimx(i, src.dir = tmp.dir, root = list("Plantation_IxF_Experiment", "Treatment", "Base")))  
    if(i == "Rotation.apsimx") (edf.rot <- extract_data_apsimx(i, src.dir = tmp.dir, root = list("Mar", "Mar")))
    if(i == "Rotation.apsimx") (edf.rot2 <- extract_data_apsimx(i, src.dir = tmp.dir, root = "Mar.Mar"))
    if(i == "SCRUM.apsimx") (edf.scrm <- extract_data_apsimx(i, src.dir = tmp.dir, root = "Crop Comparisions.CropCompBase"))
    cat("\n")
  }
}

### Testing extraction of initialwater and Solutes ----

if(run.extract.tests){
  
  ex.dir <- auto_detect_apsimx_examples()
  
  ex.dir.list <- dir(ex.dir, recursive = FALSE, pattern = "apsimx$")
  ### Only select the simulations which do not have multiple simulations
  
  ex.dir.list4 <- ex.dir.list[c(2, 16, 17, 18, 24, 29, 30, 34)]
  
  ex.dir.list4 <- c("Barley.apsimx", "Maize.apsimx", "Mungbean.apsimx", "Oats.apsimx",     
                    "RedClover.apsimx", "Sorghum.apsimx", "Soybean.apsimx", "Wheat.apsimx")
  
  for(i in ex.dir.list4){
    (edf.iw <- extract_data_apsimx(i, src.dir = ex.dir, node = "Soil",
                                  soil.child = "InitialWater"))
    
    (edf.sol <- extract_data_apsimx(i, src.dir = ex.dir, node = "Soil",
                                   soil.child = "Solute"))  
    
    (edf.sol.NO3 <- extract_data_apsimx(i, src.dir = ex.dir, node = "Soil",
                                       soil.child = "Solute", parm = "NO3"))    
  }
}

### Testing whether operations can be extracted
if(FALSE){
  
  inspect_apsimx("FodderBeet.apsimx", ex.dir, node = "Other", parm = 2)
  
  inspect_apsimx("FodderBeet.apsimx", ex.dir, node = "Other", 
                 parm = list(1, 3, 8, 4))

  tmpp.dir <- "~/Dropbox/apsimx-other-issues/matteolongo/fodderbeet_quadr"
  setwd(tmpp.dir)
  dir()
  inspect_apsimx("fodderbeet_quadr.apsimx", src.dir = tmpp.dir, node = "Other", parm = 3)
  inspect_apsimx("fodderbeet_quadr.apsimx", src.dir = tmpp.dir, node = "Operations", 
                 root = "quadr_0_l_fodderbeet")
  edfo <- extract_data_apsimx("fodderbeet_quadr.apsimx", src.dir = tmpp.dir, node = "Operations", 
                              root = "quadr_0_l_fodderbeet")
}
