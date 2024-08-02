## Test reading and writing soils in XML format
## Author: Fernando E. Miguez
## Date: 2022-02-16
##
## Required: R package apsimx 2.3.2
require(apsimx)
require(ggplot2)

apsimx_options(warn.versions = FALSE)

run.apsimx.soils <- get(".run.local.tests", envir = apsimx.options)

run.apsimx.soils <- FALSE ## It takes 23 minutes to run the code below

if(run.apsimx.soils){
  
  extd.dir <- system.file("extdata", package = "apsimx")
  
  sls <- read_apsim_soils("Clarion.soils", src.dir = extd.dir)
  
  ## All soils in APSoil
  apsoil.dir <- "~/Dropbox/apsimx-other/APSoil"
  
  ## Reading in all this data takes a while
  start <- Sys.time()
  asls <- read_apsim_soils("APSRU-Australia-soils.soils", src.dir = apsoil.dir)
  end <- Sys.time()
  
}

### Testing fixing a soil profile

run.apsimx.soils.fix <- get(".run.local.tests", envir = apsimx.options)

try.isric <- try(get_isric_soil_profile(lonlat = c(-93, 42), fix = TRUE, verbose = FALSE), silent = TRUE)

## If the above fails it is very likely that the server is down
if(inherits(try.isric, 'try-error')) run.apsimx.soils.fix <- FALSE
run.apsimx.soils.fix <- FALSE

if(run.apsimx.soils.fix){
  
  tmp.dir <- tempdir()
  setwd(tmp.dir)
  ex.dir <- auto_detect_apsimx_examples()
  
  file.copy(file.path(ex.dir, "Wheat.apsimx"), ".")  
  
  lon <- seq(1, 2, length.out = 30)
  lat <- seq(48, 49, length.out = 30)
  
  for(i in seq_along(lon)){
    
    sp <- try(get_isric_soil_profile(lonlat = c(lon[i], lat[i]), fix = TRUE, verbose = FALSE), silent = TRUE)
    if(inherits(sp, "try-error")){
      cat("No soil for this location. Lon:", lon[i], "Lat:", lat[i], "\n")
      next
    }
    
    edit_apsimx_replace_soil_profile("Wheat.apsimx", soil.profile = sp,
                                     edit.tag = paste0("-", i))
    sim1 <- try(apsimx(paste0("Wheat-", i, ".apsimx")), silent = TRUE)
    
    if(inherits(sim1, "try-error"))
      cat("Simulation:", i, "failed\n")
  }
}


