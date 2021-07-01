## Example simulation for a point using APSIM
## Author: Fernando E. Miguez
## Date: 2021-06-08
##
## Required: R package apsimx 1.992
require(apsimx)
require(ggplot2)

apsimx_options(warn.versions = FALSE)

run.apsimx.point.sim <- get(".run.local.tests", envir = apsimx.options)

tmpd <- tempdir()
## tmpd <- "../tests"
## setwd(tmpd)

if(run.apsimx.point.sim && FALSE){

  site.name <- "ARK1"
  site.coords <- c(-90.75973, 34.72834)
  
  ## Copy Maize from Examples 
  exdir <- auto_detect_apsimx_examples()
  file.copy(file.path(exdir, "Maize.apsimx"), tmpd)
  
  ## Get weather from NASAPOWER
  ## Important: this is just an example. I recommend looking into
  ## whether this is a good source of weather data for this location.
  ## This takes a few seconds
  pmet <- get_power_apsim_met(site.coords, 
                              dates = c("2000-01-01", "2020-12-31"))
  check_apsim_met(pmet) ## Some missing values for solar radiation
  pmet <- impute_apsim_met(pmet) ## Automatically impute 
  check_apsim_met(pmet) ## Now it is okay
  write_apsim_met(pmet, wrt.dir = ".", filename = paste0(site.name, ".met")) ## Write to file
  
  ## Edit the filename for weather
  edit_apsimx("Maize.apsimx", node = "Weather",
              value = paste0(site.name, ".met"),
              overwrite = TRUE)
  
  inspect_apsimx("Maize.apsimx", node = "Weather")
  
  ## Also change the clock
  edit_apsimx("Maize.apsimx", node = "Clock",
              parm = c("Start", "End"),
              value = c("2000-01-01T00:00:00", "2020-12-31T00:00:00"),
              overwrite = TRUE)
  
  inspect_apsimx("Maize.apsimx")
  
  ## Change planting and harvest date
  edit_apsimx("Maize.apsimx", node = "Crop", 
              parm = "StartDate", value = "1-may", 
              overwrite = TRUE)
  
  edit_apsimx("Maize.apsimx", node = "Crop", 
              parm = "EndDate", value = "1-oct", 
              overwrite = TRUE)

  inspect_apsimx("Maize.apsimx", node = "Crop")
  
  sim0 <- apsimx("Maize.apsimx")
  
  ## Results with default soil
  ggplot(sim0, aes(x = Date, y = Maize.AboveGround.Wt)) + geom_point()
  
  ## Now let's get the soil data from SSURGO
  sp0 <- get_ssurgo_soil_profile(site.coords)
  
  ## Very important: these are just 'template' soil profiles from 
  ## where to start. These need to be checked and if you have any site 
  ## specific information you should use it
  plot(sp0[[1]]) ## Visualize properties
  plot(sp0[[1]], property = "water")
  ## You might want to use the APSIM-X GUI in addition
  
  ## Also, here you can modify the metadata, since it is a list
  sp0[[1]]$metadata$NearestTown <- "Marianna"
  ## You might want to populate the soilwat or swim options
  ## using the helper functions soilwat_parms or swim_parms
  
  edit_apsimx_replace_soil_profile("Maize.apsimx",
                                   soil.profile = sp0[[1]],
                                   overwrite = TRUE)  

  sim1 <- apsimx("Maize.apsimx")  
  
  ## Results with ssurgo soil
  ggplot(sim1, aes(x = Date, y = Maize.AboveGround.Wt)) + geom_point()
  
  cmp <- compare_apsim(sim0, sim1, variable = "Maize.AboveGround.Wt")
  plot(cmp, variable = "Maize.AboveGround.Wt") ## similar but not identical
  
  ## There is no site information
  inspect_apsimx("Maize.apsimx", node = "Soil", parm = "Site")
  ## However, the nearest town was edited as expected
  inspect_apsimx("Maize.apsimx", node = "Soil", parm = "NearestTown")
  
  ## If we still want to edit the Site
  edit_apsimx("Maize.apsimx", node = "Soil", 
              parm = "Site", value = "AK1", 
              overwrite = TRUE)
  
  inspect_apsimx("Maize.apsimx", node = "Soil")
}
