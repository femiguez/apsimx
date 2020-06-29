## This is my way of doing this now
require(apsimx)
apsimx_options(warn.versions = FALSE)

extd.dir <- system.file("extdata", package = "apsimx")

run.test.edit.apsimx.batch <- get(".run.local.tests", envir = apsimx.options)

tmp.dir <- tempdir()

if(run.test.edit.apsimx.batch){

  file.copy(paste0(extd.dir,"/","Wheat.apsimx"), tmp.dir)

  inspect_apsimx("Wheat.apsimx", src.dir = tmp.dir, node = "SurfaceOrganicMatter")

  edit1time <- system.time(edit_apsimx("Wheat.apsimx", src.dir = tmp.dir,
                                       wrt.dir = tmp.dir,
                                       node = "SurfaceOrganicMatter", 
                                       parm = "InitialResidueMass", value = 600))

  inspect_apsimx("Wheat-edited.apsimx", src.dir = tmp.dir, node = "SurfaceOrganicMatter")

  ## Using the 'batch' method
  ## Let's say I want to edit 'SurfaceOrganicMatter'
  ## InitialResidueMass from 500 to 650
  ## Understanding the structure of JSON files
  parms <- list(".Simulations.Simulation.Field.SurfaceOrganicMatter.InitialResidueMass" = 650)
  edit2time <- system.time(edit_apsimx_batch("Wheat.apsimx", src.dir = tmp.dir, parms = parms))
  inspect_apsimx("Wheat.apsimx", src.dir = tmp.dir, node = "SurfaceOrganicMatter")
  
  ## What is the difference in 'elapsed' real time?
  edit2time[[3]]/edit1time[[3]]
} 
