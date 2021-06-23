### Testing the spanking new inspect_apsimx_json
require(apsimx)

run.inspect.apsimx.json <- FALSE

if(run.inspect.apsimx.json){
  
  extd.dir <- system.file("extdata", package = "apsimx")
  
  inspect_apsimx_json("Maize.apsimx", src.dir = extd.dir, parm = "^Version")
  
  ## This returns an error as expected
  ## inspect_apsimx_json("Maize.apsimx", src.dir = extd.dir, parm = "Version")
  
  inspect_apsimx_json("Maize.apsimx", src.dir = extd.dir, parm = "ApsimVersion")
  
  ## Next level is Simulation
  inspect_apsimx_json("Maize.apsimx", src.dir = extd.dir, parm = "Clock")
}