### Testing the spanking new inspect_apsimx_json
require(apsimx)

run.inspect.apsimx.json <- FALSE

if(run.inspect.apsimx.json){
  
  extd.dir <- system.file("extdata", package = "apsimx")
  
  inspect_apsimx_json("Maize.apsimx", src.dir = extd.dir, parm = "^Version")
  
  inspect_apsimx_json("Maize.apsimx", src.dir = extd.dir, parm = "Version")
  
  inspect_apsimx_json("Maize.apsimx", src.dir = extd.dir, parm = "ApsimVersion")
  
  inspect_apsimx_json("Maize.apsimx", src.dir = extd.dir, parm = "Clock")
  
  inspect_apsimx_json("Maize.apsimx", src.dir = extd.dir, parm = "Soil") 
  
  inspect_apsimx_json("Maize.apsimx", src.dir = extd.dir, parm = "Soil") 
  
  inspect_apsimx_json("Maize.apsimx", src.dir = extd.dir, parm = "Dekalb") 
  
  inspect_apsimx_json("Maize.apsimx", src.dir = extd.dir, parm = "10-jan") 

  ## These parameters do not return a single path
  ## Ideally, I would be able to either return multiple paths or use
  ## a jsonpath for the search instead of a single keyword
  inspect_apsimx_json("Maize.apsimx", src.dir = extd.dir, parm = "Water") 
  inspect_apsimx_json("Maize.apsimx", src.dir = extd.dir, parm = "Cultivar") 
}