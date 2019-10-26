## Run a few tests for the examples
require(apsimx)
apsimx_options(warn.versions = FALSE)
ex.dir <- auto_detect_apsimx_examples()

## Run inspect tests

run.inspect.tests <- TRUE

if(run.inspect.tests){
  
  ## Barley
  i <- "Barley.apsimx"
  inspect_apsimx(i, src.dir = ex.dir, node = "Clock")        
  inspect_apsimx(i, src.dir = ex.dir, node = "Weather")        
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Water")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "SoilWater")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Organic")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "InitialWater")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Analysis")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "InitialN")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "CERESSoilTemperature")
  inspect_apsimx(i, src.dir = ex.dir, node = "SurfaceOrganicMatter")
  inspect_apsimx(i, src.dir = ex.dir, node = "MicroClimate")
  inspect_apsimx(i, src.dir = ex.dir, node = "Crop")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("SowingFertiliser",NA))

  ## Maize
  i <- "Maize.apsimx"
  inspect_apsimx(i, src.dir = ex.dir, node = "Clock")        
  inspect_apsimx(i, src.dir = ex.dir, node = "Weather") 
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Physical")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "SoilWater")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Organic")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Chemical")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "InitialWater")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "InitialN")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Chemical")
  inspect_apsimx(i, src.dir = ex.dir, node = "SurfaceOrganicMatter")
  inspect_apsimx(i, src.dir = ex.dir, node = "MicroClimate")
  inspect_apsimx(i, src.dir = ex.dir, node = "Crop")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('SowingRule',NA))
  
  ## Oats
  i <- "Oats"
  inspect_apsimx(i, src.dir = ex.dir, node = "Clock")        
  inspect_apsimx(i, src.dir = ex.dir, node = "Weather")        
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "SoilWater")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Organic")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "InitialWater")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Analysis")
  inspect_apsimx(i, src.dir = ex.dir, node = "SurfaceOrganicMatter")
  inspect_apsimx(i, src.dir = ex.dir, node = "MicroClimate")
  inspect_apsimx(i, src.dir = ex.dir, node = "Crop")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('SowingRule',NA))
}

inspect.replacement.test <- TRUE

if(inspect.replacement.test){
  
  ## Define the highest level
  inspect_apsimx_replacement("Factorial", src.dir = ex.dir, 
                             root = list("Experiment", 1),
                             node = "Base", node.child = "Clock")
  
  inspect_apsimx_replacement("Factorial", src.dir = ex.dir, 
                             root = list("Experiment", 1),
                             node = "Base", node.child = "Weather")
  
  inspect_apsimx_replacement("Factorial", src.dir = ex.dir, 
                             root = list("Experiment", 1),
                             node = "Base", node.child = "Field",
                             node.subchild = "Soil",
                             display.available = TRUE)
  
  ## If we want to look at the second Factorial 'Experiment'
  inspect_apsimx_replacement("Factorial", src.dir = ex.dir, 
                             root = list("Experiment", 2),
                             node = "Base", node.child = "Field",
                             node.subchild = "Soil",
                             display.available = TRUE)
  
}

 
