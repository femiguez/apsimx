## Run a few tests for the examples
require(apsimx)
apsimx_options(warn.versions = FALSE)

## Run inspect tests

run.inspect.tests <- get(".run.local.tests", envir = apsimx.options)

if(run.inspect.tests) ex.dir <- auto_detect_apsimx_examples()

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
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Chemical")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "InitialN")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "CERESSoilTemperature")
  inspect_apsimx(i, src.dir = ex.dir, node = "SurfaceOrganicMatter")
  inspect_apsimx(i, src.dir = ex.dir, node = "MicroClimate")
  inspect_apsimx(i, src.dir = ex.dir, node = "Crop")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("SowingFertiliser",NA))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("Sow on a fixed date",NA))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("Sow on a fixed date",1))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("Sow on a fixed date",2))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("Sow on a fixed date",3))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("Sow on a fixed date",5))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("Sow on a fixed date",6))

  ## Maize
  i <- "Maize.apsimx"
  inspect_apsimx(i, src.dir = ex.dir, node = "Clock")        
  inspect_apsimx(i, src.dir = ex.dir, node = "Weather") 
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Physical")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "SoilWater")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Organic")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Chemical")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "InitialWater")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "InitialN")
  inspect_apsimx(i, src.dir = ex.dir, node = "SurfaceOrganicMatter")
  inspect_apsimx(i, src.dir = ex.dir, node = "MicroClimate")
  inspect_apsimx(i, src.dir = ex.dir, node = "Crop")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('SowingRule',NA))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('SowingFertiliser',NA))
  
  ## Oats
  i <- "Oats"
  inspect_apsimx(i, src.dir = ex.dir, node = "Clock")        
  inspect_apsimx(i, src.dir = ex.dir, node = "Weather")        
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "SoilWater")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Organic")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "InitialWater")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Chemical")
  inspect_apsimx(i, src.dir = ex.dir, node = "SurfaceOrganicMatter")
  inspect_apsimx(i, src.dir = ex.dir, node = "MicroClimate")
  inspect_apsimx(i, src.dir = ex.dir, node = "Crop")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('SowingFert',NA))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('SowingRule1',NA))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('SowingRule1',1))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('SowingRule1',2))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('SowingRule1',3))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('SowingRule1',4))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('SowingRule1',5))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('SowingRule1',6))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('SowingRule1',7))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('SowingRule1',8))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('SowingRule1',9))
}

run.inspect.print.path.tests <- get(".run.local.tests", envir = apsimx.options)

if(run.inspect.print.path.tests){
  
  i <- "Barley.apsimx"
  pp <- inspect_apsimx(i, src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Clock") 
    stop("Error in inspect_apsimx, Barley, Clock, print.path")
  pp <- inspect_apsimx(i, src.dir = ex.dir, parm = "Start", print.path = TRUE)
  if(pp != ".Simulations.Simulation.Clock.Start") 
    stop("Error in inspect_apsimx, Barley, Start, print.path")
  pp <- inspect_apsimx(i, node = "Weather", src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Weather") 
    stop("Error in inspect_apsimx, Barley, Weather, print.path")
  pp <- inspect_apsimx(i, node = "Soil", src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Soil") 
    stop("Error in inspect_apsimx, Barley, Soil, Water, print.path")
  pp <- inspect_apsimx(i, node = "Soil", soil.child = "Water",
                       parm = "DUL", src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Soil.SoilWater.DUL") 
    stop("Error in inspect_apsimx, Barley, Soil, SoilWater, DUL, print.path")
  pp <- inspect_apsimx(i, node = "Manager", src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field") 
    stop("Error in inspect_apsimx, Barley, Manager, print.path")
  pp <- inspect_apsimx(i, node = "Manager", parm = list("SowingFertiliser",NA),
                       src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.SowingFertiliser") 
    stop("Error in inspect_apsimx, Barley, Manager, SowingFertiliser, print.path")
  pp <- inspect_apsimx(i, node = "Manager", parm = list("SowingFertiliser",1),
                       src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.SowingFertiliser.Amount") 
    stop("Error in inspect_apsimx, Barley, Manager, SowingFertiliser, Amount, print.path")
}


inspect.replacement.test <- get(".run.local.tests", envir = apsimx.options)

if(inspect.replacement.test){
  
  ## Define the highest level
  inspect_apsimx_replacement("Factorial", src.dir = ex.dir, 
                             root = list("Experiment", 1),
                             node = "Base", node.child = "Clock")
  ## inspect_apsimx version
  inspect_apsimx("Factorial.apsimx", src.dir = ex.dir,
                 root = c("^Experiment","Base"),
                 node = "Clock")
  
  inspect_apsimx_replacement("Factorial", src.dir = ex.dir, 
                             root = list("Experiment", 1),
                             node = "Base", node.child = "Weather")
  
  ## inspect_apsimx version
  inspect_apsimx("Factorial.apsimx", src.dir = ex.dir,
                 root = c("^Experiment","Base"),
                 node = "Weather")
  
  inspect_apsimx_replacement("Factorial", src.dir = ex.dir, 
                             root = list("Experiment", 1),
                             node = "Base", node.child = "Field",
                             node.subchild = "Soil",
                             display.available = TRUE)
  
  ## inspect_apsimx version
  inspect_apsimx("Factorial.apsimx", src.dir = ex.dir,
                 root = c("^Experiment","Base"),
                 node = "Soil")
  
  ## If we want to look at the second Factorial 'Experiment'
  inspect_apsimx_replacement("Factorial", src.dir = ex.dir, 
                             root = list("Experiment", 2),
                             node = "Base", node.child = "Field",
                             node.subchild = "Soil",
                             display.available = TRUE)
  
  ## Look at Thickness
  inspect_apsimx_replacement("Factorial", src.dir = ex.dir, 
                             root = list("Experiment", 2),
                             node = "Base2", node.child = "Field",
                             node.subchild = "Soil",
                             node.subsubchild = "Water",
                             display.available = TRUE)
  
}

inspect.replacement.test2 <- get(".run.local.tests", envir = apsimx.options)

extd.dir <- system.file("extdata", package = "apsimx")

if(inspect.replacement.test2){
  
  ## Looking at values within Leaf
  inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                             src.dir = extd.dir, 
                             node = "Soybean",
                             display.available = TRUE)
  
  inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                             src.dir = extd.dir, 
                             node = "Soybean",
                             node.child = "Leaf",
                             display.available = TRUE)
  
  ## We can look at parameters at this level
  inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                             src.dir = extd.dir, 
                             node = "Soybean",
                             node.child = "Leaf",
                             parm = "Albedo")
                             
  inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                             src.dir = extd.dir, 
                             node = "Soybean",
                             node.child = "Leaf",
                             parm = "Gsmax350")
                             
  inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                             src.dir = extd.dir, 
                             node = "Soybean",
                             node.child = "Leaf",
                             parm = "R50")
  
  inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                             src.dir = extd.dir, 
                             node = "Soybean",
                             node.child = "Leaf",
                             node.subchild = "Photosynthesis",
                             node.subsubchild = "RUE")
  
  inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                             src.dir = extd.dir, 
                             node = "Soybean",
                             node.child = "Leaf",
                             node.subchild = "Photosynthesis",
                             node.subsubchild = "RUE",
                             parm = "FixedValue")
}
 

inspect.replacement.test.parm.path <- get(".run.local.tests", envir = apsimx.options)

if(inspect.replacement.test.parm.path){
  
  pp <- inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                                   src.dir = extd.dir, 
                                   print.path = TRUE)
  
  pp <- inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                                   src.dir = extd.dir, 
                                   node = "Soybean",
                                   print.path = TRUE)
  
  pp <- inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                                   src.dir = extd.dir, 
                                   node = "Soybean",
                                   node.child = "Leaf",
                                   print.path = TRUE)
  
  pp <- inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                                   src.dir = extd.dir, 
                                   node = "Soybean",
                                   node.child = "Leaf",
                                   parm = "Albedo",
                                   print.path = TRUE)
  
  pp <- inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                                   src.dir = extd.dir, 
                                   node = "Soybean",
                                   node.child = "Leaf",
                                   parm = "Gxmax350",
                                   print.path = TRUE)
  
}
