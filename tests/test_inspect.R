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
  inspect_apsimx(i, src.dir = ex.dir, node = "MicroClimate")  
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Physical")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "SoilWater")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "SoilWater", parm = "SummerDate")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "SoilWater", parm = "SummerCona")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Organic")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "InitialWater")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Chemical")
  inspect_apsimx(i, src.dir = ex.dir, node = "SurfaceOrganicMatter")
  inspect_apsimx(i, src.dir = ex.dir, node = "Crop")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("Fertilise", NA))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("Sow on a fixed date", NA))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("Sow on a fixed date", 1))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("Sow on a fixed date", 2))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("Sow on a fixed date", 3))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("Sow on a fixed date", 4))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("Sow on a fixed date", 5))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("Sow on a fixed date", 6))
  inspect_apsimx(i, src.dir = ex.dir, node = "Report")
  inspect_apsimx(i, src.dir = ex.dir, node = "Report", parm = "VariableNames")
  inspect_apsimx(i, src.dir = ex.dir, node = "Report", parm = "EventNames")

  ## Maize
  i <- "Maize.apsimx"
  inspect_apsimx(i, src.dir = ex.dir, node = "Clock")        
  inspect_apsimx(i, src.dir = ex.dir, node = "Weather") 
  inspect_apsimx(i, src.dir = ex.dir, node = "MicroClimate")  
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Physical")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "SoilWater")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Organic")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Chemical")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "InitialWater")
  inspect_apsimx(i, src.dir = ex.dir, node = "SurfaceOrganicMatter")
  inspect_apsimx(i, src.dir = ex.dir, node = "Crop")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('variable', NA))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('Fertilise', NA))
  inspect_apsimx(i, src.dir = ex.dir, node = "Report", parm = "VariableNames")
  inspect_apsimx(i, src.dir = ex.dir, node = "Report", parm = "EventNames")
  
  ## Oats
  i <- "Oats"
  inspect_apsimx(i, src.dir = ex.dir, node = "Clock")        
  inspect_apsimx(i, src.dir = ex.dir, node = "Weather")   
  inspect_apsimx(i, src.dir = ex.dir, node = "MicroClimate")  
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Water")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "SoilWater")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "SoilWater", parm = "SummerDate")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Organic")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Chemical")
  inspect_apsimx(i, src.dir = ex.dir, node = "SurfaceOrganicMatter")
  inspect_apsimx(i, src.dir = ex.dir, node = "Crop")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('Fert', NA))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('variable', NA))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('variable', 1))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('variable', 2))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('variable', 3))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('variable', 4))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('variable', 5))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('variable', 6))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('variable', 7))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('variable', 8))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list('variable', 9))
  inspect_apsimx(i, src.dir = ex.dir, node = "Report", parm = "VariableNames")
  inspect_apsimx(i, src.dir = ex.dir, node = "Report", parm = "EventNames")
  
  ## OilPalm is good for testing Report
  i <- "OilPalm.apsimx"
  inspect_apsimx(i, src.dir = ex.dir, node = "Clock")        
  inspect_apsimx(i, src.dir = ex.dir, node = "Weather")    
  inspect_apsimx(i, src.dir = ex.dir, node = "MicroClimate")  
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Physical")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "SoilWater")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "SoilWater", parm = "SummerDate")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Organic")
  inspect_apsimx(i, src.dir = ex.dir, node = "Soil", soil.child = "Chemical")
  inspect_apsimx(i, src.dir = ex.dir, node = "SurfaceOrganicMatter")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager")
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("Palm Management", NA))
  inspect_apsimx(i, src.dir = ex.dir, node = "Manager", parm = list("Palm Management", 3))
  inspect_apsimx(i, src.dir = ex.dir, node = "Report")
  inspect_apsimx(i, src.dir = ex.dir, node = "Report", parm = list("Annual", NA))
  inspect_apsimx(i, src.dir = ex.dir, node = "Report", parm = list("Monthly", NA))
  inspect_apsimx(i, src.dir = ex.dir, node = "Report", parm = list("Annual", "Variable"))
  inspect_apsimx(i, src.dir = ex.dir, node = "Report", parm = list("Monthly", "Event"))
  
  
}

run.inspect.print.path.tests <- get(".run.local.tests", envir = apsimx.options)

if(run.inspect.print.path.tests){
  
  i <- "Barley.apsimx"
  #### Clock ####
  pp <- inspect_apsimx(i, src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Clock") 
    stop("Error in inspect_apsimx, Barley, Clock, print.path")
  pp <- inspect_apsimx(i, src.dir = ex.dir, parm = "Start", print.path = TRUE)
  if(pp != ".Simulations.Simulation.Clock.Start") 
    stop("Error in inspect_apsimx, Barley, Start, print.path")
  #### Weather ####
  pp <- inspect_apsimx(i, node = "Weather", src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Weather") 
    stop("Error in inspect_apsimx, Barley, Weather, print.path")
  #### Soil - Metadata ####
  pp <- inspect_apsimx(i, node = "Soil", src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Soil") 
    stop("Error in inspect_apsimx, Barley, Soil, Metadata, print.path")
  pp <- inspect_apsimx(i, node = "Soil", src.dir = ex.dir, 
                       parm = "Latitude", print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Soil.Latitude") ## I think this currently works as expected... ??
    stop("Error in inspect_apsimx, Barley, Soil, Metadata, Latitude, print.path")
  #### Soil - Physical ####
  pp <- inspect_apsimx(i, node = "Soil", soil.child = "Physical",
                       src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Soil.Physical") 
    stop("Error in inspect_apsimx, Barley, Soil, Physical, print.path")
  pp <- inspect_apsimx(i, node = "Soil", soil.child = "Physical",
                       parm = "DUL", src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Soil.Physical.DUL") 
    stop("Error in inspect_apsimx, Barley, Soil, Physical, DUL, print.path")
  pp <- inspect_apsimx(i, node = "Soil", soil.child = "Physical",
                       parm = "Barley XF", src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Soil.Physical.Barley XF") 
    stop("Error in inspect_apsimx, Barley, Soil, Physical, Barley XF, print.path")
  #### Soil - SoilWater ####
  pp <- inspect_apsimx(i, node = "Soil", soil.child = "SoilWater",
                       src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Soil.SoilWater") 
    stop("Error in inspect_apsimx, Barley, Soil, SoilWater, print.path")
  pp <- inspect_apsimx(i, node = "Soil", soil.child = "SoilWater", parm = "Salb",
                       src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Soil.SoilWater.Salb") 
    stop("Error in inspect_apsimx, Barley, Soil, SoilWater, SWCON, print.path")
  pp <- inspect_apsimx(i, node = "Soil", soil.child = "SoilWater", parm = "SWCON",
                       src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Soil.SoilWater.SWCON") 
    stop("Error in inspect_apsimx, Barley, Soil, SoilWater, SWCON, print.path")
  #### Soil - Chemical ####
  pp <- inspect_apsimx(i, node = "Soil", soil.child = "Chemical",
                       src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Soil.Chemical") 
    stop("Error in inspect_apsimx, Barley, Soil, Chemical, print.path")
  pp <- inspect_apsimx(i, node = "Soil", soil.child = "Chemical", parm = "PH",
                       src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Soil.Chemical.PH") 
    stop("Error in inspect_apsimx, Barley, Soil, Chemical, PH, print.path")
  #### Soil - InitialWater ####
  pp <- inspect_apsimx(i, node = "Soil", soil.child = "InitialWater",
                       src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Soil.Water") 
    stop("Error in inspect_apsimx, Barley, Soil, InitialWater, print.path")
  pp <- inspect_apsimx(i, node = "Soil", soil.child = "InitialWater", parm = "RelativeTo",
                       src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Soil.Water.RelativeTo") 
    stop("Error in inspect_apsimx, Barley, Soil, InitialWater, FractionFull, print.path")
  #### Soil - Organic ####
  pp <- inspect_apsimx(i, node = "Soil", soil.child = "Organic",
                       src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Soil.Organic") 
    stop("Error in inspect_apsimx, Barley, Soil, Organic, print.path")
  pp <- inspect_apsimx(i, node = "Soil", soil.child = "Organic", parm = "Carbon",
                       src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Soil.Organic.Carbon") 
    stop("Error in inspect_apsimx, Barley, Soil, Organic, Carbon, print.path")
  #### Soil - InitialN ####
  ## Note (2023-12-10) Need to change this to NO3, NH3, Urea
  # pp <- inspect_apsimx(i, node = "Soil", soil.child = "InitialN",
  #                      src.dir = ex.dir, print.path = TRUE)
  # if(pp != ".Simulations.Simulation.Field.Soil.InitialN") 
  #   stop("Error in inspect_apsimx, Barley, Soil, InitialN, print.path")
  #### Soil - CERESSoilTemperature and Nutrient ####
  ## There is not much here to test or implement at the moment

  #### Manager ####
  pp <- inspect_apsimx(i, node = "Manager", src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field") 
    stop("Error in inspect_apsimx, Barley, Manager, print.path")
  pp <- inspect_apsimx(i, node = "Manager", parm = list("Fertilise", NA),
                       src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Fertilise at sowing") 
    stop("Error in inspect_apsimx, Barley, Manager, Fertilise at sowing, print.path")
  pp <- inspect_apsimx(i, node = "Manager", parm = list("Fertilise", 3),
                       src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.Fertilise at sowing.Amount") 
    stop("Error in inspect_apsimx, Barley, Manager, Fertilise at sowing, Amount, print.path")
  pp <- inspect_apsimx(i, node = "Surface", 
                       src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.SurfaceOrganicMatter") 
    stop("Error in inspect_apsimx, Barley, SurfaceOrganicMatter, print.path")
  pp <- inspect_apsimx(i, node = "Surface", parm = "InitialResidueMass",
                       src.dir = ex.dir, print.path = TRUE)
  if(pp != ".Simulations.Simulation.Field.SurfaceOrganicMatter.InitialResidueMass") 
    stop("Error in inspect_apsimx, Barley, SurfaceOrganicMatter, InitialResidueMass print.path")
  pp <- inspect_apsimx(i, node = "MicroClimate", 
                       src.dir = ex.dir, print.path = TRUE)
  pp <- inspect_apsimx(i, node = "MicroClimate", parm = "SoilHeatFluxFraction",
                       src.dir = ex.dir, print.path = TRUE)
}


inspect.replacement.test <- get(".run.local.tests", envir = apsimx.options)

if(inspect.replacement.test){
  
  ## Define the highest level
  inspect_apsimx_replacement("Factorial", src.dir = ex.dir, 
                             root = list("Experiment", 1),
                             node = "Base", node.child = "Clock")
  ## inspect_apsimx version
  inspect_apsimx("Factorial.apsimx", src.dir = ex.dir,
                 root = c("Experiment", "Base"),
                 node = "Clock")
  
  inspect_apsimx_replacement("Factorial", src.dir = ex.dir, 
                             root = list("Experiment", 1),
                             node = "Base", node.child = "Weather")
  
  ## inspect_apsimx version
  inspect_apsimx("Factorial.apsimx", src.dir = ex.dir,
                 root = c("Experiment", "Base"),
                 node = "Weather")
  
  inspect_apsimx_replacement("Factorial", src.dir = ex.dir, 
                             root = list("Experiment", 1),
                             node = "Base", node.child = "Field",
                             node.subchild = "Soil",
                             display.available = TRUE)
  
  ## inspect_apsimx version
  inspect_apsimx("Factorial.apsimx", src.dir = ex.dir,
                 root = c("Experiment", "Base"),
                 node = "Soil")
  
  ## If we want to look at the second Factorial 'Experiment'
  inspect_apsimx_replacement("Factorial", src.dir = ex.dir, 
                             root = list("Experiment", 2),
                             node = "Base", node.child = "Field",
                             node.subchild = "Soil",
                             display.available = TRUE)
  
  ## Look at Water
  inspect_apsimx_replacement("Factorial", src.dir = ex.dir, 
                             root = list("Experiment", 2),
                             node = "Base", node.child = "Field",
                             node.subchild = "Soil",
                             node.subsubchild = "Water",
                             display.available = TRUE)
  
  ## For a specific parameter
  inspect_apsimx_replacement("Factorial", src.dir = ex.dir, 
                             root = list("Experiment", 2),
                             node = "Base", node.child = "Field",
                             node.subchild = "Soil",
                             node.subsubchild = "Water",
                             parm = "Depth")
}

inspect.replacement.test2 <- get(".run.local.tests", envir = apsimx.options)

extd.dir <- system.file("extdata", package = "apsimx")

if(inspect.replacement.test2){
  
  ## Testing inspect_apsimx on MaizeSoybean.apsimx
  pp <- inspect_apsimx("MaizeSoybean.apsimx", src.dir = extd.dir,
                       node = "Weather",
                       root = "SimulationSoybean",
                       parm = "FileName",
                       print.path = TRUE)
  
  if(pp != ".Simulations.SimulationSoybean.Weather.FileName")
    stop("pp does not match for inspect_apsimx MaizeSoybean.apsimx root = SimulationSoybean", call. = FALSE)
  
  pp <- inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                                   root = "SimulationSoybean", 
                                   node = "Weather", parm = "FileName", print.path = TRUE)
  
  if(pp != ".Simulations.SimulationSoybean.Weather.FileName")
    stop("pp does not match for inspect_apsimx_replacement MaizeSoybean.apsimx root = SimulationSoybean", call. = FALSE)
  
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

  ## Soybean Cultivars
  pp <- inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                                   src.dir = extd.dir, 
                                   node = "Soybean",
                                   node.child = "Cultivars",
                                   node.subchild = "USA",
                                   node.subsubchild = "PioneerP22T61_MG22",
                                   parm = "Vegetative.Target.FixedValue",
                                   print.path = TRUE)
  
  if(pp != ".Simulations.Replacements.Soybean.Cultivars.USA.PioneerP22T61_MG22.Vegetative.Target.FixedValue") 
    stop("Error in inspect_apsimx_replacement, MaizeSoybean, PioneerP22T61_MG22, Vegetative.Target.FixedValue, print.path")

  pp <- inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                                   src.dir = extd.dir, 
                                   node = "Soybean",
                                   node.child = "Cultivars",
                                   node.subchild = "USA",
                                   node.subsubchild = "PioneerP22T61_MG22",
                                   parm = "EarlyFlowering",
                                   print.path = TRUE)
  
  if(pp != ".Simulations.Replacements.Soybean.Cultivars.USA.PioneerP22T61_MG22.EarlyFlowering") 
    stop("Error in inspect_apsimx_replacement, MaizeSoybean, PioneerP22T61_MG22, EarlyFlowering, print.path")
  
  ## Testing the root
  inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                             src.dir = extd.dir, 
                             root = "SimulationSoybean",
                             node = "Weather",
                             parm = "FileName")

  inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                             src.dir = extd.dir, 
                             root = "SimulationMaize",
                             node = "Weather",
                             parm = "FileName")
  
  ## WheatRye version
  inspect_apsimx_replacement("WheatRye.apsimx", src.dir = extd.dir,
                             node = "Wheat", node.child = "Cultivars",
                             node.subchild = "USA", node.subsubchild = "Yecora")
  
  ## Now with parameter
  inspect_apsimx_replacement("WheatRye.apsimx", src.dir = extd.dir,
                             node = "Wheat", node.child = "Cultivars",
                             node.subchild = "USA", node.subsubchild = "Yecora",
                             parm = "MinimumLeafNumber")
  ## This only works for the first one
  inspect_apsimx_replacement("WheatRye.apsimx", src.dir = extd.dir,
                             node = "Wheat", node.child = "Cultivars",
                             node.subchild = "USA", node.subsubchild = "Yecora",
                             parm = "Vrn")
                             
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
                                   parm = "Gsmax350",
                                   print.path = TRUE)
  
  ### Testing replacement with new simulations (Dec 2023)
  ### Not normal testing as these files are not distributed with APSIM
  ### In the future, I will create some and distribute with the package
  
  if(FALSE){
    
    csm.dir <- "~/Desktop/csm/lab_05"
    
    pp1 <- inspect_apsimx_replacement("MaizeSoybean_replacement.apsimx",
                                      src.dir = csm.dir,
                                      node = "Maize",
                                      node.child = "Leaf",
                                      node.subchild = "Photosynthesis",
                                      node.subsubchild = "RUE",
                                      parm = "FixedValue",
                                      display.available = FALSE,
                                      print.path = TRUE)
    
    ### Need to look at whether I can edit this
    
    
  }
}

