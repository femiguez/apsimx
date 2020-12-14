## Testing inspect for Classic
require(apsimx)

apsimx_options(warn.versions = FALSE)

## Run inspect tests
## inspect_apsim returns relative paths (an xpath expression), but still a character
## inspect_apsim_xml returns absolute paths 

run.inspect.tests <- get(".run.local.tests", envir = apsimx.options)

if(run.inspect.tests){
  
  extd.dir <- system.file("extdata", package = "apsimx")
  ## Clock and clock path
  inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Clock")
  pp <- inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Clock", 
                      parm = "start", print.path = TRUE)
  if(pp != ".//clock/start_date") stop("Clock path is incorrect")
  inspect_apsim_xml("Millet.apsim", src.dir = extd.dir, parm = pp)
  ## Weather and weather path
  inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Weather")
  pp <- inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Weather", 
                      parm = "filename", print.path = TRUE)
  if(pp != ".//metfile/filename") stop("Weather path is incorrect")
  inspect_apsim_xml("Millet.apsim", src.dir = extd.dir, parm = pp)
  ## Soil Metadata
  inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil", soil.child = "Metadata")
  pp <- inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil",
                      soil.child = "Metadata", parm = "LocalName", print.path = TRUE)
  if(pp != ".//Soil/LocalName") stop("Soil Metadata path is incorrect")
  inspect_apsim_xml("Millet.apsim", src.dir = extd.dir, parm = pp)
  ## Soil Water
  inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil", soil.child = "Water")
  pp <- inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil",
                      soil.child = "Water", parm = c("Barley", "KL"), print.path = TRUE)
  if(pp != ".//Soil/Water/SoilCrop[@name='Barley']/KL") stop("Soil Water KL path is incorrect")
  ## In this case there are multiple elements for KL, so the path needs to be different
  ## If we want to identify a single value. There are many KLs, one for each crop, and there 
  ## are 8 crops here
  inspect_apsim_xml("Millet.apsim", src.dir = extd.dir, parm = ".//SoilCrop[@name='Barley']/KL/double[1]")
  pp <- inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil",
                      soil.child = "Water", parm = "SummerCona", print.path = TRUE)
  if(pp != ".//Soil/SoilWater/SummerCona") stop("Soil Water SummerCona path is incorrect")
  inspect_apsim_xml("Millet.apsim", src.dir = extd.dir, parm = pp)
  ## Soil Organic Matter
  inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil", soil.child = "OrganicMatter")
  pp <- inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil",
                      soil.child = "OrganicMatter", parm = "OC", print.path = TRUE)
  if(pp != ".//Soil/SoilOrganicMatter/OC") stop("Soil Organic Matter OC path is incorrect")
  ## In this case there are multiple values of OC, so we need to select one
  inspect_apsim_xml("Millet.apsim", src.dir = extd.dir, parm = paste0(pp, "/double[1]"))
  ### Analysis
  inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil", soil.child = "Analysis")
  pp <- inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil",
                      soil.child = "Analysis", parm = "PH", print.path = TRUE)
  if(pp != ".//Soil/Analysis/PH") stop("Soil Analysis PH path is incorrect")
  inspect_apsim_xml("Millet.apsim", src.dir = extd.dir, parm = paste0(pp, "/double[1]"))
  ## Initial Water
  inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil", soil.child = "InitialWater")
  pp <- inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil",
                      soil.child = "InitialWater", parm = "FractionFull", print.path = TRUE)
  if(pp != ".//Soil/InitialWater/FractionFull") stop("Soil InitialWater FractionFull path is incorrect")
  inspect_apsim_xml("Millet.apsim", src.dir = extd.dir, parm = pp)
  ## Sample
  inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil", soil.child = "Sample")
  pp <- inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil",
                      soil.child = "Sample", parm = "NO3", print.path = TRUE)
  inspect_apsim_xml("Millet.apsim", src.dir = extd.dir, parm = paste0(pp, "/double[1]"))
  ## Surface Organic Matter
  inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "SurfaceOrganicMatter")
  pp <- inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "SurfaceOrganicMatter",
                      parm = "type", print.path = TRUE)
  inspect_apsim_xml("Millet.apsim", src.dir = extd.dir, parm = pp)
  ## Crop or Manager
  inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Crop", parm = list("sow",NA)) 
  ## Crop with Parameter, This returns the full absolute path
  pp <- inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Crop", parm = list("sow",7),
                      print.path = TRUE)
  ## Strangely the full path does not work here, but just "cultivar" does
  inspect_apsim_xml("Millet.apsim", src.dir = extd.dir, parm = "cultivar")
  
}