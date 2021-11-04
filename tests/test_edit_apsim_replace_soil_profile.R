## Testing whether this function works
require(apsimx)
require(sf)

## Even though I can run this externally, I will set it so it only
## runs locally
run.soil.profile <- get(".run.local.tests", envir = apsimx.options)

tmp.dir <- tempdir()

if(run.soil.profile){
## First create a soil profile
sp <- apsimx_soil_profile(nlayers = 15, 
                          crops = c("Barley", "Chickpea","Lucerne","Maize",
                                    "Perennial Grass","Sorghum","Wheat","Millet"))

## Find a file from 'Classic' such as 'Millet'
extd.dir <- system.file("extdata", package = "apsimx")

inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil",
              soil.child = "Water")

inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil",
              soil.child = "OrganicMatter")

inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil",
              soil.child = "Analysis")

inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Soil",
              soil.child = "Sample", parm = "Thickness")

## Edit it, replacing with information from the soil profile
edit_apsim_replace_soil_profile("Millet.apsim", soil.profile = sp, 
                                edit.tag = "-newsoil",
                                src.dir = extd.dir, wrt.dir = tmp.dir)

inspect_apsim("Millet-newsoil.apsim", src.dir = tmp.dir,
                  node = "Soil")

inspect_apsim("Millet-newsoil.apsim", src.dir = tmp.dir, 
              node = "Soil", soil.child = "Water")

inspect_apsim("Millet-newsoil.apsim", src.dir = tmp.dir, 
              node = "Soil", soil.child = "OrganicMatter")

inspect_apsim("Millet-newsoil.apsim", src.dir = tmp.dir, 
              node = "Soil", soil.child = "Analysis")

inspect_apsim("Millet-newsoil.apsim", src.dir = tmp.dir, 
              node = "Soil", soil.child = "Sample")

## Should add another example using SSURGO data

## Read the main files
chorizon <- read.csv(paste0(extd.dir,"/ISUAG/SSURGO/ISUAG_SSURGO_chorizon.csv"))
component <- read.csv(paste0(extd.dir,"/ISUAG/SSURGO/ISUAG_SSURGO_component.csv"))
mapunit <- read.csv(paste0(extd.dir,"/ISUAG/SSURGO/ISUAG_SSURGO_mapunit.csv"))
mapunit.shp <- st_read(paste0(extd.dir,"/ISUAG/SSURGO/ISUAG_SSURGO_Mapunits.shp"), quiet = TRUE)

sp <- ssurgo2sp(mapunit = mapunit, 
                component = component, 
                chorizon = chorizon,
                mapunit.shp = mapunit.shp,
                nlayers = 20, nmapunit = 2)

## For first soil
metadata <- attributes(sp[[1]])
metadata$names <- NULL; metadata$class <- NULL; metadata$row.names <- NULL;

asp <- apsimx_soil_profile(nlayers = 20,
                           Thickness = sp[[1]]$Thickness * 10,
                           BD = sp[[1]]$BD,
                           AirDry = sp[[1]]$AirDry,
                           LL15 = sp[[1]]$LL15,
                           DUL = sp[[1]]$DUL,
                           SAT = sp[[1]]$SAT,
                           KS = sp[[1]]$KS,
                           Carbon = sp[[1]]$Carbon,
                           crop.LL = sp[[1]]$LL15,
                           soil.bottom = 200,
                           metadata = metadata,
                           crops = c("Barley", "Chickpea","Lucerne","Maize",
                                     "Perennial Grass","Sorghum","Wheat","Millet"))

## Edit it, replacing with information from the soil profile
edit_apsim_replace_soil_profile("Millet.apsim", soil.profile = asp, 
                                edit.tag = "-newsoil-ssurgo",
                                src.dir = extd.dir, wrt.dir = tmp.dir)

}

## Testing the feature for APSIM Next Gen

run.soil.profile <- get(".run.local.tests", envir = apsimx.options)

if(run.soil.profile){
 
  extd.dir <- system.file("extdata", package = "apsimx")
  
  sp <- apsimx_soil_profile()
  
  swat <- soilwat_parms(Thickness = rep(50, 10), SWCON = runif(10, 0.2, 0.4))
  
  sp$soilwat <- swat
  
  list.files(tmp.dir)
  
  file.copy(file.path(extd.dir, "MaizeSoybean.apsimx"), tmp.dir)
 
  edit_apsimx_replace_soil_profile("MaizeSoybean.apsimx", soil.profile = sp,
                                   src.dir = tmp.dir, wrt.dir = tmp.dir,
                                   root = "SimulationSoybean") 

  inspect_apsimx("MaizeSoybean-edited.apsimx", src.dir = tmp.dir,
                 node = "Soil", soil.child = "Physical",
                 root = "SimulationSoybean")
    
  inspect_apsimx("MaizeSoybean-edited.apsimx", src.dir = tmp.dir,
                 node = "Soil", soil.child = "Physical", parm = "DUL",
                 root = "SimulationSoybean")
  
  inspect_apsimx("MaizeSoybean-edited.apsimx", src.dir = tmp.dir,
                 node = "Soil", soil.child = "SoilWater",
                 root = "SimulationSoybean")
  
  inspect_apsimx("MaizeSoybean-edited.apsimx", src.dir = tmp.dir,
                 node = "Soil", soil.child = "Physical", parm = "DUL",
                 root = "SimulationMaize")
  
}