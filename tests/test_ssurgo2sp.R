## Testing the essence of the ssurgo workflow
require(apsimx)
require(sf)
apsimx_options(warn.versions = FALSE)

extd.dir <- system.file("extdata", package = "apsimx")

run.test.ssurgo.workflow <- get(".run.local.tests", envir = apsimx.options)

tmp.dir <- tempdir()

if(run.test.ssurgo.workflow){

  ## Read the main files
  chorizon <- read.csv(paste0(extd.dir,"/ISUAG/SSURGO/ISUAG_SSURGO_chorizon.csv"))
  component <- read.csv(paste0(extd.dir,"/ISUAG/SSURGO/ISUAG_SSURGO_component.csv"))
  mapunit <- read.csv(paste0(extd.dir,"/ISUAG/SSURGO/ISUAG_SSURGO_mapunit.csv"))
  mapunit.shp <- sf::st_read(paste0(extd.dir,"/ISUAG/SSURGO/ISUAG_SSURGO_Mapunits.shp"), quiet = TRUE)
  
  ## Get tables from SSURGO
  srgtbls <- get_ssurgo_tables(lonlat = c(-93, 42))
  chorizon <- srgtbls$chorizon
  component <- srgtbls$component
  mapunit <- srgtbls$mapunit
  mapunit.shp <- srgtbls$mapunit.shp
  
  sp <- ssurgo2sp(mapunit = mapunit, 
                  component = component, 
                  chorizon = chorizon,
                  mapunit.shp = mapunit.shp,
                  nlayers = 20)
  
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
                            metadata = metadata)
  
  check_apsimx_soil_profile(asp)

  edit_apsimx_replace_soil_profile("WheatRye.apsimx",
                                   src.dir = extd.dir,
                                   wrt.dir = tmp.dir,
                                   soil.profile = asp,
                                   edit.tag = "-sp")
  
  inspect_apsimx("WheatRye-sp.apsimx", src.dir = tmp.dir, node = "Soil")
  
  inspect_apsimx("WheatRye-sp.apsimx", src.dir = tmp.dir, 
                 node = "Soil", soil.child = "Physical")
  
  ## If I want to actually run the model
  file.copy(paste0(extd.dir,"/Ames.met"),tmp.dir)
  
  inspect_apsimx("WheatRye-sp.apsimx", src.dir = tmp.dir, node = "Clock")
  
  sim <- apsimx("WheatRye-sp.apsimx", src.dir = tmp.dir, value = "report")
  
}