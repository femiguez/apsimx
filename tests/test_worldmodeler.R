#### Testing get_worldmodeler_soil_profile
require(apsimx)
apsimx_options(warn.versions = FALSE)

run.test.worldmodeler <- get(".run.local.tests", envir = apsimx.options)

if(run.test.worldmodeler && Sys.info()[["user"]] == "fernandomiguez"){
  
  ### Simple example which cleans up
  sp0 <- get_worldmodeler_soil_profile(lonlat = c(-93, 42))
  plot(sp0[[1]], property = "water")
  
  tdir <- "~/Dropbox/apsimx-other/worldmodeler"
  get_worldmodeler_soil_profile(lonlat = c(-93, 42), wrt.dir = tdir, 
                                       filename = "temp_soils.soils")
  
  lonlat.mat <- rbind(c(-93, 42), c(-92, 41), c(-91, 40))
  get_worldmodeler_soil_profile(lonlat = lonlat.mat, wrt.dir = tdir, 
                                filename = "mat_temp_soils.soils")
  
  sp1 <- read_apsim_soils("temp_soils.soils", src.dir = tdir)

  plot(sp1[[1]], property = "Carbon")  
  
  sps <- read_apsim_soils("mat_temp_soils.soils", src.dir = tdir)
  
  plot(sps[[2]], property = "Carbon")  
  
}

if(run.test.worldmodeler && Sys.info()[["user"]] == "fernandomiguez"){
  
  am0 <- get_worldmodeler_apsim_met(lonlat = c(-93, 42), dates = c("1990-01-01", "2020-12-31"))
  
  if(inherits(am0[[1]], 'met')){
    plot(am0[[1]], met.var = "rain", cumulative = TRUE, climatology = TRUE)
  }
  
  tdir <- "~/Dropbox/apsimx-other/worldmodeler"

  lonlat.mat <- rbind(c(-93, 42), c(-92, 41), c(-91, 40))
 
  am1 <- get_worldmodeler_apsim_met(lonlat = lonlat.mat, 
                                    dates = c("1990-01-01", "2020-12-31"))
  
  if(inherits(am1[[1]], 'met')){
    plot(am1[[1]], met.var = "rain", cumulative = TRUE, climatology = TRUE)
  } 
  
  if(inherits(am1[[2]], 'met')){
    plot(am1[[2]], met.var = "rain", cumulative = TRUE, climatology = TRUE)
  } 
  
  am2 <- get_worldmodeler_apsim_met(lonlat = lonlat.mat, 
                                    dates = c("1990-01-01", "2020-12-31"),
                                    wrt.dir = tdir,
                                    filenames = c("marshall1.met", "marshall2.met", "marshall3.met"))
  
  sapply(am2, class)
  
  if(inherits(am2[[1]], 'met')){
    plot(am2[[1]], met.var = "rain", cumulative = TRUE, climatology = TRUE)
  } 
}