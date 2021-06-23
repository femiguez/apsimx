## Testing downloading soil data from ISRIC
## In repeated testing it seems that speed slows down when multiple queries are requested

require(apsimx)
require(raster)

apsimx_options(warn.versions = FALSE)

run.test.isric.workflow <- get(".run.local.tests", envir = apsimx.options)

if(run.test.isric.workflow){
  
  # This takes a few seconds
  system.time(sp1 <- get_isric_soil_profile(lonlat = c(-93, 42))) ## 1.508 seconds
  system.time(sp2 <- get_ssurgo_soil_profile(lonlat = c(-93, 42)))  ## 1.858 seconds
  
  plot(sp1)
  
  plot(sp2[[1]])
  
  ## Only get SSURGO tables
  stbls <- get_ssurgo_tables(lonlat = c(-93, 42))
}
