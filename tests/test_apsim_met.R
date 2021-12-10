## Testing apsim_met suite of functions
## Some observations:
##
## 1. check should never fail. It should always return warnings
## 2. napad should work for many different tricky situations

require(apsimx)

run.apsim.met <- get(".run.local.tests", envir = apsimx.options)

if(run.apsim.met){
  
  ## Testing napad and impute
  ## Buenos Aires 34d 36 12, 58 22 54 W
  bsas.lat <- unit_conv("34d 36' 12\" W", from = "degrees", to = "decimal")
  bsas.lon <- unit_conv("58d 22' 54\" S", from = "degrees", to = "decimal")
  
  pwr <- get_power_apsim_met(lonlat = c(bsas.lon, bsas.lat), dates = c("2010-01-01", "2015-12-31"))
  check_apsim_met(pwr)
  gsd <- get_gsod_apsim_met(lonlat = c(bsas.lon, bsas.lat), dates = c("2010-01-01", "2015-12-31"))
  check_apsim_met(gsd)
  gsd$radn <- pwr$radn
  check_apsim_met(gsd)
  gsd.impt <- impute_apsim_met(gsd)
  check_apsim_met(gsd.impt)
  
  ## Test for Brazil (Sao Paulo)
  ## 23 33′S 46 38′W
  sp.lat <- unit_conv("23d 33' 0\" W", from = "degrees", to = "decimal")
  sp.lon <- unit_conv("46d 38' 0\" S", from = "degrees", to = "decimal")
  
  pwr <- get_power_apsim_met(lonlat = c(sp.lon, sp.lat), dates = c("2010-01-01", "2015-12-31"))
  check_apsim_met(pwr)
  gsd <- get_gsod_apsim_met(lonlat = c(sp.lon, sp.lat), dates = c("2010-01-01", "2015-12-31"), fillin.radn = TRUE)
  check_apsim_met(gsd)
  gsd.np <- napad_apsim_met(gsd)
  gsd.imp <- impute_apsim_met(gsd.np)
  check_apsim_met(gsd.imp)
  
}