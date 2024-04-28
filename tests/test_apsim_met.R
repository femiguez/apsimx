## Testing apsim_met suite of functions
## Some observations:
##
## 1. check should never fail. It should always return warnings
## 2. napad should work for many different tricky situations
## 3. plotting and different combinations

require(apsimx)

run.apsim.met <- get(".run.local.tests", envir = apsimx.options)

if(.Platform$OS.type == "unix"){
  internet <- !is.null(utils::nsl("google.com"))
}else{
  internet <- FALSE  
}
  
if(run.apsim.met && internet){
  
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
  
  plot(pwr, met.var = "rain", years = 2010:2015, cumulative = TRUE, climatology = TRUE)
  
  gsd <- get_gsod_apsim_met(lonlat = c(sp.lon, sp.lat), dates = c("2010-01-01", "2015-12-31"), fillin.radn = TRUE)
  check_apsim_met(gsd)
  gsd.np <- napad_apsim_met(gsd)
  gsd.imp <- impute_apsim_met(gsd.np)
  check_apsim_met(gsd.imp) ## Rain variable is missing
  plot(gsd.imp, met.var = "radn", years = 2010:2015, cumulative = TRUE, climatology = TRUE)
  
  ## Test for Daymet data
  dmt <- get_daymet2_apsim_met(lonlat = c(-93, 42), years = c(2000, 2020))
  check_apsim_met(dmt)
  dmt.np <- napad_apsim_met(dmt)  
  dmt.imp <- impute_apsim_met(dmt.np)
  check_apsim_met(dmt.imp)
  
  summary(dmt.imp)
  plot(dmt.imp)
  plot(dmt.imp, met.var = "rain", years = 2010:2015, cumulative = TRUE, climatology = TRUE)
}

## Testing the feature for adding a column to a met file
if(run.apsim.met){
  
  ## Testing adding a column
  extd.dir <- system.file("extdata", package = "apsimx")
  ames <- read_apsim_met("Ames.met", src.dir = extd.dir)
  
  ## ames$vp <- 5 This does not work and it is not supposed to 
  
  vp <- data.frame(vp = abs(rnorm(nrow(ames), 2)))
  attr(vp, "units") <- "(hPa)"
  
  ames$vp <- vp

  val <- abs(rnorm(nrow(ames), 2))
  nm <- "vp"
  ames <- add_column_apsim_met(ames, value = val, name = "vp", units = "(hPa)")
  
  ## Extra tests
  ames <- ames[ames$year < 2018, ]
  ames <- apsimx:::pp_apsim_met(ames)
  ames <- tt_apsim_met(ames, dates = c("01-05", "30-10"), method = "Classic_TT")
  
  plot(ames, met.var = "photoperiod")
  plot(ames, met.var = "Classic_TT")
    
}

if(run.apsim.met && internet){
 
  iem <- get_iem_apsim_met(lonlat = c(-93.77, 42.02), 
                           dates = c("2013-01-01","2013-12-31"))
  
  pwr <- get_power_apsim_met(lonlat = c(-93.77, 42.02), 
                           dates = c("2013-01-01","2013-12-31"))
  
  dmt <- get_daymet_apsim_met(lonlat = c(-93.77, 42.02), 
                              years = 2013)
  
  cmp <- compare_apsim_met(iem, pwr[, 1:6], dmt[, 1:6], 
                           labels = c("IEM", "POWER", "DAYMET"))
  
  cmp <- compare_apsim_met(iem, pwr[, 1:6], dmt[, 1:6], 
                           met.var = "rain",
                           labels = c("IEM", "POWER", "DAYMET"))

  plot(cmp)
  plot(cmp, plot.type = "ts")
  plot(cmp, plot.type = "ts", met.var = "rain", cumulative = TRUE)

}

#### Different graph tests ----
if(run.apsim.met && internet){
  
  ## Testing the graph functionality
  pwr <- get_power_apsim_met(lonlat = c(-93.77, 42.02), 
                             dates = c("1990-01-01","2020-12-31"))
  
  ### default plotting options
  plot(pwr)
  plot(pwr, climatology = TRUE)
  plot(pwr, climatology = TRUE, years = 2015:2018)
  plot(pwr, climatology = TRUE, cumulative = TRUE)
  plot(pwr, climatology = TRUE, years = 2015:2018, cumulative = TRUE)
  
  plot(pwr, summary = TRUE)
  plot(pwr, summary = TRUE, climatology = TRUE)
  plot(pwr, summary = TRUE, met.var = "rain", climatology = TRUE)
  
  
  
  ## Plotting using the density option ----
  plot(pwr, plot.type = "density")
  plot(pwr, plot.type = "density", met.var = "radn")
  plot(pwr, plot.type = "density", met.var = "rain")
  
  ## Plotting using the density option
  plot(pwr, plot.type = "density", climatology = TRUE)
  plot(pwr, plot.type = "density", met.var = "radn", climatology = TRUE)
  plot(pwr, plot.type = "density", met.var = "rain", climatology = TRUE)
  
  plot(pwr, plot.type = "density", climatology = TRUE, years = c(2012, 2019))
  plot(pwr, plot.type = "density", met.var = "radn", climatology = TRUE, years = c(2012, 2019))
  plot(pwr, plot.type = "density", met.var = "rain", climatology = TRUE, years = c(2012, 2019))

  plot(pwr, plot.type = "density", summary = TRUE)
  plot(pwr, plot.type = "density", met.var = "radn_sum", summary = TRUE)
  plot(pwr, plot.type = "density", met.var = "rain", summary = TRUE)
  
  ## Testing the options with individual years
  plot(pwr, plot.type = "density", summary = TRUE, years = c(2012:2015))
  plot(pwr, plot.type = "density", met.var = "radn", summary = TRUE, years = c(2012, 2019))
  plot(pwr, plot.type = "density", met.var = "rain", summary = TRUE, years = c(2015, 2017))
  
  ## Testing with climatology and years
  plot(pwr, plot.type = "density", met.var = "rain", summary = TRUE, climatology = TRUE, years = c(2012, 2019))
  plot(pwr, plot.type = "density", met.var = "radn_sum", summary = TRUE, climatology = TRUE, years = c(2012, 2019))
  plot(pwr, plot.type = "density", met.var = "avg_maxt", summary = TRUE, climatology = TRUE, years = c(2012, 2019))

}


#### Testing anomaly features ----
if(run.apsim.met && internet){
  
  ## Testing the graph functionality
  pwr <- get_power_apsim_met(lonlat = c(-93.77, 42.02), 
                             dates = c("1990-01-01","2020-12-31"))
  
  apwr1 <- summary(pwr, anomaly = TRUE)
  apwr2 <- summary(pwr, anomaly = "rain_sum")
  apwr3 <- summary(pwr, anomaly = "rain")
  apwr4 <- summary(pwr, anomaly = "maxt")
  apwr5 <- summary(pwr, anomaly = c("maxt", "rain"))
  
  plot(pwr, plot.type = "anomaly", summary = TRUE, met.var = "rain")
  plot(pwr, plot.type = "anomaly", summary = TRUE, years = 2012:2015)
  
  plot(pwr, plot.type = "anomaly", summary = TRUE, met.var = c("avg_maxt", "rain_sum"))
  plot(pwr, plot.type = "anomaly", summary = TRUE, years = 2012:2015, met.var = c("avg_maxt", "rain_sum"))
  plot(pwr, plot.type = "anomaly", summary = TRUE, years = 2012:2015, met.var = c("rain_sum", "avg_maxt"))
  
}
