## Testing apsim_met suite of functions
## Some observations:
##
## 1. check should never fail. It should always return warnings
## 2. napad should work for many different tricky situations
## 3. plotting and different combinations

require(apsimx)
require(mgcv)
require(ggplot2)
packageVersion("apsimx")
packageVersion("GSODR")
packageVersion("data.table")

run.apsim.met <- get(".run.local.tests", envir = apsimx.options)

if(.Platform$OS.type == "unix"){
  internet <- !is.null(nsl("google.com"))
}else{
  internet <- FALSE  
}
run.apsim.met <- FALSE  ## This does not work because GSODR does not work

if(run.apsim.met && internet){
  
  ## Testing napad and impute
  ## Buenos Aires 34d 36 12, 58 22 54 W
  bsas.lat <- unit_conv("34d 36' 12\" W", from = "degrees", to = "decimal")
  bsas.lon <- unit_conv("58d 22' 54\" S", from = "degrees", to = "decimal")
  
  pwr <- get_power_apsim_met(lonlat = c(bsas.lon, bsas.lat), dates = c("2010-01-01", "2015-12-31"))
  dim(pwr)
  check_apsim_met(pwr)
  gsd <- get_gsod_apsim_met(lonlat = c(bsas.lon, bsas.lat), dates = c("2010-01-01", "2015-12-31"))
  dim(gsd)
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
  
  plot(pwr, plot.type = "anomaly")
  plot(pwr, plot.type = "anomaly", summary = TRUE, met.var = "rain")
  plot(pwr, plot.type = "anomaly", summary = TRUE, years = 2012:2015)
  
  plot(pwr, plot.type = "anomaly", summary = TRUE, met.var = c("avg_maxt", "rain_sum"))
  plot(pwr, plot.type = "anomaly", summary = TRUE, years = 2012:2015, met.var = c("avg_maxt", "rain_sum"))
  plot(pwr, plot.type = "anomaly", summary = TRUE, years = 2012:2015, met.var = c("rain_sum", "avg_maxt"))
  
  pwr <- tt_apsim_met(pwr, dates = c("01-05", "30-10"), method = "Classic_TT")
  
  summary(pwr)
  summary(pwr, compute.frost = TRUE)
  
  plot(pwr, met.var = "Classic_TT")
  plot(pwr, met.var = "Classic_TT", plot.type = "density")
  plot(pwr, met.var = "Classic_TT", plot.type = "anomaly", years = 2012:2019)
  
  plot(pwr, plot.type = "anomaly", summary = TRUE, met.var = c("rain", "high_classic_tt"))
  plot(pwr, plot.type = "anomaly", summary = TRUE, met.var = c("rain", "high_classic_tt"),
       years = 2012:2016)
}

if(run.apsim.met && internet){
  
  #### Will use this code to identify the potential issue with 
  #### modifying a iem.met object
  iem.met <- get_iem_apsim_met(lonlat = c(-93.77, 42.02), 
                                dates = c("2000-01-01","2020-12-31"))
  
  iem.met <- tt_apsim_met(iem.met, method = "Classic_TT", dates = c("1-5", "31-10"))
  
  names(iem.met)
  ### This works as expected now and it removes the units properly
  iem.met$Classic_TT <- NULL 
    
  iem.met <- tt_apsim_met(iem.met, method = "Classic_TT", dates = c("1-5", "31-10"))
    
  iem.met <- remove_column_apsim_met(iem.met, "Classic_TT")

  ### This should return an error  
  try(iem.met$hola <- 100, silent = TRUE)
  
}

### Next test should be for creating the 'by' argument

#### Comparing met files with different year ranges ----

if(run.apsim.met){
  
  pwr1 <- get_power_apsim_met(lonlat = c(-93, 42), dates = c("2000-01-01", "2010-12-31"))
  pwr2 <- get_power_apsim_met(lonlat = c(-93, 42), dates = c("2011-01-01", "2024-12-31"))
  
  ### This has a better error message
  merge.test1 <- try(compare_apsim_met(pwr1, pwr2), silent = TRUE)
  
  ### What about one year in common?
  
  pwr3 <- get_power_apsim_met(lonlat = c(-93, 42), dates = c("2000-01-01", "2011-12-31"))
  
  ### This should work but it should give a message
  merge.test2 <- compare_apsim_met(pwr2, pwr3)
  
  dmt <- get_daymet_apsim_met(lonlat = c(-93, 42), years = c(2000, 2023))
  
  merge.test3 <- try(compare_apsim_met(pwr3, dmt, labels = c("POWER", "DAYMET")), silent = TRUE)
  ### The number of columns is the same
  ### This gives an error because some column names are different
  
  merge.test4 <- compare_apsim_met(pwr3[, 1:6], dmt[, 1:6], labels = c("POWER", "DAYMET"))
  plot(merge.test4, met.var = "rain", plot.type = "ts", cumulative = TRUE)
  plot(merge.test4, plot.type = "ts")
  
  #### How can I fix the previous gam fit?
  mm4 <- merge.test4$met.mrg
  
  ggplot(data = mm4, aes(x = dates, y = radn.1)) + 
    geom_point() + 
    geom_smooth(formula = y ~ s(x, bs = "cs", k = 70, fx = TRUE))
  
  
  merge.test5 <- compare_apsim_met(pwr3[, 1:6], dmt[, 1:6], labels = c("POWER", "DAYMET"),
                                   check = FALSE)
  
  plot(merge.test5, met.var = "rain", plot.type = "ts", cumulative = TRUE)
  
  ### Scramble the order of columns
  dmt.attr <- attributes(dmt)
  dmt2 <- subset(dmt, select = names(dmt)[8:1])
  attr(dmt2, "filename") <- dmt.attr$filename
  attr(dmt2, "site") <- dmt.attr$site
  attr(dmt2, "latitude") <- dmt.attr$latitude
  attr(dmt2, "longitude") <- dmt.attr$longitude
  attr(dmt2, "tav") <- dmt.attr$tav
  attr(dmt2, "colnames") <- rev(dmt.attr$colnames)
  attr(dmt2, "units") <- dmt.attr$units
  attr(dmt2, "comments") <- dmt.attr$comments
  attr(dmt2, "class") <- dmt.attr$class
  attr(dmt2, "amp") <- dmt.attr$amp
  head(dmt2)

  merge.test6 <- try(compare_apsim_met(pwr3, dmt2, labels = c("POWER", "DAYMET")), silent = TRUE)
  ### this results in an error as it should

  merge.test7 <- compare_apsim_met(pwr3[, 1:6], dmt2[, 3:8], labels = c("POWER", "DAYMET"))  
  plot(merge.test7)
  plot(merge.test7, met.var = "rain", plot.type = "ts", cumulative = TRUE)
  
  summary(pwr3)
  summary(dmt2)
  
  plot(pwr3, summary = TRUE, climatology = TRUE)
  plot(dmt2, summary = TRUE, climatology = TRUE)

  plot(pwr3, met.var = "rain", summary = TRUE, climatology = TRUE)
  plot(dmt2, met.var = "rain", summary = TRUE, climatology = TRUE)
  
  ### Testing the extractor operation, this works now
  ### DO I need to create other methods?
  ### I have created `[<-.met` and `[[<-.met`
  ### which basically send an error message and ask instead to use 'add_column_apsim_met'
  dmt5 <- dmt[, 1:4]
  class(dmt5)
  dmt6 <- dmt[1:365, 1:4]
  class(dmt6)
  
  summary(dmt6)
  
}
