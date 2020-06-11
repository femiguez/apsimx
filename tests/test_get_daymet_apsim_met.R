## Testing the get_daymet_apsim_met function
## Note (2020-02-09)
## I've ran this test several times now on Mac and Windows
## I won't run it again until it is needed
## However, note that the source of weather data can
## have a large impact on final yield estimates
## the difference is 171.1547 g per meter squared
## with higher values for power over daymet
library(FedData)
library(nasapower)
library(apsimx)
apsimx_options(warn.versions = FALSE)

run.test.daymet <- get(".run.local.tests", envir = apsimx.options)

username <- Sys.info()[["user"]]

if(run.test.daymet && username == "fernandomiguez"){

  lonlat <- c(-93, 42)
  
  dmet <- get_daymet_apsim_met(lonlat = lonlat, 
                               years = 2015:2016,
                               width.height = c(1e-5,1e-5),
                               label = "CIA",
                               filename = "cia-daymet.met",
                               cleanup = FALSE)  
  
  cia.daymet <- read_apsim_met("cia-daymet.met")
  
  check_apsim_met(cia.daymet)
}

## Does the file produced by DayMet work in APSIM?

run.test.maize.daymet <- get(".run.local.tests", envir = apsimx.options)

if(run.test.maize.daymet && username == "fernandomiguez"){

  extd.dir <- system.file("extdata", package = "apsimx")

  edit_apsimx("Maize.apsimx", 
              node = "Clock", 
              src.dir = extd.dir, 
              wrt.dir = ".",
              parm = c("Start","End"),
              value = c("2015-01-01","2016-12-30"),
              overwrite = FALSE)

  inspect_apsimx("Maize-edited.apsimx", node = "Clock")

  edit_apsimx("Maize-edited.apsimx", node = "Weather", value = "cia-daymet.met",
              overwrite = TRUE)

  inspect_apsimx("Maize-edited.apsimx", node = "Weather")

  maize.daymet <- apsimx("Maize-edited.apsimx", value = "report")

}

## What if I get data from POWER?

run.test.power <- get(".run.local.tests", envir = apsimx.options)

if(run.test.power && username == "fernandomiguez"){

  lonlat <- c(-93, 42)

  pwr <- get_power_apsim_met(lonlat = lonlat,
                             dates = c("2015-01-01","2016-12-31"),
                             filename = "cia-power.met")
  
  pwr.met <- read_apsim_met("cia-power.met")
  
  check_apsim_met(pwr.met)
  
  pwr.met$radn <- ifelse(pwr.met$radn == -99, NA, pwr.met$radn)
    
  pwr.met2 <- impute_apsim_met(pwr.met, verbose = TRUE)
  
  check_apsim_met(pwr.met2)
  
  write_apsim_met(pwr.met2, wrt.dir = ".", filename = "cia-power2.met")
}

run.test.maize.power <- get(".run.local.tests", envir = apsimx.options)

if(run.test.maize.daymet && username == "fernandomiguez"){
  
  edit_apsimx("Maize-edited.apsimx", node = "Weather", value = "cia-power2.met",
              edit.tag = "-power")
  
  maize.power <- apsimx("Maize-edited-power.apsimx", value = "report")
  
  yld.diff <- maize.power$Maize.Grain.Wt - maize.daymet$Maize.Grain.Wt
}

