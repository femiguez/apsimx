## Testing the calculation of thermal time in APSIM
require(apsimx)
require(ggplot2)

run.test.apsim.met.tt <- FALSE

if(run.test.apsim.met.tt){
  
  tmp.dir <- tempdir()
  
  ex.dir <- auto_detect_apsimx_examples()
  
  file.copy(file.path(ex.dir, "Maize.apsimx"), tmp.dir)
  
  inspect_apsimx("Maize.apsimx", tmp.dir, node = "Report")
  
  edit_apsimx("Maize.apsimx", src.dir = tmp.dir, 
              node = "Report", parm = "EventNames",
              value = "[Clock].EndOfDay", overwrite = TRUE)

  edit_apsimx("Maize.apsimx", src.dir = tmp.dir, 
              node = "Report", parm = "VariableNames",
              value = "[Maize].Phenology.AccumulatedTT", overwrite = TRUE)
  
  inspect_apsimx("Maize.apsimx", src.dir = tmp.dir, node = "Manager",
                 parm = list("SowingRule", NA))
  
  inspect_apsimx("Maize.apsimx", src.dir = tmp.dir, node = "Clock")
  
  sim0 <- apsimx("Maize.apsimx", src.dir = tmp.dir)
 
  ggplot(data = sim0, aes(x = Date, y = Maize.Phenology.AccumulatedTT)) + 
    geom_line()
  
  ## Bring metfile here
  inspect_apsimx("Maize.apsimx", src.dir = tmp.dir, node = "Weather")
  
  file.copy(file.path(ex.dir, "WeatherFiles/Dalby.met"), tmp.dir)
  
  dalby <- read_apsim_met("Dalby.met", src.dir = tmp.dir)
  
  ## What is the emergence date for 1990
  sim0[sim0$Maize.Phenology.CurrentStageName == "Emergence",]
  sim0[sim0$Maize.Phenology.CurrentStageName == "HarvestRipe",]
  
  dalby.tt <- tt_apsim_met(dalby, dates = c("07-01", "28-05"), x_temp = c(10, 20, 30, 40), y_tt = c(0, 10, 20, 0))
  
  sim0$year <- as.numeric(format(sim0$Date, "%Y"))
  sim1991 <- subset(sim0, year == 1991)
 
  dalby.tt.1991 <- subset(dalby.tt, year == 1991)
  
  ggplot() + 
    geom_line(data = sim1991, aes(x = Date, y = Maize.Phenology.AccumulatedTT, color = "APSIM")) + 
    geom_line(data = dalby.tt.1991, aes(x = Date, y = APSIM_TT, color = "apsimx"))
  
  
  file.remove(file.path(tmp.dir, "Maize.apsimx"))
  
  
}

## Test for napad and impute
if(FALSE){
  
  dts <- c("1990-01-01", "2020-12-31")
  iem <- get_iem_apsim_met(lonlat = c(-93.77, 42), dates = dts)
  pwr <- get_power_apsim_met(lonlat = c(-93.77, 42), dates = dts)
  
  nrow(iem); nrow(pwr)

  dmt <- get_daymet2_apsim_met(lonlat = c(-93.77, 42), years = c(1990, 2020))    
  nrow(iem); nrow(pwr); nrow(dmt)
  
  check_apsim_met(iem)
  check_apsim_met(pwr)
  check_apsim_met(dmt)

  ## Fix IEM
  iem[iem$radn < 0, "radn"] <- NA
  ## The problem is that met radn has a lot of NA's
  iem2 <- impute_apsim_met(iem)
  check_apsim_met(pwr)
  
  dmt2 <- napad_apsim_met(dmt)
  dmt3 <- impute_apsim_met(dmt2)
  check_apsim_met(dmt3)
}

