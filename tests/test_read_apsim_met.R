## Testing read_apsim_met
require(apsimx)
require(nasapower)
apsimx_options(warn.versions = FALSE)

## setwd(tempdir())

run.test.apsim.met <- get(".run.local.tests", envir = apsimx.options)

if(run.test.apsim.met){
  ex.dir <- auto_detect_apsimx_examples()
  extd.dir <- system.file("extdata", package = "apsimx")

  lmf <- list.files(paste0(ex.dir,"/WeatherFiles"))

  lmf2 <- lmf[lmf %in% c("Dalby.met","Goond.met","Ingham.met","lincoln.met","Popondetta.met",
                         "VCS_Ruakura.met","WaggaWagga.met")]
}

if(run.test.apsim.met){
  for(i in lmf2){
    tmp <- read_apsim_met(file = i, src.dir = paste0(ex.dir,"/WeatherFiles"), verbose = FALSE)
    cat("Read:",i,"\n")
  }
}

## Test multiple imputation function

run.test.impute.apsim.met <- get(".run.local.tests", envir = apsimx.options)

## One trick to only test this locally?
username <- Sys.info()[["user"]]

## I need to change this test so that it downloads data instead of distributing 
## nasapower met data with the package
if(run.test.impute.apsim.met && username == "fernandomiguez"){
  
  ## This will not write a file to disk
  pwr <- get_power_apsim_met(lonlat = c(-93,42), dates = c("2012-01-01","2012-12-31"))
  ## Note that missing data is coded as -99
  summary(pwr)
  ## Check for reasonable ranges 
  check_apsim_met(pwr)
  ## replace -99 with NA
  pwr$radn <- ifelse(pwr$radn == -99, NA, pwr$radn)
  ## Impute using linear interpolation
  pwr.imptd <- impute_apsim_met(pwr, verbose = TRUE)
  summary(pwr.imptd)
  
}
