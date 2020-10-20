## Testing GSOD in general and the radiation fill in feature
##
require(apsimx)

## This is for UGANDA
lonlat <- c(33, 1)
dates <- c("2000-01-01", "2019-01-01")

if(FALSE){
  ## Without filling in missing radiation data
  gsd <- get_gsod_apsim_met(lonlat = lonlat, dates = dates)
  ## Test fillin.radn feature
  gsd2 <- get_gsod_apsim_met(lonlat = lonlat, dates = dates, fillin.radn = TRUE)
  
  ## Test padding and imputation
  gsd.napad <- napad_apsim_met(gsd)
  gsd2.napad <- napad_apsim_met(gsd2)
  
  ## This shows that impute does not work on the edges
  gsd2.impt <- impute_apsim_met(gsd2.napad)
}


