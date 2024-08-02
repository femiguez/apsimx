## Testing the check_apsimx function with a variety of files
require(apsimx)
apsimx_options(warn.versions = FALSE)

run.apsimx.check <- get(".run.local.tests", envir = apsimx.options)

if(run.apsimx.check){

  ex.dir <- auto_detect_apsimx_examples()
  
  ex.dir.list <- dir(ex.dir, pattern = "apsimx$")
  
  swr <- NULL
  
  for(i in seq_along(ex.dir.list)){
    
    file.to.check <- ex.dir.list[i]
    if(i %in% c(2, 16, 17, 18, 24, 29, 30, 34)){
      cat("Simulation check:", file.to.check, "\n")
      check_apsimx(file.to.check, ex.dir)
      ## inspect_apsimx(file.to.check, ex.dir, node = "Soil", soil.child = "Organic")
    }
    
    if(i %in% c(1)){
      if(file.to.check == "AgPasture.apsimx"){
        pps <- inspect_apsimx(file.to.check, src.dir = ex.dir, node = "Other", parm = list(1, 3:7), print.path = TRUE)
        for(j in pps){
          rut <- strsplit(j, ".", fixed = TRUE)[[1]][3]
          if(rut == "Harvested Pasture by Production Year") next
          if(rut == "PastureByWaterAndNitrogen") next
          if(rut == "CO2xBaseTemperature") next
          if(rut == "CO2xHeatOnset ") next
          check_apsimx(file.to.check, ex.dir, root = rut)
        }
      }
    }
  }
}