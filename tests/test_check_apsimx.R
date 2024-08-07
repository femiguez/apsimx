## Testing the check_apsimx function with a variety of files
require(apsimx)
packageVersion("apsimx")
apsimx_options(warn.versions = FALSE)

run.apsimx.check <- get(".run.local.tests", envir = apsimx.options)

if(run.apsimx.check){

  ex.dir <- auto_detect_apsimx_examples()
  ex.dir.list <- dir(ex.dir, pattern = "apsimx$")

  for(.j in seq_along(ex.dir.list)){
    
    file.to.check <- ex.dir.list[.j]
    if(file.to.check == "Slurp.apsimx") next
    
    if(.j %in% c(2, 16, 17, 18, 24, 29, 30, 34)){
      cat("Simulation check:", file.to.check, "\n")
      check_apsimx(file.to.check, ex.dir)
    }
    
    if(.j %in% c(4, 5, 6, 7, 8, 9, 10, 11)){
      cat("Simulation to inspect:", file.to.check, "\n")
      capo <- capture.output(inspect_apsimx(file.to.check, ex.dir, node = "Other", parm = 3))
    }

    if(.j %in% c(1, 3)){
      if(file.to.check == "BiomassRemovalFromPlant.apsimx"){
        check_apsimx(file.to.check, ex.dir, root = "UsingFixedDates")  
        check_apsimx(file.to.check, ex.dir, root = "SendingDatesFromOpperations")
      }
      if(file.to.check == "AgPasture.apsimx"){
        pps <- inspect_apsimx(file.to.check, src.dir = ex.dir, node = "Other", parm = list(1, 3:7), print.path = TRUE)
        for(.jj in pps){
          rut <- strsplit(.jj, ".", fixed = TRUE)[[1]][3]
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