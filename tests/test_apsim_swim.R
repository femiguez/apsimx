## Test APSIM SWIM code for 'Classic'
require(apsimx)

## This test does not need APSIM Classic to run
run.test.apsim.swim <- TRUE

## Only write to a temp directory
tmp.dir <- tempdir()

extd.dir <- system.file("extdata", package = "apsimx")

if(run.test.apsim.swim){

  inspect_apsim("maize-swim.apsim", src.dir = extd.dir)  
  inspect_apsim("maize-swim.apsim", src.dir = extd.dir, node = "Weather")  
  inspect_apsim("maize-swim.apsim", src.dir = extd.dir, 
                node = "Soil", soil.child = "Water") 
  inspect_apsim("maize-swim.apsim", src.dir = extd.dir, 
                node = "Soil", soil.child = "SWIM") 
  edit_apsim("maize-swim.apsim", 
             src.dir = extd.dir, wrt.dir = tmp.dir,
             node = "Soil", soil.child = "SWIM",
             parm = "Salb", value = 0.10) 
  inspect_apsim("maize-swim-edited.apsim", src.dir = tmp.dir, 
                node = "Soil", soil.child = "SWIM") 
  edit_apsim("maize-swim-edited.apsim", 
             src.dir = tmp.dir, wrt.dir = tmp.dir,
             overwrite = TRUE,
             node = "Soil", soil.child = "SWIM",
             parm = "WaterTableDepth", value = 1500)
  inspect_apsim("maize-swim-edited.apsim", src.dir = tmp.dir, 
                node = "Soil", soil.child = "SWIM") 
  
}
