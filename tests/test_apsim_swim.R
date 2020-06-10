## Test APSIM SWIM code for 'Classic'
require(apsimx)

run.test.apsim.swim <- TRUE

extd.dir <- system.file("extdata", package = "apsimx")

if(run.test.apsim.swim){

  inspect_apsim("maize-swim.apsim", src.dir = extd.dir)  
  inspect_apsim("maize-swim.apsim", src.dir = extd.dir, node = "Weather")  
  inspect_apsim("maize-swim.apsim", src.dir = extd.dir, 
                node = "Soil", soil.child = "Water") 
  inspect_apsim("maize-swim.apsim", src.dir = extd.dir, 
                node = "Soil", soil.child = "SWIM") 
  edit_apsim("maize-swim.apsim", 
             src.dir = extd.dir, wrt.dir = ".",
             node = "Soil", soil.child = "SWIM",
             parm = "Salb", value = 0.10) 
  inspect_apsim("maize-swim-edited.apsim", src.dir = ".", 
                node = "Soil", soil.child = "SWIM") 
  edit_apsim("maize-swim-edited.apsim", 
             src.dir = ".", wrt.dir = ".",
             overwrite = TRUE,
             node = "Soil", soil.child = "SWIM",
             parm = "WaterTableDepth", value = 1500)
  inspect_apsim("maize-swim-edited.apsim", src.dir = ".", 
                node = "Soil", soil.child = "SWIM") 
  
}
