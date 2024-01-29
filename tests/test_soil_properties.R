## Test soil properties in APSIM-X

require(apsimx)
require(ggplot2)

apsimx_options(warn.versions = FALSE)

run.soil <- get(".run.local.tests", envir = apsimx.options)

tmp.dir <- tempdir()
setwd(tmp.dir)

if(run.soil){
  
  ex.dir <- auto_detect_apsimx_examples()

  file.copy(file.path(ex.dir, "Wheat.apsimx"), tmp.dir)
  
  wheat <- apsimx("Wheat.apsimx")  
  
  inspect_apsimx("Wheat.apsimx", 
                 node = "Soil", soil.child = "Physical")

  duls <- c(0.52, 0.49, 0.488, 0.48, 0.47, 0.457, 0.452) * 1.05
  
  edit_apsimx("Wheat.apsimx", src.dir = ex.dir,
              wrt.dir = tmp.dir, node = "Soil",
              soil.child = "Physical", parm = "DUL",
              value = duls)   

  inspect_apsimx("Wheat-edited.apsimx", src.dir = tmp.dir,
                 node = "Soil", soil.child = "Physical")
  
  wheat2 <- apsimx("Wheat-edited.apsimx")

  ggplot() + 
    geom_line(data = wheat, aes(x = Date, y = Wheat.AboveGround.Wt, color = "Original DUL")) + 
    geom_line(data = wheat2, aes(x = Date, y = Wheat.AboveGround.Wt, color = "DUL times 1.05"))
  
}

if(run.soil){
  
  ## For now
  extd.dir <- system.file("extdata", package = "apsimx")
  
  inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, 
                 node = "Soil", soil.child = "Physical")

  inspect_apsimx("Maize.apsimx", src.dir = extd.dir, 
                 node = "Soil", soil.child = "Physical")
  
  inspect_apsimx("Wheat_soil.apsimx", src.dir = extd.dir, 
                 node = "Soil", soil.child = "Physical")
  
  ## CanI now just edit the Crop specific soil physical?
  edit_apsimx("Wheat_soil.apsimx", src.dir = extd.dir,
              wrt.dir = tmp.dir,
              node = "Soil", soil.child = "Physical",
              parm = "Maize KL", value = rep(0.07, 7))
  
  edit_apsimx("Wheat_soil-edited.apsimx", 
              src.dir = tmp.dir,
              overwrite = TRUE,
              node = "Soil", soil.child = "Physical",
              parm = "Wheat KL", value = rep(0.05, 7))
  
  inspect_apsimx("Wheat_soil-edited.apsimx", src.dir = tmp.dir, 
                 node = "Soil", soil.child = "Physical")
  
}