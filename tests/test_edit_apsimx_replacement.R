## Testing the edit family of functions
require(apsimx)
extd.dir <- system.file("extdata", package = "apsimx")

run.test.edit.apsimx.replacement <- get(".run.local.tests", envir = apsimx.options)

tmp.dir <- tempdir()

if(run.test.edit.apsimx.replacement){
  
  ## Inspect, edit, inspect
  inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                          node = "Soybean", 
                          node.child = "Leaf",
                          parm = "Gsmax350")
  
  edit_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                          wrt.dir = tmp.dir, 
                          node = "Soybean", 
                          node.child = "Leaf",
                          parm = "Gsmax350", value = 0.009, 
                          verbose = FALSE)
  
  inspect_apsimx_replacement("MaizeSoybean-edited.apsimx", src.dir = tmp.dir,
                          node = "Soybean", 
                          node.child = "Leaf",
                          parm = "Gsmax350")
 
  ## Example for RUE 
  ## Inspect, edit, inspect
  inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                             node = "Soybean", 
                             node.child = "Leaf",
                             node.subchild = "Photosynthesis",
                             node.subsubchild = "RUE",
                             parm = "FixedValue")
  
  edit_apsimx_replacement("MaizeSoybean.apsimx", 
                          src.dir = extd.dir, wrt.dir = tmp.dir,
                          node = "Soybean", 
                          node.child = "Leaf",
                          node.subchild = "Photosynthesis",
                          node.subsubchild = "RUE",
                          parm = "FixedValue", value = 1, verbose = FALSE)
  
  inspect_apsimx_replacement("MaizeSoybean-edited.apsimx", src.dir = tmp.dir,
                             node = "Soybean", 
                             node.child = "Leaf",
                             node.subchild = "Photosynthesis",
                             node.subsubchild = "RUE",
                             parm = "FixedValue")
  
  #### Looking at Soybean
  inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                             node = "Soybean", 
                             node.child = "Cultivars",
                             node.subchild = "USA",
                             node.subsubchild = "IA2008_MG20",
                             parm = "VegetativePhotoperiod",
                             print.path = TRUE)
}

if(run.test.edit.apsimx.replacement){
  
  inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                             node = "Weather",
                             parm = "FileName",
                             root = "SimulationMaize")

  inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                             node = "Weather",
                             parm = "FileName",
                             root = "SimulationSoybean")

  pp <- inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                                   node = "field",
                                   node.child = "Soil",
                                   node.subchild = "Physical",
                                   parm = "LL15",
                                   root = "SimulationMaize",
                                   print.path = TRUE)
  
}
