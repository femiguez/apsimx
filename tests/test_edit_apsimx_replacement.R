## Testing the edit family of functions
require(apsimx)
extd.dir <- system.file("extdata", package = "apsimx")

tmpdir <- tempdir()
setwd(tmpdir)

run.test.edit.apsimx.replacement <- TRUE

if(run.test.edit.apsimx.replacement){
  
  ## Inspect, edit, inspect
  inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                          node = "Soybean", 
                          node.child = "Leaf",
                          parm = "Gsmax350")
  
  edit_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                          wrt.dir = ".", 
                          node = "Soybean", 
                          node.child = "Leaf",
                          parm = "Gsmax350", value = 0.009, 
                          verbose = FALSE)
  
  inspect_apsimx_replacement("MaizeSoybean-edited.apsimx", src.dir = ".",
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
  
  edit_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                             node = "Soybean", 
                             node.child = "Leaf",
                             node.subchild = "Photosynthesis",
                             node.subsubchild = "RUE",
                             parm = "FixedValue", value = 1, verbose = FALSE)
  
  inspect_apsimx_replacement("MaizeSoybean-edited.apsimx", src.dir = ".",
                             node = "Soybean", 
                             node.child = "Leaf",
                             node.subchild = "Photosynthesis",
                             node.subsubchild = "RUE",
                             parm = "FixedValue")
  
}