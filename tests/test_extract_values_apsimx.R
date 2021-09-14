## Testing the function for extracting values from an arbitrary .apsimx file
## using extract_value_apsimx
require(apsimx)

run.extract.value.apsimx <- FALSE

if(run.extract.value.apsimx){
  
  ## Testing with the Barley.apsimx example
  ex.dir <- auto_detect_apsimx_examples()
  
  pp1 <- inspect_apsimx("Barley.apsimx", src.dir = ex.dir)
  ## Extract value of Start
  extract_values_apsimx("Barley.apsimx", src.dir = ex.dir,
                        parm.path = paste0(pp1, ".Start"))
  ## Extract value of End
  extract_values_apsimx("Barley.apsimx", src.dir = ex.dir,
                        parm.path = paste0(pp1, ".End"))
  ## Weather
  pp2 <- inspect_apsimx("Barley.apsimx", src.dir = ex.dir, 
                        node = "Weather")
  ## Extract weather file
  extract_values_apsimx("Barley.apsimx", src.dir = ex.dir,
                        parm.path = paste0(pp2, ".FileName"))
  ## Manager
  pp3 <- inspect_apsimx("Barley.apsimx", src.dir = ex.dir, 
                        node = "Manager", parm = list("Sowing", 1))
  ## Extract fertilizer amount
  extract_values_apsimx("Barley.apsimx", src.dir = ex.dir,
                        parm.path = paste0(pp3, ".Amount"))
  ## Row Spacing
  pp4 <- inspect_apsimx("Barley.apsimx", src.dir = ex.dir, 
                        node = "Manager", parm = list("fixed", 5))
  ## Extract the row spacing
  ex.rs <- extract_values_apsimx("Barley.apsimx", src.dir = ex.dir,
                                 parm.path = paste0(pp4, ".RowSpacing"))
  
  ## What about vectors?
  pp5 <- inspect_apsimx("Barley.apsimx", src.dir = ex.dir, node = "Soil",
                        soil.child = "Physical", parm = "DUL")
  ## Extract DUL
  ex.dul <- extract_values_apsimx("Barley.apsimx", src.dir = ex.dir,
                                  parm.path = paste0(pp5, ".DUL"))
  
  ## On the replacement example
  extd.dir <- system.file("extdata", package = "apsimx")
  pp6 <- inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                                    node = "Soybean",
                                    node.child = "Leaf", node.subchild = "ExtinctionCoeff",
                                    node.subsubchild = "XYPairs", parm = "Y")
  ## Extract it
  ext.exc <- extract_values_apsimx("MaizeSoybean.apsimx", src.dir = extd.dir,
                                   parm.path = pp6)
  ## Try RUE
  pp7 <- inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                                    node = "Soybean",
                                    node.child = "Leaf", node.subchild = "Photosynthesis", 
                                    node.subsubchild = "RUE", parm = "FixedValue")
  ext.rue <- extract_values_apsimx("MaizeSoybean.apsimx", src.dir = extd.dir, parm.path = pp7)
}