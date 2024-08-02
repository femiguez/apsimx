## Testing whether 

require(apsimx)
apsimx_options(warn.versions = FALSE)

# apsimx_options(warn.versions = FALSE,
#                exe.path = "/Applications/APSIM2022.6.7044.0.app/Contents/Resources/bin/Models",
#                examples.path = "/Applications/APSIM2022.6.7044.0.app/Contents/Resources/Examples")

run.insert <- get(".run.local.tests", envir = apsimx.options)

#### This does not work at the moment
run.insert <- FALSE

tmp.dir <- tempdir()
extd.dir <- system.file("extdata", package = "apsimx")

if(run.insert){
  
  ex.dir <- auto_detect_apsimx_examples()
  
  ## tmp.dir <- "~/Desktop/csm"
  #### Test for wheat ####
  wheat <- get_apsimx_json(model = "Wheat", wrt.dir = tmp.dir)
  
  inspect_apsimx_json("Wheat.json", src.dir = tmp.dir,
                      parm = "Version")
  
  file.copy(file.path(ex.dir, "Wheat.apsimx"), tmp.dir)  
  
  inspect_apsimx("Wheat_7382.apsimx", src.dir = tmp.dir, node = "Other", parm = 2)
  
  inspect_apsimx_json("Wheat_7382.apsimx", src.dir = tmp.dir,
                      parm = "Version")
  
  ## Versions need to match, but they don't right now even with the latest
  
  insert_replacement_node("Wheat_7382.apsimx", 
                          src.dir = tmp.dir, 
                          wrt.dir = tmp.dir,
                          rep.node = wheat)
  
  inspect_apsimx("Wheat_7382-edited.apsimx", src.dir = tmp.dir, node = "Other", parm = 2)

  #### Test for maize ####
  maize <- get_apsimx_json(model = "Maize", wrt.dir = tmp.dir)  
  insert_replacement_node("Wheat_7382-edited.apsimx", 
                          src.dir = tmp.dir,
                          wrt.dir = tmp.dir,
                          rep.node = maize,
                          edit.tag = "-maize")

  #### Test for soybean ####
  soybean <- get_apsimx_json(model = "Soybean", wrt.dir = tmp.dir)
  
  file.copy(file.path(ex.dir, "Soybean.apsimx"), tmp.dir)  
  
  insert_replacement_node("Soybean.apsimx", 
                          src.dir = extd.dir, 
                          wrt.dir = tmp.dir,
                          rep.node = soybean)
  
  sim2 <- apsimx("Soybean-edited.apsimx", src.dir = tmp.dir)
  
}