## The purpose of this test file is to consider the inspecting
## and editing capabilities of:
##
## 1. inspect_apsimx_json
## 2. edit_apsimx (node == "Other")
## 3. edit_apsimx_json
##
## I need to consider both files with a .apsimx and .json extension
require(apsimx)
apsimx_options(warn.versions = FALSE)

run.edit.apsimx.json <- get(".run.local.tests", envir = apsimx.options)

if(run.edit.apsimx.json){
  
  tmp.dir <- tempdir()
  ## Testing with an .apsimx file first
  ex.dir <- auto_detect_apsimx_examples()
  pp <- inspect_apsimx_json("Barley.apsimx", src.dir = ex.dir, parm = "Version$")
  ## Try editing
  edit_apsimx_json("Barley.apsimx", src.dir = ex.dir, wrt.dir = tmp.dir,
                   parm.path = pp[1], value = 200)
  inspect_apsimx_json("Barley-edited.apsimx", src.dir = tmp.dir, parm = "Version")
  
}