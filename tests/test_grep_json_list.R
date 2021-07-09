## Different behaviors of grep_json_list

require(apsimx)

run.grep.test <- get(".run.local.tests", envir = apsimx.options)

if(run.grep.test){
  
  file <- "Wheat.apsimx"
  src.dir <- auto_detect_apsimx_examples()
  
  apsimx_json <- jsonlite::read_json(file.path(src.dir, file))
  
  ## This function sort of works, but it is not quite what we want
  ## ans1 <- grep_json_list1("Cultivar", apsimx_json)
  
  ## A better function?
  ans <- grep_json_list("Cultivar", apsimx_json)
  
  ans <- grep_json_list("Hartog", apsimx_json)
  
  ## This does not work
  ## ans <- grep_json_list("Version", apsimx_json)

  ans <- grep_json_list("Sowing", apsimx_json)
  
  ## Testing inspect_apsimx_json
  pp0 <- inspect_apsimx_json("Wheat.apsimx", src.dir = src.dir, parm = "Version")
  
  pp1 <- inspect_apsimx_json("Wheat.apsimx", src.dir = src.dir, parm = "Simulations")
  
  pp2 <- inspect_apsimx_json("Wheat.apsimx", src.dir = src.dir, parm = "Clock") 
  
  ## pp2.1 <- inspect_apsimx_json("Wheat.apsimx", src.dir = src.dir, parm = "Start") ## Need to account for this
  
  pp3 <- inspect_apsimx_json("Wheat.apsimx", src.dir = src.dir, parm = "Weather") 
  
  ## pp3.1 <- inspect_apsimx_json("Wheat.apsimx", src.dir = src.dir, parm = "FileName") ## Need to account for this
  
  pp4 <- inspect_apsimx_json("Wheat.apsimx", src.dir = src.dir, parm = "DUL")
  
  pp6 <- inspect_apsimx_json("Wheat.apsimx", src.dir = src.dir, parm = "SWCON")
  
  pp7 <- inspect_apsimx_json("Wheat.apsimx", src.dir = src.dir, parm = "LL15")
  
  pp8 <- inspect_apsimx_json("Wheat.apsimx", src.dir = src.dir, parm = "SowingRule1") 
  
  pp9 <- inspect_apsimx_json("Wheat.apsimx", src.dir = src.dir, parm = "Hartog") 

}