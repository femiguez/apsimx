## Testing the edit function with a variety of files
require(apsimx)
apsimx_options(warn.versions = FALSE)

extd.dir <- system.file("extdata", package = "apsimx")

run.apsimx.edit <- get(".run.local.tests", envir = apsimx.options)

tmp.dir <- tempdir()

if(run.apsimx.edit){
  
  ## Inspect manager, Let's look at fertiliser first
  inspect_apsimx("maize-manager.apsimx", src.dir = extd.dir, 
                 node = "Manager", parm = list("SowingFert",NA))
  ## Inspect the fertilizer
  inspect_apsimx("maize-manager.apsimx", src.dir = extd.dir, 
                 node = "Manager", parm = list("SowingFert",1))
  ## Generate the path
  pp <- inspect_apsimx("maize-manager.apsimx", src.dir = extd.dir, 
                       node = "Manager", parm = list("SowingFert",1),
                       print.path = TRUE)
  ## Edit by moving the file to the current directory
  edit_apsimx("maize-manager.apsimx", src.dir = extd.dir, 
              wrt.dir = tmp.dir, overwrite = TRUE,
              node = "Manager", manager.child = "SowingFertiliser",
              parm = "Amount", value = 150)
  ## Inspect the new file to make sure it worked
  inspect_apsimx("maize-manager.apsimx", src.dir = tmp.dir, 
                 node = "Manager", parm = list("SowingFert",1))
  
  ## Test the maize manager folder
  edit_apsimx("maize-manager-folder.apsimx", src.dir = extd.dir, 
              wrt.dir = tmp.dir, overwrite = TRUE,
              node = "Manager", manager.child = "SowingFertiliser",
              parm = "Amount", value = 150)
  
}

run.apsim.edit.millet <- get(".run.local.tests", envir = apsimx.options)

if(run.apsim.edit.millet){
  
  ## Inspect
  pp <- inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Manager",
                  parm = list("Sow on a fixed date",5), print.path = TRUE)
  ## Edit
  edit_apsim("Millet.apsim", src.dir = extd.dir, 
             node = "Other", wrt.dir = tmp.dir,
             overwrite = TRUE,
             parm.path = pp,
             value = 8)
  
  ## Try using manager.child
  inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Manager")
  ## This prints available manager childs
  inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Manager",
                parm = list("Sow on a fixed date",NA))
  
  edit_apsim("Millet.apsim", src.dir = extd.dir, 
              wrt.dir = tmp.dir, node = "Manager",
             manager.child = "Sow on a fixed date",
             edit.tag = "-smurf",
             parm = "cultivar",
             value = "smurfs")
  
  ## Testing with Maize factorial
  pmf <- inspect_apsim("maize-factorial.apsim", src.dir = extd.dir, 
                       root = "IA-CC_Canisteo_Cover",
                       node = "Weather", print.path = TRUE)

  ## Testing with the maize-factorial.apsim file
  inspect_apsim("maize-factorial.apsim", src.dir = extd.dir, 
                node = "Weather",
                root = "IA-CC_Canisteo_Cover")

  inspect_apsim("maize-factorial.apsim", src.dir = extd.dir, 
                node = "Weather",
                root = "IA-CC_Canisteo_No-Cover")

  edit_apsim("maize-factorial.apsim", 
             src.dir = extd.dir,
             wrt.dir = tmp.dir,
             node = "Weather", 
             root = "IA-CC_Canisteo_Cover",
             value = "Ames.met")
  
  inspect_apsim("maize-factorial-edited.apsim", 
                src.dir = tmp.dir, 
                node = "Weather",
                root = "IA-CC_Canisteo_Cover")
  
  edit_apsim("maize-factorial-edited.apsim", 
             src.dir = tmp.dir,
             overwrite = TRUE,
             node = "Weather", 
             root = "IA-CC_Canisteo_No-Cover",
             value = "Boone.met")
  
  inspect_apsim("maize-factorial-edited.apsim", 
                src.dir = tmp.dir, 
                node = "Weather",
                root = "IA-CC_Canisteo_No-Cover")
}

run.apsimx.edit.maize.soil <- get(".run.local.tests", envir = apsimx.options)

if(run.apsimx.edit.maize.soil){
  
  inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, 
                 node = "Soil", parm = "Site", print.path = TRUE)
  
  edit_apsimx("Wheat.apsimx", 
              src.dir = extd.dir, wrt.dir = tmp.dir,
              node = "Soil", soil.child = "Metadata", 
              parm = "Site", value = "Ames")
  
  inspect_apsimx("Wheat-edited.apsimx", src.dir = tmp.dir, 
                 node = "Soil")
  
}

run.apsimx.edit.soil.soilwat <- get(".run.local.tests", envir = apsimx.options)

if(run.apsimx.edit.soil.soilwat){

  inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, 
                 node = "Soil", soil.child = "SoilWater", 
                 parm = "SummerCona")
    
  edit_apsimx("Wheat.apsimx", 
              src.dir = extd.dir, wrt.dir = tmp.dir,
              node = "Soil", soil.child = "SoilWater", 
              parm = "SummerCona", value = 6)
  
  inspect_apsimx("Wheat-edited.apsimx", src.dir = tmp.dir, 
                 node = "Soil", soil.child = "SoilWater", 
                 parm = "SummerCona")

  pp <- inspect_apsimx("Wheat-edited.apsimx", src.dir = tmp.dir,
                       node = "Soil", soil.child = "SoilWater",
                       parm = "SummerCona", print.path = TRUE)  
  
  edit_apsimx("Wheat-edited.apsimx", 
              src.dir = tmp.dir, wrt.dir = tmp.dir,
              node = "Other", parm.path = pp, value = 7,
              overwrite = TRUE)
  
  inspect_apsimx("Wheat-edited.apsimx", src.dir = tmp.dir, 
                 node = "Soil", soil.child = "SoilWater", 
                 parm = "SummerCona")
  
}

## Testing Report feature
if(run.apsimx.edit){
  
 ex.dir <- auto_detect_apsimx_examples()
 
 inspect_apsimx("Wheat.apsimx", src.dir = ex.dir, node = "Report")
 
 edit_apsimx("Wheat.apsimx", src.dir = ex.dir,
             node = "Report", wrt.dir = tmp.dir,
             parm = "VariableNames",
             value = "[Soil].esw")
 
 inspect_apsimx("Wheat-edited.apsimx", src.dir = tmp.dir, node = "Report")
 
 edit_apsimx("Wheat.apsimx", src.dir = ex.dir,
             node = "Report", wrt.dir = tmp.dir,
             parm = "EventNames",
             value = "[Wheat].EndOfDay")
 
 inspect_apsimx("Wheat-edited.apsimx", src.dir = tmp.dir, node = "Report")
 
 ## Running soil carbon
 edit_apsimx("Wheat.apsimx", src.dir = ex.dir,
             node = "Report", wrt.dir = tmp.dir,
             parm = "VariableNames",
             value = "[Soil].Nutrient.Organic.C",
             edit.tag = "-carbon")
 
 inspect_apsimx("Wheat-carbon.apsimx", src.dir = tmp.dir, node = "Report")
 
 sim.o <- apsimx("Wheat-carbon.apsimx", src.dir = tmp.dir)
 
 edit_apsimx("Soybean.apsimx", src.dir = ex.dir,
             node = "Report", wrt.dir = tmp.dir,
             parm = "VariableNames",
             value = "[Soil].Nutrient.Organic.C",
             edit.tag = "-carbon")
 
 inspect_apsimx("Soybean-carbon.apsimx", src.dir = tmp.dir, node = "Report")
 
 sim.o <- apsimx("Soybean-carbon.apsimx", src.dir = tmp.dir)
  
}

## Testing edit when node = "Other"

if(run.apsimx.edit){
  
  extd.dir <- system.file("extdata", package = "apsimx")
  
  pp <- inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Manager",
                       parm = list("SowingFert", NA))
  
  edit_apsimx("Wheat.apsimx", node = "Other",
              src.dir = extd.dir, wrt.dir = tmp.dir,
              parm.path = pp, parm = "Amount", value = 5,
              overwrite = TRUE)

  inspect_apsimx("Wheat.apsimx", src.dir = tmp.dir, node = "Manager",
                 parm = list("SowingFertiliser", NA))  
  
  file.copy(file.path(tmp.dir, "Wheat.apsimx"), ".")
  
}

## Testing changing variables in the outputfile
if(run.apsimx.edit){
 
  extd.dir <- system.file("extdata", package = "apsimx")
  file.copy(file.path(extd.dir, "Millet.apsim"), tmp.dir)
  
  inspect_apsim("Millet.apsim", src.dir = tmp.dir, node = "Outputfile", parm = "title")

  edit_apsim("Millet.apsim", src.dir = tmp.dir,
             node = "Outputfile", parm = "variables", 
             value = "surfaceom_wt")
    
  edit_apsim("Millet.apsim", src.dir = tmp.dir,
             node = "Outputfile",
             parm = "variables", value = "surfaceom_wt")
  
  inspect_apsim("Millet-edited.apsim", src.dir = tmp.dir, 
                node = "Outputfile", parm = "variables")
  
}