## Testing the edit function with a variety of files
require(apsimx)

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