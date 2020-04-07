## Testing the edit function with a variety of files
require(apsimx)

extd.dir <- system.file("extdata", package = "apsimx")

run.apsimx.edit <- TRUE

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
              wrt.dir = ".", overwrite = TRUE,
              node = "Manager", manager.child = "SowingFertiliser",
              parm = "Amount", value = 150)
  ## Inspect the new file to make sure it worked
  inspect_apsimx("maize-manager.apsimx", src.dir = ".", 
                 node = "Manager", parm = list("SowingFert",1))
  
}

run.apsim.edit.millet <- TRUE

if(run.apsim.edit.millet){
  
  ## Inspect
  pp <- inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Manager",
                  parm = list("Sow on a fixed date",5), print.path = TRUE)
  ## Edit
  edit_apsim("Millet.apsim", src.dir = extd.dir, 
             node = "Other", wrt.dir = ".",
             overwrite = TRUE,
             parm.path = pp,
             value = 8)
  
  ## Try using manager.child
  inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Manager")
  ## This prints available manager childs
  inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Manager",
                parm = list("Sow on a fixed date",NA))
  
  edit_apsim("Millet.apsim", src.dir = extd.dir, 
              wrt.dir = ".", node = "Manager",
             manager.child = "Sow on a fixed date",
             edit.tag = "-smurf",
             parm = "cultivar",
             value = "smurfs")
  
  
}