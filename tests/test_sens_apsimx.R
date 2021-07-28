## This test will build from simple to more complex sensitivity analysis

require(apsimx)
require(ggplot2)
apsimx_options(warn.versions = FALSE)

run.sens.apsimx <- FALSE

## WheatRye example
if(run.sens.apsimx){
  
  ## Test the effect of changing RUE
  extd.dir <- "~/Dropbox/apsimx/inst/extdata"
  file.copy(file.path(extd.dir, "WheatRye.apsimx"), ".")
  file.copy(file.path(extd.dir, "Ames.met"), ".")
  
  inspect_apsimx("WheatRye.apsimx", node = "Report")
  
  edit_apsimx("WheatRye.apsimx", node = "Report",
              parm = "VariableNames",
              value = "[Wheat].Leaf.Transpiration",
              overwrite = TRUE)
  
  
  sim0 <- apsimx("WheatRye.apsimx")
  
  ggplot(data = sim0, aes(x = Date, y = Wheat.AboveGround.Wt)) + 
    geom_point()
  
  ggplot(data = sim0, aes(x = Date, y = Wheat.Leaf.Transpiration)) + 
    geom_point()
  
  inspect_apsimx_replacement("WheatRye.apsimx",
                             node = "Wheat", node.child = "Leaf",
                             node.subchild = "Photosynthesis",
                             node.subsubchild = "RUE")
  
  pp <- "Wheat.Leaf.Photosynthesis.RUE"
  
  start <- Sys.time()
  rues <- seq(0, 3, 0.25)
  res.dat <- NULL
  
  for(i in seq_along(rues)){
    
    edit_apsimx_replacement("WheatRye.apsimx",
                            node.string = pp,
                            parm = "FixedValue",
                            value = rues[i],
                            overwrite = TRUE,
                            verbose = FALSE)
    
    sim1 <- apsimx("WheatRye.apsimx")
    
    res.dat <- rbind(res.dat, data.frame(rue = rues[i], 
                                         wheat.aboveground.wt = mean(sim1$Wheat.AboveGround.Wt),
                                         wheat.leaf.lai = mean(sim1$Wheat.Leaf.LAI),
                                         wheat.leaf.transpiration = mean(sim1$Wheat.Leaf.Transpiration)))
  }
  end <- Sys.time()
  
  elapsed.time <- end - start
  
  ggplot(data = res.dat, aes(x = rue, y = wheat.aboveground.wt)) + 
    geom_point() + 
    geom_smooth(se = FALSE)
  
  ggplot(data = res.dat, aes(x = rue, y = wheat.leaf.lai)) + 
    geom_point() + 
    geom_smooth(se = FALSE)

  ggplot(data = res.dat, aes(x = rue, y = wheat.leaf.transpiration)) + 
    geom_point() + 
    geom_smooth(se = FALSE)
  
  ggplot(data = sim1, aes(x = Date, y = Wheat.AboveGround.Wt)) + 
    geom_point()
 
  ggplot(data = sim1, aes(x = Date, y = Wheat.Leaf.LAI)) + 
    geom_point()
  
  file.remove("WheatRye.apsimx")
  file.remove("Ames.met")
  file.remove("WheatRye.db")

}

## MaizeSoybean example
if(run.sens.apsimx){
  
  ## Test the effect of changing RUE
  extd.dir <- "~/Dropbox/apsimx/inst/extdata"
  file.copy(file.path(extd.dir, "MaizeSoybean.apsimx"), ".")
  file.copy(file.path(extd.dir, "Ames.met"), ".")
  
  inspect_apsimx("MaizeSoybean.apsimx", node = "Report",
                 root = "SimulationMaize")
  
  inspect_apsimx_json("MaizeSoybean.apsimx", parm = "ApsimVersion")
  
  edit_apsimx("MaizeSoybean.apsimx",
              root = "SimulationMaize",
              node = "Report",
              parm = "VariableNames",
              value = c("[Maize].AboveGround.Wt", "[Maize].Leaf.LAI", "[Maize].Leaf.Transpiration"),
              overwrite = TRUE)
  
  sim0 <- apsimx("MaizeSoybean.apsimx")
  
  ggplot(data = sim0, aes(x = Date, y = Maize.AboveGround.Wt)) + 
    geom_point()
  
  ggplot(data = sim0, aes(x = Date, y = Maize.Leaf.LAI)) + 
    geom_point()

  ggplot(data = sim0, aes(x = Date, y = Maize.Leaf.Transpiration)) + 
    geom_point()

  pp <- "Maize.Leaf.Photosynthesis.RUE"
  
  start <- Sys.time()
  rues <- seq(0, 3, 0.25)
  res.dat <- NULL
  
  for(i in seq_along(rues)){
    
    edit_apsimx_replacement("MaizeSoybean.apsimx",
                            node.string = pp,
                            parm = "FixedValue",
                            value = rues[i],
                            overwrite = TRUE,
                            verbose = FALSE)
    
    sim1 <- apsimx("MaizeSoybean.apsimx")
    
    res.dat <- rbind(res.dat, data.frame(rue = rues[i], 
                                         maize.aboveground.wt = mean(sim1$Maize.AboveGround.Wt, na.rm = TRUE),
                                         maize.leaf.lai = mean(sim1$Maize.Leaf.LAI, na.rm = TRUE),
                                         maize.leaf.transpiration = mean(sim1$Maize.Leaf.Transpiration, na.rm = TRUE)))
  }
  end <- Sys.time()
  elapsed.time <- end - start
  
  ggplot(data = res.dat, aes(x = rue, y = maize.aboveground.wt)) + 
    geom_point() + 
    geom_smooth(se = FALSE)
  
  ggplot(data = res.dat, aes(x = rue, y = wheat.leaf.lai)) + 
    geom_point() + 
    geom_smooth(se = FALSE)
  
  ggplot(data = res.dat, aes(x = rue, y = wheat.leaf.transpiration)) + 
    geom_point() + 
    geom_smooth(se = FALSE) 
    
}