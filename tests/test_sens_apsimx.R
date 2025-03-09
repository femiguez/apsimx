## This test will build from simple to more complex sensitivity analysis

require(apsimx)
packageVersion("apsimx")
require(ggplot2)
apsimx_options(warn.versions = FALSE)

run.sens.apsimx <- FALSE

tmp.dir <- tempdir()
setwd(tmp.dir)

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
  file.remove("WheatRye.db-shm")
  file.remove("WheatRye.db-wal")
}

## MaizeSoybean example
if(run.sens.apsimx){
  
  ## Test the effect of changing RUE
  extd.dir <- "~/Dropbox/apsimx/inst/extdata"
  file.copy(file.path(extd.dir, "MaizeSoybean.apsimx"), ".")
  file.copy(file.path(extd.dir, "Ames.met"), ".")
  
  inspect_apsimx("MaizeSoybean.apsimx", node = "Report",
                 root = "SimulationMaize")
  
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
  
  ggplot(data = res.dat, aes(x = rue, y = maize.leaf.lai)) + 
    geom_point() + 
    geom_smooth(se = FALSE)
  
  ggplot(data = res.dat, aes(x = rue, y = maize.leaf.transpiration)) + 
    geom_point() + 
    geom_smooth(se = FALSE) 
  
  ## For the specific simulation
  ggplot(data = sim1, aes(x = Date, y = Maize.AboveGround.Wt)) + 
    geom_point()
  
  ggplot(data = sim1, aes(x = Date, y = Maize.Leaf.LAI)) + 
    geom_point()

  ggplot(data = sim1, aes(x = Date, y = Maize.Leaf.Transpiration)) + 
    geom_point()
  
  ### Clean up
  file.remove(dir(pattern = "MaizeSoybean"))
  file.remove("Ames.met")
}

if(run.sens.apsimx){
  
  extd.dir <- system.file("extdata", package = "apsimx")
  file.copy(file.path(extd.dir, "Wheat.apsimx"), tmp.dir)
  file.copy(file.path(extd.dir, "Ames.met"), ".")
  ## Identify a parameter of interest
  ## In this case we want to know the impact of varying the fertilizer amount
  pp <- inspect_apsimx("Wheat.apsimx", src.dir = tmp.dir, 
                       node = "Manager", parm = list("SowingFertiliser", 1))
  pp <- paste0(pp, ".Amount")
  ## Do we need to edit the Report?
  edit_apsimx("Wheat.apsimx", src.dir = tmp.dir, node = "Report",
              parm = "VariableNames", value = c("max of [Wheat].Leaf.LAI from 1-Jan to 31-Dec as MaximumLAI", 
                                                "sum of [Wheat].Leaf.Transpiration from 1-Jan to 31-Dec as AnnualLeafTranspiration"),
              overwrite = TRUE)
  ## Before the analysis
  sim0 <- apsimx("Wheat.apsimx", src.dir = tmp.dir)
  
  head(sim0)
  
  ggplot(data = sim0, aes(x = Date, y = MaximumLAI)) + 
    geom_point()
  
  ggplot(data = sim0, aes(x = MaximumLAI, y = Yield)) + 
    geom_point()
  
  ## For simplicity, we create a vector of fertilizer amounts (instead of sampling)
  start <- Sys.time()
  ferts <- seq(5, 300, length.out = 10)
  col.res <- NULL
  for(i in 1:10){
    
    edit_apsimx("Wheat.apsimx", src.dir = tmp.dir,
                node = "Other", parm.path = pp,
                value = ferts[i],
                overwrite = TRUE)
    
    sim <- apsimx("Wheat.apsimx", src.dir = tmp.dir)
    col.res <- rbind(col.res, data.frame(fertilizer.amount = ferts[i], 
                                         wheat.aboveground.wt = mean(sim$Wheat.AboveGround.Wt, na.rm = TRUE),
                                         wheat.yield = mean(sim$Yield, na.rm = TRUE),
                                         wheat.maximum.lai = mean(sim$MaximumLAI, na.rm = TRUE),
                                         wheat.annual.leaf.transpiration = mean(sim$AnnualLeafTranspiration, na.rm = TRUE)))
  }
  end <- Sys.time()
  elapsed.time <- end - start
  
  ggplot(col.res, aes(x = fertilizer.amount, y = wheat.aboveground.wt)) + 
    geom_point()
  
  ggplot(col.res, aes(x = fertilizer.amount, y = wheat.yield)) + 
    geom_point()

  ggplot(col.res, aes(x = fertilizer.amount, y = wheat.maximum.lai)) + 
    geom_point()

  ggplot(col.res, aes(x = fertilizer.amount, y = wheat.annual.leaf.transpiration)) + 
    geom_point()
  
  ### Clean up
  file.remove(dir(pattern = "Wheat"))
  file.remove("Ames.met")

}

if(run.sens.apsimx){
  
  extd.dir <- system.file("extdata", package = "apsimx")
  file.copy(file.path(extd.dir, "Wheat.apsimx"), ".")
  file.copy(file.path(extd.dir, "Ames.met"), ".")
  ## Identify a parameter of interest
  ## In this case we want to know the impact of varying the fertilizer amount
  ## and the plant population
  pp1 <- inspect_apsimx("Wheat.apsimx", src.dir = ".", 
                       node = "Manager", parm = list("SowingFertiliser", 1))
  pp1 <- paste0(pp1, ".Amount")
  
  pp2 <- inspect_apsimx("Wheat.apsimx", src.dir = tmp.dir, 
                       node = "Manager", parm = list("SowingRule1", 9))
  pp2 <- paste0(pp2, ".Population")
  
  grd <- expand.grid(parm1 = c(50, 100, 150), parm2 = c(100, 200, 300))
  names(grd) <- c("Fertiliser", "Population")
  
  start <- Sys.time()
  sns <- sens_apsimx("Wheat.apsimx", src.dir = tmp.dir,
                     parm.paths = c(pp1, pp2),
                     grid = grd,
                     cores = 2L)
  end <- Sys.time()
  ### Time elapsed with 2 cores (Mac 2017): 1.158267 (minutes)
  ### Time elapsed with 1 core (Mac 2017): 1.921198 (minutes)
  ### Time elapsed with 2 cores (Mac 2021): 55.6 (seconds)
  
  summary(sns)
  summary(sns, select = "Wheat.AboveGround.Wt")
  summary(sns, select = "AboveGround")
  ## summary(sns, select = "AboveGround", formula = 2L)

  sns.nn <- sens_apsimx("Wheat.apsimx", src.dir = tmp.dir,
                        parm.paths = c(pp1, pp2),
                        grid = grd, summary = "none",
                        cores = 2L)
  
  summary(sns.nn, select = "Wheat.AboveGround.Wt")
  
  ## it is possible to use the package sensitivity to design an experiment
  library(sensitivity)
  X.mrrs <- morris(factors = c("Fertiliser", "Population"), r = 3, 
                     design = list(type = "oat", levels = 3, grid.jump = 1), 
                     binf = c(5, 50), bsup = c(200, 300))
  
  sns2 <- sens_apsimx("Wheat.apsimx", src.dir = tmp.dir,
                      parm.paths = c(pp1, pp2),
                      grid = X.mrrs$X,
                      cores = 2L)
  ## These are the sensitivity results for AboveGround.Wt only
  sns.res.ag <- tell(X.mrrs, sns2$grid.sims$Wheat.AboveGround.Wt)
  sns.res.ag
  plot(sns.res.ag)
  
  ### Clean up
  file.remove(dir(pattern = "Wheat"))
  file.remove(dir(pattern = "Ames.met"))
  
}

#### Create Classic example ####
if(run.sens.apsimx){
  
  apsim_options(warn.versions = FALSE)
  ex.dir <- auto_detect_apsim_examples()
  extd.dir <- system.file("extdata", package = "apsimx")
  file.copy(file.path(extd.dir, "Millet.apsim"), ".")  
  file.copy(file.path(ex.dir, "MetFiles", "Goond.met"), ".")

  pp1 <- inspect_apsim("Millet.apsim", src.dir = tmp.dir, 
                        node = "Manager", 
                       parm = list("Sow", 2),
                       print.path = TRUE)
  
  pp2 <- inspect_apsim("Millet.apsim", src.dir = tmp.dir, 
                       node = "Manager", 
                       parm = list("Sow", 5), 
                       print.path = TRUE)

  grd <- expand.grid(parm1 = c("15-nov", "1-dec", "15-dec"), parm2 = c(5, 7, 9))
  names(grd) <- c("date", "density")    
  
  edit_apsim("Millet.apsim", node = "Clock",
             parm = "start_date", value = "01/01/1980",
             overwrite = TRUE)
  
  sim0 <- apsim("Millet.apsim")
  
  sns <- sens_apsim("Millet.apsim",
                     parm.paths = c(pp1, pp2),
                     grid = grd)
  
  summary(sns)
  summary(sns, select = "millet_biomass")
  
  sns2 <- sens_apsim("Millet.apsim",
                    parm.paths = c(pp1, pp2),
                    grid = grd,
                    summary = "none")
  
  sns2$grid.sims[1:5, 1:5]  
  
}

#### Testing the cores argument ####

run.sens.apsimx.cores <- FALSE

if(run.sens.apsimx.cores){
  
  extd.dir <- system.file("extdata", package = "apsimx")
  file.copy(file.path(extd.dir, "Wheat.apsimx"), ".")
  file.copy(file.path(extd.dir, "Ames.met"), ".")
  ## Identify a parameter of interest
  ## In this case we want to know the impact of varying the fertilizer amount
  ## and the plant population
  pp1 <- inspect_apsimx("Wheat.apsimx", src.dir = ".", 
                        node = "Manager", parm = list("SowingFertiliser", 1))
  pp1 <- paste0(pp1, ".Amount")
  
  pp2 <- inspect_apsimx("Wheat.apsimx", src.dir = tmp.dir, 
                        node = "Manager", parm = list("SowingRule1", 9))
  pp2 <- paste0(pp2, ".Population")
  
  grd <- expand.grid(parm1 = c(50, 100, 150), parm2 = c(100, 200, 300))
  names(grd) <- c("Fertiliser", "Population")
  
  ## dim(grd) ## 9 simulations
  start <- Sys.time()
  sns <- sens_apsimx("Wheat.apsimx", src.dir = tmp.dir,
                     parm.paths = c(pp1, pp2),
                     grid = grd)
  (Sys.time() - start)
  ## This takes 1.55 minutes (Mac)
  ## This now (Jan 2024) takes 2.14 minutes (Mac)
  ## Mac 2017 (Aug 2024): 1.800195
  ## Mac 2021 (Mar 2025): 1.5 minutes
  summary(sns)
  summary(sns, select = "Wheat.AboveGround.Wt", scale = TRUE)
  summary(sns, select = "AboveGround", scale = TRUE)
  
  start <- Sys.time()
  sns.c2 <- sens_apsimx("Wheat.apsimx", src.dir = tmp.dir,
                        parm.paths = c(pp1, pp2),
                        grid = grd, cores = 2L)
  (Sys.time() - start)
  ## This takes 1.29 minutes (Jan 2024)
  ## Windows: NA
  ## Mac 2017 (Aug 2024): 1.319598
  ## Mac 2021 (Mar 2025): 54 seconds 
  ## Are they the same?
  (diff.sns.vs.sns.c2 <- sum(colSums(sns$grid.sims - sns.c2$grid.sims)))
  
  if(abs(diff.sns.vs.sns.c2) > 0.001)
    stop("Simulations with 2 cores do not match")
  
  start <- Sys.time()
  sns.c4 <- sens_apsimx("Wheat.apsimx", src.dir = tmp.dir,
                        parm.paths = c(pp1, pp2),
                        grid = grd, cores = 4)
  (Sys.time() - start)
  ## Mac 2017 (Aug 2024) takes: 1.161401 minutes
  ## Mac 2021 (Mar 2025) takes: 33.9 seconds
  (diff.sns.vs.sns.c4 <- sum(colSums(sns$grid.sims - sns.c4$grid.sims)))
  
  if(abs(diff.sns.vs.sns.c4) > 0.001)
    stop("Simulations with 4 cores do not match")

  #### Summary = 'none' and 1L core
  start <- Sys.time()
  sns.s0 <- sens_apsimx("Wheat.apsimx", src.dir = tmp.dir,
                        parm.paths = c(pp1, pp2),
                        summary = "none",
                        grid = grd)
  (Sys.time() - start)
  
  start <- Sys.time()
  sns.s0.c2 <- sens_apsimx("Wheat.apsimx", src.dir = tmp.dir,
                           parm.paths = c(pp1, pp2),
                           summary = "none",
                           grid = grd,
                           cores = 2L)
  (Sys.time() - start)
  ### The difference in times here are: Mac 2017 (1 core) - 1.63 vs. (2 cores) - 1.1
  ### The difference in times here are: Mac 2021 (1 core) - 1.52 vs. (2 cores) - 54.4 seconds
  ## library(arsenal) 
  
  ## Data.frames are not identical, but summary results are
  ## cm1 <- arsenal::comparedf(sns.s0$grid.sims, sns.s0.c2$grid.sims)
  sns.s0$grid.sims[78, "Fertiliser"]
  sns.s0.c2$grid.sims[78, "Fertiliser"]
  ## The results are the same, but in different order
  
  summary(sns.s0, select = "AboveGround.Wt", scale = TRUE)
  summary(sns.s0.c2, select = "AboveGround.Wt", scale = TRUE)
  
  summary(sns.s0, select = "AboveGround.N", scale = TRUE)
  summary(sns.s0.c2, select = "AboveGround.N", scale = TRUE)
  
  summary(sns.s0, select = "Yield", scale = TRUE)
  summary(sns.s0.c2, select = "Yield", scale = TRUE)
  
  ### What if I reorder the simulations?
  snsd1 <- sns.s0$grid.sims
  snsd2 <- sns.s0.c2$grid.sims

  snsd1.o <- snsd1[order(snsd1$Fertiliser, snsd1$Population, snsd1$Date), ]
  snsd2.o <- snsd2[order(snsd2$Fertiliser, snsd2$Population, snsd2$Date), ]
  ##cmpd12 <- arsenal::comparedf(snsd2.o, snsd2.o)   
  
  ### Clean up
  file.remove(dir(pattern = "Wheat"))
  file.remove(dir(pattern = "Ames.met"))

}

#### Testing passing of soil profiles -----

run.sens.apsimx.soils <- FALSE

if(run.sens.apsimx.soils){
  
  extd.dir <- system.file("extdata", package = "apsimx")
  file.copy(file.path(extd.dir, "Wheat.apsimx"), ".")
  file.copy(file.path(extd.dir, "Ames.met"), ".")
  ## Identify a parameter of interest
  ## In this case we want to know the impact of varying the fertilizer amount
  ## and the plant population
  pp1 <- inspect_apsimx("Wheat.apsimx", src.dir = ".", 
                        node = "Manager", parm = list("SowingFertiliser", 1))
  pp1 <- paste0(pp1, ".Amount")
  
  pp2 <- inspect_apsimx("Wheat.apsimx", src.dir = tmp.dir, 
                        node = "Manager", parm = list("SowingRule1", 9))
  pp2 <- paste0(pp2, ".Population")
  
  grd <- expand.grid(parm1 = c(50, 100, 150), parm2 = c(100, 200, 300), parm3 = c(1, 2))
  names(grd) <- c("Fertiliser", "Population", "soil.profile")
  
  inspect_apsimx("Wheat.apsimx", node = "Other", parm = list(1, 1, 5, 0), print.path = TRUE)

  ### Getting soils
  sps <- get_ssurgo_soil_profile(lonlat = c(-93, 42), nsoil = 2, fix = TRUE)
  
  plot(sps[[1]], property = "water")
  plot(sps[[1]], property = "Carbon")
  
  plot(sps[[2]], property = "water")
  plot(sps[[2]], property = "Carbon")

  ## Base simulation
  sim0 <- apsimx("Wheat.apsimx", src.dir = tmp.dir)
  
  start <- Sys.time()
  sns <- sens_apsimx("Wheat.apsimx", src.dir = tmp.dir,
                     parm.paths = c(pp1, pp2, 'soil.profile'),
                     soil.profiles = sps,
                     grid = grd)
  (Sys.time() - start) 
  ## Taking 3.32 minutes (Mac 2017 - Aug 2024)
  ## Taking 3.1 minutes (Mac 2021 - Mar 2025)
  
  snsd <- sns$grid.sims
  
  ggplot(data = snsd, aes(x = Fertiliser, y = Yield, color = as.factor(soil.profile))) + 
    facet_wrap(~ Population) + 
    geom_line()

  ggplot(data = snsd, aes(x = Fertiliser, y = Wheat.AboveGround.Wt, color = as.factor(soil.profile))) + 
    facet_wrap(~ Population) + 
    geom_line()
  
  ### Compare soil profiles
  cmps <- compare_apsim_soil_profile(sps[[1]], sps[[2]])
  
  plot(cmps, plot.type = "depth", soil.var = c("DUL"))
  plot(cmps, plot.type = "depth", soil.var = c("LL15")) ## Should implement 'water'
  plot(cmps, plot.type = "depth", soil.var = "Carbon")

  ### Clean up
  file.remove("Wheat.apsimx")
  file.remove("Ames.met")
}

#### Multiple soils simulation ----
### I need to think more about the additional tests that I need to 
### to ensure that the feature of cycling through soils works
### Why did I think that there can be more than one column of soils that
### Need to be replaced? I think it is because of multiple simulations
### In one simulation run I could need to replace the soil of several 'roots'
### How do I do this?

run.multiple.soils.sens.apsimx <- FALSE

if(run.multiple.soils.sens.apsimx){
  
  ### Is AgPasture the best example of a multiple simulation file?
  extd.dir <- system.file("extdata", package = "apsimx")  
  file.copy(file.path(extd.dir, "MaizeSoybean.apsimx"), ".")
  file.copy(file.path(extd.dir, "Ames.met"), ".")
  
  inspect_apsimx("MaizeSoybean.apsimx", ".", node = "Other",
                 parm = 2)
  
  inspect_apsimx("MaizeSoybean.apsimx", ".", node = "Other",
                 parm = list(1, 2, 5, 0))
  
  check_apsimx("MaizeSoybean.apsimx", root = "SimulationSoybean")
  check_apsimx("MaizeSoybean.apsimx", root = "SimulationMaize")
  
  sim0 <- apsimx("MaizeSoybean.apsimx")

  dir()
  dim(sim0)
  
  ggplot(data = sim0[sim0$SimulationName == "SimulationMaize", ], 
         aes(x = Date, y = Maize.Total.Wt)) + 
    geom_line()
  
  ggplot(data = sim0[sim0$SimulationName == "SimulationSoybean", ], 
         aes(x = Date, y = Soybean.Total.Wt)) + 
    geom_line()
  
  ### Inspecting the soil
  inspect_apsimx("MaizeSoybean.apsimx", node = "Soil",
                 root = "SimulationSoybean",
                 soil.child = "Physical")
  
  inspect_apsimx("MaizeSoybean.apsimx", node = "Soil",
                 root = "SimulationMaize",
                 soil.child = "Physical")

  inspect_apsimx("MaizeSoybean.apsimx", node = "Soil",
                 root = "SimulationMaize") ### Nicollet
  inspect_apsimx("MaizeSoybean.apsimx", node = "Soil",
                 root = "SimulationSoybean") ### Nicollet
  
  #### Extracting alternative soils for this location?
  sps <- get_ssurgo_soil_profile(c(-93.76911, 42.02204), nsoil = 2, fix = TRUE)

  #### Which soils are these?
  sps[[1]]$metadata$SoilType
  sps[[2]]$metadata$SoilType
  
  plot(sps[[1]], property = "water")
  plot(sps[[2]], property = "water")
  
  #### Now I want to edit these soils for each simulation... Will it work?
  #### What if I ONLY change the soils? It does not work
  soil.paths <- c("SimulationSoybean.Soil", "SimulationMaize.Soil")
  grd <- expand.grid(soil1 = 1:2, soil2 = 1:2)
  names(grd) <- soil.paths
  
  start <- Sys.time()
  sns.soil0 <- sens_apsimx("MaizeSoybean.apsimx", 
                           parm.paths = soil.paths,
                           convert = rep(FALSE, 2),
                           replacement = rep(FALSE, 2),
                           grid = grd,
                           summary = "none",
                           soil.profiles = sps)
  (Sys.time() - start)
  
  head(sns.soil0$grid.sims[, c(1, 2, 3, 9)])

  snsM <- subset(sns.soil0$grid.sims, SimulationName == "SimulationMaize") 
  ggplot(data = snsM, 
         aes(x = Date, y = Maize.Total.Wt, color = as.factor(SimulationMaize.Soil))) + 
    geom_line() + 
    ylim(c(1000, 2000))
  
  snsS <- subset(sns.soil0$grid.sims, SimulationName == "SimulationSoybean") 
  ggplot(data = snsS, 
         aes(x = Date, y = Soybean.Total.Wt, color = as.factor(SimulationSoybean.Soil))) + 
    geom_line()
  
  ### Can I use more than one core?
  start <- Sys.time()
  sns.soil0.c2 <- sens_apsimx("MaizeSoybean.apsimx", 
                              parm.paths = soil.paths,
                              convert = rep(FALSE, 2),
                              replacement = rep(FALSE, 2),
                              grid = grd,
                              summary = "none",
                              soil.profiles = sps,
                              cores = 2L)
  (Sys.time() - start)
  #### There appears to be no benefit with such a small number of simulations,
  #### but it runs...
  
  
}
