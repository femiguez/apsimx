## Testing the capability of optimizing parameters
require(apsimx)
require(ggplot2)
apsimx_options(warn.versions = FALSE)

## 1. Simulate data from a model with known parameters
## 2. Change the parameter values and some some random "noise"
## 3. Run the optimization algorithm
## 4. Compare optimized parameter values with original ones

extd.dir <- system.file("extdata", package = "apsimx")

## extd.dir <- "~/Dropbox/apsimx/inst/extdata"

## I won't routinely run this
if(FALSE){
  
  ## One model run takes ~4 seconds
  ## The original values for these were 1.5 for RUE and 90 for BasePhyllochron
  system.time(sim0 <- apsimx("Wheat-opt-ex.apsimx", src.dir = extd.dir))

  pp1 <- inspect_apsimx_replacement("Wheat-opt-ex.apsimx", 
                                    src.dir = extd.dir, 
                                    node = "Wheat", 
                                    node.child = "Leaf",
                                    node.subchild = "Photosynthesis",
                                    node.subsubchild = "RUE",
                                    parm = "FixedValue",
                                    print.path = TRUE,
                                    display.available = FALSE, 
                                    verbose = FALSE)
  
  pp2 <- inspect_apsimx_replacement("Wheat-opt-ex.apsimx", 
                                    src.dir = extd.dir, 
                                    node = "Wheat", 
                                    node.child = "Cultivars",
                                    node.subchild = "USA",
                                    node.subsubchild = "Yecora",
                                    parm = "BasePhyllochron",
                                    print.path = TRUE,
                                    display.available = FALSE, 
                                    verbose = FALSE)
  
  edit_apsimx_replacement("Wheat-opt-ex.apsimx")
  
  ## Visualize the data
  ggplot(sim0, aes(Date, Wheat.AboveGround.Wt)) + 
    geom_line()
  ggplot(sim0, aes(Date, Wheat.Leaf.LAI)) + 
    geom_line()
  ggplot(sim0, aes(Date, Wheat.Phenology.Stage)) + 
    geom_line()

  ## Create 'observed' data
  sample.dates <- seq(from = as.Date("2016-10-01"), to = as.Date("2017-06-01"), length.out = 10)

  set.seed(12345)
  obs0 <- sim0[sim0$Date %in% sample.dates, c("Date","Wheat.Phenology.Stage","Wheat.Leaf.LAI","Wheat.AboveGround.Wt")]
  obs0$Wheat.Phenology.Stage <- obs0$Wheat.Phenology.Stage + rnorm(10, 0, sd = 0.1)
  obs0$Wheat.Leaf.LAI[5:10] <- obs0$Wheat.Leaf.LAI[5:10] + rnorm(6, 0, sd = 0.5)
  obs0$Wheat.AboveGround.Wt[3:10] <- obs0$Wheat.AboveGround.Wt[3:10] + rnorm(8, 0, sd = 3)

  obsWheat <- obs0
  save(obsWheat, file = "obsWheat.rda")

  data(obsWheat)
  ## We will try to optimize only 2 parameters
  ## RUE: default 1.5
  ## Phyllochron for cultivar Yecora Fixed Value 90
  inspect_apsimx_replacement("Wheat-opt-ex.apsimx", 
                             src.dir = extd.dir, 
                             node = "Wheat", 
                             node.child = "Leaf",
                             node.subchild = "Photosynthesis",
                             node.subsubchild = "RUE",
                             parm = "FixedValue",
                             print.path = TRUE,
                             display.available = FALSE, 
                             verbose = FALSE)

  inspect_apsimx_replacement("Wheat-opt-ex.apsimx", 
                             src.dir = extd.dir, 
                             node = "Wheat", 
                             node.child = "Cultivars",
                             node.subchild = "USA",
                             node.subsubchild = "Yecora",
                             parm = "BasePhyllochron",
                             print.path = TRUE,
                             display.available = FALSE, 
                             verbose = FALSE)
  
  ## Run the model with incorrect parameters
  ## RUE = 1.2
  ## Phyllochron = 120
  tmp <- tempdir()
  setwd(tmp)
  file.copy(file.path(extd.dir, "Ames.met"), ".")
  file.copy(file.path(extd.dir, "Wheat-opt-ex.apsimx"), ".")
  
  system.time(sim.b4 <- apsimx("Wheat-opt-ex.apsimx", src.dir = tmp)) ## This takes 4-5 seconds
  
  ## write.csv(sim.b4, "wheat-sim-b4-opt.csv", row.names = FALSE)

  sim.b4.s <- subset(sim.b4, Date > as.Date("2016-09-30") & Date < as.Date("2017-07-01"))
  ## phenology
  ggplot() + 
    geom_point(data = obsWheat, aes(x = Date, y = Wheat.Phenology.Stage)) +
    geom_line(data = sim.b4.s, aes(x = Date, y = Wheat.Phenology.Stage)) + 
    ggtitle("Phenology")
  ## LAI
  ggplot() + 
    geom_point(data = obsWheat, aes(x = Date, y = Wheat.Leaf.LAI)) +
    geom_line(data = sim.b4.s, aes(x = Date, y = Wheat.Leaf.LAI)) + 
    ggtitle("LAI")
  ## Biomass
  ggplot() + 
    geom_point(data = obsWheat, aes(x = Date, y = Wheat.AboveGround.Wt)) +
    geom_line(data = sim.b4.s, aes(x = Date, y = Wheat.AboveGround.Wt)) + 
    ggtitle("Biomass (g/m2)")
  
  ## Optimization
  pp1 <- "Wheat.Leaf.Photosynthesis.RUE.FixedValue"
  pp2 <- "Wheat.Cultivars.USA.Yecora.BasePhyllochron"
  
  # edit_apsimx_replacement("Wheat-b4-opt.apsimx",
  #                         src.dir = ".",
  #                         overwrite = TRUE,
  #                         node = "Wheat", node.child = "Cultivars",
  #                         node.subchild = "USA", node.subsubchild = "Yecora",
  #                         parm = "BasePhyllochron", value = 110)
  
  ## This takes about ~7.5 minutes
  start <- Sys.time()
  wop <- optim_apsimx("Wheat-opt-ex.apsimx", 
                       parm.paths = c(pp1, pp2),
                       data = obsWheat, 
                       weights = "mean",
                       replacement = c(TRUE, TRUE),
                       initial.values = c(1.2, 120))
  end <- Sys.time()
  
  ## This took 8.8 minutes
  start <- Sys.time()
  wop.h <- optim_apsimx("Wheat-opt-ex.apsimx", 
                          src.dir = extd.dir, 
                          parm.paths = c(pp1, pp2),
                          data = obsWheat, 
                          weights = "mean",
                          replacement = c(TRUE, TRUE),
                          initial.values = c(1.2, 120),
                          hessian = TRUE)
  end <- Sys.time()
  
  sim.opt <- apsimx("Wheat-opt-ex.apsimx", src.dir = extd.dir, value = "report")  

  sim.opt.s <- subset(sim.opt, Date > as.Date("2016-09-30") & Date < as.Date("2017-07-01"))
  
  ## For vignette
  ## write.csv(sim.opt.s, file = "wheat-sim-opt.csv", row.names = FALSE)
  
  ## phenology
  ggplot() + 
    geom_point(data = obsWheat, aes(x = Date, y = Wheat.Phenology.Stage)) +
    geom_line(data = sim.opt.s, aes(x = Date, y = Wheat.Phenology.Stage)) + 
    ggtitle("Phenology")
  ## LAI
  ggplot() + 
    geom_point(data = obsWheat, aes(x = Date, y = Wheat.Leaf.LAI)) +
    geom_line(data = sim.opt.s, aes(x = Date, y = Wheat.Leaf.LAI)) + 
    ggtitle("LAI")
  ## Biomass
  ggplot() + 
    geom_point(data = obsWheat, aes(x = Date, y = Wheat.AboveGround.Wt)) +
    geom_line(data = sim.opt.s, aes(x = Date, y = Wheat.AboveGround.Wt)) + 
    ggtitle("Biomass (g/m2)")

  ## What about nloptr?
  start <- Sys.time()
  wop.n <- optim_apsimx("Wheat-opt-ex.apsimx", 
                        src.dir = extd.dir, 
                        parm.paths = c(pp1, pp2),
                        data = obsWheat, 
                        weights = "mean",
                        replacement = c(TRUE, TRUE),
                        initial.values = c(1.2, 120),
                        type = "nloptr",
                        opts = list("algorithm" = "NLOPT_LN_NELDERMEAD"))
  end <- Sys.time()
  
  ## Test BayesianTools??? Would need to embbed it inside optim
  ## How long does this take in Windows laptop? 500 iter, 9 chains: 3464 seconds ~ 1 hour
  ## 500 iter, 3 chains: 18 minutes
  ## 5000 iter, 3 chains: 4 hours, but still not good enough
  start <- Sys.time()
  wop.mcmc <- optim_apsimx("Wheat-opt-ex.apsimx", 
                           src.dir = extd.dir, 
                           parm.paths = c(pp1, pp2),
                           data = obsWheat, 
                           weights = "mean",
                           replacement = c(TRUE, TRUE),
                           initial.values = c(1.2, 120),
                           type = "mcmc",
                           parallel = FALSE,
                           settings = list(iterations = 5000))
  end <- Sys.time()
  
}

#### Classic ####
if(FALSE){

  tmp <- tempdir()
  setwd(tmp)
  
  apsim_options(warn.versions = FALSE)
  
  ## Testing optimization for an APSIM Classic example
  ## Maize
  if(file.exists("Maize.apsim")) file.remove("Maize.apsim")
  if(file.exists("Maize.xml")) file.remove("Maize.xml")
  if(file.exists("Maize.out")) file.remove("Maize.out")
  if(file.exists("Maize.sum")) file.remove("Maize.sum")
  
  extd.dir <- system.file("extdata", package = "apsimx")
  file.copy(file.path(extd.dir, "Maize-op.apsim"), "./Maize.apsim")
  file.copy("C:/Program Files (x86)/APSIM710-r4207/Model/Maize.xml", ".")

  inspect_apsim("Maize.apsim")
  
  inspect_apsim("Maize.apsim", node = "Crop",
                parm = list("Sow", 3))
  
  edit_apsim("Maize.apsim", node = "Clock",
             parm = "start_date", value = "01/01/1987",
             overwrite = TRUE)
  
  sim0 <- apsim("Maize.apsim")
  
  obsMaize <- data.frame(Date = as.Date(c("1988-02-23", "1989-02-25")),
                         biomass = c(4600, 5000),
                         yield = c(350, 570))
  
  pp1 <- inspect_apsim_xml("Maize.xml", parm = "sc401/rue")
  pp2 <- inspect_apsim_xml("Maize.xml", parm = "sc401/GNmaxCoef")
  
  ## Optimize a single parameter
  op01 <- optim_apsim("Maize.apsim", 
                     crop.file = "Maize.xml",
                     parm.paths = pp2,
                     data = obsMaize,
                     hessian = TRUE,
                     weights = "mean")
  
  
  op1 <- optim_apsim("Maize.apsim", 
                     crop.file = "Maize.xml",
                     parm.paths = c(pp1, pp2),
                     data = obsMaize,
                     weights = "mean")

  file.remove("Maize.xml")
  file.copy("C:/Program Files (x86)/APSIM710-r4207/Model/Maize.xml", ".")
  
  op2 <- optim_apsim("Maize.apsim", 
                     crop.file = "Maize.xml",
                     parm.paths = c(pp1, pp2),
                     data = obsMaize,
                     hessian = TRUE,
                     weights = "mean")
  
  # op2.u <- optim_apsim("Maize.apsim", 
  #                      crop.file = "Maize.xml",
  #                      parm.paths = c(pp1, pp2),
  #                      type = "ucminf",
  #                      data = obsMaize,
  #                      hessian = TRUE,
  #                      weights = "mean")
  
  op2nl <- optim_apsim("Maize.apsim", 
                     crop.file = "Maize.xml",
                     parm.paths = c(pp1, pp2),
                     data = obsMaize,
                     type = "nloptr",
                     weights = "mean",
                     opts = list("algorithm" = "NLOPT_LN_NELDERMEAD"))
  
  sim2 <- apsim("Maize.apsim")
  
  ## Can I also optimize management?
  pp3 <- inspect_apsim("Maize.apsim", node = "Crop",
                       parm = list("Sow", 17),
                       print.path = TRUE)

  pp4 <- inspect_apsim("Maize.apsim", node = "Crop",
                       parm = list("Sow", 28),
                       print.path = TRUE)

  ## The goal of this is to show that you 
  ## can try to optimize parameters which are in the
  ## .apsim and .xml file
  op3 <- optim_apsim("Maize.apsim", 
                     crop.file = "Maize.xml",
                     parm.paths = c(pp2, pp4),
                     xml.parm = c(TRUE, FALSE),
                     data = obsMaize,
                     weights = "mean",
                     hessian = TRUE)

  inspect_apsim("Maize.apsim", node = "Crop",
                parm = list("Sow", 17),
                print.path = TRUE)
  
  inspect_apsim("Maize.apsim", node = "Crop",
                 parm = list("Sow", 28),
                 print.path = TRUE)

  
  if(file.exists("Maize.apsim")) file.remove("Maize.apsim")
  if(file.exists("Maize.xml")) file.remove("Maize.xml")
  if(file.exists("Maize.out")) file.remove("Maize.out")
  if(file.exists("Maize.sum")) file.remove("Maize.sum")
}

## Testing the optimization of a single parameter 
## comparing methods
if(FALSE){
  
  tmp <- tempdir()
  setwd(tmp)
  
  extd.dir <- system.file("extdata", package = "apsimx")
  
  file.copy(file.path(extd.dir, "Ames.met"), ".")
  file.copy(file.path(extd.dir, "Wheat-opt-ex.apsimx"), ".")
  
  sim0 <- apsimx("Wheat-opt-ex.apsimx")

  pp1 <- "Wheat.Leaf.Photosynthesis.RUE.FixedValue"
  pp2 <- "Wheat.Cultivars.USA.Yecora.BasePhyllochron"
  
  ## Testing the "unreliable" method
  start <- Sys.time()
  wop.1 <- optim_apsimx("Wheat-opt-ex.apsimx", 
                        parm.paths = pp1,
                        data = obsWheat, 
                        replacement = TRUE,
                        initial.values = 1.2)
  end <- Sys.time() ## It took 3.25 minutes
  
  ## Plus hessian
  file.remove("Wheat-opt-ex.apsimx")
  file.copy(file.path(extd.dir, "Wheat-opt-ex.apsimx"), ".")
  
  start <- Sys.time()
  wop.h.1 <- optim_apsimx("Wheat-opt-ex.apsimx", 
                          parm.paths = pp1,
                          data = obsWheat, 
                          replacement = TRUE,
                          hessian = TRUE,
                          initial.values = 1.2)
  end <- Sys.time() ## It took 3.4 minutes
 
  ## Erase and try Brent 
  file.remove("Wheat-opt-ex.apsimx")
  file.copy(file.path(extd.dir, "Wheat-opt-ex.apsimx"), ".")
 
  start <- Sys.time()
  wop.b.1 <- optim_apsimx("Wheat-opt-ex.apsimx", 
                          parm.paths = pp1,
                          data = obsWheat, 
                          method = "Brent",
                          lower = 0.5, upper = 2,
                          replacement = TRUE,
                          initial.values = 1.2)
  end <- Sys.time() ## It took 2.18 minutes 
  ## Brent, naturally, will not provide the best solution if
  ## the solution is outside the interval (lower, upper)
  
  ## Erase and try L-BFGS-B
  file.remove("Wheat-opt-ex.apsimx")
  file.copy(file.path(extd.dir, "Wheat-opt-ex.apsimx"), ".")
  
  start <- Sys.time()
  wop.bfgs.1 <- optim_apsimx("Wheat-opt-ex.apsimx", 
                          parm.paths = pp1,
                          data = obsWheat, 
                          method = "L-BFGS-B",
                          lower = 0.5,
                          hessian = TRUE,
                          replacement = TRUE,
                          initial.values = 1.2,
                          control = list(trace = 3))
  end <- Sys.time() ## It took 7.02 minutes 
  
  ## Next step is to test whether weighting affects the estimate of the vcov
  ## and how different it is to the Bayesian analysis
  
  ## Erase and try L-BFGS-B
  file.remove("Wheat-opt-ex.apsimx")
  file.copy(file.path(extd.dir, "Wheat-opt-ex.apsimx"), ".")
  
  start <- Sys.time()
  wop.bfgs.2 <- optim_apsimx("Wheat-opt-ex.apsimx", 
                             parm.paths = pp1,
                             data = obsWheat,
                             weights = "mean",
                             method = "L-BFGS-B",
                             lower = 0.5,
                             hessian = TRUE,
                             replacement = TRUE,
                             initial.values = 1.2,
                             control = list(trace = 3))
  end <- Sys.time() ## It took 4.16 minutes 
  
  ## Testing with two parameters
  file.remove("Wheat-opt-ex.apsimx")
  file.copy(file.path(extd.dir, "Wheat-opt-ex.apsimx"), ".")
  ## Testing with two parameters
  start <- Sys.time()
  wop.2 <- optim_apsimx("Wheat-opt-ex.apsimx", 
                        parm.paths = c(pp1, pp2),
                        data = obsWheat, 
                        replacement = c(TRUE, TRUE),
                        hessian = TRUE,
                        initial.values = c(1.2, 120))
  end <- Sys.time() ## It took 15.7 minutes
  
  ## Testing with two parameters, without the hessian
  file.remove("Wheat-opt-ex.apsimx")
  file.copy(file.path(extd.dir, "Wheat-opt-ex.apsimx"), ".")
  ## Testing with two parameters
  start <- Sys.time()
  wop.nh.2 <- optim_apsimx("Wheat-opt-ex.apsimx", 
                        parm.paths = c(pp1, pp2),
                        data = obsWheat, 
                        replacement = c(TRUE, TRUE),
                        initial.values = c(1.2, 120))
  end <- Sys.time() ## It took 13.97 minutes
  
  file.remove("Wheat-opt-ex.apsimx")
  file.copy(file.path(extd.dir, "Wheat-opt-ex.apsimx"), ".")
  ## Testing with two parameters
  start <- Sys.time()
  wop.3 <- optim_apsimx("Wheat-opt-ex.apsimx", 
                        parm.paths = c(pp1, pp2),
                        data = obsWheat, 
                        type = "ucminf",
                        weights = "mean",
                        replacement = c(TRUE, TRUE),
                        hessian = TRUE,
                        initial.values = c(1.2, 120))
  end <- Sys.time() ## It took 28.97 minutes and it did not find the right answer
}