## Testing teh capability of optimizing parameters
require(apsimx)
library(ggplot2)
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
  system.time(sim0 <- apsimx("Wheat-opt-ex-orig.apsimx", src.dir = extd.dir, value = "report"))

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
                             print.path = TRUE,
                             display.available = FALSE, 
                             verbose = FALSE)
  
  ## Run the model with incorrect parameters
  ## RUE = 1.2
  ## Phyllochron = 120
  system.time(sim.b4 <- apsimx("Wheat-opt-ex.apsimx", src.dir = extd.dir, value = "report"))
  
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
                       src.dir = extd.dir, 
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
  start <- Sys.time()
  wop.mcmc <- optim_apsimx("Wheat-opt-ex.apsimx", 
                           src.dir = extd.dir, 
                           parm.paths = c(pp1, pp2),
                           data = obsWheat, 
                           weights = "mean",
                           replacement = c(TRUE, TRUE),
                           initial.values = c(1.2, 120),
                           type = "mcmc",
                           settings = list(iterations = 500, nrChains = 3))
  end <- Sys.time()
  
}