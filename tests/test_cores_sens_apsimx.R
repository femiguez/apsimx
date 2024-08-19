## Script to test cores functionality

library(apsimx)
packageVersion("apsimx")
library(future)
options(apsimx.warn.versions = FALSE)

apsimx_options(warn.versions = FALSE)

run.test.two.cores.sns <- FALSE

if(run.test.two.cores.sns){

    tmp.dir <- tempdir()
    setwd(tmp.dir)
    ex.dir <- auto_detect_apsimx_examples()

    file.copy(file.path(ex.dir, "Wheat.apsimx"), ".")

    pp1 <- inspect_apsimx("Wheat.apsimx", src.dir = ".",
                           node = "Manager", parm = list("ertilise", 1))
    pp1 <- paste0(pp1, ".Amount")

    pp2 <- inspect_apsimx("Wheat.apsimx", src.dir = ".",
                           node = "Manager", parm = list("Sow using", 10))
    pp2 <- paste0(pp2, ".Population")

    ## The names in the grid should (partially) match the parameter path names
    grd <- expand.grid(Fertilise = c(50, 100, 150), Population = c(100, 200, 300))

    ## This takes 2-3 minutes
    system.time(sns0 <- sens_apsimx(file = "Wheat.apsimx",
                                    src.dir = ".",
                                    parm.paths = c(pp1, pp2),
                                    grid = grd))
    ## date        elapsed
    ## 2022-11-22      125 (2.08 minutes)
    ## 2024-05-05      121 (2.0 minutes) - Mac 2017
    ## 2024-08-05      110 (1.9 minutes) - Mac 2017
    ## 2024-08-19      63.68 (Dell Precision 5680)
    ## 2024-05-05      95.8 (Mac Pro 2021)
    ## 2024-08-12      61.92 (Dell Precision 7865)
    
    ## The two core simulation seems to work when number of simulations are even for n - 1
    system.time(sns1 <- sens_apsimx(file = "Wheat.apsimx",
                                    src.dir = ".",
                                    parm.paths = c(pp1, pp2),
                                    grid = grd,
                                    cores = 2))

    ## Make sure that data.frames are the same

    ## date        user system elapsed
    ## 2022-11-22   292     11    100 (1.67 minutes. It takes 84% of the time compared to 1 core)
    ## 2024-03-06   64      3.3    37.259 (0.52 minutes 57% of the time)
    ## 2024-08-12                  34.87 (Dell Precision 7865)
    ## 2024-08-19                  40.47 (Dell Precision 5680)
    ## 2024-08-19                  18.16 (Dell Precision 5680 - 8 cores)
    ## 2024-05-05                  55.77 (Mac Pro 2021)
    ## 2024-05-05                  27 (Mac Pro 2021 - 8 cores)

    ## Are the results identical?
    is.it.zero <- sum(colSums(sns0$grid.sims - sns1$grid.sims))
    if(is.it.zero > 0.5)
      stop("Results are not identical when cores = 1 and cores = 2")

    ## Testing having uneven number of simulations to parallelize
    grd2 <- grd[1:8, ]

    system.time(sns2 <- sens_apsimx(file = "Wheat.apsimx",
                                    src.dir = ".",
                                    parm.paths = c(pp1, pp2),
                                    grid = grd2,
                                    cores = 2))

    nrow.sns2 <- nrow(sns2$grid.sims)
    ## Compare results
    is.it.zero <- sum(colSums(sns1$grid.sims[1:8,] - sns2$grid.sims))

    if(is.it.zero > 0.5)
      stop("Results are not identical when cores = 1 and cores = 2")

}

run.test.more.cores.sns <- FALSE

if(run.test.more.cores.sns){

  tmp.dir <- tempdir()
  setwd(tmp.dir)
  ex.dir <- auto_detect_apsimx_examples()

  file.copy(file.path(ex.dir, "Wheat.apsimx"), ".")

  pp1 <- inspect_apsimx("Wheat.apsimx", src.dir = ".",
                        node = "Manager", parm = list("Fertilise", 1))
  pp1 <- paste0(pp1, ".Amount")

  pp2 <- inspect_apsimx("Wheat.apsimx", src.dir = ".",
                        node = "Manager", parm = list("Sow using", 10))
  pp2 <- paste0(pp2, ".Population")

  ## The names in the grid should (partially) match the parameter path names
  grd <- expand.grid(Fertilise = c(50, 100, 150), Population = c(100, 200, 300))

  system.time(sns3 <- sens_apsimx(file = "Wheat.apsimx",
                                 src.dir = ".",
                                 parm.paths = c(pp1, pp2),
                                 grid = grd,
                                 cores = 3))

  ## Compare results
  is.it.zero <- sum(colSums(sns1$grid.sims - sns3$grid.sims))

  if(is.it.zero > 0.5)
    stop("Results are not identical when cores = 2 and cores = 3")

  grd2 <- grd[1:8, ]

  system.time(sns4 <- sens_apsimx(file = "Wheat.apsimx",
                                  src.dir = ".",
                                  parm.paths = c(pp1, pp2),
                                  grid = grd2,
                                  cores = 4))

  ## Compare results
  is.it.zero <- sum(colSums(sns2$grid.sims - sns4$grid.sims))

  if(is.it.zero > 0.5)
    stop("Results are not identical when cores = 2 and cores = 3 (smaller grid)")

  ## Trying more cores
  system.time(sns5 <- sens_apsimx(file = "Wheat.apsimx",
                                  src.dir = ".",
                                  parm.paths = c(pp1, pp2),
                                  grid = grd,
                                  cores = 4))

  is.it.zero <- sum(colSums(sns1$grid.sims - sns5$grid.sims))

  if(is.it.zero > 0.5)
    stop("Results are not identical when cores = 3 and cores = 4")


}


run.test.even.more.cores.sns <- FALSE

if(run.test.even.more.cores.sns){

  tmp.dir <- tempdir()
  setwd(tmp.dir)
  ex.dir <- auto_detect_apsimx_examples()

  file.copy(file.path(ex.dir, "Wheat.apsimx"), ".")

  pp1 <- inspect_apsimx("Wheat.apsimx", src.dir = ".",
                        node = "Manager", parm = list("Fertilise", 1))
  pp1 <- paste0(pp1, ".Amount")

  pp2 <- inspect_apsimx("Wheat.apsimx", src.dir = ".",
                        node = "Manager", parm = list("Sow using", 10))
  pp2 <- paste0(pp2, ".Population")

  ## The names in the grid should (partially) match the parameter path names
  grd <- expand.grid(Fertilise = c(50, 100, 150), Population = c(100, 200, 300))

  system.time(sns3 <- sens_apsimx(file = "Wheat.apsimx",
                                 src.dir = ".",
                                 parm.paths = c(pp1, pp2),
                                 grid = grd,
                                 cores = 4))
  ## 2024-08-12 Elapsed time: 21.95 (Dell Precision 7865)

  system.time(sns8 <- sens_apsimx(file = "Wheat.apsimx",
                                  src.dir = ".",
                                  parm.paths = c(pp1, pp2),
                                  grid = grd,
                                  cores = 8))
  ## 2024-08-12 Elapsed time: 15.91 (Dell Precision 7865)

  ## Compare results
  is.it.zero <- sum(colSums(sns3$grid.sims - sns8$grid.sims))

  if(is.it.zero > 0.5)
    stop("Results are not identical when cores = 2 and cores = 3 (smaller grid)")

  grd2 <- expand.grid(Fertilise = c(50, 100, 150, 200, 250), Population = c(100, 200, 300, 400, 500))

  ## Trying more cores
  system.time(sns10 <- sens_apsimx(file = "Wheat.apsimx",
                                   src.dir = ".",
                                   parm.paths = c(pp1, pp2),
                                   grid = grd2,
                                   cores = 10))

}

#### Testing the 'save' feature ----

run.test.save.cores.sns <- FALSE

if(run.test.save.cores.sns){

  tmp.dir <- tempdir()
  setwd(tmp.dir)
  ex.dir <- auto_detect_apsimx_examples()

  file.copy(file.path(ex.dir, "Wheat.apsimx"), ".")

  pp1 <- inspect_apsimx("Wheat.apsimx", src.dir = ".",
                        node = "Manager", parm = list("Fertilise", 1))
  pp1 <- paste0(pp1, ".Amount")

  pp2 <- inspect_apsimx("Wheat.apsimx", src.dir = ".",
                        node = "Manager", parm = list("Sow using", 10))
  pp2 <- paste0(pp2, ".Population")

  ## The names in the grid should (partially) match the parameter path names
  grd <- expand.grid(Fertilise = c(50, 100, 150), Population = c(100, 200, 300))

  ## This takes 1 minute
  system.time(sns01 <- sens_apsimx(file = "Wheat.apsimx",
                                  src.dir = ".",
                                  parm.paths = c(pp1, pp2),
                                  grid = grd,
                                  save = TRUE))

  ## This takes 1 minute
  system.time(sns02 <- sens_apsimx(file = "Wheat.apsimx",
                                  src.dir = ".",
                                  parm.paths = c(pp1, pp2),
                                  grid = grd,
                                  save = "wheat_int_results.csv"))

}
