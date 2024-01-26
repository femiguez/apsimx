## Testing the compare_apsim function
require(apsimx)
require(ggplot2)
apsimx_options(warn.versions = FALSE)

run.test.compare.apsimx <- get(".run.local.tests", envir = apsimx.options)

extd.dir <- system.file("extdata", package = "apsimx")

if(run.test.compare.apsimx){
  
  data(obsWheat)
  sim.opt <- read.csv(file.path(extd.dir, "wheat-sim-opt.csv"))
  sim.opt$Date <- as.Date(sim.opt$Date)

  ## Testing compare_apsim
  cap <- compare_apsim(obsWheat, sim.opt, labels = c("obs", "sim"))
  
  plot(cap)
  plot(cap, plot.type = "diff")
  plot(cap, plot.type = "ts")
  
  plot(cap, variable = "AboveGround")
  plot(cap, variable = "AboveGround", plot.type = "diff")
  plot(cap, variable = "AboveGround", plot.type = "ts")
  
  ## Adding the capability when the length of the index is equal to 2
  obsPheno <- read.csv("~/Dropbox/apsimx-other/pheno_optim/obsPheno.csv")
  simPheno <- read.csv("~/Dropbox/apsimx-other/pheno_optim/simPheno.csv")
  
  obsPheno$Date <- as.Date(obsPheno$Date)
  simPheno$Date <- as.Date(simPheno$Date)
  
  cmp <- compare_apsim(obsPheno, simPheno, index = c("report", "Date"), labels = c("Obs", "Sim"))
  
  plot(cmp)
  plot(cmp, by = "report")
  plot(cmp, by = "report", se = FALSE)
  plot(cmp, by = "report", se = FALSE, facet = TRUE)
  plot(cmp, by = "report", plot.type = "ts", facet = TRUE, se = FALSE)
  
  plot(cmp, by = "report", plot.type = "ts", facet = TRUE, se = FALSE) + 
    ggplot2::theme(legend.position = "top")
  
}

if(FALSE){
  
  ### Testing inspired by a more complicated dataset?
  sim.wheat <- read_apsim_all(src.dir = "~/Dropbox/apsimx-other/KeLiu/mcmc_test/mcmc_test")
  sim.wheat$outfile <- as.factor(sim.wheat$outfile)
  ## obs.wheat <- as.data.frame(readxl::read_excel("~/Dropbox/apsimx-other/KeLiu/mcmc_test/mcmc_test/Obs.xlsx"))
  obs.wheat$outfile <- as.factor(obs.wheat$outfile)
  obs.wheat$Date <- as.Date(obs.wheat$Date)

  cmp1 <- compare_apsim(obs.wheat, sim.wheat, 
                        labels = c("obs", "sim"))
  
  cmp1 <- compare_apsim(obs.wheat, sim.wheat, index = c("outfile", "Date"), 
                        labels = c("obs", "sim"))
  
  plot(cmp1)
  plot(cmp1, by = "outfile")
  plot(cmp1, by = "outfile", se = FALSE)
  plot(cmp1, by = "outfile", plot.type = "ts", facet = TRUE, se = FALSE)
  
}