## Testing the compare_apsim function
require(apsimx)
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
  
  ## cmp0 <- compare_apsim(obsPheno, simPheno)
  cmp <- compare_apsim(obsPheno, simPheno, index = c("report", "Date"), labels = c("Obs", "Sim"))
  
  plot(cmp)
  plot(cmp, by = "report")
  plot(cmp, by = "report", se = FALSE)
  plot(cmp, by = "report", se = FALSE, facet = TRUE)
  plot(cmp, by = "report", plot.type = "ts", facet = TRUE, se = FALSE)
  
}