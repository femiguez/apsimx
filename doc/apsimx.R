## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(apsimx)
library(ggplot2)

## ----apsimx-options, echo = FALSE, eval = FALSE-------------------------------
#  apsimx_options(warn.versions = FALSE)
#  ava <- apsim_version(verbose = FALSE)
#  aiu <- apsim_version(which = "inuse")
#  ##if(is.na(ava[2,2])) stop("Need APSIM-X to create this vignette")

## ----create-temp-dir, echo = TRUE, eval = TRUE--------------------------------
## Will only write file to a temporary directory for these Examples
tmp.dir <- tempdir()

## ----apsimx-example, eval = FALSE---------------------------------------------
#  maize <- apsimx_example("Maize")

## ----read-in-maize-example, eval = TRUE, echo = FALSE-------------------------
extd.dir <- system.file("extdata", package = "apsimx")
## maize <- read_apsimx("Maize.db", src.dir = extd.dir)
maize <- read.csv(paste0(extd.dir, "/Maize.csv"))
maize$Date <- as.Date(maize$Date)

## ----summary-ggplot-maize-----------------------------------------------------
summary(maize)
## Simple data plotting
ggplot(data = maize , aes(x = Date, y = Maize.AboveGround.Wt)) + 
  geom_point()

## ----inspect-apsimx-----------------------------------------------------------
extd.dir <- system.file("extdata", package = "apsimx")
inspect_apsimx("Maize.apsimx", src.dir = extd.dir, node = "Weather")

## ----inspect-maize-oc---------------------------------------------------------
inspect_apsimx("Maize.apsimx", src.dir = extd.dir, 
               node = "Soil", soil.child = "Organic")

## ----edit-maize-oc------------------------------------------------------------
ocs <- c(1.5, 1.4, 1.3, 1.2, 1.1, 1.0, 0.9)
edit_apsimx("Maize.apsimx", 
            src.dir = extd.dir,
            wrt.dir = ".",
            node = "Soil",
            soil.child = "Organic", 
            parm = "Carbon", value = ocs)

## ----inspect-maize-oc-check---------------------------------------------------
inspect_apsimx("Maize-edited.apsimx", src.dir = ".", 
               node = "Soil", soil.child = "Organic")

## ----apsimx-wheat-no-run, eval = FALSE----------------------------------------
#  sim <- apsimx("Wheat.apsimx", src.dir = extd.dir, value = "report")

## ----apsimx-wheat-no-run-two, eval = FALSE------------------------------------
#  ex.dir <- auto_detect_apsimx_examples()
#  ## Copy 'Wheat' file to a temporary directory
#  ## (or change as needed)
#  tmp.dir2 <- tempdir()
#  file.copy(paste0(ex.dir, "/", "Wheat.apsimx"), tmp.dir2)
#  sim <- apsimx("Wheat.apsimx", src.dir = tmp.dir2, value = "report")

## ----apsimx-wheat-read, echo = FALSE------------------------------------------
## sim <- read_apsimx("Wheat.db", src.dir = extd.dir)
sim <- read.csv(paste0(extd.dir, "/Wheat.csv"))
sim$Date <- as.Date(sim$Date)

## ----apsimx-wheat-summary-----------------------------------------------------
## Calcualte summary statistics on all variables
summary(sim)
## Plot data
ggplot(data = sim, aes(x = Date, y = Yield)) + geom_point()
## Inspect the Wheat .apsimx file
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Crop")
## This only displays the available 'Manager' components
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Manager")
## Looking more in-depth into 'SowingRule1'
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, 
               node = "Manager", parm = list("SowingRule1", NA))

## ----apsimx-wheat-cultivar-only-----------------------------------------------
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, 
               node = "Manager", parm = list("SowingRule1", 6))
## We can print and store the path to this parameter
pp <- inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, 
               node = "Manager", parm = list("SowingRule1", 6),
               print.path = TRUE)

## ----Millet-------------------------------------------------------------------
inspect_apsim("Millet.apsim", src.dir = extd.dir, node  = "Manager")

## ----Millet-sow---------------------------------------------------------------
inspect_apsim("Millet.apsim", src.dir = extd.dir, node  = "Manager",
              parm = list("Sow on a fixed date", NA))

## ----Millet-sow-plt-dens------------------------------------------------------
inspect_apsim("Millet.apsim", src.dir = extd.dir, node  = "Manager",
              parm = list("Sow on a fixed date",5), print.path = TRUE)
## Or store it in an object for later editing
pp <- inspect_apsim("Millet.apsim", src.dir = extd.dir, node = "Manager",
              parm = list("Sow on a fixed date", 5), print.path = TRUE)

## ----Millet-edit--------------------------------------------------------------
edit_apsim("Millet.apsim", src.dir = extd.dir, wrt.dir = tmp.dir,
           node = "Other", parm.path = pp, value = 8, 
           edit.tag = "-pp")

## ----Millet-edit-inspect------------------------------------------------------
inspect_apsim("Millet-pp.apsim", src.dir = tmp.dir,
              node = "Manager",
              parm = list("Sow on a fixed date", NA))

## ----removing-Millet-pp, echo = FALSE, eval = TRUE----------------------------
## Apparently this is not needed
file.remove(paste0(tmp.dir, "/Millet-pp.apsim"))

## ----inspect-replacement-node-------------------------------------------------
inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                           node = "Maize", display.available = TRUE)

## ----inspect-replacement-node-child-------------------------------------------
inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                           node = "Maize", node.child = "Phenology",
                           display.available = TRUE)

## ----inspect-replacement-node-subchild----------------------------------------
inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                           node = "Maize", node.child = "Phenology",
                           node.subchild = "ThermalTime",
                           display.available = TRUE)

## ----inspect-replacement-node-subchild-subsubchild----------------------------
inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                           node = "Maize", node.child = "Phenology",
                           node.subchild = "ThermalTime", 
                           node.subsubchild = "BaseThermalTime",
                           display.available = TRUE) 

## ----inspect-replacement-node-subchild-subsubchild-temp-response--------------
inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                           node = "Maize", node.child = "Phenology",
                           node.subchild = "ThermalTime", 
                           node.subsubchild = "BaseThermalTime",
                           node.sub3child = "Response") 

## ----inspect-replacement-node-subchild-subsubchild-temp-response-parm---------
inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                           node = "Maize", node.child = "Phenology",
                           node.subchild = "ThermalTime", 
                           node.subsubchild = "BaseThermalTime",
                           node.sub3child = "Response",
                           parm = "Y") 

## ----inspect-replacement-soybean-cultivar-node--------------------------------
inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                           node = "Soybean", display.available = TRUE) 

## ----inspect-replacement-soybean-cultivar-node-child--------------------------
inspect_apsimx_replacement("MaizeSoybean.apsimx", 
                           src.dir = extd.dir,
                           node = "Soybean", 
                           node.child = "Cultivars",
                           node.subchild = "USA",
                           node.subsubchild = "PioneerP22T61_MG22",
                           display.available = FALSE) 

## ----edit-replacement---------------------------------------------------------
edit_apsimx_replacement("MaizeSoybean.apsimx", 
                        src.dir = extd.dir, wrt.dir = tmp.dir,
                        node = "Maize", node.child = "Phenology",
                        node.subchild = "ThermalTime", 
                        node.subsubchild = "BaseThermalTime",
                        node.sub3child = "Response",
                        parm = "Y", value = c(0, 12, 20, 28, 0))

## ----inspect-edit-replacement-------------------------------------------------
inspect_apsimx_replacement("MaizeSoybean-edited.apsimx", 
                           src.dir = tmp.dir,
                           node = "Maize", 
                           node.child = "Phenology",
                           node.subchild = "ThermalTime", 
                           node.subsubchild = "BaseThermalTime",
                           node.sub3child = "Response",
                           parm = "Y") 

## ----inspect-factorial-0, eval = FALSE----------------------------------------
#  ## There are multiple 'Experiments' so we need to pick one
#  inspect_apsimx_replacement("Factorial", src.dir = extd.dir,
#                             root = list("Experiment", NA))
#  ##These positions matched  Experiment   1 2 3 4 5
#  ##Error in inspect_apsimx_replacement("Factorial", src.dir = ex.dir, root = ##list("Experiment", : Multiple root nodes found. Please provide a position

## ----inspect-factorial--------------------------------------------------------
## There are multiple 'Experiments' so we need to pick one
inspect_apsimx_replacement("Factorial", src.dir = extd.dir,
                           root = list("Experiment", 1))
## We need to provide a node
inspect_apsimx_replacement("Factorial", src.dir = extd.dir,
                           root = list("Experiment", 1), 
                           node = "Base", display.available = TRUE)
## We need to provide a node child
inspect_apsimx_replacement("Factorial", src.dir = extd.dir,
                           root = list("Experiment", 1), 
                           node = "Base", node.child = "Clock",
                           display.available = TRUE)

## ----inspect-apsimx-factorial-1, eval = FALSE---------------------------------
#  inspect_apsimx("Factorial.apsimx", src.dir = extd.dir)
#  ## Simulation structure:
#  ## list Name: Simulations
#  # list length: 8
#  # list names: $type ExplorerWidth Version Name Children IncludeInDocumentation Enabled ReadOnly
#  # Children: Yes
#  # Children length: 6
#  # Children Names: Experiment RangeExperiment OperationsExpt Compound ManagerExpt DataStore
#  # Error in inspect_apsimx("Factorial", src.dir = ex.dir) :
#  #   more than one simulation found and no root node label has been specified
#  #  select one of the children names above

## ----inspect-apsimx-factorial-2-----------------------------------------------
inspect_apsimx("Factorial.apsimx", src.dir = extd.dir,
               root = c("RangeExperiment", "Base2"),
               node = "Weather")

## ----edit-apsimx-factorial-2--------------------------------------------------
edit_apsimx("Factorial.apsimx", 
            src.dir = extd.dir, wrt.dir = tmp.dir,
            root = c("RangeExperiment", "Base2"),
            node = "Weather", 
            value = "Ames.met")

## ----apsim-verions-tail, eval = FALSE-----------------------------------------
#  ava <- apsim_version()
#  aiu <- apsim_version(which = "inuse")

