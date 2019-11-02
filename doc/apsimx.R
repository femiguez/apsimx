## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(apsimx)
library(ggplot2)

## ----apsimx-options, echo = FALSE----------------------------------------
apsimx_options(warn.versions = FALSE)
ava <- apsim_version(verbose = FALSE)
##if(is.na(ava[2,2])) stop("Need APSIM-X to create this vignette")

## ----create-temp-dir, echo = FALSE, eval = FALSE-------------------------
#  tmp.dir <- tempdir()
#  setwd(tmp.dir)

## ----apsimx-example, eval = FALSE----------------------------------------
#  maize <- apsimx_example("Maize")

## ----read-in-maize-example, eval = TRUE, echo = FALSE--------------------
extd.dir <- system.file("extdata", package = "apsimx")
maize <- read_apsimx("Maize.db", src.dir = extd.dir)

## ----summary-ggplot-maize------------------------------------------------
summary(maize)
## Simple data plotting
ggplot(data = maize , aes(x = Date, y = Maize.AboveGround.Wt)) + 
  geom_point()

## ----inspect-apsimx------------------------------------------------------
ex.dir <- auto_detect_apsimx_examples()
inspect_apsimx("Maize", src.dir = ex.dir, node = "Weather")

## ----inspect-maize-oc----------------------------------------------------
inspect_apsimx("Maize.apsimx", src.dir = ex.dir, node = "Soil",
               soil.child = "Organic")

## ----edit-maize-oc-------------------------------------------------------
ocs <- c(1.5,1.4,1.3,1.2,1.1,1.0,0.9)
edit_apsimx("Maize.apsimx", src.dir = ex.dir,
             wrt.dir = ".",
             node = "Soil",
             soil.child = "Organic", 
             parm = "Carbon", value = ocs)

## ----inspect-maize-oc-check----------------------------------------------
inspect_apsimx("Maize-edited.apsimx", src.dir = ".", 
               node = "Soil",
               soil.child = "Organic")

## ----apsimx-wheat, eval = FALSE------------------------------------------
#  ## One example ('Wheat') is included with the package for the vignette
#  extd.dir <- system.file("extdata", package = "apsimx")
#  sim <- apsimx("Wheat.apsimx", src.dir = extd.dir, value = "report")

## ----apsimx-wheat-read, echo = FALSE-------------------------------------
sim <- read_apsimx("Wheat.db", src.dir = extd.dir)

## ----apsimx-wheat-summary------------------------------------------------
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
               node = "Manager", parm = list("SowingRule1",NA))

## ----inspect-replacement-node--------------------------------------------
inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                            node = "Maize", display.available = TRUE)

## ----inspect-replacement-node-child--------------------------------------
inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                            node = "Maize", node.child = "Phenology",
                           display.available = TRUE)

## ----inspect-replacement-node-subchild-----------------------------------
inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                            node = "Maize", node.child = "Phenology",
                           node.subchild = "ThermalTime",
                           display.available = TRUE)

## ----inspect-replacement-node-subchild-parm------------------------------
inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                            node = "Maize", node.child = "Phenology",
                            node.subchild = "ThermalTime", parm = c("X","Y")) 

## ----inspect-replacement-soybean-cultivar-node---------------------------
inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                            node = "Soybean", display.available = TRUE) 

## ----inspect-replacement-soybean-cultivar-node-child---------------------
inspect_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                           node = "Soybean", 
                           node.child = "PioneerP22T61_MG22",
                           display.available = FALSE) 

## ----edit-replacement----------------------------------------------------
edit_apsimx_replacement("MaizeSoybean.apsimx", src.dir = extd.dir,
                        node = "Maize", node.child = "Phenology",
                        node.subchild = "ThermalTime", 
                        node.subsubchild = "BaseThermalTime",
                        parm = "Y", value = c(0,12,20,28,0))

## ----inspect-edit-replacement--------------------------------------------
inspect_apsimx_replacement("MaizeSoybean-edited.apsimx", src.dir = ".",
                            node = "Maize", node.child = "Phenology",
                            node.subchild = "ThermalTime", parm = c("X","Y")) 

## ----inspect-factorial-0, eval = FALSE-----------------------------------
#  ## There are multiple 'Experiments' so we need to pick one
#  inspect_apsimx_replacement("Factorial", src.dir = extd.dir,
#                             root = list("Experiment",NA))
#  ##These positions matched  Experiment   1 2 3 4 5
#  ##Error in inspect_apsimx_replacement("Factorial", src.dir = ex.dir, root = ##list("Experiment", : Multiple root nodes found. Please provide a position

## ----inspect-factorial---------------------------------------------------
## There are multiple 'Experiments' so we need to pick one
inspect_apsimx_replacement("Factorial", src.dir = extd.dir,
                           root = list("Experiment",1))
## We need to provide a node
inspect_apsimx_replacement("Factorial", src.dir = extd.dir,
                           root = list("Experiment",1), 
                           node = "Base", display.available = TRUE)
## We need to provide a node child
inspect_apsimx_replacement("Factorial", src.dir = extd.dir,
                           root = list("Experiment",1), 
                           node = "Base", node.child = "Clock",
                           display.available = TRUE)

## ----filetypes-----------------------------------------------------------
ex.dir <- auto_detect_apsimx_examples()
apsimx_filetype("Barley", src.dir = ex.dir)
extd.dir <- system.file("extdata", package = "apsimx")
## This is an older XML 'Maize' file which is no longer distributed
apsimx_filetype("Maize_old", src.dir = extd.dir)

## ----Millet--------------------------------------------------------------
extd.dir <- system.file("extdata", package = "apsimx")
inspect_apsim("Millet.apsim", src.dir = extd.dir, node  = "Manager")

## ----Millet-sow----------------------------------------------------------
inspect_apsim("Millet.apsim", src.dir = extd.dir, node  = "Manager",
              parm = list("Sow on a fixed date",NA))

## ----Millet-sow-plt-dens-------------------------------------------------
inspect_apsim("Millet.apsim", src.dir = extd.dir, node  = "Manager",
              parm = list("Sow on a fixed date",5), print.path = TRUE)
## Or store it in an object for later editing
pp <- inspect_apsim("Millet.apsim", src.dir = extd.dir, node  = "Manager",
              parm = list("Sow on a fixed date",5), print.path = TRUE)

## ----Millet-edit---------------------------------------------------------
edit_apsim("Millet", src.dir = extd.dir, wrt.dir = ".",
           node = "Other", parm.path = pp, value = 8, 
           edit.tag = "-pp")

## ----removing-Millet-pp, echo = FALSE, eval = TRUE-----------------------
file.remove("Millet-pp.apsimx")

