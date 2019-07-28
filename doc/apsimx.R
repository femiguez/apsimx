## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(apsimx)
library(ggplot2)

## ----apsimx-example, eval = TRUE-----------------------------------------
maize <- apsimx_example("Maize")
summary(maize)
## Simple data plotting
ggplot(data = maize , aes(x = Date, y = Maize.AboveGround.Wt)) + 
  geom_point()

## ----inspect-apsimx------------------------------------------------------
ex.dir <- auto_detect_apsimx_examples()
inspect_apsimx("Maize", src.dir = ex.dir, node = "Weather")

## ----inspect-maize-oc----------------------------------------------------
inspect_apsimx("Maize.apsimx", src.dir = ex.dir, node = "Soil",
               soil.child = "OrganicMatter")

## ----edit-maize-oc-------------------------------------------------------
ocs <- c(1.5,1.4,1.3,1.2,1.1,1.0,0.9)
edit_apsimx("Maize.apsimx", src.dir = ex.dir,
             wrt.dir = ".",
             node = "Soil",
             soil.child = "OrganicMatter", 
             parm = "OC", value = ocs)

## ----inspect-maize-oc-check----------------------------------------------
inspect_apsimx("Maize-edited.apsimx", src.dir = ".", 
               node = "Soil",
               soil.child = "OrganicMatter")

## ----edit-apsimx-delete, echo = TRUE-------------------------------------
## delete the created file
file.remove("./Maize-edited.apsimx")

## ----apsimx--------------------------------------------------------------
## One example ('Wheat') is included with the package for the vignette
ex.dir <- system.file("extdata", package = "apsimx")
sim <- apsimx("Wheat.apsimx", src.dir = ex.dir, value = "report")
## Calcualte summary statistics on all variables
summary(sim)
## Plot data
ggplot(data = sim, aes(x = Date, y = Yield)) + geom_point()
## Inspect the Wheat .apsimx file
inspect_apsimx("Wheat", src.dir = ex.dir, node = "Crop")
inspect_apsimx("Wheat", src.dir = ex.dir, node = "Manager")

## ----filetypes-----------------------------------------------------------
ex.dir <- auto_detect_apsimx_examples()
apsimx_filetype("Barley", src.dir = ex.dir)
extd.dir <- system.file("extdata", package = "apsimx")
## This is an older XML 'Maize' file which is no longer distributed
apsimx_filetype("Maize", src.dir = extd.dir)

