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

## ----edit-apsimx---------------------------------------------------------
ex.dir <- auto_detect_apsimx_examples()
ocs <- c(1.5,1.4,1.3,1.2,1.1,1.0,0.9)
edit_apsimx_xml("Maize.apsimx", src.dir = ex.dir,
                wrt.dir = ".",
                node = "Soil",
                soil.child = "OrganicMatter", 
                parm = "OC", value = ocs)

## ----edit-apsimx-delete, echo = TRUE-------------------------------------
## delete the created file
system("rm ./Maize-edited.apsimx")

## ----apsimx--------------------------------------------------------------
## Detect 'Examples' directory
ex.dir <- auto_detect_apsimx_examples()
## Run simulation, this will take a while
sim <- apsimx("Wheat.apsimx", src.dir = ex.dir, value = "report", cleanup = 1)
## Calcualte summary statistics on all variables
summary(sim)
## Plot data
ggplot(data = sim, aes(x = Date, y = Yield)) + geom_point()
## Inspect the Wheat .apsimx file
inspect_apsimx("Wheat", src.dir = ex.dir, node = "Crop")
inspect_apsimx("Wheat", src.dir = ex.dir, node = "Manager")

