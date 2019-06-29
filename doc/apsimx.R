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
edit_apsimx("Maize.apsimx", src.dir = ex.dir,
            wrt.dir = ".",
            node = "Soil",
            soil.child = "OrganicMatter", 
            parm = "OC", value = ocs)

## ----edit-apsimx-delete, echo = FALSE------------------------------------
## delete the created file
system("rm ./Maize-edited.apsimx")

