## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
require(apsimx)

## ----run-apsimx, eval = FALSE--------------------------------------------
#  ## Mac
#  run.string.mac <- "mono /Applications/APSIM2019.06.25.3956.app/Contents/Resources/Bin/Models.exe ./Wheat.apsimx"
#  system(command = run.string.mac, intern = TRUE)
#  ## Debian
#  run.string.debian <- "mono /usr/local/lib/apsim/2019.06.25.3956/Bin/Models.exe ./Wheat.apsimx"
#  system(command = run.string.debian, intern = TRUE)

## ----sisd----------------------------------------------------------------
sessionInfo()
Sys.info()[["sysname"]]
Sys.info()[["release"]]
Sys.info()[["version"]]
if(.Platform$OS.type == "unix") system(command = "mono --version")
R.version

