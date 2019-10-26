apsimx
R package for APSIM-X
This package allows for interaction with APSIM ("Next Generation") or
APSIM-X. It can inspect, edit, run and read APSIMX files. APSIMX now
uses file type JSON for .apsimx. The
package can inspect and edit XML and JSON files. The inspecting and
editing files is an area that I'm currently improving. Since
version 0.95, apsimx can inspect, run and read APSIM 'Classic'.

* Package requirements:

* Other R packages: DBI, jsonlite, knitr, RSQLite, xml2

* APSIMX:

* and/or APSIM (7.x) 'Classic'

The new APSIM uses the mono framework to run on Windows, Mac and Linux
(Debian). Mono should be installed first (in Mac and Linux).

**Mono framework download**:
https://www.mono-project.com/download/stable/

**APSIMX download**:
https://www.apsim.info/Products/Downloads.aspx

If you need other R packages to interact with the previous version(s) of
APSIM (7.x) try 'APSIM' and 'apsimr'.

If you want to install this package from github try in R:

> library(devtools) \
> devtools::install_github("femiguez/apsimx") \
> library(apsimx)

If you are interested in the vignettes, first you need to make sure
that you have a version of APSIM-X installed and then use this instead
of the second line above:

> devtools::install_github("femiguez/apsimx", build_vignettes = TRUE,
> build_opts = c("--no-resave-data", "--no-manual")) 

If you have any questions contact Fernando E. Miguez (femiguez *at* iastate.edu)
