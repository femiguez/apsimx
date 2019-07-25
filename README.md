apsimx
R package for APSIM-X
This package allows for interaction with APSIM ("Next Generation") or
APSIM-X. It can inspect, edit, run and read APSIMX files. APSIMX now
uses two different file types for .apsimx files: XML and JSON. This
package can inspect and edit XML files and it can inspect JSON
files. The editing of JSON files is slightly more limited thatn for
XML at the moment (July 21st).

* Package requirements:

* Other R packages: DBI, jsonlite, knitr, RSQLite, xml2

* APSIMX:

The new APSIM uses the mono framework to run on Windows, Mac and Linux
(Debian). Mono should be installed first (in Mac and Linux).

**Mono framework download**:
https://www.mono-project.com/download/stable/

**APSIMX download**:
https://www.apsim.info/Products/Downloads.aspx

If you need an R package to interact with the previous version(s) of
APSIM (7.x) try 'APSIM' and 'apsimr'.

If you want to install this package from github try in R:

library(devtools)

devtools::install_github("femiguez/apsimx")

If you have any questions contact Fernando E. Miguez (femiguez *at* iastate.edu)
