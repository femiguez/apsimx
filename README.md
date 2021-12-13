**apsimx: R package for APSIM-X (NextGen) and APSIM Classic (7.x)**

[![CRAN](http://www.r-pkg.org/badges/version/apsimx)](https://CRAN.R-project.org/package=apsimx)
[![CRAN
downloads total](https://cranlogs.r-pkg.org/badges/grand-total/apsimx)](https://github.com/r-hub/cranlogs.app)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/apsimx)](https://cran.r-project.org/package=apsimx)


This package allows for interaction with APSIM-X ("Next Generation")
and/or APSIM 'Classic' (7.10). It can inspect, edit, run and read
APSIM files in both platforms. The format is JSON for APSIM-X and XML
for 'Classic'.

**Package requirements**

* Imported R packages: DBI, jsonlite, knitr, RSQLite, tools, utils, xml2

* Suggested R packages: BayesianTools, datasets, daymetr, FedData, ggplot2,
  GSODR, listviewer, maps, mvtnorm, nasapower, nloptr, reactR, rmarkdown, soilDB, sp, spData, sf, ucminf

* APSIMX:

* and/or APSIM (7.x) 'Classic'

NOTE ON REQUIREMENTS:

* Current (Nov 2021) versions of APSIM Next Gen do not require additional software
to run on Mac or Linux. The dotnet runtime environment is shipped with the image/package.

* For current versions (Nov 2021), on Windows, the 'Microsoft Windows Desktop Runtime' 3.1.14 is required. The installer also updated GTK3 to 3.24.20 during the installation.

* For older versions of APSIM Next Gen (before Sept 2021) the mono framework was required to run on Mac and Linux (Debian). Mono should be installed first (in Mac and Linux). 

**If you are running the latest version of APSIM Next Gen (Sept 2021 or later), you do 
not need to install the Mono Framework**.

**Mono framework download**:
https://www.mono-project.com/download/stable/

**.Net Core download for Mac**:
https://dotnet.microsoft.com/download

**APSIMX download**:
https://www.apsim.info/download-apsim/

Since APSIM Next Gen 2021.04.01 (at least) .NET framework 4.6 or higher is 
required for Windows and I have had to update the Mono framework to 
6.12 on Mac (again this applies to pre Sep 2021).

If you want to install this package from github try in R:

> library(devtools) \
> devtools::install_github("femiguez/apsimx") \
> library(apsimx)

or the lightweight 'remotes' package

> library(remotes) \
> remotes::install_github("femiguez/apsimx") \
> library(apsimx)

It is **not** necessary to build the vignettes as they are also at:
https://femiguez.github.io/apsimx-docs/

*Note*: Building the vignettes does not require the presence of APSIM-X as I
have recently eliminated the APSIM-X dependency.

Still, if you want to build the vignettes, then use this instead
of the second line above:

> devtools::install_github("femiguez/apsimx", build_vignettes = TRUE,
> build_opts = c("--no-resave-data", "--no-manual")) 

If you do build the vignettes, there is an introduction to the package

> vignette("apsimx")

and a document which might help you build your own scripts either in R
or other languages

> vignette("apsimx-scripts")

If you have any questions contact Fernando E. Miguez (femiguez *at* iastate.edu)
