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

* Suggested R packages: BayesianTools, datasets, daymetr, FedData, future, ggplot2,
  GSODR, listviewer, maps, mvtnorm, nasapower, nloptr, reactR, rmarkdown, soilDB, sp, spData, sf, ucminf

* APSIMX:

* and/or APSIM (7.x) 'Classic'

NOTE ON REQUIREMENTS:

* Current versions of APSIM Next Gen do not require additional software
to run on Mac or Linux. The dotnet runtime environment is shipped with the image/package.

* For current versions, on Windows, the 'Microsoft Windows Desktop Runtime' 3.1.14 is required. The installer also updated GTK3 to 3.24.20 during the installation.

* For older versions of APSIM Next Gen (before Sept 2021) the mono framework was required to run on Mac and Linux (Debian). Mono should be installed first (in Mac and Linux). 

**If you are running the latest version of APSIM Next Gen, you do 
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

Some papers that cite this package:

14\. van Versendaal et al.  Integrating Field Data and a Modeling Approach to Inform Optimum Planting Date × Maturity Group for Soybeans under Current and Future Weather Conditions in Kansas. Sustainability 2023, 15(2), 1081; https://doi.org/10.3390/su15021081

13\. R. H. K. Rathnappriya et al. Global Sensitivity Analysis of Key Parameters in the APSIMX-Sugarcane Model to Evaluate Nitrate Balance via Treed Gaussian Process. Agronomy 2022, 12(8), 1979; https://doi.org/10.3390/agronomy12081979

12\. Francisco Palmero, Ana J.P. Carcedo, Ricardo J. Haro, Ezequiel D. Bigatton, Fernando Salvagiotti, Ignacio A. Ciampitti. Modeling drought stress impacts under current and future climate for peanut in the semiarid pampas region of Argentina. Field Crops Research. 2022. https://doi.org/10.1016/j.fcr.2022.108615.

11\. Elsa Lagerquist, Iris Vogeler, Uttam Kumar, Göran Bergkvist, Marcos Lana, Christine A. Watson, David Parsons,
Assessing the effect of intercropped leguminous service crops on main crops and soil processes using APSIM NG.
Agricultural Systems. 2024. https://doi.org/10.1016/j.agsy.2024.103884.

10\. Daniel Pasquel, Davide Cammarano, Sébastien Roux, Annamaria Castrignanò, Bruno Tisseyre, Michele Rinaldi, Antonio Troccoli, James A. Taylor. Downscaling the APSIM crop model for simulation at the within-field scale,
Agricultural Systems. 2023. https://doi.org/10.1016/j.agsy.2023.103773.

9\. Tommaso Tadiello, Mara Gabbrielli, Marco Botta, Marco Acutis, Luca Bechini, Giorgio Ragaglini, Andrea Fiorini, Vincenzo Tabaglio, Alessia Perego. A new module to simulate surface crop residue decomposition: Description and sensitivity analysis. Ecological Modelling. Volume 480. 2023. https://doi.org/10.1016/j.ecolmodel.2023.110327.

8\. Lopez-Cruz, M., Aguate, F.M., Washburn, J.D. et al. Leveraging data from the Genomes-to-Fields Initiative to investigate genotype-by-environment interactions in maize in North America. Nat Commun 14, 6904 (2023). https://doi.org/10.1038/s41467-023-42687-4

7\. Ignacio Massigoge, Ana Carcedo, Jane Lingenfelser, Trevor Hefley, P.V. Vara Prasad, Dan Berning, Sara Lira, Carlos D. Messina, Charles W. Rice, Ignacio Ciampitti. Maize planting date and maturity in the US central Great Plains: Exploring windows for maximizing yields. European Journal of Agronomy. Volume 149. 2023. 
https://doi.org/10.1016/j.eja.2023.126905.

6\. Yang, Xuening and Zhang, Xuanze and Zhao, Zhigan and Ma, Ning and Tian, Jing and Xu, Zhenwu and Zhang, Junmei and Zhang, Yongqiang, Rainfall and Maximum Temperature are Dominant Climatic Factors Influencing Apsim-Maize Cultivar Parameters Sensitivity in Semiarid Regions. Available at SSRN: https://ssrn.com/abstract=4693866 or http://dx.doi.org/10.2139/ssrn.4693866

5\. Determining site-specific corn nitrogen rate over time with APSIM model. L.J. Thompson, S. Archontoulis, and L.A. Puntel. https://doi.org/10.3920/978-90-8686-947-3_138

4\. Kheir, A. M. S., Mkuhlani, S., Mugo, J. W., Elnashar, A., Nangia, V., Devare, M., & Govind, A. (2023). Integrating APSIM model with machine learning to predict wheat yield spatial distribution. Agronomy Journal, 115, 3188–3196. https://doi.org/10.1002/agj2.21470

3\. Acceptability and Evaluation of APSIM-Qryza for Promoting Water and Nitrogen Productivity in Paddy Fields. 
Shayan Hosseinpour, Hemmatollah Pirdashti, Mohammad Kaveh, Hamze Dokoohaki. https://doi.org/10.21203/rs.3.rs-2677879/v1

2\. Augmenting agroecosystem models with remote sensing data and machine learning increases overall estimates of nitrate-nitrogen leaching. Matthew Nowatzke, Luis Damiano, Fernando E Miguez, Gabe S McNunn, Jarad Niemi, Lisa A Schulte, Emily A Heaton and Andy VanLoocke. 20 October 2022. Environmental Research Letters, Volume 17, Number 11
DOI: 10.1088/1748-9326/ac998b

1\. Laurent, A., Cleveringa, A., Fey, S. et al. Late-season corn stalk nitrate measurements across the US Midwest from 2006 to 2018. Sci Data 10, 192 (2023). https://doi.org/10.1038/s41597-023-02071-9
