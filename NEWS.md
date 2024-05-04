**NEWS**

## apsimx 2.7.X

- sens_apsimx can now accept an argument called 'soil.profiles'. This opens the door
for conducting simulations on a spatial grid, by carefully passing weather and soils profiles.
Since now package 'future' is used for parallelization, this makes it easy to run it on a parallel
environment.

- sens_apsimx cores argument now requires package 'future'. 
This was done to support both unix and windows.

- edit_apsimx Editing different simulations used to require specifying 'root'. 
Now if the parameter path is complete, this is not required. In this case
node should equal "Other"

- check_apsim_met checks presence and values of tav and amp. New tav_apsim_met function
can be used to calculate and insert tav in an APSIM met file

- inspect_apsimx is a bit more robust. When node = "Other" the simulation structure will be displayed
according to the value of parm (0, 1, 2). If 'parm' is a parameter path, the root elements will be 'guessed' from the string.

## apsimx 2.6.2 - 2024-01-28

- Most change are to adjust to new structure in the APSIM simulations
- soil profiles now can contain an object of class 'initialwater_parms' that can be used to set the initial water. This caused problems in the past because of changing 'physical' without changing the initial water
- unexported function get_worldmodeler_soil_profile

## apsimx 2.5.1 - 2023-12-10

- Some of the changes are due to changes in APSIM Next Gen
- Need to add alternative 'root' for replacements

## apsimx 2.3

- Fixed bug in napad_apsim_met and also wrote more extensive tests (test_apsim_met)
- Added method confint for optim_apsim class and fixed bug related to this issue
- Added package 'ucminf' as an additional optimization type
- optim functions data argument data frame can contain missing data
- function get_daymet_apsim_met (which used to depend on the FedData package) is deprecated. 
It has been replaced by the previous get_daymet2_apsim_met function.
- remove use of raster package

## apsimx 2.2

- By deault it runs for newer versions of APSIM Next Gen (Sep 2021 or newer) (Classic is unchanged)
- For Mac and Linux, you might need to set 'mono = TRUE' for compatibility with older versions of APSIM Next Gen
- New functions: plot.met and summary.met for 'met' objects
- New function: carbon_stocks for calculating carbon stocks for an object of class 'soil_profile'
- New functions: comparing soil profiles 'compare_apsim_soil_profile' and associated plot function
- More options in 'unit_conv' function. 
- Edits to writing scripts vignette
- Various bug fixes and improvements.

## apsimx 2.0

- Sensitivity analysis functions for APSIM Next Gen and Classic
- Parameter Estimation functions for APSIM Next Gen and Classic
- Work in progress function **inspect_apsimx_json** for .json or .apsimx files
- Helper function (**grep_json_list** - not needed for most users)
- Unreleased **edit_apsimx_json** function.
- Various bug fixes in edit and inspect functions
- Creating synthetic soil profiles from ISRIC (**get_isric_soil_profile**)
- Depracated **get_daymet_apsim_met** (replaced with **get_daymet2_apsim_met**)
- Creating synthetic soil profile from SSURGO (**get_ssurgo_soil_profile** and **get_ssurgo_tables**)
- Passing extra arguments to APSIM call through **xargs_apsimx**

## apsimx 1.977

- Much improved handling of APSIM-X output. However, this led to some default changes. Argument 'value' in function 'apsimx' and 'read_apsimx' is 'report' instead of 'all'. The default for 'apsim' was also changed for consitency. Hopefully, this will not impact too many scripts.

- Multi-site optimization is now possible (for Classic and Next Gen). In this case, the index argument likely needs to be adjusted to 'index = c("outfile", "Date") for 'Classic' and 'index = c("report", "Date")' for 'Next Generation'

- Several improvements to 'get' functions for fetching weather data.

- Fixed bug when using 'root' argument in 'edit_apsim'

