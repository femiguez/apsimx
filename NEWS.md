**NEWS**

**apsimx 2.2**

- By deault it runs for newer versions of APSIM Next Gen (Sep 2021 or newer) (Classic is unchanged)
- For Mac and Linux, you might need to set 'mono = TRUE' for compatibility with older versions of APSIM Next Gen
- New functions: plot.met and summary.met for 'met' objects
- New function: carbon_stocks for calculating carbon stocks for an object of class 'soil_profile'
- New functions: comparing soil profiles 'compare_apsim_soil_profile' and associated plot function
- More options in 'unit_conv' function. 
- Edits to writing scripts vignette
- Various bug fixes and improvements.

**apsimx 2.0**

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

**apsimx 1.977**

- Much improved handling of APSIM-X output. However, this led to some default changes. Argument 'value' in function 'apsimx' and 'read_apsimx' is 'report' instead of 'all'. The default for 'apsim' was also changed for consitency. Hopefully, this will not impact too many scripts.

- Multi-site optimization is now possible (for Classic and Next Gen). In this case, the index argument likely needs to be adjusted to 'index = c("outfile", "Date") for 'Classic' and 'index = c("report", "Date")' for 'Next Generation'

- Several improvements to 'get' functions for fetching weather data.

- Fixed bug when using 'root' argument in 'edit_apsim'

