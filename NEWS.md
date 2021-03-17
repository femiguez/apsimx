**NEWS**

**apsimx 1.978**


**apsimx 1.977**

- Much improved handling of APSIM-X output. However, this led to some default changes. Argument 'value' in function 'apsimx' and 'read_apsimx' is 'report' instead of 'all'. The default for 'apsim' was also changed for consitency. Hopefully, this will not impact too many scripts.

- Multi-site optimization is now possible (for Classic and Next Gen). In this case, the index argument likely needs to be adjusted to 'index = c("outfile", "Date") for 'Classic' and 'index = c("report", "Date")' for 'Next Generation'

- Several improvements to 'get' functions for fetching weather data.

- Fixed bug when using 'root' argument in 'edit_apsim'

