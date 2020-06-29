## Testing the 'edit_apsim_xml' function
require(apsimx)

## I guess I can run this non-locally
extd.dir <- system.file("extdata", package = "apsimx")

tmp.dir <- tempdir()
 
values <- paste(rep(1.7, 12), collapse = " ")
 
edit_apsim_xml("Maize75.xml", 
                src.dir = extd.dir,
                wrt.dir = tmp.dir, 
                parm.path = ".//Model/rue",
                value = values)

pp1 <- edit_apsim_xml("Maize75.xml", 
                      src.dir = extd.dir,
                      wrt.dir = tmp.dir, 
                      parm.path = ".//Model/rue",
                      value = values,
                      verbose = FALSE)

pp2 <- edit_apsim_xml("Maize75.xml", 
                      src.dir = extd.dir,
                      wrt.dir = tmp.dir, 
                      parm.path = ".//rue",
                      value = values,
                      verbose = FALSE)
