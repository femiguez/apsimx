## These are tests that I will run to reproduce the part of the vignette
## That depends on APSIM-X. In this way the vignette can be build independently
## of APSIM-X, but the testing side-effect of the vignette is still present
require(apsimx)
apsimx_options(warn.versions = FALSE)

extd.dir <- system.file("extdata", package = "apsimx")

## Copy 'Maize.apsimx' to 'extdata'
ex.dir <- auto_detect_apsimx_examples()
file.copy(from = paste0(ex.dir,"/","Maize.apsimx"),
          to = ".", copy.mode = FALSE)
maize <- apsimx("Maize.apsimx", src.dir = extd.dir)

## Running 'Wheat' example
file.copy(from = paste0(ex.dir,"/","Wheat.apsimx"),
          to = ".", copy.mode = FALSE)
wheat <- apsimx("Wheat.apsimx", src.dir = extd.dir)

## Running 'MaizeSoybean' example
file.copy(from = paste0(extd.dir,"/","Ames.met"),
          to = ".")
file.copy(from = paste0(extd.dir,"/","MaizeSoybean.apsimx"),
          to = ".")
maize.soy <- apsimx("MaizeSoybean.apsimx")

## The code below should only be run from my laptop and only when a new
## version of APSIM-X is released
## This updates the 'Wheat' and 'Maize' example files in case they have
## changed

aiu <- apsim_version(which = "inuse", verbose = FALSE)

## Need to change this for new versions of APSIM-X
apsimx.new.version <- as.numeric(strsplit(aiu, ".", fixed = TRUE)[[1]][4]) == 4007

my.laptop <- grepl("fernandomiguez", Sys.info()[["user"]])
## This is only for my laptop
if(my.laptop & apsimx.new.version){
  my.extd <- "~/Dropbox/apsimx/inst/extdata"
  file.copy(from = paste0(ex.dir,"/","Maize.apsimx"),
            to = my.extd, copy.mode = FALSE, 
            overwrite = TRUE)
  file.copy(from = paste0(ex.dir,"/","Wheat.apsimx"),
            to = my.extd, copy.mode = FALSE,
            overwrite = TRUE)
}