## Testing read_apsim_all function
require(apsimx)

run.examples <- FALSE

extd.dir <- system.file("extdata", package = "apsimx")

maize.out <- read_apsim("Maize.out", src.dir = extd.dir)

millet.out <- read_apsim("Millet.out", src.dir = extd.dir)

## Running all examples
os <- .Platform$OS.type

if(os == "windows" && run.examples){
  
  apsim_options(warn.versions = FALSE)
  ex.classic.dir <- apsimx:::auto_detect_apsim_examples()
  apsim.examples <- dir(ex.classic.dir, pattern = "apsim$")
  
  tmp.dir <- tempdir()
  setwd(tmp.dir)
  
  for(i in apsim.examples){
    if(grepl("\\s", i)) next
    file.copy(file.path(ex.classic.dir, i), to = file.path(tmp.dir, i))
    cat("Example:", i)
    sim <- try(apsim(file = i, simplify = FALSE), silent = TRUE)
    if(inherits(sim, "try-error")){
      cat(" Failed. \n")
      next
    }else{
      cat("; object class:", class(sim), " ")
      cat("; Done. \n")  
    }
  }
}