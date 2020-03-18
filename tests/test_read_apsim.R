## Testing read_apsim_all function
require(apsimx)

extd.dir <- system.file("extdata", package = "apsimx")

maize.out <- read_apsim("Maize.out", src.dir = extd.dir)

millet.out <- read_apsim("Millet.out", src.dir = extd.dir)
