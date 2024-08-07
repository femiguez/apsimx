
## Testing initialwater with a variety of files
require(apsimx)
packageVersion("apsimx")
apsimx_options(warn.versions = FALSE)

run.apsimx.initialwater <- get(".run.local.tests", envir = apsimx.options)

#### Testing InitialWater ----
if(run.apsimx.initialwater){
  
  tmp.dir <- tempdir()
  ex.dir <- auto_detect_apsimx_examples()
  ex.dir.list <- dir(ex.dir, pattern = "apsimx$")
  
  #### I will skip files that require 'root'
  ##ex.dir.list2 <- ex.dir.list[c(2, 4, 16:19, 23, 24, 29, 30, 34)]
  ex.dir.list3 <- c("Barley.apsimx", "Canola.apsimx", "Maize.apsimx", "Mungbean.apsimx", 
                     "Oats.apsimx", "OilPalm.apsimx", "Potato.apsimx", "RedClover.apsimx",
                     "Sorghum.apsimx", "Soybean.apsimx", "Wheat.apsimx")
  for(.i in ex.dir.list3){
    if(.i == "Slurp.apsimx") stop("This is bananas", call. = FALSE)
    cat("Simulation:", .i, "\n")
    #### 1) Inspect ----
    cat("Inspecting...\n")
    inspect_apsimx(.i, ex.dir, node = "Soil", soil.child = "InitialWater")
    pp <- inspect_apsimx(.i, ex.dir, node = "Soil", soil.child = "InitialWater", print.path = TRUE)
    #### 2) Extract ----
    cat("Extracting...\n")
    edf.iw <- extract_data_apsimx(.i, ex.dir, node = "Soil", soil.child = "InitialWater")
    #### 3) Edit ----
    cat("Editing...\n")
    if(!file.exists(file.path(tmp.dir, .i))) file.copy(from = file.path(ex.dir, .i), to = tmp.dir)
    if(!is.null(edf.iw$second) && nrow(edf.iw$second) > 0){
      initialvalues <- rep(0.3, nrow(edf.iw$second))
      edit_apsimx(.i, tmp.dir, node = "Soil", 
                  soil.child = "InitialWater",
                  parm = "InitialValues",
                  value = initialvalues)      
      #### 4) Check ----
      cat("Checking...\n")
      edited.file <- paste0(tools::file_path_sans_ext(.i), "-edited.apsimx")
      inspect_apsimx(edited.file, tmp.dir, node = "Soil", soil.child = "InitialWater")
      check_apsimx(edited.file, tmp.dir)
    }
    if(file.exists(file.path(tmp.dir, edited.file))) file.remove(file.path(tmp.dir, edited.file))
  }
}

