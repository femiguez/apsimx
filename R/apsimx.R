#' Run an APSIM-X Simulation
#' 
#' A valid apsimx file can be run from within R. The main goal is to make running APSIM-X
#' simple, especially for large scale simulations or parameter optimization
#' 
#' @title Run an APSIM-X simulation
#' @name apsimx
#' @description The goal is to run apsimx from R. It basically calls 'system' to run from the command line.
#' @param file file name to be run (the extension .apsimx is optional)
#' @param src.dir directory containing the .apsimx file to be run (defaults to the current directory)
#' @param silent whether to print messages from apsim executable
#' @param value how much output to return:
#'              option 'all' returns all components of the simulation;
#'              option 'report' returns only the 'main' report component;
#'              option 'none' does not create a data.frame but it generates the databases from APSIM-X
#' @param cleanup level of file cleanup:
#'                0 does nothing; 
#'                1 deletes the .db created by APSIM-X;
#'                2 is not implemented yet.
#' @export
#' @examples 
#' \dontrun{
#' ## See function 'apsimx_example' 
#' }
#'

apsimx <- function(file = "", src.dir=".",
                   silent = FALSE, 
                   value = c("all","report","none"),
                   cleanup = c(0,1,2)){
  
  value <- match.arg(value)
  
  if(file == "") stop("need to specify file name")
  
  ## The following lines are somewhat not needed
  fileNames <- dir(path = src.dir, pattern=".apsimx$",ignore.case=TRUE)
  
  file <- match.arg(file, fileNames, several.ok=FALSE)
  
  if(length(fileNames)==0){
    stop("There are no .apsimx files in the specified directory to edit.")
  }
  
  if(length(grep(".apsimx$",file)) != 0){
    ## I assume the extention was included
    ## Only use the name from here 
    file <- strsplit(file, ".", fixed = TRUE)[[1]][1]
  }
  
  file.name.path <- paste0(src.dir,"/",file)
  
  ## This function will run an APSIM file
  ## This work on MacOS and it might work on other 'unix'
  mono <- system("which mono", intern = TRUE)
  ada <- auto_detect_apsimx()
  run.strng <- paste0(mono," ",ada," ",file.name.path,".apsimx")
  ## Use the system function
  system(command = run.strng, ignore.stdout = silent)
  
  if(value != "none"){
    ans <- read_apsimx(file = file, src.dir = src.dir, value = value)
  }else{
    if(value == "none" & !silent){
      cat("APSIM created .db files, but nothing is returned \n")
    }
  }
  
  if(cleanup == 1){
    ## Default is not to cleanup
    if(value == "none") stop("do not clean up if you choose value = 'none' ")
    ## Delete the apsim-generated sql database 
    system(paste0("rm ",file.name.path,".db"))
  }
  
  if(value != "none")
    return(ans)
}

## This is an internal function so I won't export/document it
auto_detect_apsimx <- function(){
  
  ## This is only valid for MacOS for now
  if(.Platform$OS.type != "unix"){
    stop("not implemented yet")
  }
  ## If APSIM-X is installed it will be in /Applications/
  ## look into Applications folder
  laf <- list.files("/Applications/")
  find.apsim <- grep("APSIM",laf)
  if(find.apsim == 0) stop("APSIM-X not found")
  apsimx.name <- laf[find.apsim]
  ## Apsim executable
  st1 <- "/Applications/"
  st3 <- "/Contents/Resources/Bin/Models.exe" 
  apsimx_dir <- paste0(st1,apsimx.name,st3)
  return(apsimx_dir)
}

#' Auto detect where apsimx examples are located 
#' 
#' @title Auto detect where apsimx examples are located
#' @name auto_detect_apsimx_examples
#' @description simple function to detect where APSIM-X examples are located
#' @return will create a directory pointing to APSIM-X distributed examples
#' @export
#' @examples 
#' \dontrun{
#' ex.dir <- auto_detect_apsimx_examples()
#' }
#' 

auto_detect_apsimx_examples <- function(){
  
  ## This is only valid for MacOS for now
  if(.Platform$OS.type != "unix"){
    stop("not implemented yet")
  }
  ## If APSIM-X is installed it will be in /Applications/
  ## look into Applications folder
  laf <- list.files("/Applications/")
  find.apsim <- grep("APSIM",laf)
  if(find.apsim == 0) stop("APSIM-X not found")
  apsimx.name <- laf[find.apsim]
  ## Apsim executable
  st1 <- "/Applications/"
  st3 <- "/Contents/Resources/Examples" 
  apsimx_dir <- paste0(st1,apsimx.name,st3)
  return(apsimx_dir)
}

#' Access Example APSIM-X Simulations
#' 
#' @title Access Example APSIM-X Simulations
#' @name apsimx_example
#' @description simple function to run some of the built-in APSIM-X examples
#' @param example run an example from built-in APSIM-X. Options are Wheat, Barley, Maize, Oats, Sugarcane
#' @param silent whether to print standard output from the APSIM-X execution
#' @export
#' @examples 
#' \dontrun{
#' wheat <- apsimx_example("Wheat")
#' maize <- apsimx_example("Maize")
#' barley <- apsimx_example("Barley")
#' 
#' ggplot(data = wheat , aes(x = Date, y = Yield)) + 
#'   geom_point()
#' }
#' 

apsimx_example <- function(example = "Wheat", silent = FALSE){

  ## This is only valid for MacOS for now
  if(.Platform$OS.type != "unix"){
    stop("not implemented yet")
  }
  
  ## Run a limited set of examples
  ex.ch <- c("Wheat","Maize", "Oats","Sugarcane")
  example <- match.arg(example, choices = ex.ch)
  ## This works on MacOS and it might work on other 'unix' 
  mono <- system("which mono", intern = TRUE)
  ada <- auto_detect_apsimx()
  ex.dir <- auto_detect_apsimx_examples()
  ex <- paste0(ex.dir,"/",example)
  run.strng <- paste0(mono," ",ada," ",ex,".apsimx")
  ## Run APSIM
  system(command = run.strng, ignore.stdout = silent)
  ## Create database connection
  ans <- read_apsimx(paste0(example,".db"), src.dir = ex.dir, value = "report")
  ## Dangerous cleanup
  system(paste0("rm ",ex,".db"))
  ## Return data frame
  ## Add the date
  ans$Date <- as.Date(sapply(ans$Clock.Today, function(x) strsplit(x, " ")[[1]][1]))
  return(ans)
  
}

#' Read APSIM-X generated .db files
#' 
#' @title Read APSIM-X generated .db files
#' @name read_apsimx
#' @description read SQLite databases created by APSIM-X runs. One file at a time.
#' @param file file name
#' @param src.dir source directory where file is located
#' @param value either 'report' or 'all'
#' @export
#' 

read_apsimx <- function(file = "", src.dir = ".",
                        value = c("report","all")){
  
  if(file == "") stop("need to specify file name")
  
  fileNames <- dir(path = src.dir, pattern=".db$",ignore.case=TRUE)
  
  if(length(fileNames)==0){
    stop("There are no .db files in the specified directory to read.")
  }
  
  value <- match.arg(value)
  
  if(length(grep(".db$",file)) != 0){
    ## I assume the extention was included
    ## Only use the name from here 
    ## This strips the extension
    file <- strsplit(file, ".", fixed = TRUE)[[1]][1]
  }
  
  file.name.path <- paste0(src.dir,"/",file)
  
  con <- DBI::dbConnect(RSQLite::SQLite(), paste0(file.name.path,".db"))
  ## create data frame for each table
  tbl0 <- DBI::dbGetQuery(con, "SELECT * FROM Report")
  tbl1 <- DBI::dbGetQuery(con, "SELECT * FROM _Checkpoints")
  tbl2 <- DBI::dbGetQuery(con, "SELECT * FROM _InitialConditions")
  tbl3 <- DBI::dbGetQuery(con, "SELECT * FROM _Messages")
  tbl4 <- DBI::dbGetQuery(con, "SELECT * FROM _Simulations")
  tbl5 <- DBI::dbGetQuery(con, "SELECT * FROM _Units")
  ## Disconnect
  DBI::dbDisconnect(con)
  
  ## Return list
  if(value == "all"){
    tbl0$Date <- as.Date(sapply(tbl0$Clock.Today, function(x) strsplit(x, " ")[[1]][1]))
    ans <- list(Report = tbl0, Checkpoints = tbl1, InitialConditions = tbl2,
                Messages = tbl3, Simualtions = tbl4, Units = tbl5)
    return(ans)
  }
  ## Return data.frame
  if(value == "report"){
    tbl0$Date <- as.Date(sapply(tbl0$Clock.Today, function(x) strsplit(x, " ")[[1]][1]))
    return(tbl0)
  }
}

#' Read all APSIM-X generated .db files in a directory
#' 
#' @title Read all APSIM-X generated .db files in a directory
#' @name read_apsimx_all
#' @description Like 'read_apsimx', but it reads all .db files in a directory. 
#' @param src.dir source directory where files are located
#' @param value either 'report' or 'all' (only 'report' implemented at the moment)
#' @note Warning: very simple function at the moment, not optimized for memory or speed.
#' @export
#' 

read_apsimx_all <- function(src.dir = ".", value = c("report","all")){
  
  ## This is super memorey hungry and not efficient at all, but it might work 
  ## for now
  
  value <- match.arg(value)
  
  fileNames <- dir(path = src.dir, pattern=".db$",ignore.case=TRUE)
  
  ans <- NULL
  
  for(i in fileNames){
    
    tmp <- read_apsimx(fileNames[i], value = value)
    tmp.d <- data.frame(file.name = fileNames[i], tmp)
    ans <- rbind(ans, tmp)
    
  }
  return(ans)
}

#' Import packages needed for apsimx to work correctly
#' @import DBI RSQLite knitr xml2 jsonlite
NULL
