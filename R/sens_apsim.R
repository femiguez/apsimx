#' @title Sensitivity Analysis for APSIM Next Generation simulation
#' @name sens_apsim
#' @rdname sens_apsim
#' @description It is a wrapper for running APSIM and evaluating different parameters values
#' @param file file name to be run (with extension .apsim)
#' @param src.dir directory containing the .apsim file to be run (defaults to the current directory)
#' @param crop.file name of auxiliary xml file where parameters are stored. If this is missing, it is 
#'                  assumed that the parameters to be edited are in the main simulation file.
#' @param parm.paths absolute or relative paths of the coefficients to be evaluated. 
#'             It is recommended that you use \code{\link{inspect_apsim}} for this
#' @param parm.vector.index Index to evaluate a specific element of a parameter vector.  At the moment it is
#' possible to only edit one element at a time. This is because there is a conflict when generating multiple
#' elements in the candidate vector for the same parameter.
#' @param xml.parm TRUE or FALSE for each parameter. Indicating whether it is part of 
#' an xml file. Its length should be equal to the length or \sQuote{parm.paths}.
#' @param grid grid of parameter values for the evaluation. It can be a data.frame.
#' @param summary function name to use to summarize the output to be a sinlge row (default is the mean).
#' @param root root argument for \code{\link{edit_apsim}}
#' @param verbose whether to print progress in percent and elapsed time.
#' @param cores number of cores to use for parallel evaluation
#' @param save whether to save intermediate results. By default they will be saved as a
#' \sQuote{csv} file using the name of the apsim file. This will replace \sQuote{apsim} with \sQuote{csv}.
#' It is also possible to provide the file name here (for example: \sQuote{Some_results.csv}).
#' @param ... additional arguments (none used at the moment).
#' @note The summary function is stored as an attribute of the data frame \sQuote{grid.sims}.
#' @return object of class \sQuote{sens_apsim}, but really just a list with results from the evaluations.
#' @export
#' @examples 
#' \donttest{
#' ## See the vignette for examples
#' }
#' 

sens_apsim <- function(file, src.dir = ".", 
                       crop.file,
                       parm.paths,
                       parm.vector.index,
                       xml.parm,
                       grid,
                       summary = c("mean", "max", "var", "sd", "none"),
                       root,
                       verbose = TRUE,
                       cores = 1L,
                       save,
                       ...){
  
  if(missing(file))
    stop("file is missing with no default")
  
  .check_apsim_name(file)
  .check_apsim_name(src.dir)
  
  if(cores > 1L){
    stop("This feature has not been implemented yet for Classic. 
         Please submit an issue in github if you need it")    
  }

  if(src.dir != ".") stop("At the moment it is not possible \n
                          to change the source directory.")
  
  ## This might offer suggestions in case there is a typo in 'file'
  file.names <- dir(path = src.dir, pattern = ".apsim$", ignore.case = TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsim files in the specified directory to run.")
  }
  
  file <- match.arg(file, file.names)
  
  summary <- match.arg(summary)
  
  if(missing(parm.vector.index)){
    parm.vector.index <- rep(-1, length(parm.paths))
  }else{
    if(length(parm.vector.index) != length(parm.paths))
      stop("parm.vector.index should have length equal to parm.paths") 
    if(!is.numeric(parm.vector.index))
      stop("parm.vector.index should be numeric")
  }
  
  ## What this does, is pick the crop.file to be edited when it is not missing
  if(!missing(crop.file)){
    aux.file <- crop.file
    if(missing(xml.parm)){
      cfile <- rep(TRUE, length(parm.paths))  
    }else{
      cfile <- xml.parm
    } 
  }else{
    aux.file <- file
    if(missing(xml.parm)){
      cfile <- rep(FALSE, length(parm.paths))
    }else{
      cfile <- xml.parm
    } 
  }

  if(missing(grid))
    stop("grid argument is missing")
  
  grid <- as.data.frame(grid)
  
  if(ncol(grid) != length(parm.paths))
    stop("Number of columns in grid should be equal to the number of parameters")
  
  ## Check that the name in the grid appears somewhere in the parameter path
  for(i in seq_along(parm.paths)){
    ippgn <- grepl(names(grid)[i], parm.paths[i], ignore.case = TRUE)
    if(!ippgn){
      cat("Name in grid:", names(grid)[i], "\n")
      cat("parameter name", parm.paths[i], "\n")
      warning("names in grid object do not match parameter path name")  
    }
  }
  
  col.sim <- NULL
  start <- Sys.time()
  
  for(i in 1:dim(grid)[1]){
    
    ## Need to edit the parameters in the simulation file or replacement
    for(j in seq_along(parm.paths)){
      ## Edit the specific parameters with the corresponding values
      if(parm.vector.index[j] <= 0){
        par.val <- grid[i, j]  
      }else{
        stop("Submit an issue in github if you need this feature", call. = FALSE)
      }
      
      if(cfile[j]){
        ## Here I'm editing an auxiliary file ending in .xml
        edit_apsim_xml(file = aux.file, 
                       src.dir = src.dir,
                       parm.path = parm.paths[j],
                       overwrite = TRUE,
                       value = as.character(par.val),
                       verbose = FALSE)        
      }else{
        ## Here I'm editing the main simulation file .apsim
        edit_apsim(file = file, 
                   node = "Other",
                   src.dir = src.dir,
                   parm.path = parm.paths[j],
                   overwrite = TRUE,
                   value = as.character(par.val),
                   verbose = FALSE)        
      }
    }
    
    ## Run simulation  
    sim <- try(apsim(file = file, src.dir = src.dir,
                      silent = TRUE, cleanup = TRUE, value = "report"),
               silent = TRUE)
    
    if(inherits(sim, "try-error") && i == 1){
      stop("Simulation failed for initial parameter combination")
    }
    
    if(inherits(sim, "try-error") && i > 1){
      mat <- matrix(ncol = ncol(sim[,col.class.numeric]))
      sim.sd <- as.data.frame(mat)
      names(sim.sd) <- nms.sim
      col.sim <- rbind(col.sim, sim.sd)
      next
    } 
    
    ## Extract basic information from sim
    col.class.numeric <- which(sapply(sim, class) == "numeric") ## Which columns are numeric
    nms.sim <- names(sim[, col.class.numeric]) ## Names of the columns
    
    if(summary == "mean"){
      sim.s <- colMeans(sim[, col.class.numeric], na.rm = TRUE)
      sim.sd <- as.data.frame(t(sim.s))
    }
    
    if(summary == "max"){
      sim.s <- apply(sim[, col.class.numeric], 2, max, na.rm = TRUE)
      sim.sd <- as.data.frame(t(sim.s))
    }
    
    if(summary == "var"){
      sim.s <- apply(sim[, col.class.numeric], 2, var, na.rm = TRUE)
      sim.sd <- as.data.frame(t(sim.s))
    }
    
    if(summary == "sd"){
      sim.s <- apply(sim[, col.class.numeric], 2, sd, na.rm = TRUE)
      sim.sd <- as.data.frame(t(sim.s))
    }
    
    if(summary == "none"){
      sim.sd <- cbind(grid[i, ,drop = FALSE], sim, row.names = NULL)
    }
    
    col.sim <- rbind(col.sim, sim.sd)
    
    if(save != FALSE){
      if(isTRUE(save)){
        save.file.name <- paste0(tools::file_path_sans_ext(file), ".csv")
        utils::write.csv(col.sim, 
                         file = save.file.name,
                         row.names = FALSE)
      }else{
        if(!is.character(save))
          stop("'save' argument should be a character", call. = FALSE)
        if(tools::file_ext(save) != "csv")
          stop("'save' only accepts 'csv' as an extension", call. = FALSE)
        save.file.name <- save
        utils::write.csv(col.sim, 
                         file = save.file.name,
                         row.names = FALSE)
      }
    }
    
    if(verbose){
      nrow.grid <- nrow(grid)
      old.prev.div <- 0
      
      if(nrow.grid <= 10){
        dftm <- difftime(Sys.time(), start)
        cat("Progress:", round((i/nrow.grid) * 100), "%. Time elapsed:", dftm, units(dftm)," \n")  
      }else{
        progress.step <- ifelse(nrow.grid <= 20, 10, 5)
        
        prev.div <- round((i/nrow.grid) * 100) %/% progress.step
        
        if(prev.div > old.prev.div){
          dftm <- difftime(Sys.time(), start)
          cat("Progress:", round((i/nrow(grid)) * 100), "%. Time elapsed:", dftm, units(dftm)," \n")  
          old.prev.div <- prev.div
        } 
      }
    }
  }
  
  if(verbose && !isFALSE(save))
    cat("Saved results as:", save.file.name, "\n")
  
  if(summary != "none"){
    cdat <- cbind(grid, col.sim)  
  }else{
    cdat <- col.sim
  }
  
  attr(cdat, "summary") <- summary
  
  ans <- structure(list(grid.sims = cdat, grid = grid, parm.paths = parm.paths), class = "sens_apsim")
  
  return(ans) 
}


