#' Suggested reading on the topic of sensitivity analysis:
#' 
#' Pianosa et al (2016). Sensitivity analysis of environmental models: A systematic review with practical workflow.
#' \doi{10.1016/j.envsoft.2016.02.008}
#' 
#' Saltelli et al. . Global Sensitivity Analysis.
#' 
#' @title Sensitivity Analysis for APSIM Next Generation simulation
#' @name sens_apsimx
#' @rdname sens_apsimx
#' @description It is a wrapper for running APSIM-X and evaluating different parameters values
#' @param file file name to be run (the extension .apsimx is optional)
#' @param src.dir directory containing the .apsimx file to be run (defaults to the current directory)
#' @param parm.paths absolute or relative paths of the coefficients to be evaluated. 
#'             It is recommended that you use \code{\link{inspect_apsimx}} for this
#' @param convert (logical) This argument is needed if there is a need to pass a vector instead of a single value.
#' The vector can be passed as a character string (separated by spaces) and it will be converted to a 
#' numeric vector. It should be either TRUE or FALSE for each parameter.
#' @param replacement TRUE or FALSE for each parameter. Indicating whether it is part of 
#' the \sQuote{replacement} component. Its length should be equal to the length or \sQuote{parm.paths}.
#' @param grid grid of parameter values for the evaluation. It can be a data.frame.
#' @param summary function name to use to summarize the output to be a sinlge row (default is the mean).
#' @param root root argument for \code{\link{edit_apsimx_replacement}}
#' @param verbose whether to print progress in percent and elapsed time.
#' @param cores number of cores to use for parallel evaluation
#' @param save whether to save intermediate results. By default they will be saved as a
#' \sQuote{csv} file using the name of the apsim file. This will replace \sQuote{apsimx} with \sQuote{csv}.
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

sens_apsimx <- function(file, src.dir = ".", 
                        parm.paths,
                        convert,
                        replacement,
                        grid,
                        summary = c("mean", "max", "var", "sd", "none"),
                        root,
                        verbose = TRUE,
                        cores = 1L,
                        save = FALSE,
                        ...){
  
  if(missing(file))
    stop("file is missing with no default", call. = FALSE)
  
  .check_apsim_name(file)
  .check_apsim_name(src.dir)
  
  if(cores > 1L){
    
    if(.Platform$OS.type != "unix")
      stop("This is only available in 'unix' OSs at the moment", call. = FALSE)
    
    if(cores > (nrow(grid) - 1))
       stop("'cores' should be an integer smaller than the number of simulations minus one", call. = FALSE)
    detect.cores <- parallel::detectCores()  
    if(cores > detect.cores)
      stop("'cores' argument should not be higher than the number of available cores", call. = FALSE)
  }
  
  ## This might offer suggestions in case there is a typo in 'file'
  file.names <- dir(path = src.dir, pattern = ".apsimx$", ignore.case = TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsimx files in the specified directory to run.", call. = FALSE)
  }
  
  file <- match.arg(file, file.names)
  
  summary <- match.arg(summary)
  
  if(missing(convert)){
    convert <- rep(FALSE, length(parm.paths))
  }else{
    if(length(convert) != length(parm.paths))
      stop("convert should have length equal to parm.paths", call. = FALSE) 
    if(!is.logical(convert))
      stop("convert should be logical", call. = FALSE)
  }

  if(missing(replacement)) replacement <- rep(FALSE, length(parm.paths))
  
  ## If root is not present. Need to think more about this...
  # if(is.null(root) && all(replacement)){
  #   root <- list("Models.Core.Replacements", NA)
  # }else{
  #   root <- NULL
  # }
  
  if(missing(grid))
    stop("grid argument is missing")
  
  grid <- as.data.frame(grid)
  
  if(ncol(grid) != length(parm.paths))
      stop("Number of columns in grid should be equal to the number of parameters")
  
  ## Check that the name in the grid appears somewhere in the parameter path
  for(.i in seq_along(parm.paths)){
    ippgn <- grepl(names(grid)[.i], parm.paths[.i], ignore.case = TRUE)
    if(!ippgn){
      cat("Name in grid:", names(grid)[.i], "\n")
      cat("parameter name", parm.paths[.i], "\n")
      warning("names in grid object do not match parameter path name")  
    }
  }

  col.sim <- NULL
  start <- Sys.time()
  
  original.cores <- cores
  cores.last.round <- 0
  core.counter <- 1L
  sim.list <- vector("list", length = cores)
  
  for(.i in 1:dim(grid)[1]){
    
    ## Need to edit the parameters in the simulation file or replacement
    for(.j in seq_along(parm.paths)){
      ## Edit the specific parameters with the corresponding values
      if(convert[.j] <= 0){
        par.val <- grid[.i, .j]  
      }else{
        ## Converting from character to numeric. Values need to be separated by spaces.
        par.val <- as.numeric(strsplit(grid[.i, .j], " ")[[1]])  
      }
      
      if(replacement[.j]){
        pp0 <- strsplit(parm.paths[.j], ".", fixed = TRUE)[[1]]
        mpp <- paste0(pp0[-length(pp0)], collapse = ".")
        if(missing(root)){
          edit_apsimx_replacement(file = file, 
                                  src.dir = src.dir,
                                  wrt.dir = src.dir,
                                  node.string = mpp,
                                  overwrite = TRUE,
                                  parm = pp0[length(pp0)],
                                  value = par.val,
                                  verbose = FALSE) 
        }else{
          edit_apsimx_replacement(file = file, 
                                  src.dir = src.dir,
                                  wrt.dir = src.dir,
                                  node.string = mpp,
                                  overwrite = TRUE,
                                  parm = pp0[length(pp0)],
                                  value = par.val,
                                  root = root,
                                  verbose = FALSE)           
        }
      }else{
        if(missing(root)){
          edit_apsimx(file = file, 
                      src.dir = src.dir,
                      wrt.dir = src.dir,
                      node = "Other",
                      parm.path = parm.paths[.j],
                      overwrite = TRUE,
                      value = par.val,
                      verbose = FALSE)           
        }else{
          edit_apsimx(file = file, 
                      src.dir = src.dir,
                      wrt.dir = src.dir,
                      node = "Other",
                      parm.path = parm.paths[.j],
                      overwrite = TRUE,
                      value = par.val,
                      root = root,
                      verbose = FALSE)           
        }
      }
    }
    
    ## Run simulation  
    ## The first simulation should not be parallelized
    if(cores == 1L || .i == 1){
      sim <- try(apsimx(file = file, src.dir = src.dir,
                        silent = TRUE, cleanup = TRUE, value = "report"),
                 silent = TRUE)  
      
      if(inherits(sim, "try-error") && .i == 1){
        stop("Simulation failed for initial parameter combination")
      }
      
      ## Extract basic information from sim
      if(.i == 1){
        col.class.numeric <- which(sapply(sim, class) == "numeric") ## Which columns are numeric
        nms.sim <- names(sim[, col.class.numeric, drop = FALSE]) ## Names of the columns
        ncol.class.numeric <- ncol(sim[, col.class.numeric, drop = FALSE]) 
      }
      
      if(inherits(sim, "try-error") && .i > 1){
        ## This will skip to the next iteration
        ## If summary is not equal to none, then we need just one row
        ## If summary is equal to none we fill it with an empty data.frame
        ## with number of rows equal to the number in sim
        if(summary != "none"){
          mat <- matrix(ncol = ncol.class.numeric)  
        }else{
          mat <- matrix(nrow = nrow(sim), ncol = ncol.class.numeric)  
        }
        sim.sd <- as.data.frame(mat)
        names(sim.sd) <- nms.sim
        col.sim <- rbind(col.sim, sim.sd)
        next
      } 
      sim.sd <- sens_summary(sim, summary = summary, grid = grid, i.index = .i, col.class.numeric = col.class.numeric)
    }
    
    ## First trying this as a proof-of-concept
    if(cores == 2L && .i > 1 && original.cores == 2L){
      ## If the number of simulations fit well within the 
      ## number of cores
      if(verbose > 1) cat("Iteration:", .i, "\n")
      num.sim.rem <- (dim(grid)[1] - 1) %% 2
      ## The first simulation will not be parallelized
      if(.Platform$OS.type == "unix"){
        if(core.counter == 1L){
          ## Maybe I should edit the file and then delete it 
          ## to prevent conflicts with the file below
          edit_apsimx(file = file,
                      src.dir = src.dir,
                      wrt.dir = src.dir,
                      node = "Soil",
                      parm = "RecordNumber",
                      value = 1,
                      edit.tag = "-1",
                      verbose = FALSE)
          sim1 <- parallel::mcparallel(apsimx(file = paste0(tools::file_path_sans_ext(file), "-1.apsimx"), 
                                                  src.dir = src.dir,
                                                  silent = TRUE, 
                                                  cleanup = TRUE, value = "report"))
          if(verbose > 1) cat("Started simulation on first core \n")
          core.counter <- core.counter + 1
          if(verbose > 1) cat("Remaining simulations:", dim(grid)[1] - .i, "\n")
          if(num.sim.rem == 0){
            next
          }else{
            if(dim(grid)[1] - .i > 0) next            
          }
        }else{
          if(verbose > 1) cat("Started simulation on second core \n")
          sim2 <- parallel::mcparallel(apsimx(file = file, src.dir = src.dir,
                                                  silent = TRUE, cleanup = TRUE, value = "report"))
          core.counter <- 1L
        }
        if(verbose > 1) cat("About to merge simulations from the two cores \n")
        if(num.sim.rem == 0){
          simc <- parallel::mccollect(list(sim1, sim2), wait = TRUE)  
        }else{
          if(dim(grid)[1] - .i > 0){
            simc <- parallel::mccollect(list(sim1, sim2), wait = TRUE)  
          }else{
            simc <- parallel::mccollect(sim1)  
          }          
        }

        if(verbose > 1){
          cat("Class of object returned by mccollect:", class(simc), "\n")
          cat("Class of object 1 returned by mccollect:", class(simc[[1]]), "\n")
          if(dim(grid)[1] - .i > 0) cat("Class of object 2 returned by mccollect:", class(simc[[2]]), "\n")
          cat("Completed simulation from both cores \n")
          cat("Iteration:", .i, "\n")          
        }
        ## If both simulations fail then 'next'?
        if(num.sim.rem == 0){
            if(inherits(simc[[1]], "try-error") && inherits(simc[[2]], "try-error")){
              if(verbose > 1) cat("Both simulations failed \n")
              if(summary != "none"){
                mat <- matrix(ncol = ncol.class.numeric)  
              }else{
                mat <- matrix(nrow = nrow(sim), ncol = ncol.class.numeric)  
              }
              sim.sd <- as.data.frame(mat)
              names(sim.sd) <- nms.sim
              sim.sd <- rbind(sim.sd, sim.sd) ## This duplicates the empty data.frame result
              col.sim <- rbind(col.sim, sim.sd)
              next
            }
            ## If only the first simulation fails
            if(inherits(simc[[1]], "try-error") && !inherits(simc[[2]], "try-error")){
              if(verbose > 1){
                cat("Error type:", simc[[1]], "\n")
                cat("First simulation failed \n")
              }
              if(summary != "none"){
                mat <- matrix(ncol = ncol.class.numeric)  
              }else{
                mat <- matrix(nrow = nrow(sim), ncol = ncol.class.numeric)  
              }
              sim.sd1 <- as.data.frame(mat)
              names(sim.sd1) <- nms.sim
              sim.sd2 <- sens_summary(simc[[2]], summary = summary, grid = grid, i.index = .i, col.class.numeric = col.class.numeric)
              sim.sd <- rbind(sim.sd1, sim.sd2) 
            }
            ## If only the second simulation fails
            if(!inherits(simc[[1]], "try-error") && inherits(simc[[2]], "try-error")){
              if(verbose > 1){
                cat("Error type:", simc[[2]], "\n")
                cat("Second simulation failed \n")
              }
              if(summary != "none"){
                mat <- matrix(ncol = ncol.class.numeric)  
              }else{
                mat <- matrix(nrow = nrow(sim), ncol = ncol.class.numeric)  
              }
              sim.sd2 <- as.data.frame(mat)
              names(sim.sd2) <- nms.sim
              sim.sd1 <- sens_summary(simc[[1]], summary = summary, grid = grid, i.index = .i, col.class.numeric = col.class.numeric)
              sim.sd <- rbind(sim.sd1, sim.sd2) 
            }
            ## If both simulations are successful
            if(!inherits(simc[[1]], "try-error") && !inherits(simc[[2]], "try-error")){
              ## Before they are merged both should be data.frames
              sim.sd1 <- sens_summary(simc[[1]], summary = summary, grid = grid, i.index = .i, col.class.numeric = col.class.numeric)
              sim.sd2 <- sens_summary(simc[[2]], summary = summary, grid = grid, i.index = .i, col.class.numeric = col.class.numeric)
              sim.sd <- do.call(rbind, list(sim.sd1, sim.sd2))
            }         
        }else{
          if(dim(grid)[1] - .i > 0){
            if(inherits(simc[[1]], "try-error") && inherits(simc[[2]], "try-error")){
              if(verbose > 1) cat("Both simulations failed \n")
              if(summary != "none"){
                mat <- matrix(ncol = ncol.class.numeric)  
              }else{
                mat <- matrix(nrow = nrow(sim), ncol = ncol.class.numeric)  
              }
              sim.sd <- as.data.frame(mat)
              names(sim.sd) <- nms.sim
              sim.sd <- rbind(sim.sd, sim.sd) ## This duplicates the empty data.frame result
              col.sim <- rbind(col.sim, sim.sd)
              next
            }
            ## If only the first simulation fails
            if(inherits(simc[[1]], "try-error") && !inherits(simc[[2]], "try-error")){
              if(verbose > 1){
                cat("Error type:", simc[[1]], "\n")
                cat("First simulation failed \n")
              }
              if(summary != "none"){
                mat <- matrix(ncol = ncol.class.numeric)  
              }else{
                mat <- matrix(nrow = nrow(sim), ncol = ncol.class.numeric)  
              }
              sim.sd1 <- as.data.frame(mat)
              names(sim.sd1) <- nms.sim
              sim.sd2 <- sens_summary(simc[[2]], summary = summary, grid = grid, i.index = .i, col.class.numeric = col.class.numeric)
              sim.sd <- rbind(sim.sd1, sim.sd2) 
            }
            ## If only the second simulation fails
            if(!inherits(simc[[1]], "try-error") && inherits(simc[[2]], "try-error")){
              if(verbose > 1){
                cat("Error type:", simc[[2]], "\n")
                cat("Second simulation failed \n")
              }
              if(summary != "none"){
                mat <- matrix(ncol = ncol.class.numeric)  
              }else{
                mat <- matrix(nrow = nrow(sim), ncol = ncol.class.numeric)  
              }
              sim.sd2 <- as.data.frame(mat)
              names(sim.sd2) <- nms.sim
              sim.sd1 <- sens_summary(simc[[1]], summary = summary, grid = grid, i.index = .i, col.class.numeric = col.class.numeric)
              sim.sd <- rbind(sim.sd1, sim.sd2) 
            }
            ## If both simulations are successful
            if(!inherits(simc[[1]], "try-error") && !inherits(simc[[2]], "try-error")){
              ## Before they are merged both should be data.frames
              sim.sd1 <- sens_summary(simc[[1]], summary = summary, grid = grid, i.index = .i, col.class.numeric = col.class.numeric)
              sim.sd2 <- sens_summary(simc[[2]], summary = summary, grid = grid, i.index = .i, col.class.numeric = col.class.numeric)
              sim.sd <- do.call(rbind, list(sim.sd1, sim.sd2))
            }         
          }else{
            if(inherits(simc, "try-error")){
              if(summary != "none"){
                mat <- matrix(ncol = ncol.class.numeric)  
              }else{
                mat <- matrix(nrow = nrow(sim), ncol = ncol.class.numeric)  
              }
              sim.sd <- as.data.frame(mat)
              names(sim.sd) <- nms.sim
            }else{
              sim.sd <- sens_summary(simc[[1]], summary = summary, grid = grid, i.index = .i, col.class.numeric = col.class.numeric)
            }
          }          
        }

        file.remove(file.path(src.dir, paste0(tools::file_path_sans_ext(file), "-1.apsimx")))
      }
    }
    
    if(original.cores > 2L && .i > 1){
      ## The first simulation will not be parallelized
      if(.Platform$OS.type == "unix"){
        ## How many simulations are remaining?
        ## If the number of simulations remaining are less than the number of cores change a few things
        num.sim.rem <- dim(grid)[1] - .i

        if(verbose > 1){
          cat("Iteration:", .i, "\n")
          cat("Number of simulations remaining:", num.sim.rem, "\n")          
        }

        while(core.counter <= cores){
          ## Maybe I should edit the file and then delete it 
          ## to prevent conflicts with the file below
          edit_apsimx(file = file,
                      src.dir = src.dir,
                      wrt.dir = src.dir,
                      node = "Soil",
                      parm = "RecordNumber",
                      value = core.counter,
                      edit.tag = paste0("-", core.counter),
                      verbose = FALSE)
          
          sim.list[[core.counter]] <- parallel::mcparallel(apsimx(file = paste0(tools::file_path_sans_ext(file), "-", core.counter, ".apsimx"), 
                                                                  src.dir = src.dir,
                                                                  silent = TRUE, 
                                                                  cleanup = TRUE, value = "report"))
          break           
        }
        if(core.counter < cores){
          if(verbose > 1) cat("Core counter :", core.counter, "\n")
          core.counter <- core.counter + 1
          next
        } 
        
        if(core.counter >= cores){
          if(verbose > 1) cat("Core counter :", core.counter, "\n")
          core.counter <- core.counter + 1
        } 
        
        if(verbose > 1){
          print(sapply(sim.list, class))
          
          cat("Length sim.list:", length(sim.list), "\n")
          cat("Number of simulations remaining:", num.sim.rem, "\n")          
        }

        simc <- parallel::mccollect(sim.list, wait = TRUE)
        core.counter <- 1L
        
        if(num.sim.rem < cores && cores.last.round < 1){
          cores <- num.sim.rem
          sim.list <- vector("list", length = cores)
          cores.last.round <- 2
        }

        ## Identify which simulations failed
        sim.class <- sapply(simc, class)
        
        if(any(sim.class == "try-error")){
          if(verbose > 1) cat("Simulations which failed:", which(sim.class == "try-error"), "\n")
          ## How can I handle it if some simulations failed?
          sims.lst <- vector("list", length = length(simc))
          for(j in seq_len(length(simc))){
            if(inherits(simc[[j]], "try-error")){
              if(summary != "none"){
                mat <- matrix(ncol = ncol.class.numeric)  
              }else{
                mat <- matrix(nrow = nrow(sim), ncol = ncol.class.numeric)  
              }
              sim.sd.na <- as.data.frame(mat)
              names(sim.sd.na) <- nms.sim
              sims.lst[[j]] <- sim.sd.na
            }else{
              sims.lst[[j]] <- sens_summary(simc[[j]], summary = summary, grid = grid, i.index = .i, col.class.numeric = col.class.numeric)  
            }
          }
          sim.sd <- do.call(rbind, sims.lst)
        }

        ## If all simulations are successful
        if(all(sim.class != "try-error")){
          ## Before they are merged both should be data.frames
          sims.lst <- vector("list", length = length(simc))
          for(j in seq_len(length(simc))){
            sims.lst[[j]] <- sens_summary(simc[[j]], summary = summary, grid = grid, i.index = .i, col.class.numeric = col.class.numeric)
          }
          sim.sd <- do.call(rbind, sims.lst)
        }

        for(j in seq_len(length(simc))){
          file.remove(file.path(src.dir, paste0(tools::file_path_sans_ext(file), "-", j, ".apsimx")))  
        }
      }
    }

    ## This combines the simulations regardless of cores argument
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
        cat("Progress:", round((.i/nrow.grid) * 100), "%. Time elapsed:", round(dftm, 2), units(dftm)," \n")  
      }else{
        progress.step <- ifelse(nrow.grid <= 20, 10, 5)
        
        prev.div <- round((.i/nrow.grid) * 100) %/% progress.step
        
        if(prev.div > old.prev.div){
          dftm <- difftime(Sys.time(), start)
          cat("Progress:", round((.i/nrow(grid)) * 100), "%. Time elapsed:", round(dftm, 2), units(dftm)," \n")  
          old.prev.div <- prev.div
        } 
      }
    } ## End of verbose chunk
  } ## End of big for loop
  
  if(verbose && !isFALSE(save))
    cat("Saved results as:", save.file.name, "\n")
  
  if(summary != "none"){
    cdat <- cbind(grid, col.sim)  
  }else{
    cdat <- col.sim
  }
  
  attr(cdat, "summary") <- summary
  
  ans <- structure(list(grid.sims = cdat, grid = grid, parm.paths = parm.paths), 
                   class = "sens_apsim")
 
  return(ans) 
}

## Function to apply summary 
sens_summary <- function(sim, summary = c("mean", "max", "var", "sd", "none"),
                         grid, i.index,
                         col.class.numeric){
  
  summary <- match.arg(summary)
  
  if(summary == "mean"){
    sim.s <- colMeans(sim[, col.class.numeric, drop = FALSE], na.rm = TRUE)
    sim.sd <- as.data.frame(t(sim.s))
  }
  
  if(summary == "max"){
    sim.s <- apply(sim[, col.class.numeric, drop = FALSE], 2, max, na.rm = TRUE)
    sim.sd <- as.data.frame(t(sim.s))
  }
  
  if(summary == "var"){
    sim.s <- apply(sim[, col.class.numeric, drop = FALSE], 2, var, na.rm = TRUE)
    sim.sd <- as.data.frame(t(sim.s))
  }
  
  if(summary == "sd"){
    sim.s <- apply(sim[, col.class.numeric, drop = FALSE], 2, sd, na.rm = TRUE)
    sim.sd <- as.data.frame(t(sim.s))
  }
  
  if(summary == "none"){
    sim.sd <- cbind(grid[i.index, , drop = FALSE], sim, row.names = NULL)
  }
  
  return(sim.sd)
}

#' @rdname sens_apsimx
#' @description Summary computes variance-based sensitivity indexes from an object of class \sQuote{sens_apsim}
#' @param object object of class \sQuote{sens_apsim}
#' @param ... additional arguments (none used at the moment)
#' @param scale if all inputs are numeric it is better to scale them. The
#' default is FALSE as some inputs might be characters or factors. In this
#' case all inputs will be treated as factors in the sum of squares decomposition.
#' @param select option for selecting specific variables in the APSIM output. It will be treated as a regular expression
#' @param warning whether to issue a warning when applying this function to an object which has not been summarized
#' @return prints to console
#' @export
#' 
summary.sens_apsim <- function(object, ..., scale = FALSE, select = "all", warning = TRUE){
  
  ## It probably does not make sense to compute this summary if the data were 
  ## not previously summarized
  smmry <- attr(object$grid.sims, "summary")
  if(smmry == "none" && warning)
    warning("It is unlikey that running 'summary' will be useful on a 
    sensitivity analysis output which was not previously summarized.")
  ## Here I compute sensitivity indexes based on the grid.sims object
  ## There are potentially many variables for which sensitivity analysis is relevant
  nms.resp.var <- setdiff(names(object$grid.sims), names(object$grid))
  
  l.nms.resp.var <- length(nms.resp.var)
  .j <- 0
  
  if(select == "all"){
    select <- nms.resp.var
  }else{
    gsel.col <- NULL
    for(i in seq_along(select)){
      gsel <- grep(select[i], nms.resp.var)
      if(length(gsel) == 0){
        cat("selection:", select[i], "\n")  
        stop("not present in simulation object", call. = FALSE)
      }
      gsel.col <- c(gsel.col, gsel)     
    }
    select <- nms.resp.var[unique(gsel.col)]
  } 

  object$grid.sims <- subset(object$grid.sims, select = select)
    
  num.resp.var <- ncol(object$grid.sims) - ncol(object$grid)
  nms.resp.var <- setdiff(names(object$grid.sims), names(object$grid))
  
  for(.i in seq_along(nms.resp.var)){
    X <- object$grid
    y <- object$grid.sims[,nms.resp.var[.i]]
    if(suppressWarnings(var(y, na.rm = TRUE) == 0) || is.character(y[1]) || !is.numeric(y)) next
    
    if(scale){
      if(any(sapply(object$grid, function(x) is.character(x) || is.factor(x))))
        stop("Scale can only be applied to numeric inputs", call. = FALSE)
      dat <- data.frame(y = y, scale(X))
    }else{
      dat <- data.frame(y, as.data.frame(sapply(X, function(x) as.factor(as.character(x)))))  
    }
    
    frml <- paste("y ~", paste(names(X), collapse = "+"))
    fit <- stats::lm(formula = frml, data = dat, na.action = "na.omit")
    if(inherits(fit, "try-error")) next
    sfit <- as.matrix(stats::anova(fit))
    cat("Variable:", nms.resp.var[.i], "\n")
    pmat <- matrix(ncol = 2, nrow = ncol(X) + 1)
    row.names(pmat) <- row.names(sfit)
    pmat[,1] <- sfit[,2] 
    pmat[,2] <- sfit[,2] / sum(sfit[,2]) * 100
    colnames(pmat) <- c("SS", "SI (%)")
    pmatd <- as.data.frame(pmat)
    pmatd <- pmatd[order(pmatd$SS, decreasing = TRUE),]
    print(knitr::kable(pmatd, digits = 0))
    cat("\n")
    .j <- .j + 1
  }
  if(.j == 0) return("No variables reported. Are they all constant?")
}
