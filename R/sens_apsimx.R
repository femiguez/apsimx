#'
#' It is possible to provide a list of soil profiles for replacement in the simulations. In this
#' case, the parameter path can be simply \sQuote{soil.profile} or \sQuote{soil_profile} if there 
#' is one single simulation. It can also be the path to \sQuote{Soil}. In this case, the path should
#' be something such as \sQuote{Simulations.SimulationName.Soil}. \sQuote{SimulationName} should
#' be replaced with the appropriate string.
#' 
#' In the grid, the column with name \sQuote{soil.profile} should contain integers that will be used to 
#' pick from the list of provided soil profiles. In this case it is possible to re-use them. 
#' For example, the values could be 1, 2, 3, etc. to select the corresponding soil profiles from 
#' the \sQuote{soil.profiles} list.
#' 
#' If the \sQuote{cores} argument is greater than 1, then the package \CRANpkg{future} is required.
#' It will first search for a future plan under options and if nothing is found it will chose an OS-appropriate plan
#' and it uses the chosen number of cores for execution. Errors, messages and warnings are normally suppressed 
#' during parallel execution, so it is important to ensure that the simulations are constructed properly.
#'
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
#' @param soil.profiles list with soil profiles for replacement (see details.)
#' @param summary function name to use to summarize the output to be a sinlge row (default is the mean).
#' @param root root argument for \code{\link{edit_apsimx_replacement}}
#' @param verbose whether to print progress in percent and elapsed time.
#' @param cores number of cores to use for parallel evaluation
#' @param save whether to save intermediate results. By default they will be saved as a
#' \sQuote{csv} file using the name of the apsim file. This will replace \sQuote{apsimx} with \sQuote{csv}.
#' It is also possible to provide the file name here (for example: \sQuote{Some_results.csv}).
#' @param ... additional arguments.
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
                        soil.profiles,
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

    if(!requireNamespace("future", quietly = TRUE)){
      warning("The 'future' package is required for this option")
      return(NULL)
    }

    if(cores > (nrow(grid) - 1))
       stop("'cores' should be an integer smaller than the number of simulations minus one", call. = FALSE)
    detect.cores <- parallel::detectCores()
    if(cores > detect.cores)
      stop("'cores' argument should not be higher than the number of available cores", call. = FALSE)

    fp.op <- getOption('future.plan')

    ### Maybe multisession needs to be the default regardless of platform
    if(is.null(fp.op)){
      oplan <- future::plan(strategy = "multisession", workers = cores)
      on.exit(oplan)
    }else{
      future::plan(fp.op, workers = cores)
    } 
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

  if(missing(grid))
    stop("grid argument is missing")

  grid <- as.data.frame(grid)

  if(ncol(grid) != length(parm.paths))
      stop("Number of columns in grid should be equal to the number of parameters")

  ## When verbose this is more compact than previous behavior
  pb <- utils::txtProgressBar(min = 1, max = nrow(grid), style = 3)

  ## Check that the name in the grid appears somewhere in the parameter path
  for(.ii in seq_along(parm.paths)){
    is.dot.present <- grepl(".", names(grid)[.ii], fixed = TRUE)
    is.soil <- grepl("soil.profile|Soil$", names(grid)[.ii]) ## This should be of length one
    if(is.dot.present && !is.soil){
      ## The first element should match simulation names
      fspe <- strsplit(names(grid)[.ii], ".", fixed = TRUE)[[1]]
      ## First and second parameter elements
      apsimx_json <- jsonlite::read_json(file.path(src.dir, file))
      simulation.names <- sapply(apsimx_json$Children, FUN = function(x) x$Name)
      ippgn1 <- grepl(fspe[1], parm.paths[.ii], ignore.case = TRUE)
      if(!ippgn1){
        cat("Name in first elelment grid name:", fspe[1], "\n")
        cat("parameter name", parm.paths[.ii], "\n")
        warning("names in first element of grid object name do not match parameter path name")
      }
      ippgn2 <- grepl(fspe[2], parm.paths[.ii], ignore.case = TRUE)
      if(!ippgn2){
        cat("Name in second element grid names:", fspe[2], "\n")
        cat("parameter name", parm.paths[.ii], "\n")
        warning("names in second element of grid object name do not match parameter path name")
      }
    }else{
      if(grepl("soil.profile|Soil$", parm.paths[.ii])){
       ### Check that there is a column in grid that corresponds to soils
        isn <- grepl("soil.profile|Soil$", names(grid)[.ii], ignore.case = TRUE)
        if(isFALSE(isn)){
          cat("Name in grid:", names(grid)[.ii], "\n")
          cat("parameter name", parm.paths[.ii], "\n")
          warning("soil.profile is in the parameter paths but not found in the grid")
        }
      }else{
        ippgn <- grepl(names(grid)[.ii], parm.paths[.ii], ignore.case = TRUE)
        if(!ippgn){
          cat("Name in grid:", names(grid)[.ii], "\n")
          cat("parameter name", parm.paths[.ii], "\n")
          warning("names in grid object do not match parameter path name")
        }        
      }
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
        ## cat("parameter path", parm.paths[.j], "\n")
        if(!grepl("soil.profile|Soil", parm.paths[.j])){
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
        }else{
         if(verbose) cat("Replacing the soil profile in simulation", .i, "\n")
         wspc <- grep("soil.profile|Soil$", names(grid)) ## Which one is the soil profile column
         
         if(length(wspc) == 0){
           cat("Result of wspc:", wspc, "\n")
           stop("'soil.profile' column not found in grid", call. = FALSE)
         }
         
         ## There can be more than one column with soils that need to be replaced
         for(.k in seq_along(wspc)){
           if(grepl("Soil$", names(grid)[wspc[.k]])){
             ## In this case there is a 'root'
             soil.node.name <- parse_root(names(grid)[wspc[.k]])
             if(soil.node.name[1] == "Simulations"){
               root <- soil.node.name[2]
             }else{
               root <- soil.node.name[1]
             }
           }
           soil.profile.index <- grid[.i, wspc[.k]]

           if(missing(root)){
             edit_apsimx_replace_soil_profile(file = file,
                                              src.dir = src.dir,
                                              wrt.dir = src.dir,
                                              soil.profile = soil.profiles[[soil.profile.index]],
                                              overwrite = FALSE,
                                              verbose = FALSE)           
           }else{
             edit_apsimx_replace_soil_profile(file = file,
                                              src.dir = src.dir,
                                              wrt.dir = src.dir,
                                              soil.profile = soil.profiles[[soil.profile.index]],
                                              overwrite = TRUE,
                                              verbose = FALSE,
                                              root = root)
           }
         }
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

    if(original.cores > 1L && .i > 1){
      ## The first simulation will not be parallelized
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
        file.to.run <- file.path(src.dir, paste0(tools::file_path_sans_ext(file),"-", core.counter, ".apsimx"))

        file.copy(from = file.path(src.dir, file), to = file.to.run)

        sim.list[[core.counter]] <- future::future(apsimx(file = paste0(tools::file_path_sans_ext(file), "-", core.counter, ".apsimx"),
                                                                src.dir = src.dir,
                                                                silent = TRUE,
                                                                cleanup = TRUE, value = "report"),
                                                   conditions = character(0))
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

      simc <- future::value(sim.list)
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

    if(isTRUE(verbose)){
      utils::setTxtProgressBar(pb, .i)
    }else{
      if(verbose >= 1L){
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
    }
  } ## End of big for loop

  close(pb)

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

#' The argument \sQuote{formula} can be as in \code{\link{lm}}. The response
#' can be omitted.
#' 
#' @rdname sens_apsimx
#' @description Summary computes variance-based sensitivity indexes from an object of class \sQuote{sens_apsim}
#' @param object object of class \sQuote{sens_apsim}
#' @param ... additional arguments (none used at the moment)
#' @param formula formula to be passed to analysis of variance. See \code{\link{formula}}.
#' @param scale if all inputs are numeric it is better to scale them. The
#' default is FALSE as some inputs might be characters or factors. In this
#' case all inputs will be treated as factors in the sum of squares decomposition.
#' @param select option for selecting specific variables in the APSIM output. It will be treated as a regular expression
#' @param warning whether to issue a warning when applying this function to an object which has not been summarized
#' @param verbose whether to print to console results of summary
#' @return prints to console if verbose and returns a data frame
#' @export
#'
summary.sens_apsim <- function(object, ..., formula, scale = FALSE, select = "all", warning = TRUE, verbose = TRUE){

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

  if(any(select == "all")){
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
  wrn <- NULL

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

    if(missing(formula) || formula == 1L){
      frml <- paste("y ~", paste(names(X), collapse = "+"))  
    }else{
      if(is.numeric(formula)){
        frml0 <- paste(names(X), collapse = "+")
        if(formula == 2L){
          frml <- paste("y ~ (", paste(names(X), collapse = "+"), ")^2")  
        }
        if(formula == 3L){
          frml <- paste("y ~ (", paste(names(X), collapse = "+"), ")^3")  
        }
        if(formula == 4L){
          frml <- paste("y ~ (", paste(names(X), collapse = "+"), ")^4")  
        }
        if(formula != 2L && formula != 3L && formula != 4L)
          stop("When 'formula' is a number it needs to be either 2, 3 or 4", call. = FALSE)
      }else{
        if(attr(stats::terms(formula), "response") == 0){
          tt <- stats::terms(formula)
          frml <- stats::reformulate(attr(tt, "term.labels"), response = "y")
        }else{
          frml <- formula                  
        }
      }
    }
    
    fit <- stats::lm(formula = frml, data = dat, na.action = "na.omit")
    if(inherits(fit, "try-error")) next
    w <- fit$weights
    ssr <- sum(if (is.null(w)) fit$residuals^2 else w * fit$residuals^2)
    mss <- sum(if (is.null(w)) fit$fitted.values^2 else w * fit$fitted.values^2)
    if(ssr < 1e-10 * mss){
      cat("Variable:", nms.resp.var[.i], "\n")
      warning("ANOVA F-tests on an essentially perfect fit are unreliable", 
              immediate. = TRUE)
    } 
    sfit <- as.matrix(stats::anova(fit, ...))
    wrn <- warnings()
    kable.caption <- paste("Variable:", nms.resp.var[.i])
    pmat <- matrix(ncol = 2, nrow = length(labels(fit)) + 1)
    row.names(pmat) <- row.names(sfit)
    pmat[,1] <- sfit[,2]
    pmat[,2] <- sfit[,2] / sum(sfit[,2]) * 100
    colnames(pmat) <- c("SS", "SI (%)")
    pmatd <- as.data.frame(pmat)
    pmatd <- pmatd[order(pmatd$SS, decreasing = TRUE),]
    if(verbose){
      print(knitr::kable(pmatd, caption = kable.caption, digits = 0))
      cat("\n")      
    }

    ansi <- data.frame(input = row.names(pmatd), SI = round(pmatd[, "SI (%)"], 1))
    names(ansi) <- c("input", paste(nms.resp.var[.i], "SI (%)"))  
    if(.j == 0){
      ans <- ansi
    }else{
      ans <- merge(ans, ansi)
    }

    .j <- .j + 1
  }
  if(.j == 0) return("No variables reported. Are they all constant?")
  
  ## Need to reorder (on average)
  rmns <- rowMeans(ans[, -1, drop = FALSE])
  ans <- ans[order(rmns, decreasing = TRUE), ]
  invisible(ans)
}

#' @rdname sens_apsimx
#' @description Print method for an object of class \sQuote{sens_apsim}
#' @param x object of class \sQuote{sens_apsim}
#' @param ... additional arguments (none used at the moment)
#' @param variables whether to print APSIM output variables (default is FALSE)
#' @param summary whether to print the full summary of the grid simulations (default is FALSE)
#' @return compact printing
#' @export
#'
print.sens_apsim <- function(x, ..., variables = FALSE, summary = FALSE){
  
  ### Print parameters in grid
  cat("Grid dimensions (rows columns):", dim(x$grid), "\n")
  ### Grid names
  cat("Grid names:", names(x$grid), "\n")
  ### Print APSIM output variables
  output.variables <- setdiff(names(x$grid.sims), names(x$grid))
  cat("Number of output variables:", length(output.variables), "\n")
  ### Summary function
  sfun <- attr(x$grid.sims, "summary")
  cat("Applied summary function:", sfun, "\n")
  cat("Objects:", names(x), "\n")
  if(variables) print(output.variables)
  ### Print data.frame summary
  if(summary) print(summary(x$grid.sims))
  
}