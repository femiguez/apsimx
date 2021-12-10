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
                        ...){
  
  if(missing(file))
    stop("file is missing with no default", call. = FALSE)
  
  .check_apsim_name(file)
  .check_apsim_name(src.dir)
  
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
  if(missing(root)) root <- list("Models.Core.Replacements", NA)
  
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
        edit_apsimx_replacement(file = file, 
                                src.dir = src.dir,
                                wrt.dir = src.dir,
                                node.string = mpp,
                                overwrite = TRUE,
                                parm = pp0[length(pp0)],
                                value = par.val,
                                root = root,
                                verbose = FALSE) 
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
    sim <- try(apsimx(file = file, src.dir = src.dir,
                      silent = TRUE, cleanup = TRUE, value = "report"),
               silent = TRUE)
    
    if(inherits(sim, "try-error") && .i == 1){
      stop("Simulation failed for initial parameter combination")
    }
    
    ## Extract basic information from sim
    if(.i == 1){
      col.class.numeric <- which(sapply(sim, class) == "numeric") ## Which columns are numeric
      nms.sim <- names(sim[, col.class.numeric]) ## Names of the columns
      ncol.class.numeric <- ncol(sim[,col.class.numeric]) 
    }

    if(inherits(sim, "try-error") && .i > 1){
      mat <- matrix(ncol = ncol.class.numeric)
      sim.sd <- as.data.frame(mat)
      names(sim.sd) <- nms.sim
      col.sim <- rbind(col.sim, sim.sd)
      next
    } 
    
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
      sim.sd <- cbind(grid[.i, , drop = FALSE], sim, row.names = NULL)
    }
    
    col.sim <- rbind(col.sim, sim.sd)
    
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
    }

  }
  
  if(summary != "none"){
    cdat <- cbind(grid, col.sim)  
  }else{
    cdat <- col.sim
  }
  
  attr(cdat, "summary") <- summary
  
  ans <- structure(list(grid.sims = cdat, grid = grid, parm.paths = parm.paths), class = "sens_apsim")
 
  return(ans) 
}


#' @rdname sens_apsimx
#' @description Summary computes variance-based sensitivity indexes from an object of class \sQuote{sens_apsim}
#' @param object object of class \sQuote{sens_apsim}
#' @param ... additional arguments (none used at the moment)
#' @param scale if all inputs are numeric it is better to scale them. The
#' default is FALSE as some inputs might be characters or factors. In this
#' case all inputs will be treated as factors in the sum of squares decomposition.
#' @param select option for selecting specific variables in the APSIM output. It will be treated as a regular expression
#' @return prints to console
#' @export
#' 
summary.sens_apsim <- function(object, ..., scale = FALSE, select = "all"){
  
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
