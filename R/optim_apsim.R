#'
#' Simple optimization for APSIM Classic
#' 
#' * This function assumes that you want to optimize parameters which are stored in
#' an auxiliary XML file. These are typically crop or cultivar specific parameters.
#' However, it is possible to optimize parameters present in the main simulation
#' file.
#' 
#' * Only one observation per day is allowed in the data.
#' 
#' * Given how APSIM Classic works, this can only be run when the main simulation
#' file is in the current directory and the crop file (or XML) 
#' should be in the same directory as the main simulation.
#' 
#' * The initial values for the optimization should be the ones in the stored
#' crop parameter file.
#' 
#' * It is suggested that you keep a backup of the original file. This function
#' will edit and overwrite the file during the optimization. 
#' 
#' * When you use the parm.vector.index you cannot edit two separate elements of
#' a vector at the same time. This should be used to target a single element of 
#' a vector only.
#' 
#' @title Optimize parameters in an APSIM simulation
#' @name optim_apsim
#' @rdname optim_apsim
#' @description It is a wrapper for running APSIM and optimizing parameters using \code{\link[stats]{optim}}
#' @param file file name to be run (the extension .apsim is optional)
#' @param src.dir directory containing the .apsim file to be run (defaults to the current directory)
#' @param crop.file name of auxiliary xml file where parameters are stored. If this is missing, it is 
#'                  assumed that the parameters to be edited are in the main simulation file.
#' @param parm.paths absolute paths of the coefficients to be optimized. 
#'             It is recommended that you use \code{\link{inspect_apsim}} or \code{\link{inspect_apsim_xml}}  for this.
#' @param data data frame with the observed data. By default is assumes there is a 'Date' column for the index.
#' @param type Type of optimization. For now, \code{\link[stats]{optim}} and, if available, \code{\link[nloptr]{nloptr}} or \sQuote{mcmc} through \code{\link[BayesianTools]{runMCMC}}.
#' @param weights Weighting method or values for computing the residual sum of squares (see Note). 
#' @param index Index for filtering APSIM output. \sQuote{Date} is currently used. (I have not tested how well it works using anything other than Date).
#' @param parm.vector.index Index to optimize a specific element of a parameter vector. At the moment it is
#' possible to only edit one element at a time. This is because there is a conflict when generating multiple
#' elements in the candidate vector for the same parameter.
#' @param ... additional arguments to be passed to the optimization algorithm. If you want confidence intervals, then include \sQuote{hessian = TRUE}.
#' @note When computing the objective function (residual sum-of-squares) different variables are combined.
#' It is common to weight them since they are in different units. If the argument weights is not supplied
#' no weighting is applied. It can be \sQuote{mean}, \sQuote{var} or a numeric vector of appropriate length.
#' @return object of class \sQuote{optim_apsim}, but really just a list with results from optim and additional information.
#' @export
#' 

optim_apsim <- function(file, src.dir = ".", 
                        crop.file, parm.paths, data, 
                        type = c("optim", "nloptr","mcmc"), 
                        weights, index = "Date",
                        parm.vector.index,
                        ...){
  .check_apsim_name(file)
  
  if(src.dir != ".") stop("At the moment it is not possible \n
                          to change the source directory.")

  ## This might offer suggestions in case there is a typo in 'file'
  file.names <- dir(path = src.dir, pattern = ".apsim$", ignore.case = TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsim files in the specified directory to run.")
  }
  
  file <- match.arg(file, file.names)
  
  file.name.path <- file.path(src.dir, file)
  
  ## optimization type
  type <- match.arg(type)
  
  if(type == "nloptr"){
    if(!requireNamespace("nloptr", quietly = TRUE)){
      warning("The nloptr package is required for this optimization method")
      return(NULL)
    }
  }
  
  if(type == "mcmc"){
    if(!requireNamespace("BayesianTools", quietly = TRUE)){
      warning("The BayesianTools package is required for this method.")
      return(NULL)
    }
  }
  
  datami <- data[,-which(names(data) == index), drop = FALSE]
  if(index == "Date") data$Date <- as.Date(data$Date)
  
  ## Setting up weights
  if(missing(weights)){
    weights <- rep(1, ncol(datami))
  }else{
    if(weights == "mean"){
      vmns <- abs(1 / apply(datami, 2, mean))  
      weights <- (vmns / sum(vmns)) * ncol(datami)
    }else{
      if(weights == "var"){
        vvrs <- 1 / apply(datami, 2, var)    
        weights <- (vvrs / sum(vvrs)) * ncol(datami)
      }else{
        if(length(weights) != ncol(datami))
          stop("Weights not of correct length")
        if(!is.numeric(weights))
          stop("Weights should be numeric")
      } 
    } 
  }
  
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
    cfile <- TRUE
  }else{
    aux.file <- file
    cfile <- FALSE
  }
  
  ## Retrieve initial value vectors
  aux.xml <- xml2::read_xml(file.path(src.dir, aux.file))
  iaux.parms <- vector("list", length = length(parm.paths))
  names(iaux.parms) <- parm.paths
    
  for(i in seq_along(parm.paths)){
    xml.node <- xml2::xml_find_first(aux.xml, parm.paths[i])
    aux.parm.text <- xml2::xml_text(xml.node)
    aux.parm.value <- as.numeric(strsplit(aux.parm.text, "\\s+")[[1]])
    iaux.parms[[i]] <- aux.parm.value
  }    
  
  obj_fun <- function(cfs, parm.paths, data, aux.file, 
                      iaux.parms, weights, index, 
                      parm.vector.index,
                      cfile = TRUE){
    
    ## Need to edit the parameters in the crop file or the main simulation
    for(i in seq_along(cfs)){
      ## Retrieve the vector of current parameters
      if(parm.vector.index[i] <= 0){
        mparm <- paste(iaux.parms[[i]] * cfs[i], collapse = " ")  
      }else{
        pvi <- parm.vector.index[i]
        iaux.parms[[i]][pvi] <- iaux.parms[[i]][pvi] * cfs[i]
        mparm <- paste(iaux.parms[[i]], collapse = " ")    
      }
      
      ## Edit the specific parameters with the corresponding values
      if(cfile){
        ## Here I'm editing an auxiliary file ending in .xml
        edit_apsim_xml(file = aux.file, 
                       src.dir = src.dir,
                       parm.path = parm.paths[i],
                       overwrite = TRUE,
                       value = as.character(mparm),
                       verbose = FALSE)        
      }else{
        ## Here I'm editing the main simulation file .apsim
        edit_apsim(file = aux.file, 
                   node = "Other",
                   src.dir = src.dir,
                   parm.path = parm.paths[i],
                   overwrite = TRUE,
                   value = as.character(mparm),
                   verbose = FALSE)        
      }
    }
    
    ## Run simulation  
    sim <- try(apsim(file = file, src.dir = src.dir,
                     silent = TRUE, cleanup = TRUE, value = "report"),
               silent = TRUE)
    
    if(inherits(sim, "try-error")) return(NA)
    
    ## Only keep those columns with corresponding names in the data
    ## and only the dates that match those in 'data'
    if(!all(names(data) %in% names(sim))) 
      stop("names in 'data' do not match names in simulation")
    
    sim.s <- subset(sim, sim$Date %in% data[[index]], select = names(data))
    
    if(nrow(sim.s) == 0L) stop("no rows selected in simulations")
    ## Assuming they are aligned, get rid of the 'index' column
    sim.s <- sim.s[,-which(names(sim.s) == index)]
    data <- data[,-which(names(data) == index)]
    ## Now I need to calculate the residual sum of squares
    ## For this to work all variables should be numeric
    diffs <- as.matrix(data) - as.matrix(sim.s)
    rss <- sum(weights * colSums(diffs^2)) 
    return(rss)
  }
  
  ## Pre-optimized RSS
  rss <- obj_fun(cfs = rep(1, length(parm.paths)),
                 parm.paths = parm.paths,
                 data = data,
                 aux.file = aux.file,
                 iaux.parms = iaux.parms,
                 weights = weights,
                 index = index,
                 parm.vector.index = parm.vector.index,
                 cfile = cfile)
  ## optimization
  if(type == "optim"){
    op <- stats::optim(par = rep(1, length(parm.paths)), 
                       fn = obj_fun, 
                       parm.paths = parm.paths, 
                       data = data, 
                       aux.file = aux.file, 
                       iaux.parms = iaux.parms,
                       weights = weights,
                       index = index,
                       parm.vector.index = parm.vector.index,
                       cfile = cfile,
                       ...)    
  }
  
  if(type == "nloptr"){
    op <- nloptr::nloptr(x0 = rep(1, length(parm.paths)),
                         eval_f = obj_fun,
                         parm.paths = parm.paths, 
                         data = data, 
                         aux.file = aux.file, 
                         iaux.parms = iaux.parms,
                         weights = weights,
                         index = index,
                         parm.vector.index = parm.vector.index,
                         cfile = cfile,
                         ...)
    op$par <- op$solution
    op$value <- op$objective 
    op$convergence <- op$status
  }
  
  if(type == "mcmc"){
    ## Setting defaults
    datami.sds <- apply(datami, 2, sd)
    mcmc.args <- list(...)
    if(is.null(mcmc.args$lower)) lower <- rep(0, length(iaux.parms) + ncol(datami))
    if(is.null(mcmc.args$upper)) upper <- c(rep(2, length(iaux.parms)), datami.sds * 100)
    if(is.null(mcmc.args$sampler)) sampler <- "DEzs"
    if(is.null(mcmc.args$settings)) stop("runMCMC settings are missing with no default")

    cfs <- c(rep(1, length(iaux.parms)), apply(datami, 2, sd))
    
    ## Create environment with objects
    assign('.file', file, mcmc.apsim.env)
    assign('.aux.file', aux.file, mcmc.apsim.env)
    assign('.src.dir', src.dir, mcmc.apsim.env)
    assign('.parm.paths', parm.paths, mcmc.apsim.env)
    assign('.data', data, mcmc.apsim.env)
    assign('.iaux.parms', iaux.parms, mcmc.apsim.env)
    assign('.index', index, mcmc.apsim.env)
    assign('.parm.vector.index', parm.vector.index, mcmc.apsim.env)
    assign('.cfile', cfile, mcmc.apsim.env)
    
    ## Pre-optimized log-likelihood
    pll <- log_lik2(cfs)
    
    cat("Pre-optimized log-likelihood", pll, "\n")
    
    nms <- c(names(iaux.parms), paste0("sd_", names(datami)))
    bayes.setup <- BayesianTools::createBayesianSetup(log_lik2, 
                                                      lower = lower,
                                                      upper = upper,
                                                      names = nms)
    
    op.mcmc <- BayesianTools::runMCMC(bayes.setup, 
                                      sampler = sampler, 
                                      settings = mcmc.args$settings)
    return(op.mcmc)

  }

  ans <- structure(list(rss = rss, iaux.parms = iaux.parms, 
                        op = op, n = nrow(data),
                        parm.vector.index = parm.vector.index),
                   class = "optim_apsim")
  return(ans)
}

#' @rdname optim_apsim
#' @description Friendly printing of optim_apsim
#' @param x object of class \sQuote{optim_apsim}
#' @param ... additional arguments (none used at the moment)
#' @param digits number of digits to round up the output
#' @param level confidence level (default 0.95)
#' @export
#' 
print.optim_apsim <- function(x, ..., digits = 3, level = 0.95){

  cat("Initial values: \n")
  
  for(i in seq_along(x$iaux.parms)){
    
    cat("\t Parameter path: ", names(x$iaux.parms)[i], "\n")
    if(x$parm.vector.index[i] > 0) cat("Vector index: ", x$parm.vector.index[i], "\n")
    cat("\t Values: ", x$iaux.parms[[i]], "\n")
  }   
  cat("\t Pre-optimized RSS: ", x$rss, "\n")
  cat("Optimized values: \n")
  
  for(i in seq_along(x$iaux.parms)){
    
    cat("\t Parameter path: ", names(x$iaux.parms)[i], "\n")
    if(x$parm.vector.index[i] > 0) cat("Vector index: ", x$parm.vector.index[i], "\n")
    if(x$parm.vector.index[i] <= 0){
      cat("\t Values: ", round(x$iaux.parms[[i]] * x$op$par[i], digits), "\n")  
    }else{
      pvi <- x$parm.vector.index[i]
      x$iaux.parms[[i]][pvi] <- x$iaux.parms[[i]][pvi] * x$op$par[i]
      cat("\t Values: ", round(x$iaux.parms[[i]], digits), "\n")        
    }
    
    if(!is.null(x$op$hessian)){
      ## I actually found this way of computing SE here:
      ## https://www.researchgate.net/post/In_R_how_to_estimate_confidence_intervals_from_the_Hessian_matrix
      degf <- x$n - length(x$iaux.parms) ## Degrees of freedom
      par.se <- sqrt((2 * (1 / (x$op$hessian[i,i])) * x$op$value) / degf)
      qTT <- -1 * stats::qt((1 - level) * 0.5, degf) ## t statistic
      cat("\t CI level: ", level, "\t SE:", par.se)
      if(x$parm.vector.index[i] <= 0){
        cat("\t Lower: ", round((x$op$par[i] - qTT * par.se) * x$iaux.parms[[i]], digits))
        cat("\t Upper: ", round((x$op$par[i] + qTT * par.se) * x$iaux.parms[[i]], digits),"\n")        
      }else{
        pvi <- x$parm.vector.index[i]
        cat("\t Lower: ", round((x$op$par[i] - qTT * par.se) * x$iaux.parms[[i]][pvi], digits))
        cat("\t Upper: ", round((x$op$par[i] + qTT * par.se) * x$iaux.parms[[i]][pvi], digits),"\n")        
      }
    }
  }   
  cat("\t Optimized RSS: ", x$op$value, "\n")
  cat("Convergence:", x$op$convergence,"\n")
}
  
## Log-likelihood for mcmc method
log_lik2 <- function(.cfs){

  .file <- get('.file', envir = mcmc.apsim.env)
  .aux.file <- get('.aux.file', envir = mcmc.apsim.env)
  .src.dir <- get('.src.dir', envir = mcmc.apsim.env)
  .parm.paths <- get('.parm.paths', envir = mcmc.apsim.env)
  .data <- get('.data', envir = mcmc.apsim.env)
  .iaux.parms <- get('.iaux.parms', envir = mcmc.apsimx.env)
  .index <- get('.index', envir = mcmc.apsim.env)
  .parm.vector.index <- get('.parm.vector.index', envir = mcmc.apsim.env)
  .cfile <- get('.cfile', envir = mcmc.apsim.env)
  
  for(i in seq_along(.cfs)){
    ## Retrieve the vector of current parameters
    if(.parm.vector.index[i] <= 0){
      mparm <- paste(.iaux.parms[[i]] * .cfs[i], collapse = " ")  
    }else{
      pvi <- .parm.vector.index[i]
      .iaux.parms[[i]][pvi] <- .iaux.parms[[i]][pvi] * .cfs[i]
      mparm <- paste(.iaux.parms[[i]], collapse = " ")    
    }
    
    ## Edit the specific parameters with the corresponding values
    if(.cfile){
      ## Here I'm editing an auxiliary file ending in .xml
      edit_apsim_xml(file = .aux.file, 
                     src.dir = .src.dir,
                     parm.path = .parm.paths[i],
                     overwrite = TRUE,
                     value = as.character(mparm),
                     verbose = FALSE)        
    }else{
      ## Here I'm editing the main simulation file .apsim
      edit_apsim(file = .aux.file, 
                 node = "Other",
                 src.dir = .src.dir,
                 parm.path = .parm.paths[i],
                 overwrite = TRUE,
                 value = as.character(mparm),
                 verbose = FALSE)        
    }
  }
  
  ## Run simulation  
  sim <- try(apsim(file = .file, src.dir = .src.dir,
                   silent = TRUE, cleanup = TRUE, value = "report"),
             silent = TRUE)
  
  if(inherits(sim, "try-error")) return(NA)
  
  ## Only keep those columns with corresponding names in the data
  ## and only the dates that match those in 'data'
  if(!all(names(.data) %in% names(sim))) 
    stop("names in 'data' do not match names in simulation")
  
  sim.s <- subset(sim, sim$Date %in% .data[[.index]], select = names(.data))
  
  if(nrow(sim.s) == 0L) stop("no rows selected in simulations")
  ## Assuming they are aligned, get rid of the 'index' column
  sim.s <- sim.s[,-which(names(sim.s) == .index)]
  data <- .data[,-which(names(.data) == .index)]
  ## Now I need to calculate the residual sum of squares
  ## For this to work all variables should be numeric
  diffs <- as.matrix(.data) - as.matrix(sim.s)
  if(ncol(diffs) == 1){
    lls <- stats::dnorm(diffs[,1], sd = .cfs[length(.cfs)], log = TRUE)
    return(sum(lls))
  }else{
    Sigma <- diag(.cfs[(length(.iaux.parms) + 1):length(.cfs)])
    lls <- mvtnorm::dmvnorm(diffs, sigma = Sigma, log = TRUE)
    return(sum(lls))    
  }
}

#' Create an apsim environment for MCMC
#' 
#' @title Environment to store data for apsim MCMC
#' @description Environment which stores data for MCMC
#' @export
#' 
mcmc.apsim.env <- new.env(parent = emptyenv())
