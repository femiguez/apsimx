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
#' * Internally, the optimization is done around the scaled value of the initial parameter
#' values. A value of 1 would correspond to the inital value of the parameter. 
#' The \sQuote{lower} and \sQuote{upper} (or \sQuote{ub} and \sQuote{lb}) are also scaled 
#' to the initial values of the parameters. So, for example, if your initial value is 20 
#' and you provide an upper bound of 5, it means that the actual upper value that you are allowing for is 100. 
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
#' @param type Type of optimization. For now, \code{\link[stats]{optim}} and, if available, \code{\link[nloptr]{nloptr}} or 
#' \sQuote{mcmc} through \code{\link[BayesianTools]{runMCMC}}. Option \sQuote{ucminf} uses the \code{\link[ucminf]{ucminf}} function.
#' @param weights Weighting method or values for computing the residual sum of squares (see Note). 
#' @param index Index for filtering APSIM output. \sQuote{Date} is currently used. (I have not tested how well it works using anything other than Date).
#' @param parm.vector.index Index to optimize a specific element of a parameter vector. At the moment it is
#' possible to only edit one element at a time. This is because there is a conflict when generating multiple
#' elements in the candidate vector for the same parameter.
#' @param xml.parm optional logical vector used when optimizing parameters which are both in the .apsim file and in the \sQuote{crop.file}.
#' If \sQuote{crop.file} is missing it is assumed that the paramters to be optimized are in the .apsim file. If \sQuote{crop.file} is
#' not missing it is assumed that they are in the \sQuote{crop.file}. If the parameters are in both, this needs to be specified in 
#' this argument.
#' @param ... additional arguments to be passed to the optimization algorithm. If you want confidence intervals, then include \sQuote{hessian = TRUE}.
#' @note When computing the objective function (residual sum-of-squares) different variables are combined.
#' It is common to weight them since they are in different units. If the argument weights is not supplied
#' no weighting is applied. It can be \sQuote{mean}, \sQuote{var} or a numeric vector of appropriate length.
#' @return object of class \sQuote{optim_apsim}, but really just a list with results from optim and additional information.
#' @export
#' 

optim_apsim <- function(file, src.dir = ".", 
                        crop.file, parm.paths, data, 
                        type = c("optim", "nloptr","mcmc", "ucminf"), 
                        weights, index = "Date",
                        parm.vector.index,
                        xml.parm,
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
  
  if(type == "ucminf"){
    if(!requireNamespace("ucminf", quietly = TRUE)){
      warning("The ucminf package is required for this method.")
      return(NULL)
    }
  }
  
  ## Index can now potentially reference two columns
  datami <- data[,-which(names(data) %in% index), drop = FALSE]
  if(any(grepl("Date", index))) data$Date <- as.Date(data$Date)
  
  ## Setting up weights
  if(missing(weights)){
    weights <- rep(1, ncol(datami))
  }else{
    if(weights == "mean"){
      weights <- abs(1 / apply(datami, 2, mean, na.rm = TRUE))  
    }else{
      if(weights == "var"){
        weights <- 1 / apply(datami, 2, var, na.rm = TRUE)    
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
  
  if(!is.logical(cfile) || length(cfile) != length(parm.paths))
    stop("xml.parm should be a logical of length equal to parm.paths")
  
  ## Retrieve initial value vectors for the auxiliary file
  iaux.parms <- vector("list", length = length(parm.paths))
  length.aux.parms <- numeric(length(parm.paths))
  names(iaux.parms) <- parm.paths
    
  for(i in seq_along(parm.paths)){
    
    if(cfile[i]){
      aux.xml <- xml2::read_xml(file.path(src.dir, aux.file))    
    }else{
      aux.xml <- xml2::read_xml(file.path(src.dir, file))
    }
    
    xml.node <- xml2::xml_find_first(aux.xml, parm.paths[i])
    length.xml.node0 <- xml2::xml_length(xml.node)
    length.xml.node1 <- length(xml2::xml_text(xml.node))
    length.xml.node <- max(length.xml.node0, length.xml.node1)
    length.aux.parms[i] <- length.xml.node 
    if(length.xml.node == 1){
      aux.parm.text <- xml2::xml_text(xml.node)
      aux.parm.value <- as.numeric(strsplit(aux.parm.text, "\\s+")[[1]])      
    }else{
      aux.parm.text <- xml2::xml_text(xml2::xml_children(xml.node))
      aux.parm.value <- as.numeric(aux.parm.text)
    }
    iaux.parms[[i]] <- aux.parm.value
  }    
  
  obj_fun <- function(cfs, parm.paths, data, file, aux.file, 
                      iaux.parms, weights, index, 
                      parm.vector.index,
                      xml.parm){
    
    ## Need to edit the parameters in the crop file or the main simulation
    for(i in seq_along(cfs)){
      ## Retrieve the vector of current parameters
      if(parm.vector.index[i] <= 0){
        if(length.aux.parms[i] == 1){
          mparm <- paste(iaux.parms[[i]] * cfs[i], collapse = " ")    
        }else{
          mparm <- iaux.parms[[i]] * cfs[i] 
        }
      }else{
        pvi <- parm.vector.index[i]
        iaux.parms[[i]][pvi] <- iaux.parms[[i]][pvi] * cfs[i]
        mparm <- paste(iaux.parms[[i]], collapse = " ")    
      }
      
      ## Edit the specific parameters with the corresponding values
      if(xml.parm[i]){
        ## Here I'm editing an auxiliary file ending in .xml
        edit_apsim_xml(file = aux.file, 
                       src.dir = src.dir,
                       parm.path = parm.paths[i],
                       overwrite = TRUE,
                       value = as.character(mparm),
                       verbose = FALSE)        
      }else{
        ## Here I'm editing the main simulation file .apsim
        edit_apsim(file = file, 
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
    
    if(!inherits(sim, "data.frame"))
      stop("the apsim simulation should return an object of class 'data.frame'. \n 
           Check that the output does simplify to a 'data.frame' instead of a 'list'.")
    
    if("outfile" %in% names(sim) && index == "Date")
      stop("If you have multiple simulations, you should modify the index argument. \n
           It is possible that 'index = c('outfile', 'Date')' might work.")
    
    ## Only keep those columns with corresponding names in the data
    ## and only the dates that match those in 'data'
    if(!all(names(data) %in% names(sim))) 
      stop("names in 'data' do not match names in simulation")
    
    if(length(index) == 1 && index == "Date"){
      sim.s <- subset(sim, sim$Date %in% data[[index]], select = names(data))  
      
      sim.s <- sim.s[order(sim.s[, index[1]]),]
      data <- data[order(data[, index[1]]),]
      
      if(!all(sim.s[[index[1]]] == data[[index[1]]]))
        stop(paste("simulations and data for", index[1], "do not match"))   
      
    }else{
      if(!is.null(data$outfile)) data$outfile <- as.factor(data$outfile)
      if(!is.null(sim$outfile)) sim$outfile <- as.factor(sim$outfile)
      sim.s0 <- merge(sim, subset(data, select = index), by = index)  
      sim.s <- subset(sim.s0, select = names(data))
      
      ## However, they need to be in the exact same order
      sim.s <- sim.s[order(sim.s[, index[1]], sim.s[ ,index[2]]),]
      data <- data[order(data[, index[1]], data[, index[2]]),]
      
      if(!all(sim.s[[index[1]]] == data[[index[1]]]))
        stop(paste("simulations and data for", index[1], "do not match"))        
      
      if(!all(sim.s[[index[2]]] == data[[index[2]]]))
        stop(paste("simulations and data for", index[2], "do not match"))   
    }

    if(ncol(sim.s) != ncol(data)){
      cat("Number of columns in data", ncol(data), "\n")
      cat("Number of columns in subset simulation", ncol(sim.s), "\n")
      stop("Number of columns in data does not equal number of columns in simulation")
    }
        
    if(nrow(sim.s) == 0L) stop("Something went wrong. No rows selected in simulations")
    ## Assuming they are aligned, get rid of the 'index' column
    sim.s <- sim.s[,-which(names(sim.s) %in% index)]
    data <- data[,-which(names(data) %in% index)]
    ## Now I need to calculate the residual sum of squares
    ## For this to work all variables should be numeric
    diffs <- as.matrix(data) - as.matrix(sim.s)
    rss <- sum(weights * colSums(diffs^2, na.rm = TRUE)) 
    return(log(rss))
  }
  
  ## Pre-optimized RSS
  pre.lrss <- obj_fun(cfs = rep(1, length(parm.paths)),
                      parm.paths = parm.paths,
                      data = data,
                      file = file,
                      aux.file = aux.file,
                      iaux.parms = iaux.parms,
                      weights = weights,
                      index = index,
                      parm.vector.index = parm.vector.index,
                      xml.parm = cfile)
  ## optimization
  if(type == "optim"){
    op <- stats::optim(par = rep(1, length(parm.paths)), 
                       fn = obj_fun, 
                       parm.paths = parm.paths, 
                       data = data, 
                       file = file,
                       aux.file = aux.file, 
                       iaux.parms = iaux.parms,
                       weights = weights,
                       index = index,
                       parm.vector.index = parm.vector.index,
                       xml.parm = cfile,
                       ...)    
  }
  
  if(type == "ucminf"){
    op <- ucminf::ucminf(par = rep(1, length(parm.paths)), 
                         fn = obj_fun, 
                         parm.paths = parm.paths, 
                         data = data, 
                         file = file,
                         aux.file = aux.file, 
                         iaux.parms = iaux.parms,
                         weights = weights,
                         index = index,
                         parm.vector.index = parm.vector.index,
                         xml.parm = cfile,
                         ...)    
  }
  
  if(type == "nloptr"){
    op <- nloptr::nloptr(x0 = rep(1, length(parm.paths)),
                         eval_f = obj_fun,
                         parm.paths = parm.paths, 
                         data = data, 
                         file = file, 
                         aux.file = aux.file, 
                         iaux.parms = iaux.parms,
                         weights = weights,
                         index = index,
                         parm.vector.index = parm.vector.index,
                         xml.parm = cfile,
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
    if(is.null(mcmc.args$parallel)) mcmc.args$parallel <- FALSE
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
    assign('.xml.parm', xml.parm, mcmc.apsim.env)
    
    ## Pre-optimized log-likelihood
    pll <- log_lik2(cfs)
    
    cat("Pre-optimized log-likelihood", pll, "\n")
    
    nms <- c(names(iaux.parms), paste0("sd_", names(datami)))
    bayes.setup <- BayesianTools::createBayesianSetup(log_lik2, 
                                                      lower = lower,
                                                      upper = upper,
                                                      names = nms,
                                                      parallel = mcmc.args$parallel)
    
    op.mcmc <- BayesianTools::runMCMC(bayes.setup, 
                                      sampler = sampler, 
                                      settings = mcmc.args$settings)
    return(op.mcmc)

  }
  
  ## pre.rss are the pre-optimized residual sum of squares
  ## op$value are the optimized weighted residual sum of squares in the log scale
  ## The next line is only true when optimizing one single variable
  ## logLik <- 0.5 * ( - N * (log(2 * pi) + 1 - log(N) + log(op$value))

  ans <- structure(list(pre.rss = exp(pre.lrss), 
                        post.rss = exp(op$value), ## This is weighted
                        weights = weights,
                        iaux.parms = iaux.parms, op = op, n = nrow(data),
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
#' @return prints to console
#' @export
#' 
print.optim_apsim <- function(x, ..., digits = 3, level = 0.95){

  cat("Initial values: \n")
  
  for(i in seq_along(x$iaux.parms)){
    
    cat("\t Parameter path: ", names(x$iaux.parms)[i], "\n")
    if(x$parm.vector.index[i] > 0) cat("Vector index: ", x$parm.vector.index[i], "\n")
    cat("\t Values: ", x$iaux.parms[[i]], "\n")
  }   
  cat("\t Pre-optimized (weighted) RSS: ", x$pre.rss, "\n")
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
      par.se <- sqrt((2 * (1 / (x$op$hessian[i,i])) * x$iaux.parms[[i]]^2) / degf)
      qTT <- -1 * stats::qt((1 - level) * 0.5, degf) ## t statistic
      cat("\t CI level: ", level, "t: ", qTT, " SE:", round(par.se * x$iaux.parms[[i]], digits),"\n")
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
  cat("\t Optimized (weighted) RSS: ", x$post.rss, "\n")
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
  .xml.parm <- get('.xml.parm', envir = mcmc.apsim.env)
  
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
    if(.xml.parm[i]){
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

  if(length(.index) == 1){
    sim.s <- subset(sim, sim[, .index, drop = FALSE] %in% .data[[.index]], select = names(.data))  
    
    sim.s <- sim.s[order(sim.s[, .index[1]]),]
    .data <- .data[order(.data[, .index[1]]),]
    
    if(!all(sim.s[[.index[1]]] == .data[[.index[1]]]))
      stop(paste("simulations and data for", .index[1], "do not match"))   
    
  }else{
    if(!is.null(.data$outfile)) .data$outfile <- as.factor(.data$outfile)
    if(!is.null(sim$outfile)) sim$outfile <- as.factor(sim$outfile)
    sim.s0 <- merge(sim, subset(.data, select = .index), by = .index)  
    sim.s <- subset(sim.s0, select = names(.data))
    
    ## However, they need to be in the exact same order
    sim.s <- sim.s[order(sim.s[, .index[1]], sim.s[ ,.index[2]]),]
    .data <- .data[order(.data[, .index[1]], .data[, .index[2]]),]
    
    if(!all(sim.s[[.index[1]]] == .data[[.index[1]]]))
      stop(paste("simulations and data for", .index[1], "do not match"))        
    
    if(!all(sim.s[[.index[2]]] == .data[[.index[2]]]))
      stop(paste("simulations and data for", .index[2], "do not match"))   
  }
    
  if(nrow(sim.s) == 0L) stop("no rows selected in simulations")
  ## Assuming they are aligned, get rid of the 'index' column
  sim.s <- sim.s[,-which(names(sim.s) == .index)]
  .data <- .data[,-which(names(.data) == .index)]
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

#' @rdname optim_apsim
#' @description Variance-Covariance for an \sQuote{optim_apsim} object
#' @param object object of class \sQuote{optim_apsim}
#' @param ... additional arguments (none used at the moment)
#' @param scaled for now only return the scaled matrix
#' @note This in the scale of the optimized parameters which are
#' scaled to be around 1.
#' @return it returns the variance-covariance matrix for an object of class \sQuote{optim_apsim}.
#' @export
#' 
vcov.optim_apsim <- function(object, ..., scaled = TRUE){
  
  if(is.null(object$op$hessian)) stop("Hessian matrix not found")
  ## Hessian matrix
  hess <- object$op$hessian
  ## Correlation matrix
  cor.mat <- stats::cov2cor(solve(hess))
  ## Standard deviation vector
  sd.vec <- numeric(nrow(hess))
  degf <- object$n - nrow(hess)
  for(i in 1:nrow(hess)){
    sd.vec[i] <- sqrt((2 * 1/(hess[i,i]) * object$iaux.parms[[i]]^2) / degf)
  }
  ans <- t(sd.vec * cor.mat) * sd.vec
  return(ans)
}

#' @rdname optim_apsim
#' @description Parameter estimates for an \sQuote{optim_apsim} object
#' @param object object of class \sQuote{optim_apsim}
#' @param ... additional arguments (none used at the moment)
#' @param scaled whether to return the scaled or unscaled estimates
#' (TRUE in the optimized scale, FALSE in the original scale)
#' @return a numeric vector with the value of the parameter estimates.
#' @export
#' 
coef.optim_apsim <- function(object, ..., scaled = FALSE){
  
  ans <- numeric(length(object$op$par))
  
  for(i in seq_along(ans)){
    if(object$parm.vector.index[i] <= 0){
      ans[i] <- object$op$par[i] * object$iaux.parms[[i]]
    }else{
      pvi <- object$parm.vector.index[i]
      ans[i] <- object$op$par[i] * object$iaux.parms[[i]][pvi]
    }
  }

  if(scaled) ans <- object$op$par
  
  names(ans) <- names(object$iaux.parms)
  
  return(ans)
}

#' @rdname optim_apsim
#' @description Confidence intervals for parameter estimates for an \sQuote{optim_apsim} object
#' @param object object of class \sQuote{optim_apsim}
#' @param parm parameter to select (it can be a regular expression)
#' @param level confidence level (default is 0.95)
#' @param ... additional arguments (none used at the moment)
#' @return a matrix with lower and upper limits and the point estimate (coef)
#' @export
#' 

confint.optim_apsim <- function(object, parm, level = 0.95, ...){
  
  if(is.null(object$op$hessian)) 
    stop("Hessian is needed to calculate confidence intervals", call. = FALSE)

  if(level < 0 || level > 1)
    stop("level should be between 0 and 1", call. = FALSE)
  
  x <- object
  
  ans <- matrix(ncol = 3, nrow = length(x$iaux.parms))
  colnames(ans) <- c("lwr", "coef", "upr")
  rownames(ans) <- names(x$iaux.parms)

  for(i in seq_along(x$iaux.parms)){
    
    if(x$parm.vector.index[i] <= 0){
      ans[i, 2] <- x$iaux.parms[[i]] * x$op$par[i]
    }else{
      pvi <- x$parm.vector.index[i]
      x$iaux.parms[[i]][pvi] <- x$iaux.parms[[i]][pvi] * x$op$par[i]
      ans[i, 2] <- x$iaux.parms[[i]]
    }
    
    ## I actually found this way of computing SE here:
    ## That post references the Introduction to R section 11.7.2
    degf <- x$n - length(x$iaux.parms) ## Degrees of freedom
    par.se <- sqrt((2 * (1 / (x$op$hessian[i,i])) * x$iaux.parms[[i]]^2) / degf)
    qTT <- -1 * stats::qt((1 - level) * 0.5, degf) ## t statistic
      if(x$parm.vector.index[i] <= 0){
        ans[i, 1] <- (x$op$par[i] - qTT * par.se) * x$iaux.parms[[i]]
        ans[i, 3] <- (x$op$par[i] + qTT * par.se) * x$iaux.parms[[i]]
      }else{
        pvi <- x$parm.vector.index[i]
        ans[i, 1] <- (x$op$par[i] - qTT * par.se) * x$iaux.parms[[i]][pvi]
        ans[i, 3] <- (x$op$par[i] + qTT * par.se) * x$iaux.parms[[i]][pvi]
      }
  }
  
  if(!missing(parm)){
    w.parm <- grep(parm, names(x$iaux.parms))
    if(length(w.parm) == 0)
      stop("'parm' not found", call. = FALSE)
    ans <- ans[w.parm, , drop = FALSE]
  }
  return(ans)
}

#' Create an apsim environment for MCMC
#' 
#' @title Environment to store data for apsim MCMC
#' @description Environment which stores data for MCMC
#' @return This is an environment, so nothing to return.
#' @export
#' 
mcmc.apsim.env <- new.env(parent = emptyenv())


