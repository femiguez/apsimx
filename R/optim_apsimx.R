#'
#' Simple optimization for APSIM Next Generation
#' 
#' * At the moment it is required to provide starting values for the parameters of interest.
#' 
#' * It is suggested that you keep a backup of the original file. This function
#' will edit and overwrite the file during the optimization.
#' 
#' * When you use the parm.vector.index you cannot edit two separate elements of
#' a vector at the same time. This should be used to target a single element of 
#' a vector only. (I can add this feature in the future if it is justified.)
#' 
#' * Internally, the optimization is done around the scaled value of the initial parameter
#' values. A value of 1 would correspond to the inital value of the parameter. 
#' The \sQuote{lower} and \sQuote{upper} (or \sQuote{ub} and \sQuote{lb}) are also scaled 
#' to the initial values of the parameters. So, for example, if your initial value is 20 
#' and you provide an upper bound of 5, it means that the actual upper value that you are allowing for is 100. 
#' 
#' * I have tested other optimizers and packages, but I think these are enough for most purposes. I tried
#' function stats::nlm (but it does not support bounds and it can fail), package 'optimx' is a bit messy and it
#' does not provide sufficient additional functionality. Package 'ucminf' seems like a good alternative, but it
#' did not perform better than the other ones.
#' 
#' @title Optimize parameters in an APSIM Next Generation simulation
#' @name optim_apsimx
#' @rdname optim_apsimx
#' @description It is a wrapper for running APSIM-X and optimizing parameters using \code{\link[stats]{optim}}
#' @param file file name to be run (the extension .apsimx is optional)
#' @param src.dir directory containing the .apsimx file to be run (defaults to the current directory)
#' @param parm.paths absolute or relative paths of the coefficients to be optimized. 
#'             It is recommended that you use \code{\link{inspect_apsimx}} for this
#' @param data data frame with the observed data. By default is assumes there is a 'Date' column for the index.
#' @param type Type of optimization. For now, \code{\link[stats]{optim}}, and, if available, \code{\link[nloptr]{nloptr}} or 
#' \sQuote{mcmc} through \code{\link[BayesianTools]{runMCMC}}. Option \sQuote{ucminf} uses the \code{\link[ucminf]{ucminf}} function.
#' @param weights Weighting method or values for computing the residual sum of squares. 
#' @param index Index for filtering APSIM output. Typically, \dQuote{Date}, but it can be c(\dQuote{report}, \dQuote{Date}) for 
#' multiple simulations
#' @param parm.vector.index Index to optimize a specific element of a parameter vector.  At the moment it is
#' possible to only edit one element at a time. This is because there is a conflict when generating multiple
#' elements in the candidate vector for the same parameter.
#' @param replacement TRUE or FALSE for each parameter. Indicating whether it is part of 
#' the \sQuote{replacement} component. Its length should be equal to the length or \sQuote{parm.paths}.
#' @param root root argument for \code{\link{edit_apsimx_replacement}}
#' @param initial.values (required) supply the initial values of the parameters. (Working on fixing this...).
#' If the parameters to be optimized correspond to a single value, then a simple numeric vector can be supplied.
#' If one or more of the parameters represent a vector in APSIM, then the initial values should be passed as a
#' list. At the moment, it is not possible to check if these are appropriate (correct name and length). 
#' @param ... additional arguments to be passed to the optimization algorithm. See \code{\link[stats]{optim}}
#' @note When computing the objective function (residual sum-of-squares) different variables are combined.
#' It is common to weight them since they are in different units. If the argument weights is not supplied
#' no weighting is applied. It can be 'mean', 'variance' or a numeric vector of appropriate length.
#' @return object of class \sQuote{optim_apsim}, but really just a list with results from optim and additional information.
#' @export
#' @examples 
#' \donttest{
#' ## See the vignette for examples
#' }
#' 

optim_apsimx <- function(file, src.dir = ".", 
                         parm.paths, data, 
                         type = c("optim", "nloptr", "mcmc", "ucminf"), 
                         weights, 
                         index = "Date",
                         parm.vector.index,
                         replacement,
                         root,
                         initial.values,
                         ...){
  
  .check_apsim_name(file)

  ## The might offer suggestions in case there is a typo in 'file'
  file.names <- dir(path = src.dir, pattern = ".apsimx$", ignore.case = TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsimx files in the specified directory to run.")
  }
  
  file <- match.arg(file, file.names)
  
  file.name.path <- file.path(src.dir, file)
  
  type <- match.arg(type)
  
  if(type == "nloptr"){
    if(!requireNamespace("nloptr", quietly = TRUE)){
      warning("The nloptr package is required for this optimization method")
      return(NULL)
    }
  }
  
  if(type == "mcmc"){
    if(!requireNamespace("BayesianTools", quietly = TRUE)){
      warning("The BayesianTools package is required for this method")
      return(NULL)
    }
  }
  
  if(type == "ucminf"){
    if(!requireNamespace("ucminf", quietly = TRUE)){
      warning("The ucminf package is required for this method")
      return(NULL)
    }
  }
  
  ## Setting up Date
  datami <- data[ ,-which(names(data) %in% index), drop = FALSE]
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
  
  ## Set up replacement
  if(missing(replacement)) replacement <- rep(FALSE, length(parm.paths))
  
  ## If root is not present
  if(missing(root)) root <- list("Models.Core.Replacements", NA)

  ## Read apsimx file (JSON)
  apsimx_json <- jsonlite::read_json(file.path(src.dir, file))
  
  ## Build the initial values
  iparms <- vector("list", length = length(parm.paths))
  names(iparms) <- parm.paths
  
  if(missing(initial.values))
    stop("Initial values should be supplied. (Working on a fix for this)", call. = FALSE)
  
  ## How do I retrieve the current value I want to optimize?
  for(i in seq_along(parm.paths)){
    if(missing(initial.values)){
      if(replacement[i]){
        if(!grepl(".Simulations.Replacements.", parm.paths[i])){
          parm.val <- extract_values_apsimx(file = file, src.dir = src.dir, 
                                            parm.path = paste0(".Simulations.Replacements.", parm.paths[i]))            
        }else{
          parm.val <- extract_values_apsimx(file = file, src.dir = src.dir, parm.path = parm.paths[i])            
        }
      }else{
        parm.val <- extract_values_apsimx(file = file, src.dir = src.dir, 
                                          parm.path = parm.paths[i])  
      }
      if(is.list(parm.val)){
        iparms[[i]] <- as.numeric(unlist(parm.val))  
      }else{
        iparms[[i]] <- as.numeric(parm.val)  
      }
    }else{
      if(is.list(initial.values)){
        iparms[[i]] <- initial.values[[i]]
      }else{
        iparms[[i]] <- as.numeric(initial.values[i])  
      }
      
    }
  }
  
  obj_fun <- function(cfs, parm.paths, data, iparms, weights,
                      index, parm.vector.index, replacement, root){
    
    ## Need to edit the parameters in the simulation file or replacement
    for(i in seq_along(cfs)){
      ## Edit the specific parameters with the corresponding values
      if(parm.vector.index[i] <= 0){
        par.val <- iparms[[i]] * cfs[i]  
      }else{
        pvi <- parm.vector.index[i]
        iparms[[i]][pvi] <- iparms[[i]][pvi] * cfs[i]  
        par.val <- iparms[[i]]
      }
      
      if(replacement[i]){
        pp0 <- strsplit(parm.paths[i], ".", fixed = TRUE)[[1]]
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
          edit_apsimx(file = file, 
                      src.dir = src.dir,
                      wrt.dir = src.dir,
                      node = "Other",
                      parm.path = parm.paths[i],
                      overwrite = TRUE,
                      value = par.val,
                      verbose = FALSE) 
        }
    }
    
    ## Run simulation  
    sim <- try(apsimx(file = file, src.dir = src.dir,
                     silent = TRUE, cleanup = TRUE, value = "report"),
               silent = TRUE)
    
    if(inherits(sim, "try-error")) return(NA)
    
    if(!inherits(sim, "data.frame"))
      stop("the apsimx simulation should return an object of class 'data.frame'. \n 
           Check that the report does simplify to a 'data.frame' instead of a 'list'.")
    
    if("report" %in% names(sim) && index == "Date")
      stop("If you have multiple simulations, you should modify the index argument. \n
           It is possible that 'index = c('report', 'Date')' might work.")
    
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
      
      if(!is.null(data$report)) data$report <- as.factor(data$report)
      if(!is.null(sim$report)) sim$report <- as.factor(sim$report)
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
    
    if(nrow(sim.s) != nrow(data)){
      cat("Number of rows in data", nrow(data), "\n")
      cat("Number of rows in subset simulation", nrow(sim.s), "\n")
      stop("Number of rows in data does not equal number of rows in simulation")
    }
    
    if(nrow(sim.s) == 0L) stop("Something went wrong. No rows selected in simulations")
    ## Assuming they are aligned, get rid of the 'Date' column
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
                      iparms = iparms,
                      weights = weights,
                      index = index,
                      parm.vector.index = parm.vector.index,
                      replacement = replacement,
                      root = root)
  ## optimization
  if(type == "optim"){
    op <- stats::optim(par = rep(1, length(parm.paths)), 
                       fn = obj_fun, 
                       parm.paths = parm.paths, 
                       data = data, 
                       iparms = iparms,
                       weights = weights,
                       index = index,
                       parm.vector.index = parm.vector.index,
                       replacement = replacement,
                       root = root,
                       ...)    
  }
  
  if(type == "ucminf"){
    
    op <- ucminf::ucminf(par = rep(1, length(parm.paths)), 
                         fn = obj_fun, 
                         parm.paths = parm.paths, 
                         data = data, 
                         iparms = iparms,
                         weights = weights,
                         index = index,
                         parm.vector.index = parm.vector.index,
                         replacement = replacement,
                         root = root,
                         ...)    
  }
  
  if(type == "nloptr"){
    op <- nloptr::nloptr(x0 = rep(1, length(parm.paths)), 
                         eval_f = obj_fun, 
                         parm.paths = parm.paths, 
                         data = data, 
                         iparms = iparms,
                         weights = weights,
                         index = index,
                         parm.vector.index = parm.vector.index,
                         replacement = replacement,
                         root = root,
                         ...)    
    op$par <- op$solution
    op$value <- op$objective 
    op$convergence <- op$status
  }
  
  if(type == "mcmc"){
    ## Setting defaults
    datami.sds <- apply(datami, 2, sd)
    mcmc.args <- list(...)
    if(is.null(mcmc.args$lower)) lower <- rep(0, length(iparms) + ncol(datami))
    if(is.null(mcmc.args$upper)) upper <- c(rep(2, length(iparms)), datami.sds * 10)
    if(is.null(mcmc.args$sampler)) sampler <- "DEzs"
    if(is.null(mcmc.args$parallel)) mcmc.args$parallel <- FALSE
    if(is.null(mcmc.args$settings)) stop("runMCMC settings are missing with no default")
   
    
    cfs <- c(rep(1, length(iparms)), apply(datami, 2, sd))
    
    ## Create environment with objects
    assign('.file', file, mcmc.apsimx.env)
    assign('.src.dir', src.dir, mcmc.apsimx.env)
    assign('.parm.paths', parm.paths, mcmc.apsimx.env)
    assign('.data', data, mcmc.apsimx.env)
    assign('.iparms', iparms, mcmc.apsimx.env)
    assign('.index', index, mcmc.apsimx.env)
    assign('.parm.vector.index', parm.vector.index, mcmc.apsimx.env)
    assign('.replacement', replacement, mcmc.apsimx.env)
    assign('.root', root, mcmc.apsimx.env)
    
    ## Pre-optimized log-likelihood
    pll <- log_lik(cfs)
    
    cat("Pre-optimized log-likelihood", pll, "\n")
    
    nms <- c(names(iparms), paste0("sd_", names(datami)))
    bayes.setup <- BayesianTools::createBayesianSetup(log_lik, 
                                                      lower = lower,
                                                      upper = upper,
                                                      names = nms,
                                                      parallel = mcmc.args$parallel)
    
    op.mcmc <- BayesianTools::runMCMC(bayes.setup, 
                                      sampler = sampler, 
                                      settings = mcmc.args$settings)
    return(op.mcmc)
  }
  
  ans <- structure(list(pre.rss = exp(pre.lrss), 
                        post.rss = exp(op$value), ## This is weighted RSS
                        weights = weights,
                        iaux.parms = iparms, op = op, n = nrow(data),
                        parm.vector.index = parm.vector.index),
                   class = "optim_apsim")
  return(ans)
}

#'
#' Extract initial values from a parameter path
#' @title Extract values from a parameter path
#' @name extract_values_apsimx
#' @param file file name to be run (the extension .apsimx is optional)
#' @param src.dir directory containing the .apsimx file to be run (defaults to the current directory)
#' @param parm.path parameter path either use inspect_apsimx or see example below
#' @return a vector with extracted parameter values from an APSIM file.
#' @export
#' @examples 
#' \donttest{
#' ## Find examples
#' extd.dir <- system.file("extdata", package = "apsimx")
#' ## Extract parameter path
#' pp <- inspect_apsimx("Maize.apsimx", src.dir = extd.dir,
#'                      node = "Manager", parm = list("Fert", 1))
#' ppa <- paste0(pp, ".Amount")
#' ## Extract value
#' extract_values_apsimx("Maize.apsimx", src.dir = extd.dir, parm.path = ppa)
#' }
extract_values_apsimx <- function(file, src.dir, parm.path){
  
  .check_apsim_name(file)
  
  file.names <- dir(path = src.dir, pattern=".apsimx$", ignore.case=TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsimx files in the specified directory to inspect.")
  }
  
  file <- match.arg(file, file.names)
  
  apsimx_json <- jsonlite::read_json(paste0(src.dir, "/", file))
  
  upp <- strsplit(parm.path, ".", fixed = TRUE)[[1]]
  upp.lngth <- length(upp)
  if(upp.lngth < 5) stop("Parameter path too short?")
  if(upp.lngth > 10) stop("Cannot handle this yet")
  ## upp[2] is typically "Simulations"
  if(apsimx_json$Name != upp[2])
    stop("Simulation root name does not match")
  wl3 <- which(upp[3] == sapply(apsimx_json$Children, function(x) x$Name))
  ## At this level I select among simulation children
  ## upp[3] is typically "Simulation"
  n3 <- apsimx_json$Children[[wl3]]
  ## Look for the first reasonable parameter
  wl4 <- which(upp[4] == sapply(n3$Children, function(x) x$Name))
  ## This is super dumb but I do not know how to do it otherwise
  ## Length is equal to 5
  if(upp.lngth == 5){
    if(upp[5] %in% names(n3$Children[[wl4]])){
      value <- n3$Children[[wl4]][[upp[5]]]
    }else{
      wl5 <- which(upp[5] == sapply(n3$Children[[wl4]]$Children, function(x) x$Name))
      if(length(wl5) == 0) stop("Parameter not found at level 5")
      value <- n3$Children[[wl4]]$Children[[wl5]][[upp[5]]]
    }
  }
  ## Length is equal to 6
  if(upp.lngth == 6){
    n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
    wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
    if(upp[6] %in% names(n4$Children[[wl5]])){
      value <- n4$Children[[wl5]][[upp[6]]]
    }else{
      if("Parameters" %in% names(n4$Children[[wl5]])){
        wp <- grep(upp[6], n4$Children[[wl5]]$Parameters)
        if(length(wp) == 0) stop("Could not find parameter")
        value <- n4$Children[[wl5]]$Parameters[[wp]]$Value
      }else{
        wl6 <- which(upp[6] == sapply(n4$Children[[wl5]]$Children, function(x) x$Name))
        if(length(wl6) == 0) stop("Could not find parameter")
        value <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]][[upp[6]]]          
      }
    }
  }
  if(upp.lngth == 7){
    n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
    wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
    n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
    wl6 <- which(upp[6] == sapply(n5$Children, function(x) x$Name))
    value <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]][[upp[7]]]
    if(is.null(value)){
      n6 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]
      if("Command" %in% names(n6)){
        gpv <- grep(upp[7], n6$Command, value = TRUE)
        value <- as.numeric(strsplit(gpv, "=")[[1]][2])
      }
    }
  }
  if(upp.lngth == 8){
    n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
    wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
    n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
    wl6 <- which(upp[6] == sapply(n5$Children, function(x) x$Name))
    n6 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]
    wl7 <- which(upp[7] == sapply(n6$Children, function(x) x$Name))
    value <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]][[upp[8]]]
    if(is.null(value)){
      n7 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]
      if("Command" %in% names(n7)){
        gpv <- grep(upp[8], n7$Command, value = TRUE)
        value <- as.numeric(strsplit(gpv, "=")[[1]][2])
      }
    }
  }
  if(upp.lngth == 9){
    n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
    wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
    n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
    wl6 <- which(upp[6] == sapply(n5$Children, function(x) x$Name))
    n6 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]
    wl7 <- which(upp[7] == sapply(n6$Children, function(x) x$Name))
    n7 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]
    wl8 <- which(upp[8] == sapply(n7$Children, function(x) x$Name))
    value <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]$Children[[wl8]][[upp[9]]] 
    if(is.null(value)){
      n8 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]$Children[[wl8]]
      if("Command" %in% names(n8)){
        gpv <- grep(upp[9], n8$Command, value = TRUE)
        value <- as.numeric(strsplit(gpv, "=")[[1]][2])
      }
    }
  }
  if(upp.lngth == 10){
    n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
    wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
    n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
    wl6 <- which(upp[6] == sapply(n5$Children, function(x) x$Name))
    n6 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]
    wl7 <- which(upp[7] == sapply(n6$Children, function(x) x$Name))
    n7 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]
    wl8 <- which(upp[8] == sapply(n7$Children, function(x) x$Name))
    n8 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]$Children[[wl8]]
    wl9 <- which(upp[9] == sapply(n8$Children, function(x) x$Name))
    value <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]$Children[[wl8]]$Children[[wl9]][[upp[10]]]
    if(is.null(value)){
      n9 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]$Children[[wl8]]$Children[[wl9]]
      if("Command" %in% names(n9)){
        gpv <- grep(upp[10], n9$Command, value = TRUE)
        value <- as.numeric(strsplit(gpv, "=")[[1]][2])
      }
    }
  }
  return(value)
}

## Log-likelihood
log_lik <- function(.cfs){

  .file <- get('.file', envir = mcmc.apsimx.env)
  .src.dir <- get('.src.dir', envir = mcmc.apsimx.env)
  .parm.paths <- get('.parm.paths', envir = mcmc.apsimx.env)
  .data <- get('.data', envir = mcmc.apsimx.env)
  .iparms <- get('.iparms', envir = mcmc.apsimx.env)
  .index <- get('.index', envir = mcmc.apsimx.env)
  .parm.vector.index <- get('.parm.vector.index', envir = mcmc.apsimx.env)
  .replacement <- get('.replacement', envir = mcmc.apsimx.env)
  .root <- get('.root', envir = mcmc.apsimx.env)

  ## Need to edit the parameters in the simulation file or replacement
  for(i in 1:length(.iparms)){
    ## Edit the specific parameters with the corresponding values
    if(.parm.vector.index[i] <= 0){
      par.val <- .iparms[[i]] * .cfs[i]  
    }else{
      pvi <- .parm.vector.index[i]
      .iparms[[i]][pvi] <- .iparms[[i]][pvi] * .cfs[i]  
      par.val <- .iparms[[i]]
    }
    
    if(.replacement[i]){
      pp0 <- strsplit(.parm.paths[i], ".", fixed = TRUE)[[1]]
      mpp <- paste0(pp0[-length(pp0)], collapse = ".")
      edit_apsimx_replacement(file = .file, 
                              src.dir = .src.dir,
                              wrt.dir = .src.dir,
                              node.string = mpp,
                              overwrite = TRUE,
                              parm = pp0[length(pp0)],
                              value = par.val,
                              root = .root,
                              verbose = FALSE) 
    }else{
      edit_apsimx(file = .file, 
                  src.dir = .src.dir,
                  wrt.dir = .src.dir,
                  node = "Other",
                  parm.path = .parm.paths[i],
                  overwrite = TRUE,
                  value = par.val,
                  verbose = FALSE) 
    }
  }
  
  ## Run simulation  
  sim <- try(apsimx(file = .file, src.dir = .src.dir,
                    silent = TRUE, cleanup = TRUE, value = "report"),
             silent = TRUE)
  
  if(inherits(sim, "try-error")) return(NA)
  
  ## Only keep those columns with corresponding names in the data
  ## and only the dates that match those in 'data'
  if(!all(names(.data) %in% names(sim))) 
    stop("names in 'data' do not match names in simulation")

  if(length(.index) == 1){
    
    sim.s <- subset(sim, sim[,.index, drop = FALSE] %in% .data[[.index]], select = names(.data))  
    sim.s <- sim.s[order(sim.s[, .index]),]
    .data <- .data[order(.data[, .index]),]
    
    if(!all(sim.s[[.index]] == .data[[.index]]))
      stop(paste("simulations and data for", .index, "do not match"))        
    
  }else{
    
    if(!is.null(.data$report)) .data$report <- as.factor(.data$report)
    if(!is.null(sim$report)) sim$report <- as.factor(sim$report)
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
  
  if(nrow(sim.s) == 0L){
    cat("number of rows in sim", nrow(sim),"\n")
    cat("number of rows in data", nrow(.data), "\n")
    print(sim)
    print(.data)
    stop("no rows selected in simulations")
  } 
  ## Assuming they are aligned, get rid of the 'Date' column
  sim.s <- sim.s[,-which(names(sim.s) == .index)]
  .data <- .data[,-which(names(.data) == .index)]
  ## Now I need to calculate the residual sum of squares
  ## For this to work all variables should be numeric
  diffs <- as.matrix(.data) - as.matrix(sim.s)
  if(ncol(diffs) == 1){
    lls <- stats::dnorm(diffs[,1], sd = .cfs[length(.cfs)], log = TRUE)
    return(sum(lls))
  }else{
    Sigma <- diag(.cfs[(length(.iparms) + 1):length(.cfs)])
    lls <- mvtnorm::dmvnorm(diffs, sigma = Sigma, log = TRUE)
    return(sum(lls))    
  }
}

## Create an environment to solve this problem?
#' Create an apsimx environment for MCMC
#' 
#' @title Environment to store data for apsimx MCMC
#' @description Environment which stores data for MCMC
#' @return This is an environment, so nothing to return.
#' @export
#' 
mcmc.apsimx.env <- new.env(parent = emptyenv())


