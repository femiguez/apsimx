#'
#' Simple optimization for APSIM Next Generation
#' 
#' Only one observation per day is allowed in the data
#' 
#' 
#' @title Optimize parameters in an APSIM Next Generation simulation
#' @name optim_apsimx
#' @rdname optim_apsimx
#' @description It is a wrapper for running APSIM-X and optimizing parameters using \code{\link{optim}}
#' @param file file name to be run (the extension .apsimx is optional)
#' @param src.dir directory containing the .apsimx file to be run (defaults to the current directory)
#' @param parm.paths absolute paths of the coefficients to be optimized. 
#'             It is recommended that you use \code{\link{inspect_apsimx}} for this
#' @param data data frame with the observed data. By default is assumes there is a 'Date' column for the index.
#' @param method Method used in the optimization. For now, only \sQuote{optim} is used.
#' @param weights Weighting method or values for computing the residual sum of squares. 
#' @param index Index for filtering APSIM output
#' @param replacement TRUE or FALSE for each parameter. Indicating whether it is part of 
#' the \sQuote{replacement} component. Its length should be equal to the length or \sQuote{parm.paths}.
#' @param root root argument for \code{\link{edit_apsimx_replacement}}
#' @param ... additional arguments to be passed to the optimization algorithm
#' @note When computing the objective function (residual sum-of-squares) different variables are combined.
#' It is common to weight them since they are in different units. If the argument weights is not supplied
#' no weighting is applied. It can be 'mean', 'variance' or a numeric vector of appropriate length.
#' @return vector of optimized coefficients
#' @export
#' 
#' 
#' 

optim_apsimx <- function(file, src.dir = ".", 
                         parm.paths, data, 
                         method = c("optim"), 
                         weights, index = "Date",
                         replacement,
                         root,
                         ...){
  
  .check_apsim_name(file)

  ## The might offer suggestions in case there is a typo in 'file'
  file.names <- dir(path = src.dir, pattern=".apsimx$", ignore.case = TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsimx files in the specified directory to run.")
  }
  
  file <- match.arg(file, file.names)
  
  file.name.path <- file.path(src.dir, file)
  
  if(index == "Date") data$Date <- as.Date(data$Date)
  if(index != "Date") stop("For now Date is the default index")
  
  ## Setting up weights
  if(missing(weights)) weights <- rep(1, length(parm.paths))
  if(weights == "mean") weights <- abs(1 / apply(data, 2, mean))
  if(weights == "var") weights <- abs(1 / apply(data, 2, var))
  
  ## Set up replacement
  if(missing(replacement)) replacement <- rep(FALSE, length(parm.paths))
  
  ## If root is not present
  if(missing(root)) root <- list("Models.Core.Replacements", NA)

  ## Read apsimx file (JSON)
  apsimx_json <- jsonlite::read_json(file.path(src.dir, file))
  
  ## Build the initial values
  iparms <- vector("list", length = length(parm.paths))
  names(iparms) <- parm.paths
  
  ## How do I retrieve the current value I want to optimize?
  ## No idea at the moment...
  for(i in seq_along(parm.paths)){
    parm.val <- extract_values_apsimx(file = file, src.dir = src.dir, 
                                      parm.path = parm.paths[i])
    iparms[[i]] <- as.numeric(parm.val)
  }
  
  obj_fun <- function(cfs, parm.paths, data, iparms, weights, replacement, root){
    
    ## Need to edit the parameters in the simulation file or replacement
    for(i in seq_along(cfs)){
      ## Edit the specific parameters with the corresponding values
      par.val <- iparms[i] * cfs[i]
      if(replacement[i]){
        pp0 <- strsplit(parm.paths[i], ".", fixed = TRUE)[[1]]
        mpp <- paste0(pp0[-length(pp0)], collapse = ".")
        edit_apsimx_replacement(file = file, 
                                src.dir = src.dir,
                                node.string = mpp,
                                overwrite = TRUE,
                                parm = pp0[length(pp0)],
                                value = par.val,
                                root = root,
                                verbose = FALSE) 
        }else{
          edit_apsimx(file = file, 
                      src.dir = src.dir,
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
    
    ## Only keep those columns with corresponding names in the data
    ## and only the dates that match those in 'data'
    if(!all(names(data) %in% names(sim))) 
      stop("names in 'data' do not match names in simulation")
    
    sim.s <- subset(sim, Date %in% data$Date, select = names(data))
    
    if(nrow(sim.s) == 0L) stop("no rows selected in simulations")
    ## Assuming they are aligned, get rid of the 'Date' column
    sim.s$Date <- NULL; data$Date <- NULL
    ## Now I need to calculate the residual sum of squares
    ## For this to work all variables should be numeric
    diffs <- as.matrix(data) - as.matrix(sim.s)
    rss <- sum((diffs * weights)^2)
    return(rss)
  }
  
  ## Pre-optimized RSS
  rss <- obj_fun(cfs = rep(1, length(parm.paths)),
                 parm.paths = parm.paths,
                 data = data,
                 iparms = iparms,
                 weights = weights,
                 replacement = replacement,
                 root = root)
  ## optimization
  op <- optim(par = rep(1, length(parm.paths)), 
              fn = obj_fun, 
              parm.paths = parm.paths, 
              data = data, 
              iparms = iparms,
              weights = weights,
              replacement = replacement,
              root = root,
              ...)
  
  ans <- structure(list(rss = rss, iaux.parms = iparms, op = op),
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
#' @export
#' @examples 
#' \donttest{
#' ## Find examples
#' ex.dir <- auto_detect_apsimx_examples()
#' ## Extract parameter path
#' pp <- inspect_apsimx("Barley.apsimx", src.dir = ex.dir,
#'                      node = "Manager", parm = list("Fert", 1))
#' ppa <- paste0(pp, ".Amount")
#' ## Extract value
#' extract_values_apsimx("Barley.apsimx", src.dir = ex.dir, parm.path = ppa)
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
  }
  if(upp.lngth == 8){
    n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
    wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
    n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
    wl6 <- which(upp[6] == sapply(n5$Children, function(x) x$Name))
    n6 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]
    wl7 <- which(upp[7] == sapply(n6$Children, function(x) x$Name))
    value <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]][[upp[8]]]
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
  }
  return(value)
}
