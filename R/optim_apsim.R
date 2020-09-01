#'
#' Simple optimization for APSIM Classic
#' 
#' This function assumes that you want to optimize parameters which are stored in
#' an auxiliary XML file. These are typically crop or cultivar specific parameters
#' 
#' Only one observation per day is allowed in the data
#' 
#' The crop file should be in the same directory as the main simulation
#' 
#' The initial values for the optimization should be the ones in the stored
#' crop parameter file
#' 
#' @title Optimize parameters in an APSIM simulation
#' @name optim_apsim
#' @rdname optim_apsim
#' @description It is a wrapper for running APSIM and optimizing parameters using \code{\link{optim}}
#' @param file file name to be run (the extension .apsim is optional)
#' @param src.dir directory containing the .apsim file to be run (defaults to the current directory)
#' @param crop.file name of auxiliary xml file where parameters are stored. If this is missing, it is 
#'                  assumed that the parameters to be edited are in the main simulation file.
#' @param parm.paths absolute or relative paths of the coefficients to be optimized. 
#'             It is recommended that you use \code{\link{inspect_apsim}} for this
#' @param data data frame with the observed data. By default is assumes there is a 'Date' column for the index.
#' @param method Method used in the optimization. For now, only \sQuote{optim} is used.
#' @param weights Weighting method or values for computing the residual sum of squares. 
#' @param index Index for filtering APSIM output
#' @param ... additional arguments to be passed to the optimization algorithm
#' @note When computing the objective function (residual sum-of-squares) different variables are combined.
#' It is common to weight them since they are in different units. If the argument weights is not supplied
#' no weighting is applied. It can be 'mean', 'variance' or a numeric vector of appropriate length.
#' @return vector of optimized coefficients
#' @export
#' 
#' 
#' 

optim_apsim <- function(file, src.dir = ".", 
                        crop.file, parm.paths, data, 
                        method = c("optim"), 
                        weights, index = "Date",
                        ...){
  
  .check_apsim_name(file)
  
  if(src.dir != ".") stop("At the moment it is not possible \n
                          to change the source directory.")
  ## Create tmp.dir where to store tmp copy of crop file
  # crop.file.tmp <- paste0(tools::file_path_sans_ext(crop.file),
  #                         "-opt.xml")
  # file.copy(from = file.path(src.dir, crop.file),
  #           to = file.path(src.dir, crop.file.tmp))
  # 
  ## The might offer suggestions in case there is a typo in 'file'
  file.names <- dir(path = src.dir, pattern=".apsim$", ignore.case = TRUE)
  
  if(length(file.names) == 0){
    stop("There are no .apsim files in the specified directory to run.")
  }
  
  file <- match.arg(file, file.names)
  
  file.name.path <- file.path(src.dir, file)
  
  if(index == "Date") data$Date <- as.Date(data$Date)
  if(index != "Date") stop("For now Date is the default index")
  
  ## Setting up weights
  if(missing(weights)) weights <- rep(1, length(parm.paths))
  if(weights == "mean") weights <- abs(1 / apply(data, 2, mean))
  if(weights == "var") weights <- abs(1 / apply(data, 2, var))
  
  ## What this does is pick the crop.file to be edited when it is not missing
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
  
  obj_fun <- function(cfs, parm.paths, data, aux.file, iaux.parms, weights, cfile = TRUE){
    
    ## Need to edit the parameters in the crop file or the main simulation
    for(i in seq_along(cfs)){
      ## Retrieve the vector of current parameters
      mparm <- paste(iaux.parms[[i]] * cfs[i], collapse = " ")
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
                 aux.file = aux.file,
                 iaux.parms = iaux.parms,
                 weights = weights,
                 cfile = cfile)
  ## optimization
  op <- optim(par = rep(1, length(parm.paths)), 
              fn = obj_fun, 
              parm.paths = parm.paths, 
              data = data, 
              aux.file = aux.file, 
              iaux.parms = iaux.parms,
              weights = weights,
              cfile = cfile,
              ...)
  
  ans <- structure(list(rss = rss, iaux.parms = iaux.parms, op = op),
                   class = "optim_apsim")
  return(ans)
}

#' @rdname optim_apsim
#' @description Friendly printing of optim_apsim
#' @param x object of class \sQuote{optim_apsim}
#' @param ... additional arguments (none used at the moment)
#' @param digits number of digits to round up the output
#' @export
#' 
print.optim_apsim <- function(x, ..., digits = 3){

  cat("Initial values: \n")
  
  for(i in seq_along(x$iaux.parms)){
    

    cat("\t Parameter path: ", names(x$iaux.parms)[i], "\n")
    cat("\t Values: ", x$iaux.parms[[i]], "\n")
  }   
  cat("\t Pre-optimized RSS: ", x$rss, "\n")
  cat("Optimized values: \n")
  
  for(i in seq_along(x$iaux.parms)){
    
    cat("\t Parameter path: ", names(x$iaux.parms)[i], "\n")
    cat("\t Values: ", round(x$iaux.parms[[i]] * x$op$par[i], digits), "\n")
  }   
  cat("\t Optimized RSS: ", x$op$value, "\n")
  cat("Convergence:", x$op$convergence,"\n")
}
  
  
  