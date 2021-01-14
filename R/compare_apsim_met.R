#' 
#' @title Compare two or more metfiles
#' @name compare_apsim_met
#' @rdname compare_apsim_met
#' @description Helper function which allows for a simple comparison among help files
#' @param ... met file objects. Should be of class 'met'
#' @param met.var meteorological variable to use in the comparison. Either \sQuote{all},
#' \sQuote{radn}, \sQuote{maxt}, \sQuote{mint}, \sQuote{rain}, \sQuote{rh}, 
#' \sQuote{wind_speed} or \sQuote{vp}. 
#' @param labels labels for plotting and identification of \sQuote{met} objects.
#' @param check whether to check met files using \sQuote{check_apsim_met}.
#' @note I have only tested this for 2 or 3 objects. The code is set up to be able to 
#' compare more, but I'm not sure that would be all that useful.
#' @export
#' @return object of class \sQuote{cmet}, which can be used for further plotting
#' @examples 
#' \dontrun{
#' require(nasapower)
#' ## Specify the location
#' lonlat <- c(-93, 42)
#' ## dates
#' dts <- c("2017-01-01","2017-12-31")
#' ## Get pwr
#' pwr <- get_power_apsim_met(lonlat = lonlat, dates = dts)
#' ## Get data from IEM
#' iem <- get_iem_apsim_met(lonlat = lonlat, dates = dts)
#' ## Compare them
#' cmet <- compare_apsim_met(pwr[,1:6], iem, labels = c("pwr","iem"))
#' ## Visualize radiation
#' plot(cmet, met.var = "radn")
#' plot(cmet, plot.type = "diff")
#' plot(cmet, plot.type = "ts")
#' ## Visualize maxt
#' plot(cmet, met.var = "maxt")
#' plot(cmet, met.var = "maxt", plot.type = "diff")
#' plot(cmet, met.var = "maxt", plot.type = "ts")
#' ## Cummulative rain
#' plot(cmet, met.var = "rain", plot.type = "ts", cummulative = TRUE)
#' }
#' 

compare_apsim_met <- function(..., 
                              met.var = c("all", "radn", "maxt", 
                                          "mint", "rain", "rh", 
                                          "wind_speed", "vp"),
                              labels,
                              check = FALSE){
  
  mets <- list(...)
  
  n.mets <- length(mets)
  
  met.var <- match.arg(met.var)
  
  if(n.mets < 2) stop("you should provide at least two met files")
  
  met1 <- mets[[1]]
  
  m.nms <- NULL
  if(!missing(labels)){
    m.nms <- labels
    if(length(labels) != n.mets)
      stop(" 'labels' lenght should be the same as the number of 'met' objects")
  } 
  
  if(!inherits(met1, "met")) stop("object should be of class 'met' ")
  
  ## Check for any issues
  if(check) check_apsim_met(met1)
  
  ## Create the 'date' for indexing
  nms1 <- names(met1)
  met.mrg <- met1
  yr <- as.character(met1$year[1])
  met.mrg$dates <- as.Date(0:c(nrow(met1) - 1), origin = as.Date(paste0(yr, "-01-01")))
  names(met.mrg) <- c(paste0(names(met1), ".1"), "dates")

  for(i in 2:n.mets){
    
    met.i <- mets[[i]]
    
    if(ncol(met1) != ncol(met.i)) stop("met files should have the same number of columns")
    if(all(!names(met1) %in% names(met.i))) stop("met files should have the same column names")
    if(check) check_apsim_met(met.i)
    
    yr <- as.character(met.i$year[1])
    met.i$dates <- as.Date(0:c(nrow(met.i) - 1), origin = as.Date(paste0(yr, "-01-01")))
    names(met.i) <- c(paste0(names(met1), ".", i), "dates")
    
    nms <- names(met.i)
    ## drop the year.i and day.i names
    met.mrg <- merge(met.mrg, met.i, by = "dates")
  }
  
  ## Calculate bias for all variables
  if(met.var == "all"){
    met.var.sel <- nms1[!(nms1 %in% c("year", "day", "dates"))]
    gvar.sel <- paste0(met.var.sel, collapse = "|")
    idx.met.mrg <- grep(gvar.sel, names(met.mrg))
    met.mrg.s <- met.mrg[,idx.met.mrg]
    
    ## Compute Bias matrix
    for(i in met.var.sel){
      cat("Variable ", i, "\n")
      tmp <- met.mrg.s[, grep(i, names(met.mrg.s))]
      if(ncol(tmp) < 2) stop("merged selected variables should be at least of length 2")

      for(j in 2:ncol(tmp)){
        cat(names(tmp)[j - 1], " vs. ", names(tmp)[j], "\n")
        if(!missing(labels))cat("labels", labels[j - 1], " vs. ", labels[j], "\n")
        fm0 <- lm(tmp[, j - 1] ~ tmp[, j])
        cat(" \t Bias: ", coef(fm0)[1], "\n")
        cat(" \t Slope: ", coef(fm0)[2], "\n")
        cat(" \t Corr: ", cor(tmp[,j - 1], tmp[, j]), "\n")
        cat(" \t RSS: ", deviance(fm0), "\n")
        cat(" \t RMSE: ", sigma(fm0), "\n")
      }
    }
  }
  
  if(met.var != "all"){
    ## Just select the appropriate variable
    idx.met.mrg <- grep(met.var, names(met.mrg))
    met.mrg.s <- met.mrg[,idx.met.mrg]
    
    cat("Variable ", met.var, "\n")
    tmp <- met.mrg.s
    for(j in 2:ncol(tmp)){
      cat(names(tmp)[j - 1], " vs. ", names(tmp)[j], "\n")
      if(!missing(labels))cat("labels", labels[j - 1], " vs. ", labels[j], "\n")
      fm0 <- lm(tmp[, j - 1] ~ tmp[, j])
      cat(" \t Bias: ", coef(fm0)[1], "\n")
      cat(" \t Slope: ", coef(fm0)[2], "\n")
      cat(" \t Corr: ", cor(tmp[,j - 1], tmp[, j]), "\n")
      cat(" \t RSS: ", deviance(fm0), "\n")
      cat(" \t RMSE: ", sigma(fm0), "\n")
    }
  }
  
  class(met.mrg) <- "met_mrg"
  attr(met.mrg, "met.names") <- m.nms
  attr(met.mrg, "length.mets") <- n.mets
  invisible(met.mrg)
}

#' Plotting function for weather data
#' @rdname compare_apsim_met
#' @description plotting function for compare_apsim_met, it requires ggplot2
#' @param x object of class \sQuote{met_mrg}
#' @param ... additional arguments, can be passed to certain ggplot2 functions
#' @param plot.type either \sQuote{vs}, \sQuote{diff}, \sQuote{ts} - for time series or \sQuote{density}
#' @param pairs pair of objects to compare, defaults to 1 and 2 but others are possible
#' @param cumulative whether to plot cumulative values (default FALSE)
#' @param met.var meteorological variable to plot 
#' @param id identification (not implemented yet)
#' @param span argument to be passed to \sQuote{geom_smooth}
#' @export
#' 
plot.met_mrg <- function(x, ..., plot.type = c("vs", "diff", "ts", "density"),
                         pairs = c(1, 2),
                         cumulative = FALSE,
                         met.var = c("radn", "maxt", "mint", "rain"),
                         id, span = 0.75){

  if(!requireNamespace("ggplot2", quietly = TRUE)){
    warning("ggplot2 is required for this plotting function")
    return(NULL)
  }
  
  m.nms <- attr(x, "met.names")
  if(max(pairs) > attr(x, "length.mets")) stop("pairs index larger than length of mets")
  
  x <- as.data.frame(unclass(x))
  
  plot.type <- match.arg(plot.type)
  met.var <- match.arg(met.var)
  
  if(cumulative && plot.type != "ts") 
    stop("cummulative is only available for plot.type = 'ts' ")
  
  if(plot.type == "vs" && met.var != "all" && !cumulative){
    tmp <- x[, grep(met.var, names(x))]
    prs <- paste0(met.var, ".", pairs)
    gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = eval(parse(text = eval(prs[1]))), 
                                  y = eval(parse(text = eval(prs[2]))))) +
                 ggplot2::geom_point() + 
                 ggplot2::xlab(paste(m.nms[pairs[1]], prs[1])) + 
                 ggplot2::ylab(paste(m.nms[pairs[2]], prs[2])) + 
                 ggplot2::geom_smooth(method = "lm") + 
                 ggplot2::geom_abline(intercept = 0, slope = 1, color = "orange")
    
    print(gp1)

  }
  
  if(plot.type == "diff" && met.var != "all" && !cumulative){
    
    prs0 <- paste0(met.var, ".", pairs)
    prs <- paste0(prs0, collapse = "|")
    tmp <- x[, grep(prs, names(x))]
    
    ## x Variable is prs[1]
    ## y Variable is prs[2] - prs[1]
    dff <- tmp[,prs0[2]] - tmp[,prs0[1]]
    
    gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = eval(parse(text = eval(prs0[1]))), 
                                                    y = dff)) +
      ggplot2::geom_point() + 
      ggplot2::xlab(paste(m.nms[pairs[1]], prs0[1])) + 
      ggplot2::ylab(paste("Difference", prs0[2], "-", prs0[1])) + 
      ggplot2::geom_smooth(method = "lm", ...) + 
      ggplot2::geom_hline(yintercept = 0, color = "orange")
  
    print(gp1)   
  }
  
  if(plot.type == "ts" && met.var != "all" && !cumulative){
    
    prs0 <- paste0(met.var, ".", pairs)
    prs <- paste0(prs0, collapse = "|")
    tmp <- x[, grep(prs, names(x))]
    tmp$dates <- x$dates ## Put it back in - kinda dumb 
    
    gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = .data[["dates"]], 
                                                    y = eval(parse(text = eval(prs0[1]))),
                                                    color = paste(m.nms[pairs[1]], prs0[1]))) +
                                                    
      ggplot2::geom_point() + 
      ggplot2::geom_smooth(span = span, ...) + 
      ggplot2::geom_point(ggplot2::aes(y = eval(parse(text = eval(prs0[2]))),
                                       color = paste(m.nms[pairs[2]], prs0[2]))) + 
      ggplot2::geom_smooth(ggplot2::aes(y = eval(parse(text = eval(prs0[2]))),
                                        color = paste(m.nms[pairs[2]], prs0[2])),
                           span = span, ...) + 
      ggplot2::xlab("Date") + 
      ggplot2::ylab(met.var) + 
      ggplot2::theme(legend.title = ggplot2::element_blank())
    
    print(gp1)   
  }
  
  if(plot.type == "ts" && met.var != "all" && cumulative){
    
    prs0 <- paste0(met.var, ".", pairs)
    prs <- paste0(prs0, collapse = "|")
    tmp <- x[, grep(prs, names(x))]
    tmp$dates <- x$dates
    tmp$cum_var1 <- cumsum(tmp[, prs0[1]])
    tmp$cum_var2 <- cumsum(tmp[, prs0[2]])
    
    gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = .data[["dates"]], 
                                                    y = .data[["cum_var1"]],
                                                    color = paste(m.nms[pairs[1]], prs0[1]))) +
      
      ggplot2::geom_line() + 
      ggplot2::geom_line(ggplot2::aes(y = .data[["cum_var2"]],
                                       color = paste(m.nms[pairs[2]], prs0[2]))) + 
      ggplot2::xlab("Date") + 
      ggplot2::ylab(paste("Cummulative ", met.var)) + 
      ggplot2::theme(legend.title = ggplot2::element_blank())
    
    print(gp1)   
  }
  
  if(plot.type == "density" && met.var != "all" && !cumulative){
    
    prs0 <- paste0(met.var, ".", pairs)
    prs <- paste0(prs0, collapse = "|")
    tmp <- x[, grep(prs, names(x))]
    
    gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = eval(parse(text = eval(prs0[1]))),
                                                    color = paste(m.nms[pairs[1]], prs0[1]))) + 
      ggplot2::geom_density() + 
      ggplot2::geom_density(ggplot2::aes(x = eval(parse(text = eval(prs0[2]))),
                                         color = paste(m.nms[pairs[2]], prs0[2]))) +
      ggplot2::xlab(met.var) + 
      ggplot2::theme(legend.title = ggplot2::element_blank())
    
    print(gp1)
  }
}




