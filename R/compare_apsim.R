#' 
#' @title Compare two or more apsim output objects
#' @name compare_apsim
#' @rdname compare_apsim
#' @description Function which allows for a simple comparison between APSIM output objects
#' @param ... data frames with APSIM output or observed data. 
#' @param variable specific variable to compare. By default all common ones are compared.
#' @param index index for merging objects. Default is \sQuote{Date}
#' @param by factor for splitting the comparison, such as a treatment effect.
#' @param labels labels for plotting and identification of objects.
#' @note \sQuote{Con Corr} is the concordance correlation coefficient (https://en.wikipedia.org/wiki/Concordance_correlation_coefficient);
#' \sQuote{ME} is the model efficiency (https://en.wikipedia.org/wiki/Nash%E2%80%93Sutcliffe_model_efficiency_coefficient)
#' @export
#' @return object of class \sQuote{out_mrg}, which can be used for further plotting
#' @examples 
#' \donttest{
#' ## Directory with files
#' extd.dir <- system.file("extdata", package = "apsimx")
#' ## Comparing observed and simulated for Wheat
#' data(obsWheat)
#' sim.opt <- read.csv(file.path(extd.dir, "wheat-sim-opt.csv"))
#' sim.opt$Date <- as.Date(sim.opt$Date)
#'
#' cap <- compare_apsim(obsWheat, sim.opt, labels = c("obs", "sim"))
#'
#' plot(cap)
#' plot(cap, plot.type = "diff")
#' plot(cap, plot.type = "ts")
#'
#' plot(cap, variable = "AboveGround")
#' plot(cap, variable = "AboveGround", plot.type = "diff")
#' plot(cap, variable = "AboveGround", plot.type = "ts")
#' }
#' 

compare_apsim <- function(..., 
                          variable,
                          index = "Date",
                          by,
                          labels){
  
  outs <- list(...)
  
  n.outs <- length(outs)
  
  if(n.outs < 2) stop("you should provide at least two data frames")
  
  out1 <- as.data.frame(outs[[1]])
  
  o.nms <- NULL
  if(!missing(labels)){
    o.nms <- labels
    if(length(labels) != n.outs)
      stop(" 'labels' lenght should be the same as the number of 'output' objects")
  } 
  
  if(!inherits(out1, "data.frame")) stop("object should be of class 'data.frame' ")

  ## Process out1
  nms1 <- names(out1)
  
  if(!index %in% nms1) 
    stop("index not found in data.frame")
  
  nms1.i <- intersect(nms1, names(outs[[2]])) ## Variables in common
  
  if(length(nms1.i) < 2) 
    stop("No names in common between the data.frames")
  
  ## The line below drops irrelevant columns and brings index to the first column
  out1 <- subset(out1, select = c(index, nms1.i[-which(nms1 == index)]))
  new.nms1 <- paste0(names(out1), ".1") ## This simply adds a 1
  out1.new.names <- gsub(paste0(index, ".1"), index, new.nms1) ## Rename Date.1 to Date
  out.mrg <- stats::setNames(out1, out1.new.names) 
  
  for(i in 2:n.outs){
    
    ## This selects only those columns present in the first object
    out.i <- subset(outs[[i]], 
                    select = c(index, nms1.i[-which(nms1.i == index)])) 
    nms.i <- names(out.i)
    new.nms.i <- paste0(nms.i, ".", i)
    names(out.i) <- gsub(paste0(index, ".", i), index, new.nms.i)
    out.mrg <- merge(out.mrg, out.i, by = index)
    
    if(nrow(out.mrg) < 1){
      stop("No data in common between data frames")
    }
  }
  
  ## Calculate bias for all variables
  if(missing(variable)){
    var.sel <- setdiff(nms1.i, index) ## All variables except index
    gvar.sel <- paste0(var.sel, collapse = "|")
    idx.out.mrg <- grep(gvar.sel, names(out.mrg))
    out.mrg.s <- out.mrg[, idx.out.mrg]
    
    ## Compute Bias matrix
    for(i in var.sel){
      cat("Variable ", i, "\n")
      tmp <- out.mrg.s[, grep(i, names(out.mrg.s))]
      if(ncol(tmp) < 2) stop("merged selected variables should be at least of length 2")
      
      for(j in 2:ncol(tmp)){
        cat(names(tmp)[j - 1], " vs. ", names(tmp)[j], "\n")
        if(!missing(labels))cat("labels", labels[j - 1], " vs. ", labels[j], "\n")
        fm0 <- lm(tmp[, j] ~ tmp[, j - 1])
        cat(" \t Bias: ", sum(tmp[, j - 1] - tmp[, j]), "\n")
        cat(" \t Intercept: ", coef(fm0)[1], "\n")
        cat(" \t Slope: ", coef(fm0)[2], "\n")
        cat(" \t RSS: ", deviance(fm0), "\n")
        cat(" \t RMSE: ", sigma(fm0), "\n")
        cat(" \t R^2: ", summary(fm0)$r.squared, "\n")
        cat(" \t Corr: ", cor(tmp[, j - 1], tmp[, j]), "\n")
        cat(" \t Con. Corr: ", con_cor(tmp[, j - 1], tmp[, j]), "\n")
        cat(" \t ME: ", mod_eff(tmp[, j - 1], tmp[, j]), "\n")

      }
    }
  }
  
  if(!missing(variable)){
    ## Just select the appropriate variable
    idx.out.mrg <- grep(variable, names(out.mrg))
    out.mrg.s <- out.mrg[, idx.out.mrg]
    
    cat("Variable ", variable, "\n")
    tmp <- out.mrg.s
    for(j in 2:ncol(tmp)){
      cat(names(tmp)[j - 1], " vs. ", names(tmp)[j], "\n")
      if(!missing(labels))cat("labels", labels[j - 1], " vs. ", labels[j], "\n")
      fm0 <- lm(tmp[, j - 1] ~ tmp[, j])
      cat(" \t Bias: ", sum(tmp[, j - 1] - tmp[, j]), "\n")
      cat(" \t Intercept: ", coef(fm0)[1], "\n")
      cat(" \t Slope: ", coef(fm0)[2], "\n")
      cat(" \t RSS: ", deviance(fm0), "\n")
      cat(" \t RMSE: ", sigma(fm0), "\n")
      cat(" \t R^2: ", summary(fm0)$r.squared, "\n")
      cat(" \t Corr: ", cor(tmp[,j - 1], tmp[, j]), "\n")
      cat(" \t Con. Corr: ", con_cor(tmp[, j - 1], tmp[, j]), "\n")
      cat(" \t ME: ", mod_eff(tmp[, j-1], tmp[, j]), "\n")
    }
  }
  
  class(out.mrg) <- "out_mrg"
  attr(out.mrg, "out.names") <- o.nms
  attr(out.mrg, "length.outs") <- n.outs
  attr(out.mrg, "index") <- index
  invisible(out.mrg)
}

#' Plotting function for weather data
#' @rdname compare_apsim
#' @description plotting function for compare_apsim, it requires ggplot2
#' @param x object of class \sQuote{out_mrg}
#' @param ... data frames with APSIM output or observed data. 
#' @param plot.type either \sQuote{vs}, \sQuote{diff}, \sQuote{ts} - for time series or \sQuote{density}
#' @param pairs pair of objects to compare, defaults to 1 and 2 but others are possible
#' @param cumulative whether to plot cummulative values (default FALSE)
#' @param variable variable to plot 
#' @param id identification (not implemented yet)
#' @param span argument passed to \sQuote{geom_smooth}
#' @export
#' 
plot.out_mrg <- function(x, ..., plot.type = c("vs", "diff", "ts", "density"),
                         pairs = c(1, 2),
                         cumulative = FALSE, ## Might not make sense for these type of graphs...
                         variable,
                         id, span = 0.75){
  
  if(!requireNamespace("ggplot2", quietly = TRUE)){
    warning("ggplot2 is required for this plotting function")
    return(NULL)
  }
  
  o.nms <- attr(x, "out.names")
  if(max(pairs) > attr(x, "length.outs")) stop("pairs index larger than length of outs")
  
  index <- attr(x, "index") ## This is normally 'Date'
  x <- as.data.frame(unclass(x))

  if(missing(variable)){
    variable <- gsub(".1", "", names(x)[2], fixed = TRUE)
  }else{
    variable <- gsub(".1", "", grep(variable, names(x), value = TRUE)[1], fixed = TRUE)
  } 
  
  if(is.na(variable) || length(variable) == 0) stop("variable not found")
  
  plot.type <- match.arg(plot.type)
  
  if(cumulative && plot.type != "ts") 
    stop("cumulative is only available for plot.type = 'ts' ")
  
  if(plot.type == "vs" && !cumulative){
    tmp <- x[, grep(variable, names(x))]
    prs <- paste0(variable, ".", pairs)
    gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = eval(parse(text = eval(prs[1]))), 
                                                    y = eval(parse(text = eval(prs[2]))))) +
      ggplot2::geom_point() + 
      ggplot2::xlab(paste(o.nms[pairs[1]], prs[1])) + 
      ggplot2::ylab(paste(o.nms[pairs[2]], prs[2])) + 
      ggplot2::geom_smooth(method = "lm", ...) + 
      ggplot2::geom_abline(intercept = 0, slope = 1, color = "orange")
    
    print(gp1)
    
  }
  
  if(plot.type == "diff" && !cumulative){
    
    prs0 <- paste0(variable, ".", pairs)
    prs <- paste0(prs0, collapse = "|")
    tmp <- x[, grep(prs, names(x))]
    
    ## x Variable is prs[1]
    ## y Variable is prs[2] - prs[1]
    dff <- tmp[,prs0[2]] - tmp[,prs0[1]]
    
    gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = eval(parse(text = eval(prs0[1]))), 
                                                    y = dff)) +
      ggplot2::geom_point() + 
      ggplot2::xlab(paste(o.nms[pairs[1]], prs0[1])) + 
      ggplot2::ylab(paste("Difference", prs0[2], "-", prs0[1])) + 
      ggplot2::geom_smooth(method = "lm", ...) + 
      ggplot2::geom_hline(yintercept = 0, color = "orange")
    
    print(gp1)   
  }
  
  if(plot.type == "ts" && !cumulative){
  
    prs0 <- paste0(variable, ".", pairs)
    prs <- paste0(prs0, collapse = "|")
    tmp <- x[, grep(prs, names(x))]
    tmp[[index]] <- x[[index]] ## Put it back in - kinda dumb 
    
    gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = .data[[index]], 
                                                    y = eval(parse(text = eval(prs0[1]))),
                                                    color = paste(o.nms[pairs[1]], prs0[1]))) +
      
      ggplot2::geom_point() + 
      ggplot2::geom_smooth(span = span, ...) + 
      ggplot2::geom_point(ggplot2::aes(y = eval(parse(text = eval(prs0[2]))),
                                       color = paste(o.nms[pairs[2]], prs0[2]))) + 
      ggplot2::geom_smooth(ggplot2::aes(y = eval(parse(text = eval(prs0[2]))),
                                        color = paste(o.nms[pairs[2]], prs0[2])),
                           span = span, ...) + 
      ggplot2::xlab(index) + 
      ggplot2::ylab(variable) + 
      ggplot2::theme(legend.title = ggplot2::element_blank())
    
    print(gp1)   
  }
  
  if(plot.type == "ts" && cumulative){
    
    prs0 <- paste0(variable, ".", pairs)
    prs <- paste0(prs0, collapse = "|")
    tmp <- x[, grep(prs, names(x))]
    tmp[[index]] <- x[[index]]
    tmp$cum_var1 <- cumsum(tmp[, prs0[1]])
    tmp$cum_var2 <- cumsum(tmp[, prs0[2]])
    
    gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = .data[[index]], 
                                                    y = .data[["cum_var1"]],
                                                    color = paste(o.nms[pairs[1]], prs0[1]))) +
      
      ggplot2::geom_line() + 
      ggplot2::geom_line(ggplot2::aes(y = .data[["cum_var2"]],
                                      color = paste(o.nms[pairs[2]], prs0[2]))) + 
      ggplot2::xlab(index) + 
      ggplot2::ylab(paste("Cumulative ", variable)) + 
      ggplot2::theme(legend.title = ggplot2::element_blank())
    
    print(gp1)   
  }
  
  if(plot.type == "density"){
    
    prs0 <- paste0(variable, ".", pairs)
    prs <- paste0(prs0, collapse = "|")
    tmp <- x[, grep(prs, names(x))]
    
    gp1 <- ggplot2::ggplot(data = tmp, ggplot2::aes(x = eval(parse(text = eval(prs0[1]))),
                                                    color = paste(o.nms[pairs[1]], prs0[1]))) + 
      ggplot2::geom_density() + 
      ggplot2::geom_density(ggplot2::aes(x = eval(parse(text = eval(prs0[2]))),
                                         color = paste(o.nms[pairs[2]], prs0[2]))) +
      ggplot2::xlab(variable) + 
      ggplot2::theme(legend.title = ggplot2::element_blank())
    
    print(gp1)
  }
}

## Model efficiency
## https://en.wikipedia.org/wiki/Nash%E2%80%93Sutcliffe_model_efficiency_coefficient
mod_eff <- function(x, y){
  sse <- sum((x - y)^2)
  ssx <- sum((x - mean(x))^2)
  ans <- 1 - sse / ssx
  ans
}

## Concordance correlation
## https://en.wikipedia.org/wiki/Concordance_correlation_coefficient
con_cor <- function(x, y){
  num <- 2 * cor(x, y) * sd(x) * sd(y)
  den <- var(x) + var(y) + (mean(x) - mean(y))^2
  ans <- num/den
  ans
}
