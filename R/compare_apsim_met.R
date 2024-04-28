#' 
#' @title Compare two or more metfiles
#' @name compare_apsim_met
#' @rdname compare_apsim_met
#' @description Helper function which allows for a simple comparison among \sQuote{met} objects
#' @param ... met file objects. Should be of class \sQuote{met}
#' @param met.var meteorological variable to use in the comparison. Either \sQuote{all},
#' \sQuote{radn}, \sQuote{maxt}, \sQuote{mint}, \sQuote{rain}, \sQuote{rh}, 
#' \sQuote{wind_speed} or \sQuote{vp}. 
#' @param labels labels for plotting and identification of \sQuote{met} objects.
#' @param check whether to check \sQuote{met} objects using \sQuote{check_apsim_met}.
#' @param verbose whether to print agreement stats to console (default is FALSE).
#' @note I have only tested this for 2 or 3 objects. The code is set up to be able to 
#' compare more, but I'm not sure that would be all that useful.
#' @export
#' @return object of class \sQuote{met_mrg}, which can be used for further plotting
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
#' ## Cumulative rain
#' plot(cmet, met.var = "rain", plot.type = "ts", cumulative = TRUE)
#' }
#' 

compare_apsim_met <- function(..., 
                              met.var = c("all", "radn", "maxt", 
                                          "mint", "rain", "rh", 
                                          "wind_speed", "vp"),
                              labels,
                              check = FALSE,
                              verbose = FALSE){
  
  mets <- list(...)
  
  n.mets <- length(mets)
  
  met.var <- match.arg(met.var)
  
  if(n.mets < 2) stop("you should provide at least two met objects", call. = FALSE)
  
  met1 <- mets[[1]]
  
  if(!inherits(met1, "met"))
    stop("The first object should be of class 'met'", call. = FALSE)
  
  m.nms <- NULL
  if(!missing(labels)){
    m.nms <- labels
    if(length(labels) != n.mets)
      stop(" 'labels' length should be the same as the number of 'met' objects", call. = FALSE)
  } 
  
  if(!inherits(met1, "met")) stop("object should be of class 'met' ", call. = FALSE)
  
  ## Check for any issues
  if(check) check_apsim_met(met1)
  
  ## Create the 'date' for indexing
  nms1 <- names(met1)
  met.mrg <- as.data.frame(met1)
  yr <- as.character(met1$year[1])
  met.mrg$dates <- as.Date(0:c(nrow(met1) - 1), origin = as.Date(paste0(yr, "-01-01")))
  names(met.mrg) <- c(paste0(names(met1), ".1"), "dates")

  for(i in 2:n.mets){
    
    if(!inherits(mets[[i]], "met")){
      stp.mssg <- paste("Object in position:", i, "is of class:", class(met.i),
                        ". Was expecting an object of class 'met'.")
      stop(stp.mssg, call. = FALSE)
    }
    
    met.i <- as.data.frame(mets[[i]])

    if(ncol(met1) != ncol(met.i)) stop("met objects should have the same number of columns", call. = FALSE)
    if(all(!names(met1) %in% names(met.i))) stop("met objects should have the same column names", call. = FALSE)
    if(check) check_apsim_met(met.i)
    
    yr <- as.character(met.i$year[1])
    met.i$dates <- as.Date(0:c(nrow(met.i) - 1), origin = as.Date(paste0(yr, "-01-01")))
    names(met.i) <- c(paste0(names(met1), ".", i), "dates")
    
    nms <- names(met.i)
    ## drop the year.i and day.i names
    met.mrg <- merge(met.mrg, met.i, by = "dates")
  }
  
  if(met.var == "all"){
    vrs <- rep(setdiff(names(met1), c("year", "day")), each = n.mets - 1)
    ans <- data.frame(variable = vrs,
                      vs = NA, labels = NA,
                      bias = NA, slope = NA, corr = NA)
    if(missing(labels)) ans$labels <- NULL
  }else{
    ans <- data.frame(variable = rep(met.var, n.mets - 1),
                      vs = NA, labels = NA,
                      bias = NA, slope = NA, corr = NA)
    if(missing(labels)) ans$labels <- NULL
  }
  
  ## Calculate bias for all variables
  if(met.var == "all"){
    met.var.sel <- nms1[!(nms1 %in% c("year", "day", "dates"))]
    gvar.sel <- paste0(met.var.sel, collapse = "|")
    idx.met.mrg <- grep(gvar.sel, names(met.mrg))
    met.mrg.s <- met.mrg[,idx.met.mrg]

    k <- 1  
    z <- 1
    ## Compute Bias matrix
    for(i in met.var.sel){
      if(verbose) cat("Variable: ", i, "\n")
      ans$variable[k] <- i
      tmp <- met.mrg.s[, grep(i, names(met.mrg.s))]
      if(ncol(tmp) < 2) stop("merged selected variables should be at least of length 2", call. = FALSE)

      for(j in 2:ncol(tmp)){
        if(verbose) cat(names(tmp)[j - 1], " vs. ", names(tmp)[j], "\n")
        ans$vs[k] <- paste(names(tmp)[j - 1], "vs.", names(tmp)[j])
        if(!missing(labels)){
          if(verbose) cat("labels:", labels[j - 1], " vs. ", labels[j], "\n")
          ans$labels[k] <- paste(labels[j - 1], "vs.", labels[j])
        } 
        fm0 <- lm(tmp[, j - 1] ~ tmp[, j])
        if(verbose) cat(" \t Bias: ", coef(fm0)[1], "\n")
        ans$bias[k] <- coef(fm0)[1]
        if(verbose) cat(" \t Slope: ", coef(fm0)[2], "\n")
        ans$slope[k] <- coef(fm0)[2]
        if(verbose) cat(" \t Corr: ", cor(tmp[,j - 1], tmp[, j]), "\n")
        ans$corr[k] <- cor(tmp[,j - 1], tmp[, j])
        if(verbose) cat(" \t RSS: ", deviance(fm0), "\n")
        ans$rss[k] <- deviance(fm0)
        if(verbose) cat(" \t RMSE: ", sigma(fm0), "\n")
        ans$rmse[k] <- sigma(fm0)
        k <- k + 1
      }
    }
  }
  
  if(met.var != "all"){
    ## Just select the appropriate variable
    idx.met.mrg <- grep(met.var, names(met.mrg))
    met.mrg.s <- met.mrg[, idx.met.mrg]
    
    if(verbose) cat("Variable ", met.var, "\n")
    ans$variable[1] <- met.var
    
    tmp <- met.mrg.s
    for(j in 2:ncol(tmp)){
      if(verbose) cat(names(tmp)[j - 1], " vs. ", names(tmp)[j], "\n")
      ans$vs[j - 1] <- paste(names(tmp)[j - 1], "vs.", names(tmp)[j])
      if(!missing(labels)){
        if(verbose) cat("labels", labels[j - 1], " vs. ", labels[j], "\n")
        ans$labels[j - 1] <- paste(labels[j - 1], "vs.", labels[j])
      }
      fm0 <- lm(tmp[, j - 1] ~ tmp[, j])
      if(verbose) cat(" \t Bias: ", coef(fm0)[1], "\n")
      ans$bias[j - 1] <- coef(fm0)[1]
      if(verbose) cat(" \t Slope: ", coef(fm0)[2], "\n")
      ans$slope[j - 1] <- coef(fm0)[2]
      if(verbose) cat(" \t Corr: ", cor(tmp[,j - 1], tmp[, j]), "\n")
      ans$corr[j - 1] <- cor(tmp[,j - 1], tmp[, j])
      if(verbose) cat(" \t RSS: ", deviance(fm0), "\n")
      ans$rss[j - 1] <- deviance(fm0)
      if(verbose) cat(" \t RMSE: ", sigma(fm0), "\n")
      ans$rmse <- sigma(fm0)
    }
  }

  attr(met.mrg, "met.names") <- m.nms
  attr(met.mrg, "length.mets") <- n.mets  
  met.mrg <- structure(list(met.mrg = met.mrg, index.table = ans),
                       class = "met_mrg")
  invisible(met.mrg)
}

#' print method for met_mrg
#' @rdname compare_apsim_met
#' @description print method for \sQuote{met_mrg}
#' @param x object of class \sQuote{met_mrg}
#' @param ... additional arguments passed to print
#' @param digits digits to print (default is 2)
#' @export
#' @return it prints the index.table data.frame
print.met_mrg <- function(x, ..., digits = 2){
  print(x$index.table, digits = digits)
}

#' Plotting function for weather data
#' @rdname compare_apsim_met
#' @description plotting function for compare_apsim_met, it requires ggplot2
#' @param x object of class \sQuote{met_mrg}
#' @param ... met file objects. Should be of class \sQuote{met}
#' @param plot.type either \sQuote{vs}, \sQuote{diff}, \sQuote{ts} - for time series or \sQuote{density}
#' @param pairs pair of objects to compare, defaults to 1 and 2 but others are possible
#' @param cumulative whether to plot cumulative values (default FALSE)
#' @param met.var meteorological variable to plot 
#' @param id identification (not implemented yet)
#' @param span argument to be passed to \sQuote{geom_smooth}
#' @return it produces a plot
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
  
  x <- x$met.mrg
  
  m.nms <- attr(x, "met.names")
  if(max(pairs) > attr(x, "length.mets")) stop("pairs index larger than length of mets")
  
  x <- as.data.frame(unclass(x))
  
  plot.type <- match.arg(plot.type)
  met.var <- match.arg(met.var)
  
  if(cumulative && plot.type != "ts") 
    stop("cumulative is only available for plot.type = 'ts' ")
  
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
      ggplot2::ylab(paste("Cumulative ", met.var)) + 
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
  invisible(gp1)
}

#' The frost free period is computed by first spliting each year (or year interval)
#' in two halves. The first and last frosts in the first and second period are found.
#' For the Northern hemisphere calendar days are used (1-365).
#' For the Southern hemisphere the year is split in two halfs, but the second half of
#' the year is used as the first part of the growing season.
#' If frost is not found a zero is returned.
#'
#' @title Summary for an APSIM met file
#' @name summary.met
#' @description Create a data.frame summarizing an object of class \sQuote{met}
#' @param object object of class \sQuote{met}
#' @param ... optional argument (none used at the moment) 
#' @param years optional argument to subset years
#' @param months optional argument to subset by months. If an integer, it should
#' be between 1 and 12. If a character, it can be in the format, for example, 
#' \sQuote{jan} or \sQuote{Jan}.
#' @param days optional argument to subset by days. It should be an integer
#'  between 1 and 31. 
#' @param julian.days optional argument to subset by julian days. It
#' should be a vector of integers between 1 and 365. Either use \sQuote{days} or
#' \sQuote{julian.days} but not both.
#' @param compute.frost logical (default FALSE). Whether to compute
#' frost statistics.
#' @param frost.temperature value to use for the calculation of the frost 
#' period (default is zero).
#' @param anomaly whether to compute the anomaly. Default is FALSE. 
#' It could be TRUE (for all variables) or a character vector for a specific set of variables.
#' @param check logical (default FALSE). Whether to \sQuote{check} the \sQuote{met} object.
#' @param verbose whether to print additional infomation to the console
#' @param na.rm whether to remove missing values. Passed to \sQuote{aggregate}
#' @param digits digits for rounding (default is 2).
#' @return an object of class \sQuote{data.frame} with attributes
#' @export
#' @examples 
#' 
#' extd.dir <- system.file("extdata", package = "apsimx")
#' ames <- read_apsim_met("Ames.met", src.dir = extd.dir)
#' 
#' summary(ames, years = 2014:2016)
#' 
summary.met <- function(object, ..., years, months, days, julian.days,
                        compute.frost = FALSE,
                        frost.temperature = 0, 
                        anomaly,
                        check = FALSE, verbose = FALSE, 
                        na.rm = FALSE, digits = 2){
  
  x <- object
  if(check) check_apsim_met(x)
  
  if(!missing(days) && !missing(julian.days))
    stop("Either use days or julian.days but not both", call. = TRUE)

  ## Summarize information by year
  if(!missing(years)){
    #### Error message if years outside the range
    x <- x[x$year %in% years,]
  } 
  
  if(!missing(months)){
    if(length(months) == 1) months <- as.integer(months)
    if(!inherits(months, "integer") && !inherits(months, "character"))
      stop("months should be either an integer or a character", call. = FALSE)
    ## Select months that fit the criteria
    date.range <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by = "day")
    dat <- data.frame(month = as.numeric(format(date.range, "%m")),
                      Month = format(date.range, "%b"),
                      day = as.numeric(format(date.range, "%j")))
    if(inherits(months, "integer")){
      if(any(months < 1) || any(months > 12))
        stop("months should be between 1 and 12", call. = FALSE)
      wch.months <- which(dat$month %in% months)
      x <- x[x$day %in% dat[wch.months, "day"],]
    }
    if(inherits(months, "character")){
      ## Months might be in upper of lower case
      Months <- format(as.Date(paste0(1, months, 2012), "%d%b%Y"), "%b")
      wch.months <- which(dat$Month %in% Months)
      x <- x[x$day %in% dat[wch.months, "day"],]
    }
  }else{
    months <- "1:12"
  }
  
  if(!missing(days)){
    if(!inherits(days, "integer"))
      stop("days should be of class integer", call. = FALSE)
    if(any(days < 1) || any(days > 31))
      stop("days should be between 1 and 31")
    ## Select days that fit the criteria
    date.range <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by = "day")
    dat <- data.frame(month = as.numeric(format(date.range, "%m")),
                      Month = format(date.range, "%b"),
                      Day = as.numeric(format(date.range, "%d")),
                      day = as.numeric(format(date.range, "%j")))
    if(is.numeric(days)){
      wch.days <- which(dat$Day %in% days)
      x <- x[x$day %in% dat[wch.days, "day"],]
    }
  }else{
    days <- 1:31
  }
  
  if(!missing(julian.days)){
    x <- x[x$day %in% julian.days, ]
    days <- range(julian.days)
  }
  
  n.years <- length(unique(x$year))
  
  if(compute.frost){
    ans <- matrix(nrow = n.years, ncol = 16)  
  }else{
    ans <- matrix(nrow = n.years, ncol = 12)  
  }
  
  ans[,1] <- sort(unique(x$year))
  x <- add_column_apsim_met(x, value = as.factor(x$year), name = "year", units = "()")
  
  if(verbose){
    cat("First year:", x$year[1], "\n")
    cat("Last year:", x$year[nrow(x)], "\n")
    cat("Total number of years:", length(unique(x$year)), "\n")    
  }

  ## Store high maxt and mint temperature by year
  if(all(is.na(x$maxt))){
    ans[,4] <- NA
    ans[,6] <- NA
    ans[,8] <- NA
  }else{
    ans[,4] <- round(stats::aggregate(maxt ~ year, data = x, FUN = max, na.rm = na.rm)$maxt, digits) 
    ans[,6] <- round(stats::aggregate(maxt ~ year, data = x, FUN = mean, na.rm = na.rm)$maxt, digits)
    ans[,8] <- round(stats::aggregate(maxt ~ year, data = x, FUN = min, na.rm = na.rm)$maxt, digits)
  }
  
  if(all(is.na(x$mint))){
    ans[,5] <- NA
    ans[,7] <- NA
    ans[,9] <- NA
  }else{
    ans[,5] <- round(stats::aggregate(mint ~ year, data = x, FUN = max, na.rm = na.rm)$mint, digits)
    ans[,7] <- round(stats::aggregate(mint ~ year, data = x, FUN = mean, na.rm = na.rm)$mint, digits)
    ans[,9] <- round(stats::aggregate(mint ~ year, data = x, FUN = min, na.rm = na.rm)$mint, digits)    
  }
  
  ## Total precipitation
  if(all(is.na(x$rain))){
    ans[,10] <- NA
  }else{
    ans[,10] <- round(stats::aggregate(rain ~ year, data = x, FUN = sum, na.rm = na.rm)$rain, digits)  
  }
  ## Total and mean radiation
  if(all(is.na(x$radn))){
    ans[,11] <- NA
    ans[,12] <- NA
  }else{
    ans[,11] <- round(stats::aggregate(radn ~ year, data = x, FUN = sum, na.rm = na.rm)$radn, digits)
    ans[,12] <- round(stats::aggregate(radn ~ year, data = x, FUN = mean, na.rm = na.rm)$radn, digits)
  }
  
  ## How do I compute the length of the growing season
  if(compute.frost){
    lat0 <- strsplit(attr(x, "latitude"), "=")[[1]][2]
    lat <- as.numeric(strsplit(lat0, "(", fixed = TRUE)[[1]][1])

    ans[,13] <- rep(0, length(unique(x$year)))
    ans[,14] <- rep(0, length(unique(x$year)))
    ## This should work regardless of the subset, but I haven't tested
    if(lat >= 0){
      ## Northern hemisphere
      ## Last frost in the spring
      length.days.by.year <- stats::aggregate(day ~ year, data = x, FUN = length)$day
      half.point <- floor(mean(length.days.by.year)/2)
      x.first <- x[x$day < half.point, ]
      if(length(unique(x.first$year)) != length(unique(x$year))){
        stop("At least one year has incomplete days. Spring frost cannot be computed.", call. = FALSE)
      }
      frosts <- which(x.first$mint < frost.temperature)
      if(length(frosts) != 0){
        x.spring.frosts <- x.first[frosts,]
        last.spring.frost <- stats::aggregate(day ~ year, data = x.spring.frosts, FUN = max)        
        zero.days <- data.frame(year = sort(unique(x$year)), day = 0)
        mrg1 <- merge(zero.days, last.spring.frost, all.x = TRUE, by = "year")
        mrg1[is.na(mrg1$day.y), "day.y"] <- 0
        ans[,13] <- mrg1[["day.y"]]
      }
      ## First frost in the fall
      x.last <- x[x$day > half.point, ]
      if(length(unique(x.last$year)) != length(unique(x$year))){
        stop("At least one year has incomplete days. Fall frost cannot be computed.", call. = FALSE)
      }
      frosts <- which(x.last$mint < frost.temperature)
      if(length(frosts) != 0){
        x.fall.frosts <- x.last[frosts,]
        first.fall.frost <- stats::aggregate(day ~ year, data = x.fall.frosts, FUN = min)        
        zero.days <- data.frame(year = sort(unique(x$year)), day = 0)
        mrg2 <- merge(zero.days, first.fall.frost, all.x = TRUE, by = "year")
        mrg2[is.na(mrg2$day.y), "day.y"] <- 0
        ans[,14] <- mrg2[["day.y"]]
      }
      ## Frost days
      tmp0 <- x[x$mint < 0,]
      if(nrow(tmp0) > 0){
        all.frost.days <- stats::aggregate(day ~ year, data = tmp0, FUN = length)
        zero.days <- data.frame(year = sort(unique(x$year)), day = 0)
        mrg3 <- merge(zero.days, all.frost.days, all.x = TRUE, by = "year")
        mrg3[is.na(mrg3$day.y), "day.y"] <- 0
        ans[,16] <- mrg3[["day.y"]]        
      }else{
        ans[,16] <- rep(0, length(unique(x$years)))
      }
      ## Frost-free period
      if(sum(ans[,13]) == 0 && sum(ans[,14]) == 0){
        ans[,15] <- stats::aggregate(day ~ year, data = x, FUN = length)$day 
      }else{
        if(all(ans[,13] > 0) && all(ans[,14] > 0)){
          ans[,15] <- ans[,14] - ans[,13]
        }else{
          ## Need to compute this by year
          yrs <- sort(unique(x$year))
          for(j in seq_along(yrs)){
            ## If first half has a zero
            if(ans[j, 13] == 0 && ans[j, 14] != 0){
              tmp <- x.fall.frosts[x.fall.frosts$year == yrs[j],]
              if(nrow(tmp) == 0){
                ans[j, 15] <- length.days.by.year[j]
              }else{
                last.fall.frost <- max(tmp$day)
                fall.frost.days <- last.fall.frost - ans[j, 14]
                ans[j, 15] <-  fall.frost.days                
              }
            }
            ## If second half has a zero
            if(ans[j, 14] == 0 && ans[j, 13] != 0){
              tmp <- x.spring.frosts[x.spring.frosts$year == yrs[j],]
              ##print(tmp)
              if(nrow(tmp) == 0){
                ans[j, 15] <- length.days.by.year[j]
              }else{
                first.spring.frost <- min(tmp$day)
                spring.frost.days <- ans[j, 13] - first.spring.frost
                ans[j, 15] <- spring.frost.days               
              }
            }
            ## Both are zero
            if(ans[j, 14] == 0 && ans[j, 13] == 0){
              ans[j, 15] <- length.days.by.year[j]
            }
            ## Both are not zero
            if(ans[j, 14] != 0 && ans[j, 13] != 0){
              ans[j, 15] <- ans[j,14] - ans[j,13]
            }
          }
        } 
      }
    }
    
    if(lat < 0){
      ## Southern hemisphere
      ## Last frost in the fall
      length.days.by.year <- stats::aggregate(day ~ year, data = x, FUN = length)$day
      half.point <- floor(mean(length.days.by.year)/2)
      x.first <- x[x$day > half.point, ]
      if(length(unique(x.first$year)) != length(unique(x$year))){
        stop("At least one year has incomplete days. Spring frost cannot be computed.", call. = FALSE)
      }
      frosts <- which(x.first$mint < frost.temperature)
      if(length(frosts) != 0){
        x.spring.frosts <- x.first[frosts,]
        last.spring.frost <- stats::aggregate(day ~ year, data = x.spring.frosts, FUN = max)        
        zero.days <- data.frame(year = sort(unique(x$year)), day = 0)
        mrg1 <- merge(zero.days, last.spring.frost, all.x = TRUE, by = "year")
        mrg1[is.na(mrg1$day.y), "day.y"] <- 0
        ans[,13] <- mrg1[["day.y"]]
      }
      ## First frost in the fall
      x.last <- x[x$day < half.point, ]
      if(length(unique(x.last$year)) != length(unique(x$year))){
        stop("At least one year has incomplete days. Fall frost cannot be computed.", call. = FALSE)
      }
      frosts <- which(x.last$mint < frost.temperature)
      if(length(frosts) != 0){
        x.fall.frosts <- x.last[frosts,]
        first.fall.frost <- stats::aggregate(day ~ year, data = x.fall.frosts, FUN = min)        
        zero.days <- data.frame(year = sort(unique(x$year)), day = 0)
        mrg2 <- merge(zero.days, first.fall.frost, all.x = TRUE, by = "year")
        mrg2[is.na(mrg2$day.y), "day.y"] <- 0
        ans[,14] <- mrg2[["day.y"]]
      }
      ## Frost days
      tmp0 <- x[x$mint < 0,]
      if(nrow(tmp0) > 0){
        all.frost.days <- stats::aggregate(day ~ year, data = tmp0, FUN = length)
        zero.days <- data.frame(year = sort(unique(x$year)), day = 0)
        mrg3 <- merge(zero.days, all.frost.days, all.x = TRUE, by = "year")
        mrg3[is.na(mrg3$day.y), "day.y"] <- 0
        ans[,16] <- mrg3[["day.y"]]        
      }else{
        ans[,16] <- rep(0, length(unique(x$year)))
      }
      ## Frost-free period
      if(sum(ans[,13]) == 0 && sum(ans[,14]) == 0){
        ans[,15] <- stats::aggregate(day ~ year, data = x, FUN = length)$day 
      }else{
        if(all(ans[,13] > 0) && all(ans[,14] > 0)){
          ##last.day <- ifelse(is_leap_year(sort(unique(x$year))), 366, 365)
          ans[,15] <- (length.days.by.year - ans[,13]) + ans[,14]
        }else{
          ## Need to compute this by year
          yrs <- sort(unique(x$year))
          for(j in seq_along(yrs)){
            ## If first half has a zero
            if(ans[j, 13] == 0 && ans[j, 14] != 0){
              tmp <- x.fall.frosts[x.fall.frosts$year == yrs[j],]
              if(nrow(tmp) == 0){
                ans[j, 15] <- length.days.by.year[j]
              }else{
                last.fall.frost <- max(tmp$day)
                fall.frost.days <- ans[j, 14] - last.fall.frost
                ans[j, 15] <-  fall.frost.days                
              }
            }
            ## If second half has a zero
            if(ans[j, 14] == 0 && ans[j, 13] != 0){
              tmp <- x.spring.frosts[x.spring.frosts$year == yrs[j],]
              ##print(tmp)
              if(nrow(tmp) == 0){
                ans[j, 15] <- length.days.by.year[j]
              }else{
                first.spring.frost <- min(tmp$day)
                spring.frost.days <- ans[j, 13] - first.spring.frost
                ans[j, 15] <- length.days.by.year[j] - spring.frost.days               
              }
            }
            ## Both are zero
            if(ans[j, 14] == 0 && ans[j, 13] == 0){
              ans[j, 15] <- length.days.by.year[j]
            }
            ## Both are not zero
            if(ans[j, 14] != 0 && ans[j, 13] != 0){
              ans[j, 15] <- (length.days.by.year[j] - ans[j,13]) + ans[j,14]
            }
          }
        } 
      }
    }
  }

  if(compute.frost){
    colnames(ans) <- c("year", "months", "days", ## 1, 2, 3
                       "high_maxt", "high_mint", ## 4 and 5
                       "avg_maxt", "avg_mint", ## 6 and 7
                       "low_maxt", "low_mint", ## 8 and 9
                       "rain_sum", "radn_sum", "radn_avg", ## 10, 11, 12
                       "first_half_frost", "second_half_frost", ## 13, 14
                       "frost_free_period", "frost_days") ## 15, 16    
  }else{
    colnames(ans) <- c("year", "months", "days", ## 1, 2, 3
                       "high_maxt", "high_mint", ## 4 and 5
                       "avg_maxt", "avg_mint", ## 6 and 7
                       "low_maxt", "low_mint", ## 8 and 9
                       "rain_sum", "radn_sum", "radn_avg") ## 10, 11, 12
  }

  #### Calculate anomalies ----
  if(!missing(anomaly)){

    vars.to.anomaly <- setdiff(colnames(ans), c("year", "months", "days"))
    #### This computes anomalies for all variables
    if(!isTRUE(anomaly)){
      v2a <- intersect(anomaly, vars.to.anomaly)
      if(length(v2a) == 0){
        v2a <- lapply(anomaly, function(x) grep(x, vars.to.anomaly, value = TRUE))
        if(length(v2a) == 0){
          stop("anomaly variable does not match an existing variable")  
        }else{
         vars.to.anomaly <- unlist(v2a) 
        }
      }else{
        vars.to.anomaly <- anomaly        
      }
    }

    for(i in vars.to.anomaly){
      tmp.var.to.anomaly <- as.data.frame(ans)[[i]]
      mean.i.var.to.anomaly <- mean(tmp.var.to.anomaly, na.rm = TRUE)
      prct.change <- ((tmp.var.to.anomaly / mean.i.var.to.anomaly) - 1) * 100
      name.i.var <- paste0("anomaly_", i)
      tmpd <- data.frame(anomaly = prct.change)
      names(tmpd) <- name.i.var
      ans <- cbind(ans, tmpd)
    }
  }
  
  ansd <- as.data.frame(ans)
  if(inherits(months, "integer")){
    if(grepl(":", deparse(months), fixed = TRUE)){
      ansd$months <- rep(paste(range(months), collapse = ":"), n.years)    
    }else{
      ansd$months <- rep(paste(months, collapse = ","), n.years)    
    }
  }else{
    ansd$months <- rep(paste(months, collapse = ","), n.years)  
  }
  
  ansd$days <- rep(deparse(days), n.years)
  
  return(ansd)
}


