#' Display available APSIM \sQuote{Classic} and APSIM-X versions
#' 
#' @name apsim_version
#' @param which either \sQuote{all} or \sQuote{inuse}
#' @param verbose whether to print the information to standard output
#' @return a data frame (all) or a vector (inuse) with APSIM-X and/or APSIM versions
#' @export
#' @examples 
#' \dontrun{
#' ## Check which apsim version are avialable
#' ava <- apsim_version(verbose = TRUE)
#' }
#' 

apsim_version <- function(which = c("all","inuse"), verbose = TRUE){
  
  ## Internal function
  fevc <- function(x) as.numeric(sub("r","",strsplit(x, "-", fixed = TRUE)[[1]][2]))
  
  which <- match.arg(which)
  
  ## Return (all): a table with APSIM and APSIM-X available versions
  ## Return (inuse): the number for the version in use
  
  ## This it the response if we choose all, now I need to populate it
  tmp.dat <- data.frame(APSIM = c("Classic", "Next Generation"))
  
  ## Let's do it by operating system
  if(verbose) cat("OS:",Sys.info()[["sysname"]],"\n")
  
  ## For unix
  if(.Platform$OS.type == "unix"){
    
    if(grepl("Darwin", Sys.info()[["sysname"]])){
        laf <- list.files("/Applications/")
        find.apsim <- grep("APSIM", laf, ignore.case = TRUE)
        if(length(find.apsim) == 0) warning("APSIM-X not found")
        apsimx.versions <- laf[find.apsim]
        
        if(length(find.apsim) > 0){ 
            tmp.mat <- matrix(NA, nrow = 2, ncol = length(find.apsim))
            tmp.mat[2,seq_along(find.apsim)] <- rev(apsimx.versions)
            ans <- data.frame(tmp.dat, as.data.frame(tmp.mat))
            names(ans) <- c("APSIM",paste0("Version.",seq_along(find.apsim)))
        }
    }
    
    if(grepl("Linux", Sys.info()[["sysname"]])){
        laf <- list.files("/usr/local/lib")
        find.apsim <- grep("apsim", laf, ignore.case = TRUE)
        if(length(find.apsim) == 0) warning("APSIM-X not found")
        apsimx.version <- paste0("/usr/local/lib/apsim/",list.files("/usr/local/lib/apsim"))
        ## Apparently only one version can be present at a time on Debian
        tmp.mat <- matrix(NA, nrow = 2, ncol = length(find.apsim))
        tmp.mat[2,length(find.apsim)] <- list.files("/usr/local/lib/apsim")
        ans <- data.frame(tmp.dat, as.data.frame(tmp.mat))
        names(ans) <- c("APSIM","Version")
    }
  }
  
  if(.Platform$OS.type == "windows"){
    ## Here I need to look for both APSIM and APSIM-X
    ## First APSIM-X
    st1 <- "C:/PROGRA~1/"
    lafx <- list.files(st1)
    find.apsimx <- grep("APSIM",lafx)
    
    ## Now look for APSIM
    st2 <- "C:/PROGRA~2/"
    laf <- list.files(st2)
    find.apsim <- grep("APSIM",laf, ignore.case = TRUE)
    
    if(length(find.apsim) == 0 && length(find.apsimx) == 0) 
      warning("Could not find APSIM or APSIM-X")
    
    ncols <- max(c(length(find.apsim),length(find.apsimx)))
    
    if(length(find.apsimx) > 0){
      apsimx.versions <- lafx[find.apsimx]
      ## I want to order apsimx.version from newest to oldest
      ## The line below is not necessary now
      ## apsimx.versions <- apsimx.versions[order(sapply(apsimx.versions, fev), decreasing = TRUE)]
      tmp.matx <- matrix(NA, nrow = 1, ncol = ncols)
      tmp.matx[1,seq_along(find.apsimx)] <- rev(apsimx.versions)
    }else{
      tmp.matx <- matrix(NA, nrow = 1, ncol = ncols) 
    }

    ## This is for APSIM 'Classic' and it should work
    if(length(find.apsim) > 0){
      apsim.versions <- laf[find.apsim]
      apsim.versions <- apsim.versions[order(sapply(apsim.versions, fevc), decreasing = TRUE)]
      tmp.matc <- matrix(NA, nrow = 1, ncol = ncols)
      tmp.matc[1,seq_along(find.apsim)] <- apsim.versions
    }else{
      tmp.matc <- matrix(NA, nrow = 1, ncol = ncols) 
    }
    ans <- data.frame(tmp.dat, as.data.frame(rbind(tmp.matc,tmp.matx)))
    names(ans) <- c("APSIM", paste0("Version.",1:ncols))
  }
  
  if(which == "all"){
    if(verbose)
      print(knitr::kable(ans))
  }
  
  if(which == "inuse"){
    ## For Unix
    if(.Platform$OS.type == "unix"){
      ## If there is only one APSIM present
        if(length(find.apsim) == 1){
            ## Darwin is different from Linux
            if(grepl("Darwin", Sys.info()[["sysname"]])) ans <- laf[find.apsim]
            if(grepl("Linux", Sys.info()[["sysname"]])) ans <- paste0("apsim", list.files("/usr/local/lib/apsim"))
        }
      ## If there are multiple APSIMs present
      if(length(find.apsim) > 1){
        len.fa <- length(find.apsim)
        fa.dt <- sapply(laf[find.apsim],.favd, simplify = FALSE)[[len.fa]]
        if(verbose) cat("APSIM-X version date:", as.character(fa.dt))
        ## This next line picks the last one
        newest.version <- laf[find.apsim][len.fa]
        ans <- newest.version
      }
      ## If the one in use is not any of the above such as a Custom build
      if(!is.na(get("exe.path", envir = apsimx::apsimx.options))){
        ans <- get("exe.path", envir = apsimx::apsimx.options)
      }
    }
    ## For Windows
    if(.Platform$OS.type == "windows"){
      ## If everything fails NAs will be returned
      ansc <- NA
      ansx <- NA
      ## If there is only one APSIM 'Classic'
      if(length(find.apsim) == 1){
        ansc <- laf[find.apsim]
      }
      ## If there are multiple APSIMs present
      if(length(find.apsim) > 1){
        apsim.versions <- sapply(laf[find.apsim],fevc)
        newest.version <- sort(apsim.versions, decreasing = TRUE)[1]
        ansc <- names(newest.version)
      }
      ## It is possible that the one in use is not the latest one
      if(!is.na(get("exe.path", envir = apsimx::apsim.options))){
        ansc <- get("exe.path", envir = apsimx::apsim.options)
      }
      ## For APSIM-X
      if(length(find.apsimx) == 1){
        ansx <- lafx[find.apsimx]
      }
      if(length(find.apsimx) > 1){
        apsimx.versions <- lafx[find.apsimx]
        len.fa <- length(find.apsimx)
        newest.version <- lafx[find.apsimx][len.fa]
        ansx <- newest.version
      }
      ## It is possible that the one in use is not the latest one
      if(!is.na(get("exe.path", envir = apsimx::apsimx.options))){
        ansx <- get("exe.path", envir = apsimx::apsimx.options)
      }
      
      ans <- data.frame(Classic = ansc, NextGeneration = ansx)
    }
    if(verbose) print(knitr::kable(ans))
  }
  invisible(ans)
}
