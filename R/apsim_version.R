#' Display available APSIM and APSIM-X versions
#' 
#' @name apsim_version
#' @param which either 'all' or 'inuse'
#' @param verbose whether to print the information to standard output
#' @return a data frame (all) or a vector (inuse) with APSIM-X and/or APSIM versions
#' @export
#' @examples 
#' \dontrun{
#' ## Check which apsim version are avialable
#' ava <- apsim_version(verbose = TRUE)
#' }
#' 

apsim_version <- function(which = c("all","inuse"),
                          verbose = TRUE){
  
  ## Internal function
  fev <- function(x) as.numeric(strsplit(x, ".", fixed = TRUE)[[1]][4])
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
    }
    
    if(grepl("Linux", Sys.info()[["sysname"]])){
      laf <- list.files("/usr/local/lib")
    }
    
    find.apsim <- grep("APSIM",laf, ignore.case = TRUE)
    
    if(length(find.apsim) == 0) warning("APSIM-X not found")
    
    if(length(find.apsim) > 0){ 
      apsimx.versions <- laf[find.apsim]
      tmp.mat <- matrix(NA, nrow = 2, ncol = length(find.apsim))
      tmp.mat[2,seq_along(find.apsim)] <- apsimx.versions
      ans <- data.frame(tmp.dat, as.data.frame(tmp.mat))
      names(ans) <- c("APSIM",paste0("Version.",seq_along(find.apsim)))
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
      apsimx.versions <- apsimx.versions[order(sapply(apsimx.versions, fev), decreasing = TRUE)]
      tmp.matx <- matrix(NA, nrow = 1, ncol = ncols)
      tmp.matx[1,seq_along(find.apsimx)] <- apsimx.versions
    }else{
      tmp.matx <- matrix(NA, nrow = 1, ncol = ncols) 
    }

    if(length(find.apsim) > 0){
      apsim.versions <- laf[find.apsim]
      apsim.versions <- apsim.versions[order(sapply(apsim.versions, fevc), decreasing = TRUE)]
      tmp.matc <- matrix(NA, nrow = 1, ncol = ncols)
      tmp.matc[1,seq_along(find.apsim)] <- apsim.versions
    }else{
      tmp.matc <- matrix(NA, nrow = 1, ncol = ncols) 
    }
    ans <- data.frame(tmp.dat, as.data.frame(rbind(tmp.matc,tmp.matx)))
    names(ans) <- c("APSIM",paste0("Version.",1:ncols))
  }
  
  if(which == "all"){
    if(verbose)
      print(kable(ans))
  }
  
  if(which == "inuse"){
    ## For Unix
    if(.Platform$OS.type == "unix"){
      ## If there is only one APSIM present
      if(length(find.apsim) == 1){
        ans <- laf[find.apsim]
      }
      ## If there are multiple APSIMs present
      if(length(find.apsim) > 1){
        apsimx.versions <- sapply(laf[find.apsim],fev)
        newest.version <- sort(apsimx.versions, decreasing = TRUE)[1]
        ans <- names(newest.version)
      }
      ## If the one in use is not any of the above such as a Custom build
      if(!is.na(apsimx::apsimx.options$exe.path)){
        ans <- fev(apsimx::apsimx.options$exe.path)
      }
    }
    ## For Windows
    if(.Platform$OS.type == "windows"){
      ## If everything fails NAs will be returned
      ansc <- NA
      ansx <- NA
      ## If there is only one APSIM
      if(length(find.apsim) == 1){
        ansc <- laf[find.apsim]
      }
      ## If there are multiple APSIMs present
      if(length(find.apsim) > 1){
        apsim.versions <- sapply(laf[find.apsim],fevc)
        newest.version <- sort(apsim.versions, decreasing = TRUE)[1]
        ansc <- as.vector(newest.version)
      }
      ## It is possible that the one in use is not the latest one
      if(!is.na(apsimx::apsim.options$exe.path)){
        ansc <- fevc(apsimx::apsim.options$exe.path)
      }
      ## For APSIM-X
      if(length(find.apsimx) == 1){
        ansx <- lafx[find.apsimx]
      }
      if(length(find.apsimx) > 1){
        apsimx.versions <- sapply(lafx[find.apsimx],fev)
        newest.version <- sort(apsimx.versions, decreasing = TRUE)[1]
        ansx <- as.vector(newest.version)
      }
      ## It is possible that the one in use is not the latest one
      if(!is.na(apsimx::apsimx.options$exe.path)){
        ansx <- fev(apsimx::apsimx.options$exe.path)
      }
      
      ans <- data.frame(Classic = ansc, NextGeneration = ansx)
    }
    if(verbose) print(kable(ans))
  }
  invisible(ans)
}