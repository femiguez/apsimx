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
                          verbose = FALSE){
  
  ## Internal function
  fev <- function(x) as.numeric(strsplit(x, ".", fixed = TRUE)[[1]][4])
  
  which <- match.arg(which)
  
  ## Return (all): a table with APSIM and APSIM-X available versions
  ## Return (inuse): the number for the version in use
  
  ## This it the response if we choose all, now I need to populate it
  tmp.dat <- data.frame(APSIM = c("Classic", "Next Generation"))
  
  ## Let's do it by operating system
  if(verbose) cat("OS:",.Platform$OS.type," ",Sys.info()[["sysname"]],"\n")
  
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
    find.apsim <- grep("APSIM",laf)
    
    if(length(find.apsim) == 0 && length(find.apsimx) == 0) 
      warning("Could not find APSIM or APSIM-X")
    
    ncols <- max(c(length(find.apsim),length(find.apsimx)))
    
    if(length(find.apsimx) > 0){
      apsimx.versions <- lafx[find.apsimx]
      tmp.matx <- matrix(NA, nrow = 2, ncol = ncols)
      tmp.matx[2,seq_along(find.apsimx)] <- apsimx.versions
    }else{
      tmp.matx <- matrix(NA, nrow = 2, ncol = ncols) 
    }

    if(length(find.apsim) > 0){
      apsim.versions <- laf[find.apsim]
      tmp.matc <- matrix(NA, nrow = 2, ncol = length(find.apsim))
      tmp.matc[2,seq_along(find.apsim)] <- apsim.versions
    }else{
      tmp.matc <- matrix(NA, nrow = 2, ncol = ncols) 
    }
    ans <- data.frame(tmp.dat, as.data.frame(rbind(tmp.matx,tmp.matc)))
    names(ans) <- c("APSIM",paste0("Version.",1:ncols))
  }
  
  if(which == "all"){
    if(verbose)
      print(kable(ans))
    return(ans)
  }
  
  if(which == "inuse"){
    if(!is.na(apsimx::apsimx.options$exe.path)){
      ans <- fev(apsimx::apsimx.options$exe.path)
      return(ans)
    }
    if(.Platform$OS.type == "unix"){
      if(length(find.apsim) == 1){
        ans <- laf[find.apsim]
      }else{
        apsimx.versions <- sapply(laf[find.apsim],fev)
        newest.version <- sort(apsimx.versions, decreasing = TRUE)[1]
        ans <- names(newest.version)
      }
    }
    if(verbose) print(ans)
    return(ans)
  }
}