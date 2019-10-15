## Auxiliary undocumented functions

exclude <- function(x, names){
  tmp <- which(x %in% names)
  ans <- x[-tmp]
  ans
}