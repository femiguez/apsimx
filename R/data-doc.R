#' Artificial observed data for Wheat
#'
#' A dataset containing the Date, phenology stage, LAI and above ground biomass for Wheat
#'
#' @title Observed wheat phenology, LAI and biomass
#' @format A data frame with 10 rows and 4 variables:
#' \describe{
#'   \item{Date}{ -date- date starting Oct 1 2016 and ending June 6 2017}
#'   \item{Wheat.Phenology.Stage}{ -numeric- phenology stage of wheat}
#'   \item{Wheat.Leaf.LAI}{ -numeric- Leaf Area Index}
#'   \item{Wheat.AboveGround.Wt}{-numeric- above ground biomass (g/m2)}
#' }
#' @source These are simulated data. For details see the APSIM documentation
"obsWheat"

#' Results from Wheat optimization example
#'
#' @title Wheat example optimization results
#' @format An object of class 'optim_apsim' 
#' \describe{
#'   \item{ wop }{ wheat optimization results }
#' }
#' @source Result of running the examples in Parameter Optimization vignette
"wop"

#' Results from Wheat optimization example plus the Hessian
#'
#' @title Wheat example optimization results plus Hessian
#' @format An object of class 'optim_apsim' 
#' \describe{
#'   \item{ wop.h }{ wheat optimization results plus Hessian}
#' }
#' @source Result of running the examples in Parameter Optimization vignette with the added Hessian
"wop.h"
