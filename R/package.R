#' @include internal.R
NULL

#' @import raster
#' @import sp
#' @useDynLib prioritizr, .registration = TRUE
NULL

#' prioritizr
#'
#' \code{Prioritizr} is an R package for solving systematic conservation 
#' prioritization problems using integer linear programming (ILP) techniques. 
#' The package offers a flexible interface for creating conservation problems 
#' using a range of different objectives and constraints that can be tailored 
#' to the specific needs of the conservation planner. Conservation problems 
#' can be solved using a variety of commercial and open-source exact algorithm 
#' solvers. In contrast to the algorithms conventionally used to solve 
#' conservation problems, such as greedy heuristics or simulated annealing, 
#' the ILP algorithms used by prioritizr are guaranteed to find optimal 
#' solutions. This package also has the functionality to read 
#' \href{http://marxan.net/}{Marxan} input data and find much cheaper 
#' solutions in a much shorter period of time than Marxan.
#'  
#' The supplemental \code{prioritizrdata} package can be downloaded with 
#' \code{prioritizr} for example datasets and worked examples of 
#' formulating conservation planning problems. See the 
#' \href{https://github.com/prioritizr}{github repository} for more details. 
#'
#' @details
#' The following vignettes are available in prioritizr:
#' 
#' \describe{
#' 
#'   \item{\href{inst/doc/prioritizr.html}{prioritizr vignette}}{provides background information 
#' on the concepts and terminology of systematic conservation prioritization underlying 
#' the functions in this package.}
#' 
#'   \item{\href{inst/doc/prioritizr.html}{quickstart vignette}}{provides a walk-through 
#' of the functions available in the prioritizr package and their usage with a simulated dataset.
#' Users should be familiar with the basic terminology of systematic conservation priortization.}
#' 
#' }
#' 
#' @name prioritizr
#' @docType package
NULL
