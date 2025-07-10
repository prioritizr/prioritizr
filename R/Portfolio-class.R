#' @include internal.R
NULL

#' @export
if (!methods::isClass("Portfolio")) methods::setOldClass("Portfolio")
NULL

#' Portfolio class
#'
#' @description
#' This class is used to represent portfolios used in optimization.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name Portfolio-class
#'
#' @family classes
#'
#' @export
Portfolio <- R6::R6Class(
  "Portfolio",
  inherit = ConservationModifier,
  public = list(

    #' @description
    #' Run the portfolio to generate solutions.
    #' @param x [optimization_problem()] object.
    #' @param solver [`Solver-class`] object.
    #' @return `list` of solutions.
    run = function(x, solver) {
      # nocov start
      cli::cli_abort("No defined $run method.", .internal = TRUE)
      # nocov end
    }
  )
)
