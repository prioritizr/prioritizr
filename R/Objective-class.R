#' @include internal.R ConservationModifier-class.R
NULL

#' @export
if (!methods::isClass("Objective")) methods::setOldClass("Objective")
NULL

#' Objective class
#'
#' @description
#' This class is used to represent the objective function used in optimization.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name Objective-class
#'
#' @family classes
Objective <- R6::R6Class(
  "Objective",
  inherit = ConservationModifier,
  public = list(

    #' @description
    #' Specify default values for weights.
    #' @param x [problem()] object.
    #' @return Invisible `TRUE`.
    default_weights = function(x) {
      return(1)
    },

    #' @description
    #' Update an optimization problem formulation.
    #' @param x [optimization_problem()] object.
    #' @return Invisible `TRUE`.
    apply = function(x) {
      # nocov start
      cli::cli_abort("No defined $apply method.", .internal = TRUE)
      # nocov end
    }
  )
)
