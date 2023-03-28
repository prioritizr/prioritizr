#' @include internal.R ConservationModifier-class.R
NULL

#' @export
if (!methods::isClass("Constraint")) methods::setOldClass("Constraint")
NULL

#' Constraint class
#'
#' @description
#' This class is used to represent the constraints used in optimization.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name Constraint-class
#'
#' @family classes
Constraint <- R6::R6Class(
  "Constraint",
  inherit = ConservationModifier,
  public = list(

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
