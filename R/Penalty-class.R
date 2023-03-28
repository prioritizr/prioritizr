#' @include internal.R ConservationModifier-class.R
NULL

#' @export
if (!methods::isClass("Penalty")) methods::setOldClass("Penalty")
NULL

#' Penalty class
#'
#' @description
#' This class is used to represent penalties used in optimization.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name Penalty-class
#'
#' @family classes
Penalty <- R6::R6Class(
  "Penalty",
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
