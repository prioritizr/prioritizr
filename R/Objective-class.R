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
#'
#' @export
Objective <- R6::R6Class(
  "Objective",
  inherit = ConservationModifier,
  public = list(

    #' @field has_targets `logical` value indicating if the objective
    #' supports targets.
    has_targets = new_waiver(),

    #' @field has_weights `logical` value indicating if the objective
    #' supports feature weights.
    has_weights = new_waiver(),

    #' @description
    #' Specify default value for the feature weights.
    #' @return A `numeric` value.
    default_weights = function() {
      1
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
