#' @include internal.R ConservationModifier-class.R
NULL

#' @export
if (!methods::isClass("Decision")) methods::setOldClass("Decision")
NULL

#' Decision class
#'
#' @description
#' This class is used to represent the decision variables used in optimization.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name Decision-class
#'
#' @family classes
Decision <- R6::R6Class(
  "Decision",
  inherit = ConservationModifier,
  public = list(

    #' @description
    #' Update an optimization problem formulation.
    #' @param x [optimization_problem()] object.
    #' @return Invisible `TRUE`.
    apply = function(x) {
      cli::cli_abort("No defined $apply method.", .internal = TRUE)
    }
  )
)
