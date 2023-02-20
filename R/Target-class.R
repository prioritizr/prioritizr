#' @include internal.R ConservationModifier-class.R
NULL

#' @export
if (!methods::isClass("Target")) methods::setOldClass("Target")
NULL

#' Target class
#'
#' @description
#' This class is used to represent targets for optimization.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name Target-class
#'
#' @family classes
Target <- R6::R6Class(
  "Target",
  inherit = ConservationModifier,
  public = list(

    #' @description
    #' Output the targets.
    #' @return [tibble::tibble()] data frame.
    output = function() {
      # nocov start
      cli::cli_abort("No defined $output method.", .internal = TRUE)
      # nocov end
    }
  )
)
