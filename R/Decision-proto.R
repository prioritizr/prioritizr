#' @include internal.R Parameters-proto.R ConservationModifier-proto.R
NULL

#' @export
if (!methods::isClass("Decision")) methods::setOldClass("Decision")
NULL

#' Decision prototype
#'
#' This prototype  used to represent the type of decision that is made
#' when prioritizing planning units. **This prototype represents
#' a recipe to make a decision, to actually specify
#' the type of decision in a planning problem, see the help page on
#' [decisions()]. Only experts should use this class directly.**
#' This class inherits from the [ConservationModifier-class()].
#'
#' @seealso [ConservationModifier-class()].
#'
#' @name Decision-class
#'
#' @aliases Decision
NULL

#' @export
Decision <- pproto("Decision", ConservationModifier)
