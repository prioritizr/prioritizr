#' @include internal.R pproto.R ConservationModifier-proto.R
NULL

#' @export
if (!methods::isClass("Objective")) methods::setOldClass("Objective")
NULL

#' Objective prototype
#'
#' This prototype is used to represent an objective that can be added to a
#' [ConservationProblem-class] object. **This prototype
#' represents a recipe to make an objective, to actually add an objective to a
#' planning  problem: see [objectives]. Only experts should use this
#' class directly.**
#'
#' @name Objective-class
#'
#' @aliases Objective
NULL

#' @export
Objective <- pproto("Objective", ConservationModifier)
