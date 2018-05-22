#' @include internal.R pproto.R ConservationModifier-proto.R
NULL

#' @export
if (!methods::isClass("Objective")) methods::setOldClass("Objective")
NULL

#' Objective prototype
#'
#' This prototype is used to represent an objective that can be added to a
#' \code{\link{ConservationProblem-class}} object. \strong{This prototype
#' represents a recipe to make an objective, to actually add an objective to a
#' planning  problem: see \code{\link{objectives}}. Only experts should use this
#' class directly.}
#'
#' @name Objective-class
#'
#' @aliases Objective
NULL

#' @export
Objective <- pproto("Objective", ConservationModifier)
