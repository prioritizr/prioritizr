#' @include internal.R Parameters-proto.R ConservationModifier-proto.R
NULL

#' @export
methods::setOldClass('Objective')

#' Objective prototype
#'
#' This prototype is used to represent an objective that can be added to a
#' \code{\link{ConservationProblem}} object. \strong{This prototype represents a
#' a recipe to make an objective, to actually add an objective to a planning 
#' problem: see \code{\link{objectives}}. Only experts should use this
#' class directly.}
#'
#' @name Objective
NULL

#' @export
Objective <- pproto(NULL, ConservationModifier)
