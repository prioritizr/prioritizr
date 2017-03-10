#' @include internal.R Parameters-proto.R ConservationModifier-proto.R
NULL

#' @export
methods::setOldClass("Penalty")

#' Penalty prototype
#'
#' This prototype is used to represent penalties that are added to the
#' objective function when making a conservation problem.
#' \strong{This prototype represents a recipe, to actually
#' add penalties to a planning problem, see the help page on
#' \code{\link{penalties}}. Only experts should use this class directly.} This
#' prototype inherits from the \code{\link{ConservationModifier-class}}.
#'
#' @seealso \code{\link{ConservationModifier-class}}.
#'
#' @name Penalty-class
#'
#' @aliases Penalty
NULL

#' @export
Penalty  <- pproto("Penalty", ConservationModifier)
