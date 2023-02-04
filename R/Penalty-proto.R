#' @include internal.R ConservationModifier-proto.R
NULL

#' @export
if (!methods::isClass("Penalty")) methods::setOldClass("Penalty")
NULL

#' Penalty prototype
#'
#' This prototype is used to represent penalties that are added to the
#' objective function when making a conservation problem.
#' **This prototype represents a recipe, to actually
#' add penalties to a planning problem, see the help page on
#' [penalties]. Only experts should use this class directly.** This
#' prototype inherits from the [`ConservationModifier-class`].
#'
#' @seealso [`ConservationModifier-class`].
#'
#' @name Penalty-class
#'
#' @aliases Penalty
NULL

#' @export
Penalty  <- pproto("Penalty", ConservationModifier)
