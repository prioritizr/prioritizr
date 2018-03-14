#' @include internal.R Parameters-proto.R ConservationModifier-proto.R
NULL

#' @export
if (!methods::isClass("Constraint")) methods::setOldClass("Constraint")
NULL

#' Constraint prototype
#'
#' This prototype is used to represent the constraints used when making a
#' prioritization. \strong{This prototype represents a recipe, to actually
#' add constraints to a planning problem, see the help page on
#' \code{\link{constraints}}. Only experts should use this class directly.} This
#' prototype inherits from the \code{\link{ConservationModifier-class}}.
#'
#' @seealso \code{\link{ConservationModifier-class}}.
#'
#' @name Constraint-class
#'
#' @aliases Constraint
NULL

#' @export
Constraint <- pproto("Constraint", ConservationModifier)
