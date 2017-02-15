#' @include internal.R Parameters-proto.R ConservationModifier-proto.R
NULL

#' @export
methods::setOldClass('Constraint')

#' Constraint prototype
#'
#' This prototype is used to represent the costraints used when making a
#' prioritization. \strong{This prototype represents a recipe, to actually
#' add constraints to a planning problem, see the help page on
#' \code{\link{constraints}}. Only experts should use this class directly.} This 
#' prototype inherits from the \code{\link{ConservationModifier-prototype}}. 
#'
#' @seealso \code{\link{ConservationModifier-prototype}}.
#'
#' @name Constraint
NULL

#' @export
Constraint <- pproto(NULL, ConservationModifier)
