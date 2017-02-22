#' @include internal.R Parameters-proto.R ConservationModifier-proto.R
NULL

#' @export
methods::setOldClass('Decision')

#' Decision prototype
#'  
#' This prototype  used to represent the type of decision that is made
#' when prioritizing planning units. \strong{This prototype represents 
#' a recipe to make a decision, to actually specify
#' the type of decision in a planning problem, see the help page on
#' \code{\link{decisions}}. Only experts should use this class directly.}
#' This class inherits from the \code{\link{ConservationModifier}}. 
#'
#' @seealso \code{\link{ConservationModifier}}.
#'
#' @name Decision
NULL

#' @export
Decision <- pproto('Decision', ConservationModifier)
