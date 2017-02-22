#' @include internal.R pproto.R Parameters-proto.R ConservationModifier-proto.R
NULL

#' @export
methods::setOldClass('Target')
  
#' Target class
#'  
#' This class is used to represent the targets used when making a
#' prioritization. \strong{This class represents a recipe, to actually 
#' add targets to a planning problem, see the help page on
#' \code{\link{targets}}. Only experts should use this class directly.} This 
#' class inherits from the \code{\link{ConservationModifier}}. 
#'
#' @seealso \code{\link{ConservationModifier}}.
#'
#' @name Target
NULL

#' @export
Target <- pproto('Target', ConservationModifier)
