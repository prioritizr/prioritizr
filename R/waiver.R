#' @export
methods::setOldClass('Waiver')

#' Waiver
#' 
#' Create a \code{waiver} object. 
#' 
#' @details This object is used to represent that the user has not manually 
#' specified a setting, and so defaults should be used. By explictly 
#' using a \code{waiver()}, this means that \code{NULL} objects can be a 
#' valid setting. The use of a "waiver" object was inspired by the
#' \emph{ggplot2} package.
#' 
#' @return \code{object} of class \code{Waiver}.
#'
#' @export
waiver <- function() structure(NULL, class = "Waiver")


