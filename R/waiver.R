#' @include internal.R
NULL

#' @export
methods::setOldClass("Waiver")

#' Waiver
#'
#' Create a \code{waiver} object.
#'
#' @details This object is used to represent that the user has not manually
#' specified a setting, and so defaults should be used. By explicitly
#' using a \code{new_waiver()}, this means that \code{NULL} objects can be a
#' valid setting. The use of a "waiver" object was inspired by the
#' \code{ggplot2} package.
#'
#' @return \code{object} of class \code{Waiver}.
#'
#' @examples
#' # create new waiver object
#' w <- new_waiver()
#'
#' # print object
#' print(w)
#'
#' # is it a waiver object?
#' is.Waiver(w)
#'
#' @export
new_waiver <- function() structure(list(), class = "Waiver")
