#' @include internal.R
NULL

#' @export
if (!methods::isClass("Waiver")) methods::setOldClass("Waiver")
NULL

#' Waiver
#'
#' Create a `waiver` object.
#'
#' @details This object is used to represent that the user has not manually
#' specified a setting, and so defaults should be used. By explicitly
#' using a `new_waiver()`, this means that `NULL` objects can be a
#' valid setting. The use of a waiver object was inspired by the
#' `ggplot2` package.
#'
#' @return A `Waiver` object.
#'
#' @examples
#' # create new waiver object
#' w <- new_waiver()
#'
#' # print object
#' print(w)
#'
#' @export
new_waiver <- function() structure(list(), class = "Waiver")


#' Is waiver?
#'
#' Check if an object is a `waiver`.
#'
#' @param x Object.
#'
#' @return A `logical` value.
#'
#' @examples
#' # create new waiver object
#' w <- new_waiver()
#'
#' # is it a waiver object?
#' is.Waiver(w)
#'
#' @noRd
is.Waiver <- function(x) inherits(x, "Waiver")
