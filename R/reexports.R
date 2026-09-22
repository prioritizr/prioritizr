#' @include internal.R
NULL

#' Package imports
#'
#' This is a dummy function that is used to avoid the package checks from
#' incorrectly producing a NOTE about the package not being used.
#'
#' @details
#' This function is required because the package check process does not
#' account for packages that are used in S4 methods. In particular,
#' the function contains code that simply involves calling a function
#' from a particular package (e.g,. `exactextractr::exact_extract`)
#' so that the package check process can detect that these packages are used.
#'
#' @return A `NULL` value.
#'
#' @noRd
package_imports <- function() {
  exactextractr::exact_extract
  NULL
}

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr %T>%
#' @export
magrittr::`%T>%`

#' @importMethodsFrom terra plot
#' @export
plot <- terra::plot

#' @importMethodsFrom terra ncell
#' @export
ncell <- terra::ncell

#' @importMethodsFrom terra nrow
#' @export
nrow <- terra::nrow

#' @importMethodsFrom terra ncol
#' @export
ncol <- terra::ncol

#' @importClassesFrom Matrix dgTMatrix dgCMatrix Matrix
#' @export
NULL
