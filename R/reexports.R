#' @include internal.R
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr %T>%
#' @export
magrittr::`%T>%`

#' @importFrom terra plot
#' @export
terra::plot

#' @importFrom terra nrow
#' @export
terra::nrow

#' @importFrom terra ncol
#' @export
terra::ncol

#' @importFrom terra ncell
#' @export
terra::ncell

#' @importFrom ape plot.phylo
#' @method plot phylo
#' @export
plot.phylo <- ape::plot.phylo
