#' @include internal.R
NULL

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

#' @importFrom ape plot.phylo
#' @method plot phylo
#' @export
plot.phylo <- ape::plot.phylo

#' @importClassesFrom Matrix dgTMatrix dgCMatrix Matrix
#' @export
NULL
