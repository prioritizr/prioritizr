#' @include internal.R
NULL

#' Branch matrix
#'
#' Phylogenetic trees depict the evolutionary relationships between different
#' species. Each branch in a phylogenetic tree represents a period of
#' evolutionary history. Species that are connected to the same branch
#' both share that same period of evolutionary history. This function creates
#' a matrix that shows which species are connected with branch. In other words,
#' it creates a matrix that shows which periods of evolutionary history
#' each species experienced.
#'
#' @param x \code{\link[ape]{phylo}} tree object.
#'
#' @return \code{\link[Matrix]{dgCMatrix-class}} sparse matrix object. Each row
#'   corresponds to a different species. Each column corresponds to a different
#'   branch. Species that inherit from a given branch are denoted with a one.
#'
#' @name branch_matrix
#'
#' @rdname branch_matrix
#'
#' @examples
#' # load data
#' data(sim_phylogeny)
#'
#' # generate species by branch matrix
#' m <- branch_matrix(sim_phylogeny)
#'
#' # plot data
#' if (requireNamespace("ape", quietly = TRUE)) {
#'   par(mfrow = c(1,2))
#'   plot(sim_phylogeny, main = "phylogeny")
#'   plot(raster(as.matrix(m)), main = "branch matrix", axes = FALSE,
#'        box = FALSE)
#' } else {
#'   message("the \"ape\" package needs to be installed to plot phylogenies")
#' }
#'
#' @export
branch_matrix <- function(x) UseMethod("branch_matrix")

#' @rdname branch_matrix
#' @method branch_matrix default
#' @export
branch_matrix.default <- function(x)
  rcpp_branch_matrix(methods::as(x, "phylo"))

#' @rdname branch_matrix
#' @method branch_matrix phylo
#' @export
branch_matrix.phylo <- function(x) {
  # check that tree is valid and return error if not
  msg <- utils::capture.output(ape::checkValidPhylo(x))
  if (any(grepl("FATAL", msg)) || any(grepl("MODERATE", msg)))
    stop(paste(msg, collapse = "\n"))
  # generate matrix
  rcpp_branch_matrix(x)
}
