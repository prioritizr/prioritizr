#' @include internal.R
NULL

#' Branch matrix
#'
#' Generate a matrix showing which species inherit from which branches.
#'
#' @param x \code{\link[ape]{phylo}} tree object.
#'
#' @return \code{\link[Matrix]{dgCMatrix-class}} matrix object. Each row 
#'   corresponds to a different species. Each column corresponds to a different
#'   branch. Species that inherit from a given branch are denoted with a one.
#'
#' @export
branch_matrix <- function(x) UseMethod('branch_matrix')

#' @export
branch_matrix.default <- function(x) 
  rcpp_branch_matrix(as(x, 'phylo'))

#' @export
branch_matrix.phylo <- function(x) {
  # check that tree is valid and return error if not
  msg <- capture.output(ape::checkValidPhylo(x))
  if (any(grepl('FATAL', msg)) || any(grepl('MODERATE', msg)))
    stop(paste(msg, collapse='\n'))
  # generate matrix
  rcpp_branch_matrix(x)
}
