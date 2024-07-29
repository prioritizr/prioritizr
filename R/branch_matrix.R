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
#' each species have experienced.
#'
#' @param x [ape::phylo()] tree object.
#'
#' @return A [`dgCMatrix-class`] sparse matrix object. Each row
#'   corresponds to a different species. Each column corresponds to a different
#'   branch. Species that inherit from a given branch are denoted with a one.
#'
#' @name branch_matrix
#'
#' @rdname branch_matrix
#'
#' @examples
#' # load data
#' sim_phylogeny <- get_sim_phylogeny()
#'
#' # generate species by branch matrix
#' m <- branch_matrix(sim_phylogeny)
#'
#' # plot data
#' \dontrun{
#' plot(sim_phylogeny, main = "phylogeny")
#' Matrix::image(m, main = "branch matrix")
#' }
#' @export
branch_matrix <- function(x) {
  assert_required(x)
  UseMethod("branch_matrix")
}

#' @rdname branch_matrix
#' @method branch_matrix default
#' @export
branch_matrix.default <- function(x)
  rcpp_branch_matrix(ape::as.phylo(x, "phylo"))

#' @rdname branch_matrix
#' @method branch_matrix phylo
#' @export
branch_matrix.phylo <- function(x) {
  # check that tree is valid and return error if not
  msg <- utils::capture.output(ape::checkValidPhylo(x))
  assert(
    !any(grepl("FATAL", msg)),
    !any(grepl("MODERATE", msg)),
    msg = paste(msg, collapse = "\n")
  )
  # generate matrix
  rcpp_branch_matrix(x)
}
