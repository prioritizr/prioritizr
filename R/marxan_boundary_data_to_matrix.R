#' @include internal.R
NULL

#' Convert *Marxan* boundary data to a matrix format
#'
#' Convert a `data.frame` object that follows the *Marxan* format
#' to a matrix format. This function is useful for converting
#' `data.frame` objects to `matrix` or `array` objects that
#' are used by the various [penalties] and
#' [constraints] functions. If the boundary data contains data for
#' a single zone, then a matrix object is returned. Otherwise if the boundary
#' data contains data for multiple zones, then an array is returned.
#'
#' @param x [problem()] (i.e., [`ConservationProblem-class`]) object that
#'   contains planning unit and zone data to ensure that the argument to
#'   `data` is converted correctly. This argument can be set to
#'   `NULL` if checks are not required (not recommended).
#'
#' @param data `data.frame` object with the columns `"id1"`,
#'   `"id2"`, and `"boundary"`. The columns `"zone1"` and
#'   `"zone2"` can also be provided to indicate zone data.
#'
#' @return `array` or [`dgCMatrix-class`] sparse matrix object.
#'
#' @examples
#' # create marxan boundary with four planning units and one zone
#' bldf1 <- expand.grid(id1 = seq_len(4), id2 = seq_len(4))
#' bldf1$boundary <- 1
#' bldf1$boundary[bldf1$id1 == bldf1$id2] <- 0.5
#'
#' # convert to matrix
#' m1 <- marxan_boundary_data_to_matrix(NULL, bldf1)
#'
#' # visualize matrix
#' \dontrun{
#' image(m1)
#' }
#' # create marxan boundary with three planning units and two zones
#' bldf2 <- expand.grid(id1 = seq_len(3), id2 = seq_len(3),
#'                      zone1 = c("z1", "z2"),
#'                      zone2 = c("z1", "z2"))
#' bldf2$boundary <- 1
#' bldf2$boundary[bldf2$id1 == bldf2$id2 & bldf2$zone1 == bldf2$zone2] <- 0.5
#' bldf2$boundary[bldf2$id1 == bldf2$id2 & bldf2$zone1 != bldf2$zone2] <- 0
#'
#' # convert to array
#' m2 <- marxan_boundary_data_to_matrix(NULL, bldf2)
#'
#' # print array
#' print(m2)
#' @export
marxan_boundary_data_to_matrix <- function(x, data) {
  marxan_connectivity_data_to_matrix(x, data, TRUE)
}
