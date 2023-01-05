#' @include internal.R
NULL

#' Convert *Marxan* boundary data to matrix format
#'
#' Convert a `data.frame` object containing *Marxan* boundary data
#' to matrix format. This function is designed specifically for
#' boundary data (not connectivity data).
#' It ensures that the output matrix correctly specifies
#' symmetric spatial relationships between planning units.
#'
#' @param x [problem()] object that
#'   contains planning unit and zone data to ensure that the argument to
#'   `data` is converted correctly. This argument can be set to
#'   `NULL` if checks are not required (not recommended).
#'
#' @param data `data.frame` object with the columns `"id1"`,
#'   `"id2"`, and `"boundary"`. The columns `"zone1"` and
#'   `"zone2"` can also be provided to indicate zone data.
#'
#' @return
#' An `array` or [`dgCMatrix-class`] sparse matrix object.
#' If the argument to `data` corresponds to a single zone,
#' then a [`dgCMatrix-class`]
#' matrix is returned. Otherwise, if the argument to `data`
#' corresponds to multiple zones, then an `array` is returned.
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
#' bldf2 <- expand.grid(
#'   id1 = seq_len(3), id2 = seq_len(3),
#'   zone1 = c("z1", "z2"),
#'   zone2 = c("z1", "z2")
#' )
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
