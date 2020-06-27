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
#' @param x [problem()] (i.e. [`ConservationProblem-class`]) object that
#'   contains planning unit and zone data to ensure that the argument to
#'   `data` is converted correctly. This argument can be set to
#'   `NULL` if checks are not required (not recommended).
#'
#' @param data `data.frame` object with the columns `"id1"`,
#'   `"id2"`, and `"boundary"`. The columns `"zone1"` and
#'   `"zone2"` can also be provided to indicate zone data.
#'
#' @return `array` or sparse matrix ([`dgCMatrix-class`]) object.
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
#' \donttest{
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
  # assert that argument to data is valid
  assertthat::assert_that(
    inherits(data, "data.frame"),
    assertthat::has_name(data, "id1"),
    assertthat::has_name(data, "id2"),
    assertthat::has_name(data, "boundary"),
    is.numeric(data$id1),
    all(data$id1 == round(data$id1)),
    assertthat::noNA(data$id1),
    is.numeric(data$id2),
    all(data$id2 == round(data$id2)),
    assertthat::noNA(data$id2),
    is.numeric(data$boundary),
    assertthat::noNA(data$boundary),
    inherits(x, c("NULL", "ConservationProblem")))
  if (assertthat::has_name(data, "zone1") ||
      assertthat::has_name(data, "zone2")) {
    assertthat::assert_that(
      assertthat::has_name(data, "zone1"),
      assertthat::has_name(data, "zone2"),
      assertthat::noNA(data$zone1),
      assertthat::noNA(data$zone2),
      is.character(data$zone1) || is.factor(data$zone1),
      is.character(data$zone2) || is.factor(data$zone2))
  }
  # if problem is not NULL, then assert that argument to data is valid
  if (!is.null(x)) {
    # assert that planning unit ids are valid
    if (is.data.frame(x$data$cost)) {
      # validate that ids in data are correct
      assertthat::assert_that(
        all(data$id1 %in% x$data$cost$id),
        all(data$id2 %in% x$data$cost$id),
        msg = paste("argument to data contains ids for planning",
                    "units that are not present in x"))
      # match the ids with their order in the cost data
      data$id1 <- match(data$id1, x$data$cost$id)
      data$id2 <- match(data$id2, x$data$cost$id)
    } else {
      n_pu <- x$number_of_total_units()
      assertthat::assert_that(
        max(data$id1) <= n_pu,
        max(data$id2) <= n_pu,
        min(data$id1) >= 1,
        min(data$id2) >= 1,
        msg = paste("argument to data contains ids for planning",
                    "units that are not present in x"))
    }
    # assert that zone names is valid
    if (number_of_zones(x) > 1 || assertthat::has_name(x, "zone1")) {
      assertthat::assert_that(
        all(as.character(data$zone1) %in% zone_names(x)),
        all(as.character(data$zone2) %in% zone_names(x)))
    }
  }
  # convert data
  if (assertthat::has_name(data, "zone1")) {
    # convert zone names to characters
    if (is.factor(data$zone1))
      data$zone1 <- as.character(data$zone1)
    if (is.factor(data$zone2))
      data$zone2 <- as.character(data$zone2)
    # set dimensions
    if (is.null(x)) {
     dims <- c(rep(max(c(data$id1, data$id2)), 2),
               rep(length(unique(c(data$zone1, data$zone2))), 2))
    } else {
     dims <- c(rep(x$number_of_total_units(), 2),
               rep(x$number_of_zones(), 2))
    }
    # convert zone names to indices
    if (!is.null(x)) {
      zone1_index <- match(data$zone1, x$zone_names())
      zone2_index <- match(data$zone2, x$zone_names())
    } else {
      zone1_index <- match(data$zone1, unique(c(data$zone1, data$zone2)))
      zone2_index <- match(data$zone2, unique(c(data$zone1, data$zone2)))
    }
    # create array with data
    out <- array(0, dim = dims)
    indices <- matrix(c(data$id1, data$id2, zone1_index, zone2_index), ncol = 4)
    out[indices] <- data$boundary
    # add data for other diagonal if missing
    pos <- data$id2 < data$id1
    data[pos, c("id1", "id2")] <- data[pos, c("id2", "id1")]
    pos <- zone2_index < zone1_index
    data[pos, c("zone1", "zone2")] <- data[pos, c("zone2", "zone1")]
    if (anyDuplicated(
          paste(data$id1, data$id2,
                data$zone1, data$zone2)) == 0) {
      out[indices[, c(2, 1, 4, 3)]] <- data$boundary
    }
  } else {
    # set dimensions
    if (is.null(x)) {
      dims <- rep(max(c(data$id1, data$id2)), 2)
    } else {
      dims <- rep(x$number_of_total_units(), 2)
    }
    # create sparse matrix
    out <- triplet_dataframe_to_matrix(data, forceSymmetric = TRUE,
                                       dims = dims)
  }
  return(out)
}
