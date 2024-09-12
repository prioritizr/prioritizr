#' @include internal.R
NULL

#' Convert *Marxan* connectivity data to matrix format
#'
#' Convert a `data.frame` object containing *Marxan* connectivity data
#' to matrix format. This function is designed specifically for
#' connectivity data (not boundary data).
#' It ensures that the output matrix correctly specifies
#' symmetric or asymmetric connectivity relationships between planning units.
#'
#' @inheritParams marxan_boundary_data_to_matrix
#'
#' @param symmetric `logical` does the connectivity data
#'  describe symmetric relationships between planning units?
#'  If the data contain asymmetric connectivity data,
#'  this parameter should be set to `FALSE`.
#'  Defaults to `TRUE`.
#'
#' @inherit marxan_boundary_data_to_matrix return
#'
#' @examples
#' \dontrun{
#' # create marxan connectivity data with four planning units and one zone,
#' # and symmetric connectivity values
#' bldf1 <- expand.grid(id1 = seq_len(4), id2 = seq_len(4))
#' bldf1$boundary <- 1
#' bldf1$boundary[bldf1$id1 == bldf1$id2] <- 0.5
#'
#' # print data
#' print(bldf1)
#'
#' # convert to matrix
#' m1 <- marxan_connectivity_data_to_matrix(NULL, bldf1)
#'
#' # print matrix
#' print(m1)
#'
#' # visualize matrix
#' image(m1)
#'
#' # create marxan connectivity data with four planning units and one zone,
#' # and asymmetric connectivity values
#' bldf2 <- expand.grid(id1 = seq_len(4), id2 = seq_len(4))
#' bldf2$boundary <- runif(nrow(bldf2))
#' bldf2$boundary[bldf1$id1 == bldf1$id2] <- 0.5
#'
#' # print data
#' print(bldf2)
#'
#' # convert to matrix
#' m2 <- marxan_connectivity_data_to_matrix(NULL, bldf2, symmetric = FALSE)
#'
#' # print matrix
#' print(m2)
#'
#' # visualize matrix
#' Matrix::image(m2)
#'
#' # create marxan connectivity with three planning units and two zones,
#' # and symmetric connectivity values
#' bldf3 <- expand.grid(
#'   id1 = seq_len(3), id2 = seq_len(3),
#'   zone1 = c("z1", "z2"),
#'   zone2 = c("z1", "z2")
#' )
#' bldf3$boundary <- 1
#' bldf3$boundary[bldf2$id1 == bldf2$id2 & bldf2$zone1 == bldf2$zone2] <- 0.5
#' bldf3$boundary[bldf2$id1 == bldf2$id2 & bldf2$zone1 != bldf2$zone2] <- 0
#'
#' # print data
#' print(bldf3)
#'
#' # convert to array
#' m3 <- marxan_connectivity_data_to_matrix(NULL, bldf3)
#'
#' # print array
#' print(m3)
#' }
#' @export
marxan_connectivity_data_to_matrix <- function(x, data, symmetric = TRUE) {
  assert_required(x)
  assert_required(data)
  assert_required(symmetric)
  internal_marxan_connectivity_data_to_matrix(x, data, symmetric)
}

internal_marxan_connectivity_data_to_matrix <- function(
  x, data, symmetric, call = fn_caller_env()) {
  # assert that argument to data is valid
  assert(
    is_inherits(x, c("NULL", "ConservationProblem")),
    is.data.frame(data),
    assertthat::has_name(data, "id1"),
    assertthat::has_name(data, "id2"),
    assertthat::has_name(data, "boundary"),
    is_count_vector(data$id1),
    all_finite(data$id1),
    is_count_vector(data$id2),
    all_finite(data$id2),
    is.numeric(data$boundary),
    all_finite(data$boundary),
    assertthat::is.flag(symmetric),
    assertthat::noNA(symmetric),
    call = call
  )
  if (
    assertthat::has_name(data, "zone1") ||
    assertthat::has_name(data, "zone2")
  ) {
    assert(
      assertthat::has_name(data, "zone1"),
      assertthat::has_name(data, "zone2"),
      is_inherits(data$zone1, c("character", "factor")),
      is_inherits(data$zone2, c("character", "factor")),
      assertthat::noNA(data$zone2),
      assertthat::noNA(data$zone1),
      call = call
    )
  }
  # if problem is not NULL, then assert that argument to data is valid
  if (!is.null(x)) {
    # assert that planning unit ids are valid
    if (
      is.data.frame(x$data$cost) &&
      !inherits(x$data$cost, c("Spatial", "sf"))
    ) {
      # validate that ids in data are correct
      assert(
        all(data$id1 %in% x$data$cost$id),
        all(data$id2 %in% x$data$cost$id),
        msg = paste(
          "{.arg data} must not contain values in the {.col id} column",
          "that do not refer to planning units in {.arg x}."
        ),
        call = call
      )
      # match the ids with their order in the cost data
      data$id1 <- match(data$id1, x$data$cost$id)
      data$id2 <- match(data$id2, x$data$cost$id)
    } else {
      n_pu <- x$number_of_total_units()
      assert(
        max(data$id1) <= n_pu,
        max(data$id2) <= n_pu,
        min(data$id1) >= 1,
        min(data$id2) >= 1,
        msg = paste(
          "{.arg data} must not contain values in the {.code id} column",
          "that do not refer to planning units in {.arg x}."
        ),
        call = call
      )
    }
    # assert that zone names is valid
    if (number_of_zones(x) > 1 || assertthat::has_name(x, "zone1")) {
      assert(
        all_match_of(as.character(data$zone1), zone_names(x)),
        all_match_of(as.character(data$zone2), zone_names(x)),
        call = call
      )
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
      dims <- c(
        rep(max(c(data$id1, data$id2)), 2),
        rep(length(unique(c(data$zone1, data$zone2))), 2)
      )
    } else {
      dims <- c(
        rep(x$number_of_total_units(), 2),
        rep(x$number_of_zones(), 2)
      )
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
    # add data for upper/lower if symmetric and missing one triangle is missing
    if (symmetric) {
      tri_missing <- anyDuplicated(c(
        paste(data$id1, data$id2, data$zone1, data$zone2),
        paste(data$id2, data$id1, data$zone2, data$zone1)
      ))
      if (identical(tri_missing, 0L)) {
        out[indices[, c(2, 1, 4, 3)]] <- data$boundary
      }
    }
  } else {
    # set dimensions
    if (is.null(x)) {
      dims <- rep(max(c(data$id1, data$id2)), 2)
    } else {
      dims <- rep(x$number_of_total_units(), 2)
    }
    # create sparse matrix
    out <- triplet_dataframe_to_matrix(
      data, forceSymmetric = symmetric, dims = dims
    )
  }
  out
}
