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
#'   `"id2"`, and `"boundary"`.
#'
#' @return
#' A [`dgCMatrix-class`] sparse matrix object.
#'
#' @section Notes:
#' In earlier versions, the function could convert boundary data
#' that pertain to multiple zones. This is no longer possible, following
#' updates to streamline the package.
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
#' @export
marxan_boundary_data_to_matrix <- function(x, data) {
  rlang::check_required(x)
  rlang::check_required(data)
  internal_marxan_boundary_data_to_matrix(x, data)
}

internal_marxan_boundary_data_to_matrix <- function(x, data,
                                                    call = fn_caller_env()) {
  assert(is.data.frame(data), call = call)
  assert(
    !assertthat::has_name(data, "zone1"),
    !assertthat::has_name(data, "zone2"),
    msg = c(
      "{.arg data} must not have the columns {.col zone1} or {.col zone2}.",
      "i" = "This is because multiple zones are not supported."
    ),
    call = call
  )

  # convert to matrix format
  m <- internal_marxan_connectivity_data_to_matrix(x, data, TRUE, call = call)

  # replace cell diagonals with total boundary lengths
  ## this is because the Marxan format uses cell values on the matrix diagonal
  ## to specify exposed boundary lengths, but here we use it to
  ## refer to total lengths
  Matrix::diag(m) <- Matrix::rowSums(m)

  # return result
  m
}
