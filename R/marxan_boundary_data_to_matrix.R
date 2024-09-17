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
#' A [`Matrix::dgCMatrix-class`] sparse matrix object.
#'
#' @section Notes:
#' In earlier versions, the function could convert boundary data
#' that pertain to multiple zones. This is no longer possible, following
#' updates to streamline the package.
#'
#' @examples
#' # create example planning unit layer
#' pu_data <-
#'   matrix(1:3, nrow = 1) %>%
#'   terra::rast() %>%
#'   setNames("id") %>%
#'   terra::as.polygons() %>%
#'   sf::st_as_sf()
#'
#' # plot planning units
#' plot(pu_data)
#'
#' # manually create Marxan boundary data for these planning units following
#' # the Marxan boundary data format specification
#' bldf <- data.frame(
#'   id1 = c(1, 2, 3, 1, 2),
#'   id2 =c(1, 2, 3, 2, 3),
#'   boundary = c(3, 2, 3, 1, 1)
#' )
#'
#' # print data
#' print(bldf)
#'
#' # convert to boundary matrix format for use in prioritizr
#' m1 <- marxan_boundary_data_to_matrix(NULL, bldf)
#'
#' # print converted matrix
#' ## we can see that the values in bldf and m1 are different,
#' ## this is because Marxan and prioritizr use different formats
#' ## for storing boundary information
#' print(m1)
#'
#' # automatically create boundary data for use in prioritizr,
#' # by using the boundary_matrix() function
#' m2 <- boundary_matrix(pu_data)
#'
#' # print matrix
#' ## we can see that the values in m1 and m2 are exactly the same,
#' ## this is because marxan_boundary_data_to_matrix() automatically
#' ## converts Marxan data to the same format as boundary_matrix()
#' print(m2)
#'
#' @export
marxan_boundary_data_to_matrix <- function(x, data) {
  assert_required(x)
  assert_required(data)
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
