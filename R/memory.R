#' Can process in memory?
#'
#' Check if a [terra::rast()] can be processed in memory?
#'
#' @param x [terra::rast()] object.
#'
#' @param n `integer` number of copies. Defaults to 1.
#'
#' @return A `logical` indicating if the object can be processed in memory.
#'
#' @noRd
terra_can_process_in_memory <- function(x, n = 1) {
  assert(
    is_inherits(x, "SpatRaster"),
    assertthat::is.count(n),
    assertthat::noNA(n)
  )
  msg <- utils::capture.output(
    y <- terra::mem_info(x = x, n = n)
  )
  # from https://github.com/rspatial/terra/blob/a72eb63cf178c637f76859476487cd8345b529bc/R/messages.R#L59
  isTRUE(round(y[[5]]) != 0)
}

#' Number of copies that can be processed in memory?
#'
#' Determine the number of copies of a [terra::rast()] that can be processed in
#' memory.
#'
#' @param x [terra::rast()] object.
#'
#' @return An `integer` value indicating if the number of copies that
#' can be processed in memory.
#'
#' @noRd
terra_n_process_in_memory <- function(x) {
  assert(
    is_inherits(x, "SpatRaster")
  )
  msg <- utils::capture.output(
    y <- terra::mem_info(x = x, n = 1)
  )
  as.integer(max(1, floor(floor(y[[2]] * y[[3]]) / y[[1]])))
}
