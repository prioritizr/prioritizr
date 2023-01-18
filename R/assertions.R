#' @include internal.R all_binary.R all_columns_any_finite.R all_columns_any_finite.R all_columns_inherit.R all_finite.R all_positive.R all_proportion.R all_rows_any_finite.R any_nonNA.R any_nonzero.R assertions_vector.R assertions_raster.R assertions_misc.R
NULL

#' Verify if assertion is met
#'
#' Verify if an assertion is met and throw a [base::warning()] if it
#' is not. This function is equivalent to [assertthat::assert_that()]
#' except that it throws warnings and not errors.
#'
#' @param x `logical` condition.
#'
#' @details
#' The function will throw warnings if any of the conditions are not met.
#'
#' @return A `logical` value.
#'
#' @noRd
verify_that <- function(..., env = parent.frame()) {
  res <- assertthat::validate_that(..., env = env)
  if (isTRUE(res))
    return(TRUE)
  warning(res, immediate. = TRUE)
  FALSE
}
