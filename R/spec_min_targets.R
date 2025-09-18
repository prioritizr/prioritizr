#' @include internal.R Method-class.R
NULL

#' Specify targets based on minima
#'
#' Specify targets that are calculated based on the minimum of one or more
#' target setting methods.
#'
#' @inheritParams spec_max_targets
#'
#' @inheritSection add_auto_targets Data calculations
#' @inherit spec_max_targets return
#'
#' @family methods
#'
#' @examples
#' # TODO
#'
#' @export
spec_min_targets <- function(x, ...) {
  # assert arguments are valid
  assert_valid_method_arg(x)
  assert(is_method(x))
  methods <- append(list(x), list(...))
  assert(
    all_elements_inherit(methods, "Method"),
    msg = "{.arg ...} must have target setting method objects."
  )
  # determine types
  method_types <- vapply(methods, FUN.VALUE = character(1), function(m) {
    m$type
  })
  method_names <- vapply(methods, FUN.VALUE = character(1), function(m) {
    m$name
  })
  type <- ifelse(
    all(method_types == "relative"),
    "relative",
    "absolute"
  )
  # return new method
  new_method(
    name = paste("min[", repr.character(method_names), "]"),
    type = type,
    fun = calc_math_targets,
    args = list(
      methods = methods,
      type = type,
      fun = min
    )
  )
}
