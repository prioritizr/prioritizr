#' @include internal.R Method-class.R
NULL

#' Specify targets based on maxima
#'
#' Specify targets that are calculated based on the maximum of one or more
#' target setting methods.
#'
#' @param x An object specifying a target setting method.
#'
#' @param ... Additional objects specifying target setting methods.
#'
#' @inheritSection add_auto_targets Data calculations
#'
#' @return An object ([`Method-class`]) for specifying targets.
#'
#' @family methods
#'
#' @examples
#' # TODO
#'
#' @export
spec_max_targets <- function(x, ...) {
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
    name = paste0("max[", repr.character(method_names), "]"),
    type = type,
    fun = calc_math_targets,
    args = list(
      methods = methods,
      type = type,
      fun = max
    )
  )
}

calc_math_targets <- function(x, features, methods, type, fun,
                              call = fn_caller_env()) {
  # assert that arguments are valid
  assert_required(x, call = call, .internal = TRUE)
  assert_required(features, call = call, .internal = TRUE)
  assert_required(methods, call = call, .internal = TRUE)
  assert_required(fun, call = call, .internal = TRUE)
  assert(
    # x
    is_conservation_problem(x),
    has_single_zone(x),
    # features
    is_integer(features),
    all(features >= 1),
    all(features <= x$number_of_features()),
    # methods
    all_elements_inherit(methods, "Method"),
    # type
    assertthat::is.string(type),
    is_match_of(type, c("relative", "absolute")),
    # fun
    is.function(fun),
    call = call,
    .internal = TRUE
  )

  # calculate target values based on specified methods
  if (identical(type, "relative")) {
    targets <- vapply(
      methods,
      FUN.VALUE = numeric(length(features)),
      function(m) {
        rlang::try_fetch(
          m$calculate_relative_targets(x, features, call = NULL),
          error = function(cnd) {
            cli::cli_abort(
              c("i" = "Can't calculate input targets."),
              call = call,
              parent = cnd
            )
          }
        )
      }
    )
  } else {
    targets <- vapply(
      methods,
      FUN.VALUE = numeric(length(features)),
      function(m) {
        rlang::try_fetch(
          m$calculate_absolute_targets(x, features, call = NULL),
          error = function(cnd) {
            cli::cli_abort(
              c("i" = "Can't calculate input targets."),
              call = call,
              parent = cnd
            )
          }
        )
      }
    )
  }

  # apply math expression
  targets <- apply(targets, 1, fun, na.rm = TRUE)

  # return targets
  targets
}
