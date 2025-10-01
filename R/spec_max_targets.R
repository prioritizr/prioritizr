#' @include internal.R TargetMethod-class.R
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
#' @inheritSection spec_jung_targets Data calculations

#'
#' @return An object ([`TargetMethod-class`]) for specifying targets.
#'
#' @family methods
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_complex_pu_raster <- get_sim_complex_pu_raster()
#' sim_complex_features <- get_sim_complex_features()
#'
#' # create base problem
#' p0 <-
#'   problem(sim_complex_pu_raster, sim_complex_features) %>%
#'   add_min_set_objective() %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with 20% targets
#' p1 <-
#'   p0 %>%
#'   add_auto_targets(method = spec_relative_targets(0.2))
#'
#' # create problem with Jung et al. (2021) targets
#' p2 <-
#'   p0 %>%
#'   add_auto_targets(method = spec_jung_targets())
#'
#' # create problem with Polak et al. (2015) targets
#' p3 <-
#'   p0 %>%
#'   add_auto_targets(method = spec_polak_targets())
#'
#' # create problem with targets based on the maximum of 20% targets,
#' # Jung et al. (2021) targets, and Polak et al. (2015) targets
#' # for each feature (separately)
#' p4 <-
#'   p0 %>%
#'   add_auto_targets(
#'     method = spec_max_targets(
#'       spec_relative_targets(0.2),
#'       spec_jung_targets(),
#'       spec_polak_targets()
#'     )
#'   )
#'
#' # solve problems
#' s <- c(solve(p1), solve(p2), solve(p3), solve(p4))
#' names(s) <- c("20% targets", "Jung targets", "Polak targets", "max targets")
#'
#' # plot solutions
#' plot(s, axes = FALSE)
#' }
#' @export
spec_max_targets <- function(x, ...) {
  # assert arguments are valid
  assert_valid_method_arg(x)
  assert(is_method(x))
  methods <- append(list(x), list(...))
  assert(
    all_elements_inherit(methods, "TargetMethod"),
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
  new_target_method(
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
    all_elements_inherit(methods, "TargetMethod"),
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
            # nocov start
            cli::cli_abort(
              c("i" = "Can't calculate input targets."),
              call = call,
              parent = cnd
            )
            # nocov end
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
            # nocov start
            cli::cli_abort(
              c("i" = "Can't calculate input targets."),
              call = call,
              parent = cnd
            )
            # nocov end
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
