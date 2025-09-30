#' @include internal.R Method-class.R
NULL

#' Specify targets based on minima
#'
#' Specify targets that are calculated based on the minimum of one or more
#' target setting methods.
#'
#' @inheritParams spec_max_targets
#'
#' @inheritSection spec_jung_targets Data calculations

#' @inherit spec_max_targets return
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
#' # create problem with targets based on the minimum of 20% targets,
#' # Jung et al. (2021) targets, and Polak et al. (2015) targets
#' # for each feature (separately)
#' p4 <-
#'   p0 %>%
#'   add_auto_targets(
#'     method = spec_min_targets(
#'       spec_relative_targets(0.2),
#'       spec_jung_targets(),
#'       spec_polak_targets()
#'     )
#'   )
#'
#' # solve problems
#' s <- c(solve(p1), solve(p2), solve(p3), solve(p4))
#' names(s) <- c("20% targets", "Jung targets", "Polak targets", "min targets")
#'
#' # plot solutions
#' plot(s, axes = FALSE)
#' }
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
