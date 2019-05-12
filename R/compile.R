#' @include internal.R ConservationProblem-proto.R OptimizationProblem-proto.R
NULL

#' Compile a problem
#'
#' Compile a conservation planning \code{\link{problem}} into an
#' (potentially mixed) integer linear programming problem.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param compressed_formulation \code{logical} should the conservation problem
#'   compiled into a compressed version of a planning problem?
#'   If \code{TRUE} then the problem is expressed using the compressed
#'   formulation. If \code{FALSE} then the problem is expressed using the
#'   expanded formulation. If \code{NA}, then the compressed is used unless one
#'   of the constraints requires the expanded formulation. This argument
#'   defaults to \code{NA}.
#'
#' @param ... not used.
#'
#' @details This function might be useful for those interested in understanding
#'   how their conservation planning \code{\link{problem}} is expressed
#'   as a mathematical problem. However, if the problem just needs to
#'   be solved, then the \code{\link{solve}} function should just be used.
#'
#'   \strong{Please note that in nearly all cases, the default argument to
#'   \code{formulation} should be used}. The only situation where manually
#'   setting the argument to \code{formulation} is desirable is during testing.
#'   Manually setting the argument to \code{formulation} will at best
#'   have no effect on the problem. At worst, it may result in
#'   an error, a misspecified problem, or unnecessarily long
#'   solve times.
#'
#' @return \code{\link{OptimizationProblem-class}} object.
#'
#' @examples
#' # build minimal conservation problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1)
#'
#' # compile the conservation problem into an optimization problem
#' o <- compile(p)
#'
#' # print the optimization problem
#' print(o)
#'
#' @export
compile <- function(x, ...) UseMethod("compile")

#' @rdname compile
#' @export
compile.ConservationProblem <- function(x, compressed_formulation = NA, ...) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
    no_extra_arguments(...),
    is.na(compressed_formulation) ||
          assertthat::is.flag(compressed_formulation))
  # sanity checks
  if (inherits(x$objective, c("MaximumUtilityObjective",
                              "MaximumCoverageObjective")) &
      !is.Waiver(x$targets))
    warning(paste("ignoring targets since the specified objective",
                  "function doesn't use targets"))
  # replace waivers with defaults
  if (is.Waiver(x$objective))
    x <- add_default_objective(x)
  if (is.Waiver(x$targets) & !inherits(x$objective,
                                       c("MaximumUtilityObjective",
                                         "MaximumCoverageObjective")))
    x <- add_default_targets(x)
  if (is.Waiver(x$decisions))
    x <- add_default_decisions(x)
  if (is.Waiver(x$solver))
    x <- add_default_solver(x)
  op <- new_optimization_problem()
  # determine if expanded formulation is required
  if (is.na(compressed_formulation))
    compressed_formulation <- all(vapply(x$constraints$ids(),
      function(i) x$constraints[[i]]$compressed_formulation, logical(1)))
  # generate targets
  if (is.Waiver(x$targets)) {
    # if objective doesn't actually use targets, create a "fake" targets tibble
    # to initialize rij matrix
    targets <- tibble::as_tibble(expand.grid(
      feature = seq_along(x$feature_names()),
      zone = seq_along(x$zone_names()),
      sense = "?",
      value = 0))
    targets$zone <- as.list(targets$zone)
  } else {
    # generate "real" targets
    targets <- x$feature_targets()
  }
  # add rij data to optimization problem
  rcpp_add_rij_data(op$ptr, x$get_data("rij_matrix"), as.list(targets),
                    compressed_formulation)
  # add decision types to optimization problem
  x$decisions$calculate(x)
  x$decisions$apply(op)
  # add objective to optimization problem
  x$objective$calculate(x)
  x$objective$apply(op, x)
  # add constraints for zones
  if (x$number_of_zones() > 1) {
    # detect if allocation constraints are mandatory
    r <- try(x$constraints$find("Mandatory allocation constraints"),
             silent = TRUE)
    # set constraint type
    ct <- ifelse(
      !inherits(r, "try-error") &&
      isTRUE(x$constraints[[r]]$get_parameter("apply constraints?") == 1L),
      "=", "<=")
    # apply constraints
    rcpp_add_zones_constraints(op$ptr, ct)
  }
  # add penalties to optimization problem
  for (i in x$penalties$ids()) {
    x$penalties[[i]]$calculate(x)
    x$penalties[[i]]$apply(op, x)
  }
  # add constraints to optimization problem
  for (i in x$constraints$ids()) {
    x$constraints[[i]]$calculate(x)
    x$constraints[[i]]$apply(op, x)
  }
  # return problem object
  op
}
