#' @include internal.R ConservationProblem-class.R OptimizationProblem-class.R
NULL

#' Compile a problem
#'
#' Compile a conservation planning problem into an
#' mixed integer linear programming problem.
#'
#' @param x [problem()] object.
#'
#' @param compressed_formulation `logical` should the conservation problem
#'   compiled into a compressed version of a planning problem?
#'   If `TRUE` then the problem is expressed using the compressed
#'   formulation. If `FALSE` then the problem is expressed using the
#'   expanded formulation. If `NA`, then the compressed is used unless one
#'   of the constraints requires the expanded formulation. This argument
#'   defaults to `NA`.
#'
#' @param ... not used.
#'
#' @details This function might be useful for those interested in understanding
#'   how their conservation planning [problem()] is expressed
#'   as a mathematical problem. However, if the problem just needs to
#'   be solved, then the [solve()] function should just be used.
#'
#'   **Please note that in nearly all cases, the default argument to
#'   `compressed_formulation` should be used**. The only situation where
#'    manually
#'   setting the argument to `formulation` is desirable is during testing.
#'   Manually setting the argument to `formulation` will at best
#'   have no effect on the problem. At worst, it may result in
#'   an error, a misspecified problem, or unnecessarily long
#'   solve times.
#'
#' @return A [optimization_problem()] object.
#'
#' @examples
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # build minimal conservation problem
#' p <-
#'   problem(sim_pu_raster, sim_features) %>%
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
  rlang::check_required(x)
  rlang::check_required(compressed_formulation)
  assert(
    is_conservation_problem(x),
    assertthat::is.flag(compressed_formulation)
  )
  assert_dots_empty()
  # sanity checks
  targets_not_supported <- c(
    "MaximumUtilityObjective",
    "MaximumCoverageObjective"
  )
  if (
    inherits(x$objective, targets_not_supported) &&
    !is.Waiver(x$targets)
  ) {
    cli_warning(
      c(
        "Targets specified for the problem will be ignored.",
        "i" = "If the targets are important, use a different objective."
      )
    )
  }
  # replace waivers with defaults
  if (is.Waiver(x$objective)) {
    cli::cli_abort(
      "{.fn problem} must have an objective.",
      "i" = paste(
        "see {.topic prioritizr::objectives} for guidance on selecting",
        "an objective."
      )
    )
  }
  if (
    is.Waiver(x$targets) &&
    !inherits(
      x$objective,
      c("MaximumUtilityObjective", "MaximumCoverageObjective"))
  ) {
    cli::cli_abort(
      "{.fn problem} must have targets.",
      "i" =
        "see {.topic prioritizr::targets} for guidance on selecting targets."
    )
  }
  # add defaults if needed
  ## this shouldn't really be needed because the
  # default functions are now applied when the problem() is created
  # nocov start
  if (is.Waiver(x$decisions))
    x <- suppressWarnings(add_binary_decisions(x))
  if (is.Waiver(x$solver))
    x <- suppressWarnings(add_default_solver(x))
  if (is.Waiver(x$portfolio))
    x <- suppressWarnings(add_shuffle_portfolio(x, 1))
  # nocov end
  # initialize optimization problems
  op <- optimization_problem()
  # determine if expanded formulation is required
  if (is.na(compressed_formulation)) {
    compressed_formulation <- all(
      vapply(
        x$constraints,
        FUN.VALUE = logical(1),
        function(i) i$compressed_formulation
      )
    )
  }
  # generate targets
  if (is.Waiver(x$targets)) {
    # if objective doesn't actually use targets, create a "fake" targets tibble
    # to initialize rij matrix
    targets <- tibble::as_tibble(
      expand.grid(
        feature = seq_along(x$feature_names()),
        zone = seq_along(x$zone_names()),
        sense = "?",
        value = 0
      )
    )
    targets$zone <- as.list(targets$zone)
  } else {
    # generate "real" targets
    targets <- x$feature_targets()
  }
  # add rij data to optimization problem
  rcpp_add_rij_data(
    op$ptr, x$get_data("rij_matrix"), as.list(targets), compressed_formulation
  )
  # add decision types to optimization problem
  x$decisions$calculate(x)
  x$decisions$apply(op)
  # add objective to optimization problem
  x$objective$calculate(x)
  x$objective$apply(op, x)
  # add constraints for zones
  if ((x$number_of_zones() > 1)) {
    # detect if mandatory allocation constraints should be applied
    if (length(x$constraints) == 0) {
      apply_mandatory <- FALSE
    } else {
      apply_mandatory <- any(
        vapply(
          x$constraints, inherits, logical(1), "MandatoryAllocationConstraint"
        )
      )
    }
    # set constraint type
    ct <- ifelse(apply_mandatory, "=", "<=")
    # apply constraints
    rcpp_add_zones_constraints(op$ptr, ct)
  }
  # add penalties to optimization problem
  for (i in seq_along(x$penalties)) {
    ## run sanity check
    if (
      inherits(x$penalties[[i]], "FeatureWeights") &&
      inherits(
        x$objective,
        c("MinimumSetObjective", "MinimumLargestShortfallObjective")
      )
    ) {
      cli_warning(
        c(
          "Weights specified for the problem will be ignored.",
          "i" = "If the weights are important, use a different objective."
        )
      )
      next()
    }
    ## apply penalty if it makes sense to do so
    x$penalties[[i]]$calculate(x)
    x$penalties[[i]]$apply(op, x)
  }
  # add constraints to optimization problem
  for (i in seq_along(x$constraints)) {
    x$constraints[[i]]$calculate(x)
    x$constraints[[i]]$apply(op, x)
  }
  # return problem object
  op
}
