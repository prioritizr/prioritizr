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
#' \dontrun{
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
#' }
#' @export
compile <- function(x, ...) {
  assert_required(x)
  UseMethod("compile")
}

#' @rdname compile
#' @export
compile.ConservationProblem <- function(x, compressed_formulation = NA, ...) {
  # assert arguments are valid
  assert_required(x)
  assert_required(compressed_formulation)
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
        "See {.topic prioritizr::objectives} for guidance on selecting",
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
        "See {.topic prioritizr::targets} for guidance on selecting targets."
    )
  }
  if (inherits(x$objective, "MinimumPenaltiesObjective")) {
    verify(
      length(x$penalties) >= 1,
      msg = c(
        "{.arg problem} does not have any penalties.",
        "i" = paste(
          "Using {.fn add_min_penalties_objective} without any penalties",
          "will likely result in poorly optimized solutions."
        ),
        "i" = paste(
          "See {.topic prioritizr::penalties} for available penalties."
        )
      )
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
  user_compressed_formulation <- compressed_formulation
  if (is.na(compressed_formulation)) {
    compressed_possible <- vapply(
      x$constraints,
      FUN.VALUE = logical(1),
      function(i) i$compressed_formulation
    )
    compressed_formulation <- all(compressed_possible)
  }
  # if expanded formulation required, then check for negative feature values
  if (!isTRUE(compressed_formulation)) {
    ## if negative values present, then throw errors
    if (
        x$has_negative_feature_data() &&
        is.na(user_compressed_formulation)
      ) {
      ## find names of problem components that require expanded formulation
      expanded_components <- vapply(
        x$constraints[!compressed_possible],
        FUN.VALUE = character(1),
        function(i) i$name
      )
      #### throw generic error message if user is relying on default
      #### behavior for prioritizr to determine if expanded formulation
      #### should be used or not
      cli::cli_abort(
        c(
          "{.fn problem} contains negative feature values.",
          "i" = paste0(
            "The following constraints are not ",
            "compatible with such values: ",
            "\"", paste(expanded_components, collapse = "\", \""), "\"."
          ),
          "i" = paste(
            "Either remove/update features with negative",
            "values, or remove these components."
          )
        )
      )
    } else if (x$has_negative_feature_data()) {
      #### throw more specific error message if user is manually trying the
      #### expanded formulation for some reason
      cli::cli_abort(
        c(
          paste(
            "{.fn problem} contains negative feature values and",
            "{.arg compressed_formulation = FALSE}"
          ),
          "i" = paste(
            "The compressed formulation must be used with negative",
            "feature values."
          ),
          "i" = paste(
            "Either remove/replace features with negative",
            "values or use {.arg compressed_formulation = TRUE}."
          )
        )
      )
    }
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
  # run calculations for constraints
  for (i in seq_along(x$constraints)) {
    x$constraints[[i]]$calculate(x)
  }
  # add locked constraints to optimization problem,
  # this the penalties and other constraints can leverage locked values
  # if required
  for (i in seq_along(x$constraints)) {
    if (inherits(
      x$constraints[[i]],
      c("LockedInConstraint", "LockedOutConstraint", "LockedManualConstraint")
    )) {
      x$constraints[[i]]$apply(op, x)
    }
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
  # add remaining constraints
  for (i in seq_along(x$constraints)) {
    if (!inherits(
      x$constraints[[i]],
      c("LockedInConstraint", "LockedOutConstraint", "LockedManualConstraint")
    )) {
      x$constraints[[i]]$apply(op, x)
    }
  }
  # return problem object
  op
}
