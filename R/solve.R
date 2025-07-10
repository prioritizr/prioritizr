#' @include internal.R ConservationProblem-class.R OptimizationProblem-class.R compile.R presolve_check.R
NULL

#' Solve
#'
#' Solve a conservation planning problem.
#'
#' @param a [problem()] object.
#'
#' @param b missing.
#'
#' @param ... arguments passed to [compile()].
#'
#' @param run_checks `logical` flag indicating whether presolve checks
#'   should be run prior solving the problem. These checks are performed using
#'   the [presolve_check()] function. Defaults to `TRUE`.
#'   Skipping these checks may reduce run time for large problems.
#'
#' @param force `logical` flag indicating if an attempt to should be
#'   made to solve the problem even if potential issues were detected during
#'   the presolve checks. Defaults to `FALSE`.
#'
#' @details
#' After formulating a conservation planning [problem()],
#' it can be solved using an exact algorithm solver (see [solvers]
#' for available solvers). If no solver has been explicitly specified,
#' then the best available exact algorithm solver will be used by default
#' (see [add_default_solver()]). Although these exact algorithm
#' solvers will often display a lot of information that isn't really that
#' helpful (e.g., nodes, cutting planes), they do display information
#' about the progress they are making on solving the problem (e.g., the
#' performance of the best solution found at a given point in time). If
#' potential issues were detected during the
#' presolve checks (see [presolve_check()])
#' and the problem is being forcibly solved (i.e., with `force = TRUE`),
#' then it is also worth checking for any warnings displayed by the solver
#' to see if these potential issues are actually causing issues
#' (e.g., *Gurobi* can display warnings that include
#' `"Warning: Model contains large matrix coefficient range"` and
#' `"Warning: Model contains large rhs"`).
#'
#' @section Output format:
#' This function will output solutions in a similar format to the
#' planning units associated with `a`. Specifically, it will return
#' solutions based on the following types of planning units.
#'
#'   \describe{
#'
#'   \item{`a` has `numeric` planning units}{The solution will be
#'    returned as a `numeric` vector. Here, each element in the vector
#'     corresponds to a different planning unit.
#'     Note that if a portfolio is used to generate multiple solutions,
#'     then a `list` of such `numeric` vectors will be returned.}
#'
#'   \item{`a` has `matrix` planning units}{The solution will be
#'     returned as a `matrix` object.
#'     Here, rows correspond to different planning units,
#'     and columns correspond to different  management zones.
#'     Note that if a portfolio is used to generate multiple solutions,
#'     then a `list` of such `matrix` objects will be returned.}
#'
#'   \item{`a` has [terra::rast()] planning units}{The solution
#'     will be returned as a [terra::rast()] object.
#'     If the argument to `x` contains multiple zones, then the object
#'     will have a different layer for each management zone.
#'     Note that if a portfolio is used to generate multiple solutions,
#'     then a `list` of [terra::rast()] objects will be returned.}
#'
#'   \item{`a` has [sf::sf()], or `data.frame` planning units}{
#'     The solution will be returned in the same data format as the planning
#'     units.
#'     Here, each row corresponds to a different planning unit,
#'     and columns contain solutions.
#'     If the argument to `a` contains a single zone, then the solution object
#'     will contain columns named by solution.
#'     Specifically, the column names containing the solution values
#'     be will named as `"solution_XXX"` where `"XXX"` corresponds to a solution
#'     identifier (e.g., `"solution_1"`).
#'     If the argument to `a` contains multiple zones, then the columns
#'     containing solutions will be named as `"solution_XXX_YYY"` where
#'     `"XXX"` corresponds to the solution identifier and `"YYY"` is the name
#'     of the management zone (e.g., `"solution_1_zone1"`).}
#'
#'   }
#'
#' @return
#' A `numeric`, `matrix`, `data.frame`, [sf::st_sf()], or
#' [terra::rast()] object containing the solution to the problem.
#' Additionally, the returned object has attributes that describe
#' optimization process or solution (see below  for examples on accessing
#' these attributes). These attributes provide the following information.
#' \describe{
#' \item{\code{objective}}{
#' \code{numeric} mathematical objective value for the solution used
#' to evaluate the prioritization during optimization.
#' }
#' \item{\code{runtime}}{
#' \code{numeric} total amount of time elapsed while during the optimization
#' process (reported in seconds). Note that this measure of time does not
#' include any data pre-processing or post-processing steps.
#' }
#' \item{\code{status}}{
#' \code{character} status of the optimization process.
#' This status typically describes
#' the reason why the optimization process terminated. For example,
#' it might indicate that the optimization process terminated because
#' an optimal solution was found, or because a pre-specified time limit
#' was reached. These status values are (mostly) obtained directly from
#' the solver software, and so we recommend consulting the solver's
#' documentation for further information on what particular status values mean.
#' Note that some solvers (e.g., Gurobi and HiGHS) will return
#' an `"OPTIMAL"` status when the solver has found a solution within the
#' pre-specified optimality gap (e.g., it has found a solution within 10% of
#' optimality), even though the solution itself may not be strictly optimal.
#' }
#' \item{\code{gap}}{
#' \code{numeric} optimality of the solution. This gap value provides an upper
#' bound of how far the solution is from optimality.
#' For example, you might specify a
#' 10% optimality gap for the optimization process (e.g., using
#' `add_highs_solver(gap = 0.1)`), and this might produce a solution that is
#' actually 5% from optimality. As such, the solution might have a gap value
#' of 0.05 (corresponding to 5%). Because this value represents an upper bound,
#' it is also possible that the solution in this example
#' -- even though it is actually 5% from optimality -- might have a gap value
#' of 7% (i.e., 0.07). Note that only some solvers are able to
#' provide this information (i.e., the *Gurobi* and *HiGHS* solvers),
#' and the gap value for other solvers will contain missing (`NA`) values.
#' }
#' }
#'
#' @seealso
#' See [problem()] to create conservation planning problems, and
#' [presolve_check()] to check problems for potential issues.
#' Also, see the [category_layer()] and [category_vector()] function to
#' reformat solutions that contain multiple zones.
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_pu_polygons <- get_sim_pu_polygons()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # build minimal conservation problem with raster data
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve the problem
#' s1 <- solve(p1)
#'
#' # print solution
#' print(s1)
#'
#' # print attributes describing the optimization process and the solution
#' print(attr(s1, "objective"))
#' print(attr(s1, "runtime"))
#' print(attr(s1, "status"))
#' print(attr(s1, "gap"))
#'
#' # calculate feature representation in the solution
#' r1 <- eval_feature_representation_summary(p1, s1)
#' print(r1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # build minimal conservation problem with polygon data
#' p2 <-
#'   problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve the problem
#' s2 <- solve(p2)
#'
#' # print solution
#' print(s2)
#'
#' # calculate feature representation in the solution
#' r2 <- eval_feature_representation_summary(p2, s2[, "solution_1"])
#' print(r2)
#'
#' # plot solution
#' plot(s2[, "solution_1"], main = "solution", axes = FALSE)
#'
#' # build multi-zone conservation problem with raster data
#' p3 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve the problem
#' s3 <- solve(p3)
#'
#' # print solution
#' print(s3)
#'
#' # calculate feature representation in the solution
#' r3 <- eval_feature_representation_summary(p3, s3)
#' print(r3)
#'
#' # plot solution
#' plot(category_layer(s3), main = "solution", axes = FALSE)
#'
#' # build multi-zone conservation problem with polygon data
#' p4 <-
#'   problem(
#'     sim_zones_pu_polygons, sim_zones_features,
#'     cost_column = c("cost_1", "cost_2", "cost_3")
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve the problem
#' s4 <- solve(p4)
#'
#' # print solution
#' print(s4)
#'
#' # calculate feature representation in the solution
#' r4 <- eval_feature_representation_summary(
#'   p4, s4[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' )
#' print(r4)
#'
#' # create new column representing the zone id that each planning unit
#' # was allocated to in the solution
#' s4$solution <- category_vector(
#'   s4[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' )
#' s4$solution <- factor(s4$solution)
#'
#' # plot solution
#' plot(s4[, "solution"])
#' }
#' @name solve
NULL

#' @rdname solve
#' @method solve ConservationProblem
#' @export solve.ConservationProblem
#' @export
solve.ConservationProblem <- function(a, b, ...,
                                      run_checks = TRUE, force = FALSE) {
  # assert arguments are valid
  assert_required(a)
  assert(
    assertthat::is.flag(run_checks),
    assertthat::noNA(run_checks),
    assertthat::is.flag(force),
    assertthat::noNA(force)
  )
  if (!rlang::is_missing(b)) {
    cli::cli_abort("{.arg b} must not be specified.") # nocov
  }
  # compile optimization problem
  opt <- compile.ConservationProblem(a, ...)
  # run presolve check to try to identify potential problems
  if (run_checks) {
    ## run checks
    presolve_res <- internal_presolve_check(opt)
    ## prepare message
    msg <- presolve_res$msg
    if (!isTRUE(force)) {
      msg <- c(
        msg,
        "i" = paste(
          "To ignore checks and attempt optimization anyway,",
          "use {.code solve(force = TRUE)}."
        )
      )
    }
    ## determine if error or warning should be thrown
    if (!isTRUE(force)) {
      f <- assert
    } else {
      f <- verify
    }
    ## throw error or warning if checks failed
    f(isTRUE(presolve_res$pass), msg = msg)
  }
  # solve problem
  sol <- a$portfolio$run(opt, a$solver)
  # check that solution is valid
  assert(is_valid_raw_solution(sol, time_limit = a$solver$data$time_limit))
  # check that desired number of solutions were found
  portfolio_number_solutions <- a$portfolio$get_data("number_solutions")
  if (!is.Waiver(portfolio_number_solutions)) {
    if (length(sol) != portfolio_number_solutions) {
      cli_warning(
        paste(
          "Portfolio could only find",
          "{.val {length(sol)}} out of",
          "{.val {portfolio_number_solutions}}",
          "solution{?s}."
        )
      )
    }
  }
  ## format solutions
  # format solutions into planning unit by zones matrix
  na_pos <- which(is.na(a$planning_unit_costs()), arr.ind = TRUE)
  sol_status <- lapply(sol, function(x) {
    m <- matrix(
      x[[1]][seq_len(a$number_of_planning_units() * a$number_of_zones())],
      nrow = a$number_of_planning_units(),
      ncol = a$number_of_zones()
    )
    m[na_pos] <- NA_real_
    m
  })
  # create solution data
  pu <- a$data$cost
  if (inherits(pu, c("SpatRaster", "Raster"))) {
    # SpatRaster or Raster planning units
    pos <- a$planning_unit_indices()
    pu <- terra::rast(pu)
    pu <- suppressWarnings(terra::setValues(pu[[1]], NA_real_))
    ret <- lapply(sol_status, function(s) {
      ret <- lapply(seq_len(ncol(s)), function(z) {
        pu[pos] <- s[, z]
        pu
      })
      ret <- terra::rast(ret)
      names(ret) <- a$zone_names()
      ret
    })
    # convert to RasterStack or RasterLayer if needed
    if (inherits(a$data$cost, c("RasterStack", "RasterBrick"))) {
      ret <- lapply(ret, raster::stack)
    } else if (inherits(a$data$cost, "RasterLayer")) {
      ret <- lapply(ret, raster::raster)
    }
    names(ret) <- paste0("solution_", seq_along(sol))
  } else if (inherits(pu, c("data.frame", "Spatial", "sf"))) {
    # Spatial* or data.frame planning units
    sol_status <- do.call(cbind, sol_status)
    if (a$number_of_zones() == 1) {
      colnames(sol_status) <- paste0("solution_", seq_along(sol))
    } else {
      colnames(sol_status) <- paste0(
        "solution_",
        rep(seq_along(sol), each = a$number_of_zones()),
        "_",
        rep(a$zone_names(), length(sol))
      )
    }
    # add in NA values for planning units that contained NA values in
    # all zones that were discarded from the mathematical formulation
    # to reduce overheads
    pos <- a$planning_unit_indices()
    if (!identical(pos, seq_len(a$number_of_total_units()))) {
      sol_status2 <- matrix(
        NA_real_,
        nrow = a$number_of_total_units(),
        ncol = ncol(sol_status)
      )
      sol_status2[pos, ] <- sol_status
      dimnames(sol_status2) <- dimnames(sol_status)
    } else {
      sol_status2 <- sol_status
    }
    # cbind solutions to planning unit data
    sol_status2 <- as.data.frame(sol_status2)
    if (inherits(pu, "Spatial")) {
      ret <- pu
      ret@data <- cbind(ret@data, sol_status2)
    } else if (inherits(pu, "sf")) {
      ret <- tibble::as_tibble(data.frame(pu, sol_status2))
      sf_col <- attr(pu, "sf_column")
      ret <- ret[, c(setdiff(names(ret), sf_col), sf_col), drop = FALSE]
      ret <- ret <- sf::st_sf(ret)
    } else {
      ret <- cbind(pu, sol_status2)
      if (inherits(pu, "tbl_df")) {
        pu <- tibble::as_tibble(pu)
      }
    }
  } else if (is.matrix(pu)) {
    # matrix planning units
    # add in NA values for planning units that contained NA values in
    # all zones that were discarded from the mathematical formulation
    # to reduce overheads
    pos <- a$planning_unit_indices()
    pu[] <- NA
    colnames(pu) <- a$zone_names()
    ret <- lapply(sol_status, function(s) {
      pu[pos, ] <- s
      pu
    })
    names(ret) <- paste0("solution_", seq_along(sol))
  } else {
    # nocov start
    cli::cli_abort(
      "Planning unit data is of an unrecognized class.",
      .internal = TRUE
    )
    # nocov end
  }
  # if ret is a list of matrices with a single column then convert to numeric
  if (is.matrix(ret[[1]]) && ncol(ret[[1]]) == 1) {
    ret <- lapply(ret, as.numeric)
  }
  # if ret is a list with a single element then extract the element
  if (length(ret) == 1) {
    ret <- ret[[1]]
  }
  # add attributes
  attr(ret, "objective") <- stats::setNames(
    vapply(sol, `[[`, numeric(1), 2), paste0("solution_", seq_along(sol))
  )
  attr(ret, "status") <- stats::setNames(
    vapply(sol, `[[`, character(1), 3), paste0("solution_", seq_along(sol))
  )
  attr(ret, "runtime") <- stats::setNames(
    vapply(sol, `[[`, numeric(1), 4), paste0("solution_", seq_along(sol))
  )
  attr(ret, "gap") <- stats::setNames(
    vapply(sol, `[[`, numeric(1), 5), paste0("solution_", seq_along(sol))
  )
  # return object
  ret
}
