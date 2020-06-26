#' @include internal.R ConservationProblem-proto.R OptimizationProblem-proto.R compile.R presolve_check.R
NULL

#' Solve
#'
#' Solve a conservation planning \code{\link{problem}}.
#'
#' @param a \code{\link{ConservationProblem-class}} or an
#'   \code{\link{OptimizationProblem-class}} object.
#'
#' @param b \code{\link{Solver-class}} object. Not used if `a` is an
#'   \code{\link{ConservationProblem-class}} object.
#'
#' @param ... arguments passed to \code{\link{compile}}.
#'
#' @param run_checks `logical` flag indicating whether presolve checks
#'   should be run prior solving the problem. These checks are performed using
#'   the \code{\link{presolve_check}} function. Defaults to `TRUE`.
#'   Skipping these checks may reduce run time for large problems.
#'
#' @param force `logical` flag indicating if an attempt to should be
#'   made to solve the problem even if potential issues were detected during
#'   the presolve checks. Defaults to `FALSE`.
#'
#' @details
#'   After formulating a conservation planning \code{\link{problem}},
#'   it can be solved using an exact algorithm solver (see \code{\link{solvers}}
#'   for available solvers). If no solver has been explicitly specified,
#'   then the best available exact algorithm solver will be used by default
#'   (see \code{\link{add_default_solver}}. Although these exact algorithm
#'   solvers will often display a lot of information that isn't really that
#'   helpful (e.g. nodes, cutting planes), they do display information
#'   about the progress they are making on solving the problem (e.g. the
#'   performance of the best solution found at a given point in time). If
#'   potential issues were detected during the
#'   presolve checks (see \code{\link{presolve_check}})
#'   and the problem is being forcibly solved (i.e. with `force = TRUE`),
#'   then it is also worth checking for any warnings displayed by the solver
#'   to see if these potential issues are actually causing issues
#'   (e.g. *Gurobi* can display warnings that include
#'   `"Warning: Model contains large matrix coefficient range"` and
#'   `"Warning: Model contains large rhs"`).
#'
#'   The object returned from this function depends on the argument to
#'   `a`. If the argument to `a` is an
#'   \code{\link{OptimizationProblem-class}} object, then the
#'   solution is returned as a `logical` `vector` showing the status
#'   of each planning unit in each zone. However, in most cases, the argument
#'   to `a` is an \code{\link{ConservationProblem-class}} object, and so
#'   the type of object returned depends on the number of solutions
#'   generated and the type data used to represent the planning units:
#'
#'   \describe{
#'
#'   \item{`numeric`}{`vector` containing the solution. Here,
#'     Each element corresponds to a different planning unit. If
#'     multiple solutions are generated, then the solution is returned as
#'     a `list` of `numeric` `vectors`.}
#'
#'   \item{`matrix`}{containing `numeric` values for the solution.
#'     Here, rows correspond to different planning units,
#'     and fields (columns) correspond to different  management zones. If
#'     multiple solutions are generated, then the solution is returned as
#'     a `list` of `matrix` objects.}
#'
#'   \item{\code{\link[raster]{Raster-class}}}{object containing the solution
#'     in pixel values. If the argument to `x` contains a single
#'     management zone, then a `RasterLayer` object will be returned.
#'     Otherwise, if the argument to `x` contains multiple zones, then a
#'     \code{\link[raster]{RasterStack-class}} object
#'     will be returned containing a different layer for each management zone.
#'     If multiple solutions are generated, then the solution is returned as
#'     a `list` of `Raster` objects.}
#'
#'   \item{\code{\link[sp]{Spatial-class}}, \code{\link[sf]{sf}}, or
#'     `data.frame`}{
#'     containing the solution in fields (columns). Here, each row
#'     corresponds to a different planning unit. If the argument to `x`
#'     contains a single zone, the fields containing solutions are named
#'     `"solution_XXX"` where `"XXX"` corresponds to the solution
#'     number. If the argument to `x` contains multiple zones, the fields
#'     containing solutions are named `"solution_XXX_YYY"` where
#'     `"XXX"` corresponds to the solution and `"YYY"` is the name
#'     of the management zone.}
#'
#'   }
#'
#'   After solving problems that contain multiple zones,
#'   it may be useful to use the \code{\link{category_layer}} or
#'   \code{\link{category_vector}} function to reformat the output.
#'
#' @return A `numeric`, `matrix`,
#'   \code{\link[raster]{RasterLayer-class}}, \code{\link[sp]{Spatial-class}},
#'   or \code{\link[sf]{sf}} object containing the solution to the problem.
#'   Additionally, the returned object will have the following additional
#'   attributes: `"objective"` containing the solution's objective,
#'   `"runtime"` denoting the number of seconds that elapsed while solving
#'   the problem, and `"status"` describing the status of the solution
#'   (e.g. `"OPTIMAL"` indicates that the optimal solution was found).
#'   In most cases, the first solution (e.g. `"solution_001"`))
#'   will contain the best solution found by the solver (note that this
#'   may not be an optimal solution depending on the gap used to solve
#'   the problem and noting that the default gap is 0.1).
#'
#' @seealso \code{\link{feature_representation}}, \code{\link{problem}},
#'   \code{\link{solvers}}, \code{\link{category_layer}},
#'   \code{\link{presolve_check}}.
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_raster, sim_pu_polygons, sim_pu_sf, sim_features,
#'      sim_pu_zones_stack, sim_pu_zones_sf, sim_features_zones)
#'
#' # build minimal conservation problem with raster data
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions()
#' \donttest{
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
#'
#' # calculate feature representation in the solution
#' r1 <- feature_representation(p1, s1)
#' print(r1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE, box = FALSE)
#' }
#' # build minimal conservation problem with polygon (Spatial) data
#' p2 <- problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve the problem
#' s2 <- solve(p2)
#'
#' # print first six rows of the attribute table
#' print(head(s2))
#'
#' # calculate feature representation in the solution
#' r2 <- feature_representation(p2, s2[, "solution_1"])
#' print(r2)
#'
#' # plot solution
#' spplot(s2, zcol = "solution_1", main = "solution", axes = FALSE, box = FALSE)
#' }
#'
#' # build minimal conservation problem with polygon (sf) data
#' p3 <- problem(sim_pu_sf, sim_features, cost_column = "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve the problem
#' s3 <- solve(p3)
#'
#' # print first six rows of the attribute table
#' print(head(s3))
#'
#' # calculate feature representation in the solution
#' r3 <- feature_representation(p3, s3[, "solution_1"])
#' print(r3)
#'
#' # plot solution
#' plot(s3[, "solution_1"])
#' }
#'
#' # build multi-zone conservation problem with raster data
#' p4 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve the problem
#' s4 <- solve(p4)
#'
#' # print solution
#' print(s4)
#'
#' # calculate feature representation in the solution
#' r4 <- feature_representation(p4, s4)
#' print(r4)
#'
#' # plot solution
#' plot(category_layer(s4), main = "solution", axes = FALSE, box = FALSE)
#' }
#' # build multi-zone conservation problem with polygon (sf) data
#' p5 <- problem(sim_pu_zones_sf, sim_features_zones,
#'               cost_column = c("cost_1", "cost_2", "cost_3")) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve the problem
#' s5 <- solve(p5)
#'
#' # print first six rows of the attribute table
#' print(head(s5))
#'
#' # calculate feature representation in the solution
#' r5 <- feature_representation(p5, s5[, c("solution_1_zone_1",
#'                                         "solution_1_zone_2",
#'                                         "solution_1_zone_3")])
#' print(r5)
#'
#' # create new column representing the zone id that each planning unit
#' # was allocated to in the solution
#' s5$solution <- category_vector(s5[, c("solution_1_zone_1",
#'                                       "solution_1_zone_2",
#'                                       "solution_1_zone_3")])
#' s5$solution <- factor(s5$solution)
#'
#' # plot solution
#' plot(s5[, "solution"])
#' }
#' @name solve
#'
#' @importFrom Matrix solve
#'
#' @exportMethod solve
#'
#' @aliases solve,OptimizationProblem,Solver-method solve,ConservationProblem,missing-method
#'
#' @export
NULL

#' @name solve
#'
#' @rdname solve
methods::setMethod(
  "solve",
  signature(a = "OptimizationProblem", b = "Solver"),
  function(a, b, ...) b$solve(a)
)

#' @name solve
#'
#' @rdname solve
methods::setMethod(
  "solve",
  signature(a = "ConservationProblem", b = "missing"),
  function(a, b, ..., run_checks = TRUE, force = FALSE) {
    # assert arguments are valid
    assertthat::assert_that(
      assertthat::is.flag(run_checks), assertthat::noNA(run_checks),
      assertthat::is.flag(force), assertthat::noNA(force))
    # assign default solver and portfolio
    if (inherits(a$solver, "Waiver"))
      a <- add_default_solver(a)
    default_portfolio <- inherits(a$portfolio, "Waiver")
    if (inherits(a$portfolio, "Waiver"))
      a <- add_default_portfolio(a)
    # run presolve check to try to identify potential problems
    if (run_checks) {
      ch <- presolve_check(a)
      if (!isTRUE(force) && !isTRUE(ch))
        stop(paste("problem failed presolve checks, for more information see",
                   "?presolve_check"))
    }
    # compile optimisation problem
    opt <- compile.ConservationProblem(a, ...)
    # solve problem
    sol <- a$portfolio$run(opt, a$solver)
    # check that solution is valid
    if (is.null(sol) || is.null(sol[[1]]$x)) {
      stop("no solution found (e.g. due to problem infeasibility or time ",
           "limits)")
    }
    ## format solutions
    # format solutions into planning unit by zones matrix
    na_pos <- which(is.na(a$planning_unit_costs()), arr.ind = TRUE)
    sol_status <- lapply(sol, function(x) {
      m <- matrix(x[[1]][seq_len(a$number_of_planning_units() *
                                 a$number_of_zones())],
                  nrow = a$number_of_planning_units(),
                  ncol = a$number_of_zones())
      m[na_pos] <- NA_real_
      return(m)
    })
    # create solution data
    pu <- a$data$cost
    if (inherits(pu, "Raster")) {
      # RasterLayer planning units
      if (raster::nlayers(pu) == 1) {
        pos <- raster::Which(!is.na(pu), cells = TRUE)
      } else {
        pos <- raster::Which(max(!is.na(pu)) > 0, cells = TRUE)
      }
      pu <- suppressWarnings(raster::setValues(pu[[1]], NA))
      ret <- lapply(sol_status, function(s) {
        ret <- lapply(seq_len(ncol(s)), function(z) {
          pu[pos] <- s[, z]
          return(pu)
        })
        if (length(ret) > 1) {
          ret <- raster::stack(ret)
        } else {
          ret <- ret[[1]]
        }
        return(ret)
      })
      names(ret) <- paste0("solution_", seq_along(sol))
    } else if (inherits(pu, c("data.frame", "Spatial", "sf"))) {
      # Spatial* or data.frame planning units
      sol_status <- do.call(cbind, sol_status)
      if (a$number_of_zones() == 1) {
        colnames(sol_status) <- paste0("solution_", seq_along(sol))
      } else {
        colnames(sol_status) <- paste0("solution_",
                                       rep(seq_along(sol),
                                          each = a$number_of_zones()), "_",
                                       rep(a$zone_names(), length(sol)))
      }
      # add in NA values for planning units that contained NA values in
      # all zones that were discarded from the mathematical formulation
      # to reduce overheads
      pos <- which(rowSums(!is.na(as.matrix(
               as.data.frame(pu)[, a$data$cost_column, drop = FALSE]))) > 0)
      if (!identical(pos, seq_len(a$number_of_total_units()))) {
        sol_status2 <- matrix(NA_real_, nrow = a$number_of_total_units(),
                              ncol = ncol(sol_status))
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
        ret <- sf::st_sf(data.frame(pu, sol_status2))
      } else {
        ret <- cbind(pu, sol_status2)
      }
    } else if (is.matrix(pu)) {
      # matrix planning units
      # add in NA values for planning units that contained NA values in
      # all zones that were discarded from the mathematical formulation
      # to reduce overheads
      pos <- which(rowSums(!is.na(pu)) > 0)
      pu[] <- NA
      colnames(pu) <- a$zone_names()
      ret <- lapply(sol_status, function(s) {
        pu[pos, ] <- s
        return(pu)
      })
      names(ret) <- paste0("solution_", seq_along(sol))
    } else {
      stop("planning unit data is of an unrecognized class")
    }
    # if ret is a list of matrices with a single column then convert to numeric
    if (is.matrix(ret[[1]]) && ncol(ret[[1]]) == 1)
      ret <- lapply(ret, as.numeric)
    # if ret is a list with a single element then extract the element
    if (length(ret) == 1 && default_portfolio)
      ret <- ret[[1]]
    # add attributes
    attr(ret, "objective") <- stats::setNames(vapply(sol, `[[`, numeric(1), 2),
                                              paste0("solution_",
                                                     seq_along(sol)))
    attr(ret, "status") <- stats::setNames(vapply(sol, `[[`, character(1), 3),
                                           paste0("solution_", seq_along(sol)))
    attr(ret, "runtime") <- stats::setNames(vapply(sol, `[[`, numeric(1), 4),
                                            paste0("solution_", seq_along(sol)))
    # return object
    return(ret)
  }
)
