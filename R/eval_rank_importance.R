#' @include internal.R ConservationProblem-class.R OptimizationProblem-class.R compile.R problem.R solve.R presolve_check.R
NULL

#' Evaluate solution importance using incremental ranks
#'
#' Calculate importance scores for the planning units selected in a solution
#' using an incremental rank procedure (based on Jung *et al.* 2021).
#'
#' @inheritParams eval_replacement_importance
#'
#' @param by_zone `logical` value indicating how budgets should be calculated
#'  when `x` has multiple zones.
#'  If `TRUE`, then the incremental rank procedure will
#'  increment budgets for each zone separately.
#'  If `FALSE`, then the incremental rank procedure will
#'  increment a single budget that is applied to all zones.
#'  Note that this parameter is only considered if `n` is specified,
#'  and does not affect processing if `budgets` is specified.
#'  Defaults to `TRUE`.
#'
#' @param objective `character` value with the name of the objective function
#' that should be used for the incremental rank procedure.
#' This function must be budget limited (e.g., cannot be
#' [add_min_set_objective()]).
#' For example, `"add_min_shortfall_objective"` can be used to specify the
#' minimum shortfall objective (per [add_min_shortfall_objective()]).
#' Defaults to `NULL` such that the incremental rank procedure will use
#' the objective specified by `x`. If using this default and `x` has the minimum
#' set objective, then the minimum shortfall objective is used
#' (per [add_min_shortfall_objective()]).
#'
#' @param extra_args `list` with additional arguments for the
#' objective function (excluding the `budget` parameter). For example, this
#' parameter can be used to supply phylogenetic data for
#' the phylogenetic diversity objective function (i.e., when using
#' `objective = "add_max_phylo_div_objective"`).
#' Defaults to `NULL` such that no additional arguments are supplied.
#'
#' @param n `integer` number of increments for the incremental rank procedure.
#'  Note that either `n` or `budgets` (not both) must be specified.
#'  If `n` is specified, then `by_zone` is considered during
#'  processing for problems with multiple zones.
#'
#' @param budgets `numeric` vector with budget thresholds for each
#'  increment in the incremental rank procedure.
#'  Note that either `n` or `budgets` (not both) must be specified.
#'
#' @param ... not used.
#'
#' @details
#' Importance scores are calculated using an incremental rank procedure.
#' Note that if a problem (per `x`) has complex constraints (i.e.,
#' constraints that do not involve locking in or locking out planning
#' units), then the `budgets` parameter must be specified.
#' The incremental rank procedure involves the following steps.
#' 1. A set of budgets are defined.
#'  If an argument to the `budgets` parameter is supplied,
#' then the budgets are defined using the `budgets`.
#' Otherwise, if an argument to the `n` parameter is supplied,
#' then the budgets are automatically calculated as a set of values --
#' with equal increments between successive values -- that range to a maximum
#' value that is equal to the total cost of `solution`.
#' For example, if considering a problem (per `x`) with a single zone, a
#' solution with a total cost of 400, and `n = 4`: then the budgets will be
#' automatically calculated as 100, 200, 300, and 400.
#' If considering a multiple zone problem and `by_zone = FALSE`, then the
#' budgets will based calculated based on the total cost of the `solution`
#' across all zones.
#' Otherwise if `by_zone = TRUE`, then the budgets are calculated and set
#' based on the total cost of planning units allocated to each zone (separately)
#' in the `solution`. Note that after running this function, you can
#' see what budgets were defined by accessing
#' attributes from the result (see below for examples).
#' 2. The problem (per `x`) is checked for potential issues.
#' This step is performed to avoid issues during subsequent optimization steps.
#' Note that this step can be skipped using `run_checks = FALSE`.
#' Also, if issues are detected and you wish to proceed anyway,
#' then use`force = TRUE` ignore any detected issues.
#' 3. The problem is modified for subsequent optimization. In particular,
#' the upper bounds for the planning units in the problem are specified
#' based on the `solution`. For problems (per `x`) that have binary decision
#' types, this step is equivalent to locking out any planning units that are
#' not selected in the `solution`. Note that this step is important
#' to ensure that all subsequent optimization processes produce solutions
#' that are nested within the `solution`.
#' 4. The problem is further modified for subsequent optimization.
#' Specifically, its objective is overwritten using the objective defined for
#' the incremental rank procedure (per `objective`) with the budget
#' defined for the first increment. When this step is
#' repeated during subsequent increments, the objective will be overwritten with
#' with the budget defined for the next increment.
#' Additionally, if an argument to the `extra_args` parameter is specified,
#' this argument is used when overwriting the objective.
#' 5. The modified problem is solved to generate a solution.
#' Due to the steps used to modify the problem (i.e., steps 3 and 4), the newly
#' generated solution will contain a subset of the selected planning units in
#' the original `solution`.
#' 6. The status of the planning units in the newly generated solution are
#' recorded for later use
#' (e.g., binary values indicating if planning units were selected or not,
#' or the proportion of each planning unit selected) .
#' 7. The problem is further modified for subsequent optimization.
#' Specifically, the status of the planning units in the newly generated
#' solution are used to set the lower bounds for the planning units in the
#' problem. For problems with binary type decision variables, this step
#' is equivalent to modifying the problem to lock in planning units that
#' were selected by the newly generated solution.
#' Additionally, the newly generated solution is used to specify the starting
#' solution for the subsequent optimization process to reduce processing time
#' (note this is only done when using the *CBC* or *Gurobi* solvers).
#' 8. Steps 4--7 are repeated for each of the remaining budget increments.
#' As increasingly greater budgets are used at higher increments,
#' the modified problem will begin to generate solutions that become
#' increasingly more similar to the original `solution`.
#' Note that the status of the planning units in each of these new solutions are
#' recorded for later use.
#' 9. The incremental optimization rank procedure has now completed.
#' The planning unit solution statuses that were previously recorded in each
#' iteration are used to compute relative importance scores.
#' These relative importance scores range between 0 and 1, with higher scores
#' indicating that a given planning unit was selected in earlier increments and
#' is more cost-effective for meeting the objective (per `objective`).
#' In particular, for a given planning unit, the importance score is calculated
#' based on the arithmetic mean of the status values.
#' For example, if we performed an incremental rank procedure with five
#' increments
#' and binary decision variables, then a planning unit might have been selected
#' in the second increment. In this example, the planning unit would have the
#' following solution statuses across the five increments: (1st increment) 0,
#' (2nd increment) 1, (3rd increment) 1, (4th increment) 1, and
#' (5th increment) 1. The mean of these values is 0.8, and so the planning unit
#' would have an importance score of 0.8. A score of 0.8 is relatively high,
#' and suggests that this planning unit is highly cost-effective.
#' 10. The importance scores are output in the same format as the planning units
#' in the problem (per `x`) (see the Solution Format section for details).
#'
#' @inheritSection eval_cost_summary Solution format
#'
#' @inherit eval_replacement_importance seealso
#'
#' @return
#' A `numeric`, `matrix`, `data.frame`,
#' [terra::rast()], or [sf::sf()] object
#' containing importance scores for the planning units in the solution.
#' Specifically, the returned object is in the
#' same format as the planning unit data in the argument to `x`.
#' The object also has the following attributes that provide information
#' on the incremental rank procedure.
#' \describe{
#' \item{\code{budgets}}{
#' \code{numeric} vector or \code{matrix} containing the budgets used for
#' each increment in the incremental rank procedure.
#' If the problem (per \code{x}) has a single zone, then the budgets
#' are a \code{numeric} vector, wherein values correspond to the
#' budgets for each increment.
#' Otherwise, if the problem (per \code{x}) has multiple zones, then
#' the budgets are a \code{matrix} and their format depends on the
#' \code{by_zone} parameter.
#' If \code{by_zone = FALSE}, then the budgets are a \code{matrix}
#' with a column for each zone and a row for each budget increment.
#' Alternatively, if \code{by_zone = TRUE}, then the \code{matrix} has
#' a single column and a row for each budget increment.
#' }
#' \item{\code{objective}}{
#' \code{numeric} mathematical objective values for each solution
#' generated by the incremental rank procedure.
#' }
#' \item{\code{runtime}}{
#' \code{numeric} total amount of time elapsed (reported in seconds)
#' during the optimization process for each solution generated
#' by the incremental rank procedure.
#' }
#' \item{\code{status}}{
#' \code{character} status of the optimization process for each
#' solution generated by the incremental rank procedure.
#' See [prioritizr::solve()] for details on interpreting these values.
#' }
#' \item{\code{gap}}{
#' \code{numeric} values describing the optimality gap for each solution
#' generated by the incremental rank procedure.
#' See [prioritizr::solve()] for details on interpreting these values.
#' }
#' }
#'
#' @family importances
#'
#' @examples
#' \dontrun{
#' # seed seed for reproducibility
#' set.seed(600)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_pu_polygons <- get_sim_pu_polygons()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create minimal problem with binary decisions
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # print solution
#' print(s1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # calculate importance scores using 10 budget increments
#' # N.B. since the objective for the incremental rank procedure is not
#' # explicitly defined and the problem has a minimum set objective, the
#' # the minimum shortfall objective is used by default
#' rs1 <- eval_rank_importance(p1, s1, n = 10)
#'
#' # print importance scores
#' print(rs1)
#'
#' # plot importance scores
#' plot(rs1, main = "rank importance (10, min shortfall obj", axes = FALSE)
#'
#' # display optimization information from the attributes
#' ## status
#' print(attr(rs1, "status"))
#' ## optimality gap
#' print(attr(rs1, "gap"))
#' ## run time
#' print(attr(rs1, "runtime"))
#' ## objective value
#' print(attr(rs1, "objective"))
#'
#' # plot relationship between objective values and budget increment
#' plot(
#'   y = attr(rs1, "objective"),
#'   x = seq_along(attr(rs1, "objective")),
#'   ylab = "objective value", xlab = "budget increment",
#'   main = "Relationship between objective values and budget increment"
#' )
#'
#' # calculate importance scores using the maximum utility objective and
#' # based on 10 different budgets
#' rs2 <- eval_rank_importance(
#'   p1, s1, n = 10, objective = "add_max_utility_objective"
#' )
#'
#' # print importance scores
#' print(rs2)
#'
#' # plot importance scores
#' plot(rs2, main = "rank importance (10, max utility obj)", axes = FALSE)
#'
#' # calculate importance scores based on 5 manually specified budgets
#'
#' # calculate 5 ranks using equal intervals
#' # N.B. we use length.out = 6 because we want 5 budgets > 0
#' budgets <- seq(0, eval_cost_summary(p1, s1)$cost[[1]], length.out = 6)[-1]
#'
#' # calculate importance using manually specified budgets
#' # N.B. since the objective is not explicitly defined and the problem has a
#' # minimum set objective, the minimum shortfall objective is used by default
#' rs3 <- eval_rank_importance(p1, s1, budgets = budgets)
#'
#' # print importance scores
#' print(rs3)
#'
#' # plot importance scores
#' plot(rs3, main = "rank importance (manual)", axes = FALSE)
#'
#' # build multi-zone conservation problem with raster data
#' p4 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve the problem
#' s4 <- solve(p4)
#' names(s4) <- paste0("zone ", seq_len(terra::nlyr(sim_zones_pu_raster)))
#'
#' # print solution
#' print(s4)
#'
#' # plot solution
#' # each panel corresponds to a different zone, and data show the
#' # status of each planning unit in a given zone
#' plot(s4, axes = FALSE)
#'
#' # calculate importance scores
#' rs4 <- eval_rank_importance(p4, s4, n = 5)
#' names(rs4) <- paste0("zone ", seq_len(terra::nlyr(sim_zones_pu_raster)))
#'
#' # plot importance
#' # each panel corresponds to a different zone, and data show the
#' # importance of each planning unit in a given zone
#' plot(rs4, axes = FALSE)
#' }
#'
#' @references
#' Jung M, Arnell A, de Lamo X, García-Rangel S, Lewis M, Mark J, Merow C,
#' Miles L, Ondo I, Pironon S, Ravilious C, Rivers M, Schepaschenko D,
#' Tallowin O, van Soesbergen A, Govaerts R, Boyle BL, Enquist BJ, Feng X,
#' Gallagher R, Maitner B, Meiri S, Mulligan M, Ofer G, Roll U, Hanson JO,
#' Jetz W, Di Marco M, McGowan J, Rinnan DS, Sachs JD, Lesiv M, Adams VM,
#' Andrew SC, Burger JR, Hannah L, Marquet PA, McCarthy JK, Morueta-Holme N,
#' Newman EA, Park DS, Roehrdanz PR, Svenning J-C, Violle C, Wieringa JJ,
#' Wynne G, Fritz S, Strassburg BBN, Obersteiner M, Kapos V, Burgess N, Schmidt-
#' Traub G, Visconti P (2021) Areas of global importance for conserving
#' terrestrial biodiversity, carbon and water. *Nature Ecology and Evolution*,
#' 5: 1499--1509.
#'
#' @aliases eval_rank_importance,ConservationProblem,numeric-method eval_rank_importance,ConservationProblem,matrix-method eval_rank_importance,ConservationProblem,data.frame-method eval_rank_importance,ConservationProblem,Spatial-method eval_rank_importance,ConservationProblem,sf-method eval_rank_importance,ConservationProblem,Raster-method eval_rank_importance,ConservationProblem,SpatRaster-method
#'
#' @name eval_rank_importance
#'
#' @rdname eval_rank_importance
#'
#' @exportMethod eval_rank_importance
methods::setGeneric("eval_rank_importance",
  function(x, solution, ...) {
    assert_required(x)
    assert_required(solution)
    assert(
      is_conservation_problem(x),
      is_inherits(
        solution,
        c(
          "numeric", "data.frame", "matrix", "sf", "SpatRaster",
          "Spatial", "Raster"
        )
      )
    )
    standardGeneric("eval_rank_importance")
  }
)

#' @name eval_rank_importance
#' @usage \S4method{eval_rank_importance}{ConservationProblem,numeric}(x,
#' solution, ..., run_checks, force, by_zone, objective, extra_args,
#' n, budgets)
#' @rdname eval_rank_importance
methods::setMethod("eval_rank_importance",
  methods::signature("ConservationProblem", "numeric"),
  function(
    x, solution, ..., run_checks = TRUE, force = FALSE,
    by_zone = TRUE, objective = NULL, extra_args = NULL, n, budgets
  ) {
    # assert valid arguments
    assert(is.numeric(solution))
    # extract planning unit and solution information
    idx <- x$planning_unit_indices()
    status <- planning_unit_solution_status(x, solution)
    in_idx <- which(status > 1e-10)
    out_idx <- which(status < 1e-10)
    # additional tests
    check_n_budgets(
      x = x, status = status, n = n, budgets = budgets, by_zone = by_zone, ...
    )
    assert_dots_empty()
    # calculate rank scores
    v <- internal_eval_rank_importance(
      x, status, in_idx, out_idx,
      run_checks, force,
      by_zone, objective, extra_args, n, budgets
    )
    # return rank scores
    out <- rep(NA_real_, length(solution))
    out[idx] <- 0
    out[idx[in_idx]] <- c(v$values)
    # add attributes
    attr(out, "budgets") <- v$budgets
    attr(out, "objective") <- v$objective
    attr(out, "runtime") <- v$runtime
    attr(out, "status") <- v$status
    attr(out, "gap") <- v$gap
    # return result
    out
  }
)

#' @name eval_rank_importance
#' @usage \S4method{eval_rank_importance}{ConservationProblem,matrix}(x,
#' solution, ..., run_checks, force, by_zone, objective, extra_args,
#' n, budgets)
#' @rdname eval_rank_importance
methods::setMethod("eval_rank_importance",
  methods::signature("ConservationProblem", "matrix"),
  function(
    x, solution, ..., run_checks = TRUE, force = FALSE,
    by_zone = TRUE, objective = NULL, extra_args = NULL, n, budgets
  ) {
    # assert valid arguments
    assert(
      is.matrix(solution),
      is.numeric(solution)
    )
    # extract planning unit and solution information
    idx <- x$planning_unit_indices()
    status <- planning_unit_solution_status(x, solution)
    in_idx <- which(status > 1e-10)
    out_idx <- which(status < 1e-10)
    # additional tests
    check_n_budgets(
      x = x, status = status, n = n, budgets = budgets, by_zone = by_zone, ...
    )
    assert_dots_empty()
    # calculate rank scores
    v <- internal_eval_rank_importance(
      x, status, in_idx, out_idx,
      run_checks, force,
      by_zone, objective, extra_args, n, budgets
    )
    # initialize matrix
    m_total <- matrix(
      NA_real_,
      nrow = x$number_of_total_units(),
      ncol = x$number_of_zones()
    )
    m_pu <- matrix(
      0,
      nrow = x$number_of_planning_units(),
      ncol = x$number_of_zones()
    )
    m_pu[in_idx] <- c(v$values)
    m_pu[is.na(x$planning_unit_costs())] <- NA_real_
    m_total[x$planning_unit_indices(), ] <- m_pu
    # add column names to matrix
    if (x$number_of_zones() > 1) {
      colnames(m_total) <- paste0("rs_", x$zone_names())
    } else {
      colnames(m_total) <- "rs"
    }
    # add attributes
    attr(m_total, "budgets") <- v$budgets
    attr(m_total, "objective") <- v$objective
    attr(m_total, "runtime") <- v$runtime
    attr(m_total, "gap") <- v$gap
    attr(m_total, "status") <- v$status
    # return result
    m_total
  }
)

#' @name eval_rank_importance
#' @usage \S4method{eval_rank_importance}{ConservationProblem,data.frame}(x,
#' solution, ..., run_checks, force, by_zone, objective, extra_args,
#' n, budgets)
#' @rdname eval_rank_importance
methods::setMethod("eval_rank_importance",
  methods::signature("ConservationProblem", "data.frame"),
  function(
    x, solution, ..., run_checks = TRUE, force = FALSE,
    by_zone = TRUE, objective = NULL, extra_args = NULL, n, budgets
  ) {
    # assert valid arguments
    assert(is.data.frame(solution))
    # extract planning unit and solution information
    idx <- x$planning_unit_indices()
    status <- planning_unit_solution_status(x, solution)
    in_idx <- which(status > 1e-10)
    out_idx <- which(status < 1e-10)
    # additional tests
    check_n_budgets(
      x = x, status = status, n = n, budgets = budgets, by_zone = by_zone, ...
    )
    assert_dots_empty()
    # calculate rank scores
    v <- internal_eval_rank_importance(
      x, status, in_idx, out_idx,
      run_checks, force,
      by_zone, objective, extra_args, n, budgets
    )
    # initialize matrix
    m_total <- matrix(
      NA_real_,
      nrow = x$number_of_total_units(),
      ncol = x$number_of_zones()
    )
    m_pu <- matrix(
      0,
      nrow = x$number_of_planning_units(),
      ncol = x$number_of_zones()
    )
    m_pu[in_idx] <- c(v$values)
    m_pu[is.na(x$planning_unit_costs())] <- NA_real_
    m_total[x$planning_unit_indices(), ] <- m_pu
    # add column names to matrix
    if (x$number_of_zones() > 1) {
      colnames(m_total) <- paste0("rs_", x$zone_names())
    } else {
      colnames(m_total) <- "rs"
    }
    m_total <- tibble::as_tibble(m_total)
    # add attributes
    attr(m_total, "budgets") <- v$budgets
    attr(m_total, "objective") <- v$objective
    attr(m_total, "runtime") <- v$runtime
    attr(m_total, "gap") <- v$gap
    attr(m_total, "status") <- v$status
    # return result
    m_total
  }
)

#' @name eval_rank_importance
#' @usage \S4method{eval_rank_importance}{ConservationProblem,Spatial}(x,
#' solution, ..., run_checks, force, by_zone, objective, extra_args,
#' n, budgets)
#' @rdname eval_rank_importance
methods::setMethod("eval_rank_importance",
  methods::signature("ConservationProblem", "Spatial"),
  function(
    x, solution, ..., run_checks = TRUE, force = FALSE,
    by_zone = TRUE, objective = NULL, extra_args = NULL, n, budgets
  ) {
    # assert valid arguments
    assert(
      is_inherits(
        solution,
        c(
          "SpatialPointsDataFrame", "SpatialLinesDataFrame",
          "SpatialPolygonsDataFrame"
        )
      )
    )
    # extract planning unit and solution information
    idx <- x$planning_unit_indices()
    status <- planning_unit_solution_status(x, solution)
    in_idx <- which(status > 1e-10)
    out_idx <- which(status < 1e-10)
    # additional tests
    check_n_budgets(
      x = x, status = status, n = n, budgets = budgets, by_zone = by_zone, ...
    )
    assert_dots_empty()
    # calculate rank scores
    v <- internal_eval_rank_importance(
      x, status, in_idx, out_idx,
      run_checks, force,
      by_zone, objective, extra_args, n, budgets
    )
    # initialize matrix
    m_total <- matrix(
      NA_real_,
      nrow = x$number_of_total_units(),
      ncol = x$number_of_zones()
    )
    m_pu <- matrix(
      0,
      nrow = x$number_of_planning_units(),
      ncol = x$number_of_zones()
    )
    m_pu[in_idx] <- c(v$values)
    m_pu[is.na(x$planning_unit_costs())] <- NA_real_
    m_total[x$planning_unit_indices(), ] <- m_pu
    # add column names to matrix
    if (x$number_of_zones() > 1) {
      colnames(m_total) <- paste0("rs_", x$zone_names())
    } else {
      colnames(m_total) <- "rs"
    }
    # prepare result
    out <- as.data.frame(m_total)
    rownames(out) <- rownames(solution@data)
    solution@data <- out
    # add attributes
    attr(solution, "budgets") <- v$budgets
    attr(solution, "objective") <- v$objective
    attr(solution, "runtime") <- v$runtime
    attr(solution, "gap") <- v$gap
    attr(solution, "status") <- v$status
    # return result
    solution
  }
)

#' @name eval_rank_importance
#' @usage \S4method{eval_rank_importance}{ConservationProblem,sf}(x, solution,
#' ..., run_checks, force, by_zone, objective, extra_args, n, budgets)
#' @rdname eval_rank_importance
methods::setMethod("eval_rank_importance",
  methods::signature("ConservationProblem", "sf"),
  function(
    x, solution, ..., run_checks = TRUE, force = FALSE,
    by_zone = TRUE, objective = NULL, extra_args = NULL, n, budgets
  ) {
    # assert valid arguments
    assert(inherits(solution, "sf"))
    # extract planning unit and solution information
    idx <- x$planning_unit_indices()
    status <- planning_unit_solution_status(x, solution)
    in_idx <- which(status > 1e-10)
    out_idx <- which(status < 1e-10)
    # additional tests
    check_n_budgets(
      x = x, status = status, n = n, budgets = budgets, by_zone = by_zone, ...
    )
    assert_dots_empty()
    # calculate rank scores
    v <- internal_eval_rank_importance(
      x, status, in_idx, out_idx,
      run_checks, force,
      by_zone, objective, extra_args, n, budgets
    )
    # initialize matrix
    m_total <- matrix(
      NA_real_,
      nrow = x$number_of_total_units(),
      ncol = x$number_of_zones()
    )
    m_pu <- matrix(
      0,
      nrow = x$number_of_planning_units(),
      ncol = x$number_of_zones()
    )
    m_pu[in_idx] <- c(v$values)
    m_pu[is.na(x$planning_unit_costs())] <- NA_real_
    m_total[x$planning_unit_indices(), ] <- m_pu
    # add column names to matrix
    if (x$number_of_zones() > 1) {
      colnames(m_total) <- paste0("rs_", x$zone_names())
    } else {
      colnames(m_total) <- "rs"
    }
    # prepare result
    out <- tibble::as_tibble(as.data.frame(m_total))
    out$geometry <- sf::st_geometry(x$data$cost)
    out <- sf::st_sf(out, crs = sf::st_crs(x$data$cost))
    # add attributes
    attr(out, "budgets") <- v$budgets
    attr(out, "objective") <- v$objective
    attr(out, "runtime") <- v$runtime
    attr(out, "status") <- v$status
    attr(out, "gap") <- v$gap
    # return result
    out
  }
)

#' @name eval_rank_importance
#' @usage \S4method{eval_rank_importance}{ConservationProblem,Raster}(x,
#' solution, ..., run_checks, force, by_zone, objective, extra_args,
#' n, budgets)
#' @rdname eval_rank_importance
methods::setMethod("eval_rank_importance",
  methods::signature("ConservationProblem", "Raster"),
  function(
    x, solution, ..., run_checks = TRUE, force = FALSE,
    by_zone = TRUE, objective = NULL, extra_args = NULL, n, budgets
  ) {
    assert(inherits(solution, "Raster"))
    # extract planning unit and solution information
    idx <- x$planning_unit_indices()
    status <- planning_unit_solution_status(x, solution)
    in_idx <- which(status > 1e-10)
    out_idx <- which(status < 1e-10)
    # additional tests
    check_n_budgets(
      x = x, status = status, n = n, budgets = budgets, by_zone = by_zone, ...
    )
    assert_dots_empty()
    # calculate rank scores
    v <- internal_eval_rank_importance(
      x, status, in_idx, out_idx,
      run_checks, force,
      by_zone, objective, extra_args, n, budgets
    )
    # initialize matrix
    m_pu <- matrix(
      0,
      nrow = x$number_of_planning_units(),
      ncol = x$number_of_zones()
    )
    m_pu[in_idx] <- c(v$values)
    m_pu[is.na(x$planning_unit_costs())] <- NA_real_
    # add column names to matrix
    if (x$number_of_zones() > 1) {
      colnames(m_pu) <- paste0("rs_", x$zone_names())
    } else {
      colnames(m_pu) <- "rs"
    }
    # prepare result
    out <- raster::as.list(solution)
    for (i in seq_along(out)) {
      out[[i]][x$planning_unit_indices()] <- m_pu[, i]
      out[[i]][raster::Which(is.na(solution[[i]]), cells = TRUE)] <- NA_real_
    }
    if (length(out) > 1) {
      out <- raster::stack(out)
    } else {
      out <- out[[1]]
    }
    names(out) <- colnames(m_pu)
    # add attributes
    attr(out, "budgets") <- v$budgets
    attr(out, "objective") <- v$objective
    attr(out, "runtime") <- v$runtime
    attr(out, "status") <- v$status
    attr(out, "gap") <- v$gap
    # return result
    out
  }
)

#' @name eval_rank_importance
#' @usage \S4method{eval_rank_importance}{ConservationProblem,SpatRaster}(x,
#' solution, ..., run_checks, force, by_zone, objective, extra_args,
#' n, budgets)
#' @rdname eval_rank_importance
methods::setMethod("eval_rank_importance",
  methods::signature("ConservationProblem", "SpatRaster"),
  function(
    x, solution, ..., run_checks = TRUE, force = FALSE,
    by_zone = TRUE, objective = NULL, extra_args = NULL, n, budgets
  ) {
    assert(inherits(solution, "SpatRaster"))
    # extract planning unit and solution information
    idx <- x$planning_unit_indices()
    status <- planning_unit_solution_status(x, solution)
    in_idx <- which(status > 1e-10)
    out_idx <- which(status < 1e-10)
    # additional tests
    check_n_budgets(
      x = x, status = status, n = n, budgets = budgets, by_zone = by_zone, ...
    )
    assert_dots_empty()
    # calculate rank scores
    v <- internal_eval_rank_importance(
      x, status, in_idx, out_idx,
      run_checks, force,
      by_zone, objective, extra_args, n, budgets
    )
    # initialize matrix
    m_pu <- matrix(
      0,
      nrow = x$number_of_planning_units(),
      ncol = x$number_of_zones()
    )
    m_pu[in_idx] <- c(v$values)
    m_pu[is.na(x$planning_unit_costs())] <- NA_real_
    # add column names to matrix
    if (x$number_of_zones() > 1) {
      colnames(m_pu) <- paste0("rs_", x$zone_names())
    } else {
      colnames(m_pu) <- "rs"
    }
    # prepare result
    out <- terra::as.list(solution)
    for (i in seq_along(out)) {
      out[[i]][x$planning_unit_indices()] <- m_pu[, i]
      out[[i]][is.na(solution[[i]])] <- NA_real_
    }
    out <- terra::rast(out)
    names(out) <- colnames(m_pu)
    # add attributes
    attr(out, "budgets") <- v$budgets
    attr(out, "objective") <- v$objective
    attr(out, "runtime") <- v$runtime
    attr(out, "status") <- v$status
    attr(out, "gap") <- v$gap
    # return result
    out
  }
)

internal_eval_rank_importance <- function(x,
                                          status, in_idx, out_idx,
                                          run_checks, force,
                                          by_zone, objective, extra_args,
                                          n, budgets, ...,
                                          call = fn_caller_env()) {

  # assert valid arguments
  assert(
    is_conservation_problem(x),
    is.matrix(status),
    is.integer(in_idx),
    is.integer(out_idx),
    assertthat::is.flag(run_checks),
    assertthat::is.flag(force),
    call = call
  )
  assert(
    length(in_idx) > 0,
    call = call,
    msg = "{.arg solution} has no selected planning units."
  )
  # assign default solver
  if (inherits(x$solver, "Waiver"))
    x <- add_default_solver(x) # nocov
  # overwrite portfolio
  x <- add_default_portfolio(x)
  # if needed, generate budgets
  if (rlang::is_missing(budgets)) {
    ## note that we need to compile the optimization problem
    ## to identify locked in planning units
    budgets <- create_budget_thresholds(
      x, status, n, by_zone, call = call
    )
  }
  # validate budgets
  assert(is.numeric(budgets) || is.matrix(budgets), call = call)
  # if needed, convert budgets to matrix
  if (!is.matrix(budgets)) {
    budgets <- matrix(budgets, ncol = 1)
  }
  # set objective
  if (!is.null(objective)) {
    ## if manually objective specified, then...
    assert(
      assertthat::is.string(objective),
      assertthat::noNA(objective),
      call = call
    )
    x <- overwrite_objective(
      x, objective, extra_args, budgets[1, ], call = call
    )
  } else {
    ## if using default objective, then...
    assert(
      !is.Waiver(x$objective),
      call = call,
      msg = "{.arg x} has no defined objective."
    )
    if (inherits(x$objective, "MinimumSetObjective")) {
      ## if has min set objective, then override it
      x <- overwrite_objective(
        x, "add_min_shortfall_objective", extra_args, budgets[1, ],
        call = call
      )
    } else {
      ## otherwise, manually override the budget for the objective
      assert(
        !is.null(x$objective$data$budget),
        is.numeric(x$objective$data$budget),
        call = call,
        msg = paste(
          "{.arg objective} must be the name of an objective function",
          "that has a {.arg budget} parameter."
        )
      )
      x$objective$data$budget <- budgets[1, ]
    }
  }
  # compile problem after updating the objective
  opt <- compile.ConservationProblem(x)
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
    f(isTRUE(presolve_res$pass), call = parent.frame(), msg = msg)
  }
  # run calculations for compiling problem
  x$solver$calculate(opt)
  # find budget indices
  budget_idx <- which(opt$row_ids() == "budget")
  assert(identical(length(budget_idx), ncol(budgets)), .internal = TRUE)
  # calculate number of decision variables for planning units
  n_vars <- opt$number_of_planning_units() * opt$number_of_zones()
  # extract planning unit indices
  idx <- x$planning_unit_indices()
  in_idx <- which(status > 1e-10)
  out_idx <- which(status < 1e-10)
  # remove optimization problem, since information is now stored in x$solver
  rm(opt)
  # lock out planning units not in the solution
  if (length(out_idx) > 0) {
    x$solver$set_variable_lb(out_idx, rep.int(0, length(out_idx)))
    x$solver$set_variable_ub(out_idx, rep.int(0, length(out_idx)))
  }
  rm(out_idx)
  # initialize objects for storing results
  sol_status <- matrix(NA_real_, nrow = nrow(budgets), ncol = length(in_idx))
  prev_sol <- c()
  prev_selected_idx <- c()
  prev_selected_vals <- c()
  obj_vals <- numeric(nrow(budgets))
  status <- character(nrow(budgets))
  gap <- numeric(nrow(budgets))
  runtime <- numeric(nrow(budgets))
  # debug
  # run incremental optimization procedure
  for (i in seq_len(nrow(budgets))) {
    ## update budgets
    x$solver$set_constraint_rhs(budget_idx, budgets[i, ])
    ## if needed, lock in previous solution
    if ((i > 1) && (length(prev_selected_idx) > 0)) {
      x$solver$set_variable_lb(prev_selected_idx, prev_selected_vals)
    }
    ## if possible, specify starting solution to speed up optimization
    if ((i > 1) && ("start_solution" %in% names(x$solver$data))) {
      x$solver$set_start_solution(prev_sol)
    }
    ## solve problem
    curr_sol <- x$solver$run()
    ## check for issues
    if (is.null(curr_sol) || is.null(curr_sol$x)) {
      # nocov start
      cli::cli_abort(
        c(
          "Can't find a solution!",
          "i" = paste(
            "This is because it is impossible to meet the",
            "targets, budgets, or constraints."
          )
        ),
        call = call
      )
      # nocov end
    }
    ## store results
    obj_vals[i] <- curr_sol$objective
    status[i] <- curr_sol$status
    gap[i] <- curr_sol$gap
    runtime[i] <- curr_sol$runtime
    ## store solution values
    prev_sol <- curr_sol$x
    prev_selected_idx <- which(prev_sol[seq_len(n_vars)] > 1e-10)
    prev_selected_vals <- prev_sol[prev_selected_idx]
    ## store solution status
    sol_status[i, ] <- prev_sol[in_idx]
  }
  # if problem only has one zone, convert from matrix to vector
  if (identical(number_of_zones(x), 1L)) {
    budgets <- c(budgets)
  }
  # return result
  list(
    # calculate scores based on average value of statuses
    values = colMeans(sol_status),
    budgets = budgets,
    objective = obj_vals,
    status = status,
    gap = gap,
    runtime = runtime
  )
}

create_budget_thresholds <- function(x, status, n, by_zone,
                                     call = fn_caller_env) {
  # assert valid arguments
  assert(
    is_conservation_problem(x),
    assertthat::is.count(n),
    assertthat::noNA(n),
    assertthat::is.flag(by_zone),
    assertthat::noNA(by_zone),
    call = call
  )
  # calculate total cost of solution
  total_cost <- suppressWarnings(internal_eval_cost_summary(x, status))
  # calculate cost of cheapest feasible solution based on locked in constraints
  ## check if any non-locked in constraints specified
  assert(
    length(x$constraints) == 0 ||
    all(
      vapply(
        x$constraints, inherits, logical(1),
        c("LockedInConstraint", "LockedOutConstraint", "LockedManualConstraint")
      )
    ),
    call = call,
    msg = c(
      paste(
        "{.arg n} cannot be specified for this {.arg x}."
      ),
      "i" = paste(
        "Instead, {.arg budgets} must be specified."
      ),
      "i" = paste(
        "This is because the budgets cannot be pre-computed from {.arg n}."
      )
    )
  )
  ## create a matrix indicating if each planning unit is locked in
  ### cells with 0s = not locked in
  ### cells with 1s = locked in
  ### cells with NAs = cost values are NAs
  n_z <- number_of_zones(x)
  n_pu <- number_of_planning_units(x)
  if (
    (length(x$constraints)) == 0 ||
    !any(
      vapply(
        x$constraints, inherits, logical(1),
        c("LockedInConstraint", "LockedOutConstraint", "LockedManualConstraint")
      )
    )
  ) {
    locked_in <- matrix(0, ncol = n_z, nrow = n_pu)
  } else {
    locked_in <- matrix(
      compile(x)$lb()[seq_len(n_z * n_pu)],
      ncol = n_z, nrow = n_pu
    )
  }
  locked_in <- locked_in * ((x$planning_unit_costs() * 0) + 1)
  ## calculate cost of cheapest feasible solution based on
  ## locked in planning units
  min_cost <- suppressWarnings(internal_eval_cost_summary(x, locked_in))
  # calculate budgets
  if (number_of_zones(x) == 1L) {
    ## if single zone
    budgets <- matrix(
      seq(min_cost$cost[[1]], total_cost$cost[[1]], length.out = n + 1)[-1],
      ncol = 1
    )
  } else {
    ## if multiple zones
    if (isTRUE(by_zone)) {
      ## create separate budget for each zone
      total_cost <- total_cost[total_cost[[1]] != "overall", , drop = FALSE]
      min_cost <- min_cost[min_cost[[1]] != "overall", , drop = FALSE]
      budgets <- vapply(
        seq_along(total_cost$cost), FUN.VALUE = numeric(n), function(i) {
          seq(min_cost$cost[[i]], total_cost$cost[[i]], length.out = n + 1)[-1]
        }
      )
    } else {
      ## create single budget across all zones
      total_cost <- total_cost[total_cost[[1]] == "overall", , drop = FALSE]
      min_cost <- min_cost[min_cost[[1]] == "overall", , drop = FALSE]
      budgets <- matrix(
        seq(min_cost$cost[[1]], total_cost$cost[[1]], length.out = n + 1)[-1],
        ncol = 1
      )
    }
  }
  # return result
  budgets
}

check_n_budgets <- function(x, status, n, budgets, by_zone, ...,
                            call = fn_caller_env()) {
  dots <- rlang::enquos(...)
  if (
    rlang::is_missing(n) &&
    rlang::is_missing(budgets) &&
    identical(length(dots), 1L) &&
    all(!nzchar(rlang::names2(dots)[[1L]]))
  ) {
    cli::cli_abort(
      c(
        "{.arg n} must be explicitly named.",
        "i" = paste0(
          "Did you mean {.code eval_rank_importance(n = ",
          rlang::as_label(dots[[1]]), ")}?"
        )
      ),
      call = call
    )
  }
  assert(
    rlang::is_missing(n) || rlang::is_missing(budgets),
    !(rlang::is_missing(n) && rlang::is_missing(budgets)),
    call = call,
    msg =
      "Exactly one of {.arg n} or {.arg budgets} must be specified (not both)."
  )
  if (!rlang::is_missing(n)) {
    assert(
      assertthat::is.count(n),
      assertthat::noNA(n),
      call = call
    )
    assert(
      n >= 2,
      call = call
    )
    n_selected <- sum(status >= 1e-10, na.rm = TRUE)
    assert(
      n <= n_selected,
      call = call,
      msg = c(
        paste(
          "{.arg n} must be less than or equal to the number of selected",
          "planning units in {.arg solution}."
        ),
        "x" = paste0("{.arg n} = {.val ", n, "}."),
        "x" = paste0(
          "{.arg solution} selected planning units = {.val ", n_selected, "}."
        )
      )
    )
  }
  if (!rlang::is_missing(budgets)) {
    if (is.numeric(budgets) && !is.matrix(budgets)) {
      assert(length(budgets) >= 2, call = call)
      budgets <- matrix(budgets, ncol = 1)
    }
    assert(
      is.matrix(budgets),
      is.numeric(budgets),
      nrow(budgets) > 1,
      assertthat::noNA(budgets),
      call = call
    )
    verify(
      isTRUE(by_zone),
      msg = c(
        "{.arg by_zone} does not have the default parameter value.",
        "i" = paste(
          "Note that {.arg by_zone} has no effect if {.arg budgets}",
          "is specified."
        )
      ),
      call = call
    )
    assert(
      ncol(budgets) == 1 ||
        ncol(budgets) == number_of_zones(x),
      call = call,
      msg = c(
        paste(
          "{.arg budgets} must have a single column, or a column for each",
          "zone in {.arg x}."
        ),
        "x" = "{.arg budgets} has {.val {ncol(budgets)}} column{?s}.",
        "x" = "{.arg x} has {.val {number_of_zones(x)}} zone{?s}."
      )
    )
  }
  invisible(TRUE)
}

overwrite_objective <- function(x, objective, extra_args, budgets,
                                call = fn_caller_env()) {
  # extract objective function
  obj_fun <- try(
    get(objective, envir = rlang::ns_env("prioritizr")),
    silent = TRUE
  )
  # additional tests
  assert(
    inherits(obj_fun, "function"),
    "budget" %in% names(formals(obj_fun)),
    call = call,
    msg = paste(
      "{.arg objective} must be the name of an objective function",
      "that has a {.arg budget} parameter."
    )
  )
  if (!is.null(extra_args)) {
    assert(is.list(extra_args), call = call)
    assert(
      !is.null(names(extra_args)) &&
        all(nzchar(names(extra_args))),
      call = call,
      msg = "All elements in {.arg extra_args} must be explicitly named."
    )
    assert(
      isTRUE(!"budget" %in% names(extra_args)),
      call = call,
      msg = c(
        "{.arg extra_args} must not have an element named {.val budget}.",
        "i" = paste(
          "This is because {.arg budget} is automatically supplied during",
          "calculations."
        )
      )
    )
    assert(
      isTRUE(!"x" %in% names(extra_args)),
      call = call,
      msg = c(
        "{.arg extra_args} must not have an element named {.val x}.",
        "i" = paste(
          "This is because {.arg x} is automatically supplied during",
          "calculations."
        )
      )
    )
    obj_fun_args <- names(formals(obj_fun))
    obj_fun_args <- setdiff(obj_fun_args, c("x", "budget"))
    if (length(obj_fun_args) > 0) {
      assert(all_match_of(names(extra_args), obj_fun_args), call =call)
    } else {
      assert(
        length(extra_args) == 0,
        call = call,
        msg = c(
          "{.arg extra_args} must be {.code NULL} for this {.arg objective}."
        )
      )
    }
  }
  # extract objective function
  obj_fun <- try(
    get(objective, envir = rlang::ns_env("prioritizr")),
    silent = TRUE
  )
  # overwrite objective
  x <- suppressWarnings(do.call(
    what = obj_fun,
    append(list(x = x, budget = budgets), extra_args)
  ))
}
