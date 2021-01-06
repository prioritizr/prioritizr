#' @include internal.R ConservationProblem-proto.R
NULL

#' Evaluate solution boundary length
#'
#' Calculate the exposed boundary length (perimeter) associated with a
#' solution to a conservation planning [problem()].
#' This summary statistic is useful for evaluating the spatial fragmentation of
#' planning units selected within a solution.
#'
#' @inheritParams add_boundary_penalties
#' @inheritParams eval_cost_summary
#'
#' @param ... not used.
#'
#' @details
#' This summary statistic is equivalent to the `Connectivity_Edge` metric
#' reported by the [*Marxan* software](https://marxansolutions.org)
#' (Ball *et al.* 2009).
#' It is calculated using the same equations used to penalize solutions
#' according to their total exposed boundary (i.e. [add_boundary_penalties()]).
#'
#' @inheritSection add_boundary_penalties Data format
#' @inheritSection eval_cost_summary Solution format
#'
#' @return
#'   [tibble::tibble()] object containing the boundary length of the
#'   solution.
#'   It contains the following columns:
#'
#'   \describe{
#'
#'   \item{summary}{`character` description of the summary statistic.
#'     The statistic associated with the `"overall"` value
#'     in this column is calculated using the entire solution
#'     (including all management zones if there are multiple zones).
#'     If multiple management zones are present, then summary statistics
#'     are also provided for each zone separately
#'     (indicated using zone names).}
#'
#'   \item{boundary}{`numeric` exposed boundary length value.
#'     Greater values correspond to solutions with greater
#'     boundary length and, in turn, greater spatial fragmentation.
#'     Thus conservation planning exercises typically prefer solutions
#'     with smaller values.}
#'
#'   }
#'
#' @references
#' Ball IR, Possingham HP, and Watts M (2009) *Marxan and relatives:
#' Software for spatial conservation prioritisation* in Spatial conservation
#' prioritisation: Quantitative methods and computational tools. Eds Moilanen
#' A, Wilson KA, and Possingham HP. Oxford University Press, Oxford, UK.
#'
#' @name eval_boundary_summary
#'
#' @seealso [problem()], [summaries], [add_boundary_penalties()].
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_raster, sim_pu_sf, sim_features,
#'      sim_pu_zones_sf, sim_features_zones)
#'
#' # build minimal conservation problem with raster data
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#'
#' # solve the problem
#' s1 <- solve(p1)
#'
#' # print solution
#' print(s1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE, box = FALSE)
#'
#' # calculate boundary associated with the solution
#' r1 <- eval_boundary_summary(p1, s1)
#' print(r1)
#'
#' # build minimal conservation problem with polygon (sf) data
#' p2 <- problem(sim_pu_sf, sim_features, cost_column = "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#'
#' # solve the problem
#' s2 <- solve(p2)
#'
#' # print first six rows of the attribute table
#' print(head(s2))
#'
#' # plot solution
#' plot(s2[, "solution_1"])
#'
#' # calculate boundary associated with the solution
#' r2 <- eval_boundary_summary(p2, s2[, "solution_1"])
#' print(r2)
#'
#' # build multi-zone conservation problem with polygon (sf) data
#' p3 <- problem(sim_pu_zones_sf, sim_features_zones,
#'               cost_column = c("cost_1", "cost_2", "cost_3")) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#'
#' # solve the problem
#' s3 <- solve(p3)
#'
#' # print first six rows of the attribute table
#' print(head(s3))
#'
#' # create new column representing the zone id that each planning unit
#' # was allocated to in the solution
#' s3$solution <- category_vector(
#'   s3[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")])
#' s3$solution <- factor(s3$solution)
#'
#' # plot solution
#' plot(s3[, "solution"])
#'
#' # calculate boundary associated with the solution
#' r3 <- eval_boundary_summary(
#'   p3, s3[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")])
#' print(r3)
#'
#' }
#' @export
eval_boundary_summary <- function(x, ...) UseMethod("eval_boundary_summary")

#' @rdname eval_boundary_summary
#' @method eval_boundary_summary default
#' @export
eval_boundary_summary.default <- function(x, ...) {
  stop("argument to x must be a ConservationProblem object")
}

#' @rdname eval_boundary_summary
#' @method eval_boundary_summary ConservationProblem
#' @export
eval_boundary_summary.ConservationProblem <- function(
  x, solution, edge_factor = rep(0.5, number_of_zones(x)),
  zones = diag(number_of_zones(x)), data = NULL, ...) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, "ConservationProblem"),
    is.numeric(edge_factor), all(is.finite(edge_factor)),
    all(edge_factor >= 0), all(edge_factor <= 1),
    length(edge_factor) == number_of_zones(x),
    inherits(zones, c("matrix", "Matrix")), all(is.finite(zones)),
    no_extra_arguments(...))
  # convert solution to status matrix format
  solution <- planning_unit_solution_status(x, solution)
  # convert zones to matrix
  zones <- as.matrix(zones)
  assertthat::assert_that(
    isSymmetric(zones), ncol(zones) == number_of_zones(x),
    min(zones) >= -1, max(zones) <= 1)
  colnames(zones) <- x$zone_names()
  rownames(zones) <- colnames(zones)
  # prepare boundary matrix data
  bm <- internal_prepare_planning_unit_boundary_data(x, data)
  if (is.Waiver(bm)) {
    bm <- internal_prepare_planning_unit_boundary_data(
      x, boundary_matrix(x$get_data("cost")))
  }
  class(bm) <- "dgCMatrix"
  # manually coerce NA values in solution to 0
  solution[!is.finite(solution)] <- 0
  # calculate overall boundary length
  v <- rcpp_boundary(edge_factor, zones, bm, solution)
  # main calculations
  if (number_of_zones(x) == 1) {
    ## store result for single zone
    out <- tibble::tibble(summary = "overall", boundary = v)
  } else {
    ## calculate boundary lengths for each zone separately
    zv <- vapply(seq_len(ncol(solution)), FUN.VALUE = numeric(1), function(z) {
      rcpp_boundary(
        edge_factor[z], matrix(1), bm, solution[, z, drop = FALSE])
    })
    ## store results for multiple zones
    out <- tibble::tibble(
      summary = c("overall", zone_names(x)), boundary = c(v, zv))
  }
  # return result
  out
}
