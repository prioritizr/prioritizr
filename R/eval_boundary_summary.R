#' @include internal.R ConservationProblem-class.R
NULL

#' Evaluate boundary length of solution
#'
#' Calculate the exposed boundary length (i.e., perimeter) associated with a
#' solution to a conservation planning problem.
#' This summary statistic is useful for evaluating the spatial fragmentation of
#' planning units selected within a solution.
#'
#' @inheritParams add_boundary_penalties
#' @inheritParams eval_cost_summary
#'
#' @details
#' This summary statistic is equivalent to the `Connectivity_Edge` metric
#' reported by the [*Marxan* software](https://marxansolutions.org)
#' (Ball *et al.* 2009).
#' It is calculated using the same equations used to penalize solutions
#' according to their total exposed boundary (i.e., [add_boundary_penalties()]).
#' See the Examples section for examples on how differences `zone` arguments
#' can be used to calculate boundaries for different combinations of zones.
#'
#' @inheritSection add_boundary_penalties Data format
#' @inheritSection eval_cost_summary Solution format
#'
#' @return
#'   A [tibble::tibble()] object containing the boundary length of the
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
#' @seealso
#' See [summaries] for an overview of all functions for summarizing solutions.
#' Also, see [add_boundary_penalties()] to penalize solutions with high
#' boundary length.
#'
#' @family summaries
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
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # calculate boundary associated with the solution
#' r1 <- eval_boundary_summary(p1, s1)
#' print(r1)
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
#' # plot solution
#' plot(s2[, "solution_1"])
#'
#' # calculate boundary associated with the solution
#' r2 <- eval_boundary_summary(p2, s2[, "solution_1"])
#' print(r2)
#'
#' # build multi-zone conservation problem with polygon data
#' p3 <-
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
#' s3 <- solve(p3)
#'
#' # print solution
#' print(s3)
#'
#' # create new column representing the zone id that each planning unit
#' # was allocated to in the solution
#' s3$solution <- category_vector(
#'   s3[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' )
#' s3$solution <- factor(s3$solution)
#'
#' # plot solution
#' plot(s3[, "solution"])
#'
#' # calculate boundary associated with the solution
#' # here we will use the default argument for zones which treats each
#' # zone as completely separate, meaning that the "overall"
#' # boundary is just the sum of the boundaries for each zone
#' r3 <- eval_boundary_summary(
#'   p3, s3[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' )
#' print(r3)
#'
#' # let's calculate the overall exposed boundary across the entire
#' # solution, assuming that the shared boundaries between planning
#' # units allocated to different zones "count" just as much
#' # as those for planning units allocated to the same zone
#'
#' # in other words, let's calculate the overall exposed boundary
#' # across the entire solution by "combining" all selected planning units
#' # together (regardless of which zone they are allocated to in the solution)
#' r3_combined <- eval_boundary_summary(
#'   p3, zones = matrix(1, ncol = 3, nrow = 3),
#'   s3[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' )
#' print(r3_combined)
#'
#' # we can see that the "overall" boundary is now less than the
#' # sum of the individual zone boundaries, because it does not
#' # consider the shared boundary between two planning units allocated to
#' # different zones as "exposed" when performing the calculations
#' }
#' @export
eval_boundary_summary <- function(x, solution,
                                  edge_factor = rep(0.5, number_of_zones(x)),
                                  zones = diag(number_of_zones(x)),
                                  data = NULL) {
  # assert that arguments are valid
  assert_required(x)
  assert_required(solution)
  assert_required(edge_factor)
  assert_required(zones)
  assert_required(data)
  assert(
    is_conservation_problem(x),
    is.numeric(edge_factor),
    all_finite(edge_factor),
    all_proportion(edge_factor),
    length(edge_factor) == number_of_zones(x),
    is_matrix_ish(zones),
    all_finite(zones)
  )
  # convert solution to status matrix format
  solution <- planning_unit_solution_status(x, solution)
  # convert zones to matrix
  zones <- as.matrix(zones)
  assert(
    isSymmetric(zones),
    ncol(zones) == number_of_zones(x),
    min(zones) >= -1,
    max(zones) <= 1
  )
  colnames(zones) <- x$zone_names()
  rownames(zones) <- colnames(zones)
  # prepare boundary matrix data
  if (is.null(data)) {
    data <- x$get_data("boundary")
    if (is.Waiver(data)) {
      data <- boundary_matrix(x$get_data("cost"))
      x$set_data("boundary", data)
    }
  }
  bm <- as_Matrix(data, "dgCMatrix")
  # subset data to planning units
  ind <- x$planning_unit_indices()
  bm <- bm[ind, ind]
  # round data to avoid numerical precision issues
  bm <- round(bm, 6)
  # verify diagonal is >= edge lengths
  verify(
    all(
      round(
        Matrix::diag(bm) - (Matrix::rowSums(bm) - Matrix::diag(bm)),
        6
      ) >= -1e-5
    ),
    msg = c(
      "{.arg data} has unexpected values.",
      "x" = paste(
        "If {.arg data} is from an older version of",
        "{.pkg prioritizr}, then use",
        "{.fn boundary_matrix} to recreate it."
      ),
      "i" = paste(
        "{.arg x} might have spatially overlapping planning units."
      ),
      "i" = paste(
        "If {.arg solution} has selected multiple spatially",
        "overlapping planning units, then results will be incorrect."
      )
    )
  )
  # compute data
  total_boundary <- Matrix::diag(bm)
  exposed_boundary <-
    Matrix::diag(bm) - (Matrix::rowSums(bm) - Matrix::diag(bm))
  Matrix::diag(bm) <- 0
  bm <- as_Matrix(Matrix::tril(Matrix::drop0(bm)), "dgCMatrix")
  # manually coerce NA values in solution to 0
  solution[!is.finite(solution)] <- 0
  # calculate overall boundary length
  v <- rcpp_boundary(
    edge_factor, zones, bm, exposed_boundary, total_boundary, solution
  )
  # main calculations
  if (number_of_zones(x) == 1) {
    ## store result for single zone
    out <- tibble::tibble(summary = "overall", boundary = v)
  } else {
    ## calculate boundary lengths for each zone separately
    zv <- vapply(seq_len(ncol(solution)), FUN.VALUE = numeric(1), function(z) {
      rcpp_boundary(
        edge_factor[z], matrix(1), bm, exposed_boundary, total_boundary,
        solution[, z, drop = FALSE]
      )
    })
    ## store results for multiple zones
    out <- tibble::tibble(
      summary = c("overall", zone_names(x)),
      boundary = c(v, zv)
    )
  }
  # return result
  out
}
