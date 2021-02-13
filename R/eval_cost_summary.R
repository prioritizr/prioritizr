#' @include internal.R ConservationProblem-proto.R
NULL

#' Evaluate solution cost
#'
#' Calculate the total cost of a solution to a conservation planning
#' [problem()].
#' For example, if the planning unit cost data describe land acquisition costs
#' (USD), then the total cost would be net cost (USD) needed to acquire
#' all planning units selected within the solution.
#'
#' @param x [problem()] (i.e. [`ConservationProblem-class`]) object.
#'
#' @param solution `numeric`, `matrix`, `data.frame`,
#'   [`Raster-class`], [`Spatial-class`],
#'   or [sf::sf()] object.
#'  The argument should be in the same format as the planning unit cost
#'  data in the argument to `x`.
#'  See the Solution format section for more information.
#'
#' @details
#' This metric is equivalent to the `Cost` metric reported by the
#' [*Marxan* software](https://marxansolutions.org) (Ball *et al.* 2009).
#' Specifically, the cost of a solution is defined as the sum of the cost
#' values, supplied when creating a [problem()] object
#' (e.g. using the `cost_column` argument),
#' weighted by the status of each planning unit in the solution.
#'
#' @section Solution format:
#' Broadly speaking, the argument to `solution` must be in the same format as
#' the planning unit data in the argument to `x`.
#' Further details on the correct format are listed separately
#' for each of the different planning unit data formats:
#'
#' \describe{
#'
#' \item{`x` has `numeric` planning units}{The argument to `solution` must be a
#'   `numeric` vector with each element corresponding to a different planning
#'   unit. It should have the same number of planning units as those
#'   in the argument to `x`. Additionally, any planning units missing
#'   cost (`NA`) values should also have missing (`NA`) values in the
#'   argument to `solution`.
#' }
#'
#' \item{`x` has `matrix` planning units}{The argument to `solution` must be a
#'   `matrix` vector with each row corresponding to a different planning
#'   unit, and each column correspond to a different management zone.
#'   It should have the same number of planning units and zones
#'   as those in the argument to `x`. Additionally, any planning units
#'   missing cost (`NA`) values for a particular zone should also have a
#'   missing (`NA`) values in the argument to `solution`.
#' }
#'
#' \item{`x` has [`Raster-class`] planning units}{The argument to `solution`
#'   be a [`Raster-class`] object where different grid cells (pixels) correspond
#'   to different planning units and layers correspond to
#'   a different management zones. It should have the same dimensionality
#'   (rows, columns, layers), resolution, extent, and coordinate reference
#'   system as the planning units in the argument to `x`. Additionally,
#'   any planning units missing cost (`NA`) values for a particular zone
#'   should also have missing (`NA`)  values in the argument to `solution`.
#' }
#'
#' \item{`x` has `data.frame` planning units}{The argument to `solution` must
#'   be a `data.frame` with each column corresponding to a different zone,
#'   each row corresponding to a different planning unit, and cell values
#'   corresponding to the solution value. This means that if a `data.frame`
#'   object containing the solution also contains additional columns, then
#'   these columns will need to be subsetted prior to using this function
#'   (see below for example with [sf::sf()] data).
#'   Additionally, any planning units missing cost
#'   (`NA`) values for a particular zone should also have missing (`NA`)
#'   values in the argument to `solution`.
#' }
#'
#' \item{`x` has [`Spatial-class`] planning units}{The argument to `solution`
#'   must be a [`Spatial-class`] object with each column corresponding to a
#'   different zone, each row corresponding to a different planning unit, and
#'   cell values corresponding to the solution value. This means that if the
#'   [`Spatial-class`] object containing the solution also contains additional
#'   columns, then these columns will need to be subsetted prior to using this
#'   function (see below for example with [sf::sf()] data).
#'   Additionally, the argument to `solution` must also have the same
#'   coordinate reference system as the planning unit data.
#'   Furthermore, any planning units missing cost
#'   (`NA`) values for a particular zone should also have missing (`NA`)
#'   values in the argument to `solution`.
#' }
#'
#' \item{`x` has [sf::sf()] planning units}{The argument to `solution` must be
#'   a [sf::sf()] object with each column corresponding to a different
#'   zone, each row corresponding to a different planning unit, and cell values
#'   corresponding to the solution value. This means that if the
#'   [sf::sf()] object containing the solution also contains additional
#'   columns, then these columns will need to be subsetted prior to using this
#'   function (see below for example).
#'   Additionally, the argument to `solution` must also have the same
#'   coordinate reference system as the planning unit data.
#'   Furthermore, any planning units missing cost
#'   (`NA`) values for a particular zone should also have missing (`NA`)
#'   values in the argument to `solution`.
#' }
#'
#' }
#'
#' @return
#'   [tibble::tibble()] object containing the solution cost.
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
#'   \item{cost}{`numeric` cost value.
#'     Greater values correspond to solutions that are more costly
#'     to implement.
#'     Thus conservation planning exercises typically prefer solutions
#'     with smaller values, because they are cheaper to implement
#'     (assuming all other relevant factors, such as feature representation,
#'     are equal).}
#'
#'   }
#'
#' @references
#' Ball IR, Possingham HP, and Watts M (2009) *Marxan and relatives:
#' Software for spatial conservation prioritisation* in Spatial conservation
#' prioritisation: Quantitative methods and computational tools. Eds Moilanen
#' A, Wilson KA, and Possingham HP. Oxford University Press, Oxford, UK.
#'
#' @name eval_cost_summary
#'
#' @seealso [problem()], [summaries].
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
#' # calculate cost of the solution
#' r1 <- eval_cost_summary(p1, s1)
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
#' # plot solution
#' plot(s2[, "solution_1"])
#'
#' # print first six rows of the attribute table
#' print(head(s2))
#'
#' # calculate cost of the solution
#' r2 <- eval_cost_summary(p2, s2[, "solution_1"])
#' print(r2)
#'
#' # manually calculate cost of the solution
#' r2_manual <- sum(s2$solution * sim_pu_sf$cost, na.rm = TRUE)
#' print(r2_manual)
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
#' # calculate cost of the solution
#' r3 <- eval_cost_summary(
#'   p3, s3[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")])
#' print(r3)
#' }
#' @export
eval_cost_summary <- function(x, solution) UseMethod("eval_cost_summary")

#' @rdname eval_cost_summary
#' @method eval_cost_summary default
#' @export
eval_cost_summary.default <- function(x, solution) {
  stop("argument to x must be a ConservationProblem object")
}

#' @rdname eval_cost_summary
#' @method eval_cost_summary ConservationProblem
#' @export
eval_cost_summary.ConservationProblem <- function(x, solution) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"))
  # convert solution to status matrix format
  solution <- planning_unit_solution_status(x, solution)
  # calculate overall cost of each planning unit
  cost_data <- x$planning_unit_costs()
  # output costs
  costs <- unname(c(colSums(cost_data * solution, na.rm = TRUE)))
  total_cost <- sum(costs)
  if (x$number_of_zones() > 1) {
    out <- tibble::tibble(
      summary = c("overall", zone_names(x)),
      cost = c(total_cost, costs))
  } else {
    out <- tibble::tibble(summary = "overall", cost = total_cost)
  }
  # return result
  out
}
