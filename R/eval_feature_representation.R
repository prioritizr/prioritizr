#' @include internal.R ConservationProblem-proto.R
NULL

#' Evaluate feature representation
#'
#' Calculate how well features are represented by a solution
#' to a conservation planning [problem()].
#' This function reports information for each and every feature,
#' and each and every zone, within a conservation planning problem.
#'
#' @inheritParams eval_cost
#'
#' @inheritSection eval_cost Solution format
#'
#' @return [tibble::tibble()] object describing feature representation.
#'   Here, each row describes a specific statistic (e.g. different management
#'   zone) for a specific feature.
#'   It contains the following columns:
#'
#'   \describe{
#'
#'   \item{statistic}{`character` description of the representation statistic.
#'     The representation statistics associated with the `"overall"` value
#'     in this column are calculated using entire solution
#'     (including all management zones if there are multiple zones).
#'     If multiple management zones are present, then representation statistics
#'     are also provided for each zone separately. In such cases,
#'     these statistics are associated with values, in this column,
#'     that correspond to the zone names.}
#'
#'   \item{feature}{`character` name of the feature.}
#'
#'   \item{total_amount}{`numeric` total amount of each feature available
#'     in the entire conservation planning problem
#'     (not just planning units selected within the solution).
#'     It is calculated as the sum of the feature data,
#'     supplied when creating a [problem()] object
#'     (e.g. presence/absence values).}
#'
#'   \item{absolute_held}{`numeric` total amount of each feature secured within
#'     the solution. It is calculated as the sum of the feature data,
#'     supplied when creating a [problem()] object
#'     (e.g. presence/absence values), weighted by the status of each
#'     planning unit in the solution (e.g. selected or not for prioritization).}
#'
#'   \item{relative_held}{`numeric` proportion of
#'     each feature secured within the solution. It is calculated
#'     by dividing values in the `"absolute_held"` column by those in the
#'     `"total_amount"` column.}
#'
#'   }
#'
#' @name eval_feature_representation
#'
#' @aliases eval_feature_representation,ConservationProblem,numeric-method eval_feature_representation,ConservationProblem,matrix-method eval_feature_representation,ConservationProblem,data.frame-method eval_feature_representation,ConservationProblem,Spatial-method eval_feature_representation,ConservationProblem,sf-method eval_feature_representation,ConservationProblem,Raster-method
#'
#' @inherit eval_cost seealso
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_raster, sim_pu_polygons, sim_pu_zones_sf, sim_features,
#'      sim_pu_zones_stack, sim_features_zones)
#'
#' # create a simple conservation planning dataset so we can see exactly
#' # how feature representation is calculated
#' pu <- data.frame(id = seq_len(10), cost = c(0.2, NA, runif(8)),
#'                  spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
#'
#' # create problem
#' p1 <- problem(pu, c("spp1", "spp2"), cost_column = "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions()
#'
#' # create a solution
#' # specifically, a data.frame with a single column that contains
#' # binary values indicating if each planning units was selected or not
#' s1 <- data.frame(s = c(1, NA, rep(c(1, 0), 4)))
#' print(s1)
#'
#' # calculate feature representation
#' r1 <- eval_feature_representation(p1, s1)
#' print(r1)
#'
#' # let's verify that feature representation calculations are correct
#' # by manually performing the calculations and compare the results with r1
#' ## calculate total amount for each feature
#' print(setNames(
#'   c(sum(pu$spp1, na.rm = TRUE),
#'     sum(pu$spp2, na.rm = TRUE)),
#'   c("spp1", "spp2")))
#'
#' ## calculate absolute amount held for each feature
#' print(setNames(
#'   c(sum(pu$spp1 * s1$s, na.rm = TRUE),
#'     sum(pu$spp2 * s1$s, na.rm = TRUE)),
#'   c("spp1", "spp2")))
#'
#' ## calculate relative amount held for each feature
#' print(setNames(
#'   c(sum(pu$spp1 * s1$s, na.rm = TRUE) / sum(pu$spp1, na.rm = TRUE),
#'     sum(pu$spp2 * s1$s, na.rm = TRUE) / sum(pu$spp2, na.rm = TRUE)),
#'   c("spp1", "spp2")))
#'
#' \dontrun{
#' # solve the problem using an exact algorithm solver
#' s1_2 <- solve(p1)
#' print(s1_2)
#'
#' # calculate feature representation in this solution
#' r1_2 <- eval_feature_representation(p1, s1_2[, "solution_1", drop = FALSE])
#' print(r1_2)
#'
#' # build minimal conservation problem with raster data
#' p2 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions()
#'
#' # solve the problem
#' s2 <- solve(p2)
#'
#' # print solution
#' print(s2)
#'
#' # calculate feature representation in the solution
#' r2 <- eval_feature_representation(p2, s2)
#' print(r2)
#'
#' # plot solution
#' plot(s2, main = "solution", axes = FALSE, box = FALSE)
#' }
#' # build minimal conservation problem with polygon (Spatial) data
#' p3 <- problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions()
#' \dontrun{
#' # solve the problem
#' s3 <- solve(p3)
#'
#' # print first six rows of the attribute table
#' print(head(s3))
#'
#' # calculate feature representation in the solution
#' r3 <- eval_feature_representation(p3, s3[, "solution_1"])
#' print(r3)
#'
#' # plot solution
#' spplot(s3, zcol = "solution_1", main = "solution", axes = FALSE, box = FALSE)
#' }
#' # build multi-zone conservation problem with raster data
#' p4 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_binary_decisions()
#' \dontrun{
#' # solve the problem
#' s4 <- solve(p4)
#'
#' # print solution
#' print(s4)
#'
#' # calculate feature representation in the solution
#' r4 <- eval_feature_representation(p4, s4)
#' print(r4)
#'
#' # plot solution
#' plot(category_layer(s4), main = "solution", axes = FALSE, box = FALSE)
#' }
#'
#' # build multi-zone conservation problem with polygon (sf) data
#' p5 <- problem(sim_pu_zones_sf, sim_features_zones,
#'               cost_column = c("cost_1", "cost_2", "cost_3")) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_binary_decisions()
#' \dontrun{
#' # solve the problem
#' s5 <- solve(p5)
#'
#' # print first six rows of the attribute table
#' print(head(s5))
#'
#' # calculate feature representation in the solution
#' r5 <- eval_feature_representation(p5, s5[, c("solution_1_zone_1",
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
NULL

#' @name eval_feature_representation
#'
#' @rdname eval_feature_representation
#'
#' @exportMethod eval_feature_representation
#'
methods::setGeneric("eval_feature_representation",
  function(x, solution) {
  standardGeneric("eval_feature_representation")
})

#' @name eval_feature_representation
#' @usage \S4method{eval_feature_representation}{ConservationProblem,numeric}(x, solution)
#' @rdname eval_feature_representation
methods::setMethod("eval_feature_representation",
  methods::signature("ConservationProblem", "numeric"),
  function(x, solution) {
    internal_eval_feature_representation(
      x, planning_unit_solution_status(x, solution))
})

#' @name eval_feature_representation
#' @usage \S4method{eval_feature_representation}{ConservationProblem,matrix}(x, solution)
#' @rdname eval_feature_representation
methods::setMethod("eval_feature_representation",
  methods::signature("ConservationProblem", "matrix"),
  function(x, solution) {
    internal_eval_feature_representation(
      x, planning_unit_solution_status(x, solution))
})

#' @name eval_feature_representation
#' @usage \S4method{eval_feature_representation}{ConservationProblem,data.frame}(x, solution)
#' @rdname eval_feature_representation
methods::setMethod("eval_feature_representation",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, solution) {
    internal_eval_feature_representation(
      x, planning_unit_solution_status(x, solution))
})

#' @name eval_feature_representation
#' @usage \S4method{eval_feature_representation}{ConservationProblem,Spatial}(x, solution)
#' @rdname eval_feature_representation
methods::setMethod("eval_feature_representation",
  methods::signature("ConservationProblem", "Spatial"),
  function(x, solution) {
    internal_eval_feature_representation(
      x, planning_unit_solution_status(x, solution))
})

#' @name eval_feature_representation
#' @usage \S4method{eval_feature_representation}{ConservationProblem,sf}(x, solution)
#' @rdname eval_feature_representation
methods::setMethod("eval_feature_representation",
  methods::signature("ConservationProblem", "sf"),
  function(x, solution) {
    internal_eval_feature_representation(
      x, planning_unit_solution_status(x, solution))
})

#' @name eval_feature_representation
#' @usage \S4method{eval_feature_representation}{ConservationProblem,Raster}(x, solution)
#' @rdname eval_feature_representation
methods::setMethod("eval_feature_representation",
  methods::signature("ConservationProblem", "Raster"),
  function(x, solution) {
    internal_eval_feature_representation(
      x, planning_unit_solution_status(x, solution))
})

internal_eval_feature_representation <- function(x, solution) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "ConservationProblem"),
    is.matrix(solution))
  # calculate amount of each feature in each planning unit
  total <- x$feature_abundances_in_total_units()
  held <-
    vapply(
      seq_len(x$number_of_zones()),
      FUN.VALUE = numeric(nrow(x$data$rij_matrix[[1]])),
      function(i) {
        rowSums(
          x$data$rij_matrix[[i]] *
            matrix(solution[, i], ncol = nrow(solution),
                   nrow = nrow(x$data$rij_matrix[[1]]), byrow = TRUE),
          na.rm = TRUE)
    })
  # prepare output
  if (x$number_of_zones() == 1) {
    out <- tibble::tibble(
      statistic = "overall",
      feature = x$feature_names(),
      total_amount = unname(c(total)),
      absolute_held = unname(c(held)),
      relative_held = unname(c(held / total)))
  } else {
    total <- c(rowSums(total), c(total))
    held <- c(rowSums(held), c(held))
    out <- tibble::tibble(
      statistic =
        rep(c("overall", x$zone_names()), each = x$number_of_features()),
      feature = rep(x$feature_names(), x$number_of_zones() + 1),
      total_amount = unname(total),
      absolute_held = unname(held),
      relative_held = unname(c(held / total)))
  }
  out
}
