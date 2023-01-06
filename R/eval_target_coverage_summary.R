#' @include internal.R ConservationProblem-proto.R
NULL

#' Evaluate target coverage
#'
#' Calculate how well feature representation [targets] are met by a solution to
#' a conservation planning [problem()].
#' It is useful for understanding if features are adequately represented by
#' a solution.
#' Note that this function can only be used with problems that contain
#' [targets].
#'
#' @inheritParams eval_cost_summary
#'
#' @param include_zone `logical` include the `zone` column in the output?
#'   Defaults to `TRUE` for problems that contain multiple zones.
#'
#' @param include_sense `logical` include the `sense` column in the output?
#'   Defaults to `TRUE` for problems that contain multiple zones.
#'
#' @inheritSection eval_cost_summary Solution format
#'
#' @return A [tibble::tibble()] object.
#'   Here, each row describes information for a different target.
#'   It contains the following columns:
#'
#'   \describe{
#'
#'   \item{feature}{`character` name of the feature associated with each
#'     target.}
#'
#'   \item{zone}{`list` of `character` zone names associated with each target.
#'     This column is in a list-column format because a single target can
#'     correspond to multiple zones (see [add_manual_targets()] for details
#'     and examples).
#'     For an example of converting the list-column format to a standard
#'     `character` column format, please see the Examples section.
#'     This column is only included if the argument to `include_zones`
#'     is `TRUE`.}
#'
#'   \item{sense}{`character` sense associated with each target.
#'     Sense values specify the nature of the target.
#'     Typically (e.g., when using the [add_absolute_targets()] or
#'     [add_relative_targets()] functions), targets are specified using sense
#'     values indicating that the total amount of a feature held within a
#'     solution (ideally) be greater than or equal to a threshold amount
#'     (i.e., a  sense value of `">="`).
#'     Additionally, targets (i.e., using the [add_manual_targets()] function)
#'     can also be specified using sense values indicating that the total
#'     amount of a feature held within a solution must be equal to a
#'     threshold amount (i.e., a sense value of `"="`) or smaller than or equal
#'     to a threshold amount (i.e., a sense value of `"<="`).
#'     This column is only included if the argument to `include_sense` is
#'     `TRUE`.}
#'
#'   \item{total_amount}{`numeric` total amount of the feature available across
#'     the entire conservation planning problem for meeting each target
#'     (not just planning units selected within the solution).
#'     For problems involving a single zone, this column is calculated
#'     as the sum of all of the values for a given feature
#'     (similar to values in the `total_amount` column produced by the
#'     [eval_feature_representation_summary()] function).
#'     For problems involving multiple zones,
#'     this column is calculated as the sum of the values for the
#'     feature associated with target (per the `"feature"` column),
#'     across the zones associated with the target (per the `"zone"` column).}
#'
#'   \item{absolute_target}{`numeric` total threshold amount associated with
#'     each target.}
#'
#'   \item{absolute_held}{`numeric` total amount held within the solution for
#'     the feature and (if relevant) zones associated with each target (per the
#'     `"feature"` and `"zone"` columns, respectively).
#'     This column is calculated as the sum of the feature data,
#'     supplied when creating a [problem()] object
#'     (e.g., presence/absence values), weighted by the status of each
#'     planning unit in the solution (e.g., selected or not for
#'     prioritization).}
#'
#'   \item{absolute_shortfall}{ `numeric` total amount by which the solution
#'     fails to meet each target.
#'     This column is calculated as the difference between the total amount
#'     held within the solution for the feature and (if relevant) zones
#'     associated with the target (i.e., `"absolute_held"` column) and the
#'     target total threshold amount (i.e., `"absolute_target"` column), with
#'     values set to zero depending on the sense specified for the target
#'     (e.g., if the target sense is `>=` then the difference is
#'     set to zero if the value in the `"absolute_held"` is smaller than
#'     that in the `"absolute_target"` column).}
#'
#'   \item{relative_target}{`numeric` proportion threshold amount associated
#'     with each target.
#'     This column is calculated by dividing the total threshold amount
#'     associated with each target (i.e., `"absolute_target"` column) by
#'     the total amount associated with each target
#'     (i.e., `"total_amount"` column).}
#'
#'   \item{relative_held}{`numeric` proportion held within the solution for the
#'     feature and (if relevant) zones associated with each target (per the
#'     `"feature"` and `"zone"` columns, respectively).
#'     This column is calculated by dividing the total amount held
#'     for each target (i.e., `"absolute_held"` column) by the
#'     total amount for with each target
#'     (i.e., `"total_amount"` column).}
#'
#'   \item{relative_shortfall}{`numeric` proportion by which the solution fails
#'     to meet each target.
#'     This column is calculated by dividing the total shortfall for
#'     each target (i.e., `"absolute_shortfall"` column) by the
#'     total amount for each target (i.e., `"total_amount"` column).}
#'
#'   \item{met}{`logical` indicating if each target is met by the solution. This
#'     column is calculated by checking if the total shortfall associated
#'     with each target (i.e., `"absolute_shortfall`" column) is equal to
#'    zero.}
#'
#' }
#'
#' @name eval_target_coverage_summary
#'
#' @aliases eval_target_coverage_summary,ConservationProblem,numeric-method eval_target_coverage_summary,ConservationProblem,matrix-method eval_target_coverage_summary,ConservationProblem,data.frame-method eval_target_coverage_summary,ConservationProblem,Spatial-method eval_target_coverage_summary,ConservationProblem,sf-method eval_target_coverage_summary,ConservationProblem,Raster-method eval_target_coverage_summary,ConservationProblem,SpatRaster-method
#'
#' @seealso
#' See [summaries] for an overview of all functions for summarizing solutions.
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
#' sim_pu_zones_polygons <- get_sim_zones_pu_polygons()
#' sim_features_zones <- get_sim_zones_features()
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
#' # calculate target coverage by the solution
#' r1 <- eval_target_coverage_summary(p1, s1)
#' print(r1, width = Inf) # note: `width = Inf` tells R to print all columns
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
#' # print first six rows of the attribute table
#' print(head(s2))
#'
#' # plot solution
#' plot(s2[, "solution_1"])
#'
#' # calculate target coverage by the solution
#' r2 <- eval_target_coverage_summary(p2, s2[, "solution_1"])
#' print(r2, width = Inf)
#'
#' # build multi-zone conservation problem with polygon data
#' p3 <-
#'   problem(
#'     sim_pu_zones_polygons, sim_features_zones,
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
#' # calculate target coverage by the solution
#' r3 <- eval_target_coverage_summary(
#'   p3, s3[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' )
#' print(r3, width = Inf)
#'
#' # create a new column with character values containing the zone names,
#' # by extracting these data out of the zone column
#' # (which is in list-column format)
#' r3$zone2 <- vapply(r3$zone, FUN.VALUE = character(1), paste, sep = " & ")
#'
#' # print r3 again to show the new column
#' print(r3, width = Inf)
#' }
#' @export
eval_target_coverage_summary <- function(x,
                                         solution,
                                         include_zone =
                                          number_of_zones(x) > 1,
                                         include_sense =
                                           number_of_zones(x) > 1) {
  # assert arguments are valid
  assertthat::assert_that(
    is_conservation_problem(x),
    assertthat::is.flag(include_zone),
    assertthat::noNA(include_zone),
    assertthat::is.flag(include_sense),
    assertthat::noNA(include_sense)
  )
  # extract targets
  assertthat::assert_that(
    !is.Waiver(x$targets),
    msg = paste(
      "argument to x does not have targets,",
      "use the eval_feature_representation() for",
      "problems without targets"
    )
  )
  targets <- x$feature_targets()
  # extract feature abundances
  abundances <- x$feature_abundances_in_total_units()
  # convert solution to status matrix format
  solution <- planning_unit_solution_status(x, solution)
  solution[is.na(solution)] <- 0
  # initialize table
  d <- targets[, c("feature", "zone", "sense"), drop = FALSE]
  attr(d, "out.attrs") <- NULL
  # add total amount column
  d$total_amount <- vapply(
    seq_len(nrow(targets)), FUN.VALUE = numeric(1), function(i) {
    z <- targets$zone[[i]]
    f <- targets$feature[[i]]
    idx <- as.matrix(data.frame(f, z))
    sum(abundances[idx])
  })
  # add absolute amount held column
  d$absolute_held <- rcpp_absolute_amount_held_by_solution(
    x$get_data("rij_matrix"), as.list(targets), solution
  )
  # update feature column with names
  d$feature <- x$feature_names()[d$feature]
  # update zone column with names
  zn <- x$zone_names()
  d$zone <- lapply(d$zone, function(z) zn[z])
  # add absolute target column`
  d$absolute_target <- targets$value
  # add absolute shortfall column
  ## initially calculate shortfalls as absolute difference between
  ## amount held and target amount
  d$absolute_shortfall <- abs(d$absolute_target - d$absolute_held)
  ## manually set shortfalls to zero if sense is >= and this is met
  d$absolute_shortfall <- ifelse(
    (targets$sense == ">=") &
      (d$absolute_held >= d$absolute_target),
    rep(0, nrow(d)),
    d$absolute_shortfall
  )
  ## manually set shortfalls to zero if sense is = and this is met
  d$absolute_shortfall <- ifelse(
    (targets$sense == "=") &
      ((d$absolute_held - d$absolute_target) < 1e-10),
    rep(0, nrow(d)),
    d$absolute_shortfall
  )
  ## manually set shortfalls to zero if sense is <= and this is met
  d$absolute_shortfall <- ifelse(
    (targets$sense == "<=") &
      (d$absolute_held <= d$absolute_target),
    rep(0, nrow(d)),
    d$absolute_shortfall
  )
  # add relative columns
  d$relative_target <- d$absolute_target / d$total_amount
  d$relative_held <- d$absolute_held / d$total_amount
  d$relative_shortfall <- d$absolute_shortfall / d$total_amount
  # coerce non-finite values to zero (caused by divide by zero issues)
  d$relative_target[!is.finite(d$relative_target)] <- 0
  d$relative_held[!is.finite(d$relative_held)] <- 0
  d$relative_shortfall[!is.finite(d$relative_shortfall)] <- 0
  # add met column
  d$met <- d$absolute_shortfall < 1e-10
  # specify column names for result
  cn <- c(
    "feature", "zone", "sense", "met", "total_amount",
    "absolute_target", "absolute_held", "absolute_shortfall",
    "relative_target", "relative_held", "relative_shortfall"
  )
  if (!isTRUE(include_zone)) cn <- setdiff(cn, "zone")
  if (!isTRUE(include_sense)) cn <- setdiff(cn, "sense")
  # return result
  d[, cn, drop = FALSE]
}
