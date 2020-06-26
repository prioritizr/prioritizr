#' @include internal.R ConservationProblem-proto.R
NULL

#' Feature representation
#'
#' Calculate how well features are represented in a solution.
#'
#' @param x [ConservationProblem-class] object.
#'
#' @param solution `numeric`, `matrix`, `data.frame`,
#'   [raster::Raster-class], [sp::Spatial-class],
#'   or [sf::sf()] object. See the Details section for more
#'   information.
#'
#' @details Note that all arguments to `solution` must correspond to the
#'   planning unit data in the argument to `x` in terms of data
#'   representation, dimensionality, and spatial attributes (if applicable).
#'   This means that if the planning unit data in `x` is a `numeric`
#'   vector then the argument to `solution` must be a `numeric` vector
#'   with the same number of elements, if the planning unit data in `x` is
#'   a [raster::RasterLayer-class] then the argument to
#'   `solution` must also be a [raster::RasterLayer-class] with
#'   the same number of rows and columns and the same resolution, extent, and
#'   coordinate reference system, if the planning unit data in `x` is a
#'   [sp::Spatial-class] or [sf::sf()] object then the
#'   argument to `solution` must also be a [sp::Spatial-class]
#'   or [sf::sf()] object, respectively, and have the same number of
#'   spatial features (e.g. polygons) and have the same coordinate reference
#'   system, if the planning units in `x` are a `data.frame` then the
#'   argument to `solution` must also be a `data.frame` with each
#'   column correspond to a different zone and each row correspond to a
#'   different planning unit, and values correspond to the allocations (e.g.
#'   values of zero or one).
#'
#'   Solutions must have planning unit statuses set to missing (`NA`)
#'   values for planning units that have missing (`NA`) cost data. For
#'   problems with multiple zones, this means that planning units must have
#'   missing (`NA`) allocation values in zones where they have missing
#'   (`NA`) cost data. In other words, planning units that have missing
#'   (`NA`) cost values in `x` should always have a missing
#'   (`NA`) value the argument to `solution`. If an argument is
#'   supplied to
#'   `solution` where this is not the case, then an error will be thrown.
#'   Please note that earlier versions of the \pkg{prioritizr}
#'   (prior to 4.0.4.1) required that such planning units always have zero
#'   values, but this has been changed to make the handling of missing values
#'   more consistent throughout the package.
#'
#'   Additionally, note that when calculating the proportion of each feature
#'   represented in the solution, the denominator is calculated using all
#'   planning units---**including any planning units with `NA` cost
#'   values in the argument to `x`**. This is exactly the same equation
#'   used when calculating relative targets for problems (e.g.
#'   `add_relative_targets`).
#'
#' @return [tibble::tibble()] object containing the amount
#'   (`"absolute_held"`) and proportion (`"relative_held"`)
#'   of the distribution of each feature held in the solution. Here, each
#'   row contains data that pertain to a specific feature in a specific
#'   management zone (if multiple zones are present). This object
#'   contains the following columns:
#'
#'   \describe{
#'
#'   \item{feature}{`character` name of the feature.}
#'
#'   \item{zone}{`character` name of the zone (not included when the
#'     argument to `x` contains only one management zone).}
#'
#'   \item{absolute_held}{`numeric` total amount of each feature secured in
#'     the solution. If the problem contains multiple zones, then this
#'     column shows how well each feature is represented in a each
#'     zone.}
#'
#'   \item{relative_held}{`numeric` proportion of the feature's
#'     distribution held in the solution. If the problem contains
#'     multiple zones, then this column shows how well each feature is
#'     represented in each zone.}
#'
#'   }
#'
#' @name feature_representation
#'
#' @aliases feature_representation,ConservationProblem,numeric-method feature_representation,ConservationProblem,matrix-method feature_representation,ConservationProblem,data.frame-method feature_representation,ConservationProblem,Spatial-method feature_representation,ConservationProblem,sf-method feature_representation,ConservationProblem,Raster-method
#'
#' @seealso [problem()], [feature_abundances()].
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_raster, sim_pu_polygons, sim_pu_zones_sf, sim_features,
#'     sim_pu_zones_stack, sim_features_zones)
#'
#'
#' # create a simple conservation planning data set so we can see exactly
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
#' s1 <- data.frame(solution = c(1, NA, rep(c(1, 0), 4)))
#' print(s1)
#'
#' # calculate feature representation
#' r1 <- feature_representation(p1, s1)
#' print(r1)
#'
#' # verify that feature representation calculations are correct
#' all.equal(r1$absolute_held, c(sum(pu$spp1 * s1[[1]], na.rm = TRUE),
#'                               sum(pu$spp2 * s1[[1]], na.rm = TRUE)))
#' all.equal(r1$relative_held, c(sum(pu$spp1 * s1[[1]], na.rm = TRUE) /
#'                               sum(pu$spp1),
#'                               sum(pu$spp2 * s1[[1]], na.rm = TRUE) /
#'                               sum(pu$spp2, na.rm = TRUE)))
#' \donttest{
#' # solve the problem using an exact algorithm solver
#' s1_2 <- solve(p1)
#' print(s1_2)
#'
#' # calculate feature representation in this solution
#' r1_2 <- feature_representation(p1, s1_2[, "solution_1", drop = FALSE])
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
#' r2 <- feature_representation(p2, s2)
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
#' spplot(s3, zcol = "solution_1", main = "solution", axes = FALSE, box = FALSE)
#' }
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
#'
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
NULL

#' @name feature_representation
#'
#' @rdname feature_representation
#'
#' @exportMethod feature_representation
#'
methods::setGeneric("feature_representation",
  function(x, solution) {
  standardGeneric("feature_representation")
})

#' @name feature_representation
#' @usage \S4method{feature_representation}{ConservationProblem,numeric}(x, solution)
#' @rdname feature_representation
methods::setMethod("feature_representation",
  methods::signature("ConservationProblem", "numeric"),
  function(x, solution) {
    # assert valid arguments
    assertthat::assert_that(
      is.numeric(solution),
      is.numeric(x$data$cost), is.matrix(x$data$cost))
    assertthat::assert_that(
      number_of_total_units(x) == length(solution),
      number_of_zones(x) == 1,
      min(solution, na.rm = TRUE) >= 0,
      max(solution, na.rm = TRUE) <= 1)
    # perform calculations
    internal_feature_representation(x, matrix(solution, ncol = 1))
})

#' @name feature_representation
#' @usage \S4method{feature_representation}{ConservationProblem,matrix}(x, solution)
#' @rdname feature_representation
methods::setMethod("feature_representation",
  methods::signature("ConservationProblem", "matrix"),
  function(x, solution) {
    # assert valid arguments
    assertthat::assert_that(
      is.matrix(solution), is.numeric(solution),
      is.matrix(x$data$cost), is.numeric(x$data$cost))
    assertthat::assert_that(
      number_of_total_units(x) == nrow(solution),
      number_of_zones(x) == ncol(solution),
      min(solution, na.rm = TRUE) >= 0,
      max(solution, na.rm = TRUE) <= 1)
    # perform calculations
    internal_feature_representation(x, solution)
})

#' @name feature_representation
#' @usage \S4method{feature_representation}{ConservationProblem,data.frame}(x, solution)
#' @rdname feature_representation
methods::setMethod("feature_representation",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, solution) {
    # assert valid arguments
    assertthat::assert_that(
      is.data.frame(solution),
      is.data.frame(x$data$cost))
    assertthat::assert_that(
      number_of_zones(x) == ncol(solution),
      number_of_total_units(x) == nrow(solution),
      is.numeric(unlist(solution)),
      min(unlist(solution), na.rm = TRUE) >= 0,
      max(unlist(solution), na.rm = TRUE) <= 1)
    # perform calculations
    internal_feature_representation(x, as.matrix(solution))
})

#' @name feature_representation
#' @usage \S4method{feature_representation}{ConservationProblem,Spatial}(x, solution)
#' @rdname feature_representation
methods::setMethod("feature_representation",
  methods::signature("ConservationProblem", "Spatial"),
  function(x, solution) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(solution, c("SpatialPointsDataFrame", "SpatialLinesDataFrame",
                           "SpatialPolygonsDataFrame")),
      class(x$data$cost)[1] == class(solution)[1])
    assertthat::assert_that(
      number_of_zones(x) == ncol(solution@data),
      sf::st_crs(x$data$cost@proj4string) == sf::st_crs(solution@proj4string),
      number_of_total_units(x) == nrow(solution@data),
      is.numeric(unlist(solution@data)),
      min(unlist(solution@data), na.rm = TRUE) >= 0,
      max(unlist(solution@data), na.rm = TRUE) <= 1)
    # perform calculations
    internal_feature_representation(x, as.matrix(solution@data))
})

#' @name feature_representation
#' @usage \S4method{feature_representation}{ConservationProblem,sf}(x, solution)
#' @rdname feature_representation
methods::setMethod("feature_representation",
  methods::signature("ConservationProblem", "sf"),
  function(x, solution) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(solution, "sf"),
      inherits(x$data$cost, "sf"))
    solution <- sf::st_drop_geometry(solution)
    assertthat::assert_that(
      sf::st_crs(x$data$cost) == sf::st_crs(solution),
      number_of_zones(x) == ncol(solution),
      number_of_total_units(x) == nrow(solution),
      is.numeric(unlist(solution)),
      min(unlist(solution), na.rm = TRUE) >= 0,
      max(unlist(solution), na.rm = TRUE) <= 1)
    # perform calculations
    internal_feature_representation(x, as.matrix(solution))
})

#' @name feature_representation
#' @usage \S4method{feature_representation}{ConservationProblem,Raster}(x, solution)
#' @rdname feature_representation
methods::setMethod("feature_representation",
  methods::signature("ConservationProblem", "Raster"),
  function(x, solution) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(solution, "Raster"),
      inherits(x$data$cost, "Raster"))
    assertthat::assert_that(
      number_of_zones(x) == raster::nlayers(solution),
      sf::st_crs(x$data$cost@crs) == sf::st_crs(solution@crs),
      is_comparable_raster(x$data$cost, solution[[1]]),
      min(raster::cellStats(solution, "min")) >= 0,
      max(raster::cellStats(solution, "max")) <= 1)
    # subset planning units with finite cost values
    pos <- x$planning_unit_indices()
    if (raster::nlayers(solution) > 1) {
      pos2 <- raster::Which(max(!is.na(solution)) == 1, cells = TRUE)
    } else {
      pos2 <- raster::Which(!is.na(solution), cells = TRUE)
    }
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA allocations in the",
           " solution")
    solution <- solution[pos2]
    if (!is.matrix(solution))
      solution <- matrix(solution, ncol = 1)
    if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(solution))))
     stop("planning units with NA cost data must have NA allocations in the",
          " solution")
    # run calculations
    internal_feature_representation(x, solution, subset_units = FALSE)
})

internal_feature_representation <- function(x, solution, subset_units = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
    is.matrix(solution))
  # subset planning units with finite cost values
  if (subset_units) {
    pos <- x$planning_unit_indices()
    pos2 <- which(rowSums(is.na(solution)) != ncol(solution))
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA allocations in the",
           " solution")
    solution <- solution[pos, , drop = FALSE]
    if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(solution))))
      stop("planning units with NA cost data must have NA allocations in the",
           " solution")
  }
  # calculate amount of each feature in each planning unit
  total <- x$feature_abundances_in_total_units()
  held <- vapply(seq_len(x$number_of_zones()),
                 FUN.VALUE = numeric(nrow(x$data$rij_matrix[[1]])),
                 function(i) {
    rowSums(x$data$rij_matrix[[i]] *
            matrix(solution[, i], ncol = nrow(solution),
                   nrow = nrow(x$data$rij_matrix[[1]]),
                   byrow = TRUE),
            na.rm = TRUE)
  })
  out <- tibble::tibble(feature = rep(x$feature_names(), x$number_of_zones()),
                        absolute_held = unname(c(held)),
                        relative_held = unname(c(held / total)))
  if (x$number_of_zones() > 1) {
    out$zone <- rep(x$zone_names(), each = x$number_of_features())
    out <- out[, c(1, 4, 2, 3), drop = FALSE]
  }
  out
}
