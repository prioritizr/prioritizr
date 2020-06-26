#' @include internal.R
NULL

#' Ferrier irreplaceability score
#'
#' Calculate irreplaceability scores for planning units selected in
#' a solution using the method outlined in Ferrier *et al.* (2000).
#' Specifically, the scores are implemented following the CLUZ decision support
#' tool (Smith 2019).
#' Here, scores are calculated separately for each feature within
#' each planning unit. Additionally, a total irreplaceability score is also
#' calculated as the sum of the irreplaceability scores for each planning unit.
#' Note that this function only works for problems with
#' a minimum set objective and a single zone.
#' It will throw an error for other types of problems.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param solution `numeric`, `matrix`, `data.frame`,
#'   \code{\link[raster]{Raster-class}}, or \code{\link[sp]{Spatial-class}}
#'   object. See the Details section for more information.
#'
#' @details
#'   The argument to `solution` must correspond
#'   to the planning unit data in the argument to `x` in terms
#'   of data representation, dimensionality, and spatial attributes (if
#'   applicable). This means that if the planning unit data in `x`
#'   is a `numeric` vector then the argument to `solution` must be a
#'   `numeric` vector with the same number of elements, if the planning
#'   unit data in `x` is a \code{\link[raster]{RasterLayer-class}} then the
#'   argument to `solution` must also be a
#'   \code{\link[raster]{RasterLayer-class}} with the same number of rows and
#'   columns and the same resolution, extent, and coordinate reference system,
#'   if the planning unit data in `x` is a \code{\link[sp]{Spatial-class}}
#'   object then the argument to `solution` must also be a
#'   \code{\link[sp]{Spatial-class}} object and have the same number of spatial
#'   features (e.g. polygons) and have the same coordinate reference system,
#'   if the planning units in `x` are a `data.frame` then the
#'   argument to `solution` must also be a `data.frame` with each
#'   column correspond to a different zone and each row correspond to
#'   a different planning unit, and values correspond to the allocations
#'   (e.g. values of zero or one). Furthermore, solutions must have planning
#'   unit statuses set to missing (`NA`)
#'   values for planning units that have missing (`NA`) cost data.
#'   If an argument is supplied to
#'   `solution` where this is not the case, then an error will be thrown.
#'
#' @return A `matrix`, \code{\link[tibble]{tibble}},
#'   \code{\link[raster]{RasterLayer-class}}, or
#'   \code{\link[sp]{Spatial-class}} object containing the scores for each
#'   planning unit selected in the solution.
#'
#' @references
#' Ferrier S, Pressey RL, and Barrett TW (2000) A new predictor of the
#' irreplaceability of areas for achieving a conservation goal, its application
#' to real-world planning, and a research agenda for further refinement.
#' *Biological Conservation*, 93: 303--325.
#'
#' Smith RJ (2019). The CLUZ plugin for QGIS: designing conservation area
#' systems and other ecological networks. *Research Ideas and Outcomes*
#' 5: e33510.
#'
#' @examples
#' # seed seed for reproducibility
#' set.seed(600)
#'
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create minimal problem with binary decisions
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(gap = 0, verbose = FALSE)
#' \donttest{
#' # solve problem
#' s1 <- solve(p1)
#'
#' # print solution
#' print(s1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE, box = FALSE)
#'
#' # calculate irreplaceability scores using Ferrier et al. 2000 method
#' fs1 <- ferrier_score(p1, s1)
#'
#' # print irreplaceability scores,
#' # each planning unit has an irreplaceability score for each feature
#' # (as indicated by the column names) and each planning unit also
#' # has an overall total irreplaceability score (in the "total" column)
#' print(fs1)
#'
#' # plot total irreplaceability scores
#' plot(fs1, axes = FALSE, box = FALSE)
#' }
#'
#' @seealso \code{\link{irreplaceability}}.
#'
#' @aliases ferrier_score,ConservationProblem,numeric-method ferrier_score,ConservationProblem,matrix-method ferrier_score,ConservationProblem,data.frame-method ferrier_score,ConservationProblem,Spatial-method ferrier_score,ConservationProblem,Raster-method
#'
#' @name ferrier_score
#'
#' @rdname ferrier_score
#'
#' @exportMethod ferrier_score
methods::setGeneric("ferrier_score",
  function(x, solution) {
  standardGeneric("ferrier_score")
})

internal_ferrier_score <- function(x, indices) {
  assertthat::assert_that(
    inherits(x, "ConservationProblem"),
    x$number_of_zones() == 1,
    is.integer(indices), length(indices) > 0)
  assertthat::assert_that(
    !any(x$feature_names() == "total"),
    msg = paste("argument to x has a feature named \"total\" and this",
                "is not permitted"))
  # extract data
  rij <- x$data$rij_matrix[[1]]
  targets <- x$feature_targets()
  # validate data
  assertthat::assert_that(all(targets$sense == ">="),
                          msg = "all targets must have a >= sense")
  assertthat::assert_that(all(targets$value >= 0),
    msg = "all targets must be greater than or equal to zero")
  assertthat::assert_that(all(rij@x >= 0),
    msg = "all feature values must be greater than or equal to zero")
  # create sparse matrix with output indices to avoid calculations
  # for planning units that are not selected in the solution
  out <- Matrix::sparseMatrix(i = 1, j = 1, x = 0, dims = dim(rij))
  out[, indices] <- 1
  out <- Matrix::drop0(out * rij)
  # calculate scores for all planning units
  s <- rcpp_ferrier_score(rij, targets$value, length(indices), out)
  # extract selected planning units
  s <- s[, indices, drop = FALSE]
  # calculate totals
  s <- cbind(t(s), Matrix::colSums(s))
  as.matrix(s)
}

#' @name ferrier_score
#' @usage \S4method{ferrier_score}{ConservationProblem,numeric}(x, solution)
#' @rdname ferrier_score
methods::setMethod("ferrier_score",
  methods::signature("ConservationProblem", "numeric"),
  function(x, solution) {
    # assert valid arguments
    assertthat::assert_that(
      is.numeric(solution), sum(solution, na.rm = TRUE) > 1e-10,
      is.numeric(x$data$cost), is.matrix(x$data$cost),
      number_of_total_units(x) == length(solution),
      number_of_zones(x) == 1,
      min(solution, na.rm = TRUE) >= 0,
      max(solution, na.rm = TRUE) <= 1)
    # subset planning units with finite cost values
    pos <- x$planning_unit_indices()
    pos2 <- which(!is.na(solution))
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA values in the",
           " solution")
    # calculate scores
    indices <- which(solution[pos] > 1e-10)
    v <- internal_ferrier_score(x, indices)
    # return scores
    out <- matrix(NA_real_, nrow = x$number_of_total_units(),
                  ncol = x$number_of_features() + 1,
                  dimnames = list(NULL, c(x$feature_names(), "total")))
    out[pos, ] <- 0
    out[pos[indices], ] <- c(v)
    out
})

#' @name ferrier_score
#' @usage \S4method{ferrier_score}{ConservationProblem,matrix}(x, solution)
#' @rdname ferrier_score
methods::setMethod("ferrier_score",
  methods::signature("ConservationProblem", "matrix"),
  function(x, solution) {
    # assert valid arguments
    assertthat::assert_that(
      is.matrix(solution), is.numeric(solution),
      sum(solution, na.rm = TRUE) > 1e-10,
      is.matrix(x$data$cost), is.numeric(x$data$cost),
      number_of_total_units(x) == nrow(solution),
      number_of_zones(x) == 1,
      min(solution, na.rm = TRUE) >= 0,
      max(solution, na.rm = TRUE) <= 1,
      ncol(solution) == 1)
    # subset planning units with finite cost values
    pos <- x$planning_unit_indices()
    pos2 <- which(!is.na(solution))
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA values in the",
           " solution")
    # calculate scores
    indices <- which(solution[pos] > 1e-10)
    v <- internal_ferrier_score(x, indices)
    # return scores
    out <- matrix(NA_real_, nrow = x$number_of_total_units(),
                  ncol = x$number_of_features() + 1,
                  dimnames = list(NULL, c(x$feature_names(), "total")))
    out[pos, ] <- 0
    out[pos[indices], ] <- c(v)
    out
})

#' @name ferrier_score
#' @usage \S4method{ferrier_score}{ConservationProblem,data.frame}(x, solution)
#' @rdname ferrier_score
methods::setMethod("ferrier_score",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, solution) {
    # assert valid arguments
    assertthat::assert_that(
      is.data.frame(solution),
      sum(as.matrix(solution), na.rm = TRUE) >= 1e-10,
      number_of_zones(x) == 1,
      number_of_total_units(x) == nrow(solution),
      is.data.frame(x$data$cost),
      is.numeric(unlist(solution)),
      min(unlist(solution), na.rm = TRUE) >= 0,
      max(unlist(solution), na.rm = TRUE) <= 1,
      ncol(solution) == 1)
    # subset planning units with finite cost values
    solution_matrix <- as.matrix(solution)
    pos <- x$planning_unit_indices()
    pos2 <- which(!is.na(solution_matrix))
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA values in the",
           " solution")
    # calculate scores
    indices <- which(solution_matrix[pos] > 1e-10)
    v <- internal_ferrier_score(x, indices)
    # return scores
    out <- matrix(NA_real_, nrow = x$number_of_total_units(),
                  ncol = x$number_of_features() + 1,
                  dimnames = list(NULL, c(x$feature_names(), "total")))
    out[pos, ] <- 0
    out[pos[indices], ] <- c(v)
    tibble::as_tibble(out)
})

#' @name ferrier_score
#' @usage \S4method{ferrier_score}{ConservationProblem,Spatial}(x, solution)
#' @rdname ferrier_score
methods::setMethod("ferrier_score",
  methods::signature("ConservationProblem", "Spatial"),
  function(x, solution) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(solution, c("SpatialPointsDataFrame", "SpatialLinesDataFrame",
                           "SpatialPolygonsDataFrame")),
      number_of_zones(x) == 1,
      number_of_total_units(x) == nrow(solution@data),
      class(x$data$cost)[1] == class(solution)[1],
      is.numeric(unlist(solution@data)),
      min(unlist(solution@data), na.rm = TRUE) >= 0,
      max(unlist(solution@data), na.rm = TRUE) <= 1,
      ncol(solution@data) == 1)
    # subset planning units with finite cost values
    solution_matrix <- as.matrix(solution@data)
    pos <- x$planning_unit_indices()
    pos2 <- which(!is.na(solution_matrix))
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA values in the",
           " solution")
    # calculate scores
    indices <- which(solution_matrix[pos] > 1e-10)
    v <- internal_ferrier_score(x, indices)
    # return scores
    out <- matrix(NA_real_, nrow = x$number_of_total_units(),
                  ncol = x$number_of_features() + 1,
                  dimnames = list(NULL, c(x$feature_names(), "total")))
    out[pos, ] <- 0
    out[pos[indices], ] <- c(v)
    out <- as.data.frame(out)
    rownames(out) <- rownames(solution@data)
    solution@data <- out
    solution
})

#' @name ferrier_score
#' @usage \S4method{ferrier_score}{ConservationProblem,Raster}(x, solution)
#' @rdname ferrier_score
methods::setMethod("ferrier_score",
  methods::signature("ConservationProblem", "Raster"),
  function(x, solution) {
    assertthat::assert_that(
      inherits(solution, "Raster"),
      number_of_zones(x) == 1,
      raster::compareCRS(x$data$cost@crs, solution@crs),
      is_comparable_raster(x$data$cost, solution[[1]]),
      min(raster::cellStats(solution, "min")) >= 0,
      max(raster::cellStats(solution, "max")) <= 1,
      raster::nlayers(solution) == 1)
    # subset planning units with finite cost values
    pos <- x$planning_unit_indices()
    pos2 <- raster::Which(!is.na(solution), cells = TRUE)
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA values in the",
           " solution")
    solution_matrix <- solution[pos2]
    if (!is.matrix(solution_matrix))
      solution_matrix <- matrix(solution_matrix, ncol = 1)
    # calculate replacement costs
    indices <- which(solution_matrix > 1e-10)
    v <- internal_ferrier_score(x, indices)
    # return scores
    out <- list(x$data$cost[[1]])[rep(1, ncol(v))]
    for (i in seq_along(out)) {
      out[[i]][!is.na(out[[i]])] <- 0
      out[[i]][pos2[indices]] <- v[, i]
    }
    out <- raster::stack(out)
    names(out) <- c(x$feature_names(), "total")
    out
})
