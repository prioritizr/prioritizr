#' @include internal.R
NULL

#' Evaluate solution importance using Ferrier scores
#'
#' Calculate importance scores for planning units selected in
#' a solution following Ferrier *et al.* (2000).
#' **Please note that
#' the mathematical formulation for computing these scores needs verification,
#' and so this functionality should be considered experimental at this point in
#' time.**
#'
#' @inheritParams eval_replacement_importance
#'
#' @details
#' Importance scores are reported separately for each feature within
#' each planning unit. Additionally, a total importance score is also
#' calculated as the sum of the scores for each feature.
#' Note that this function only works for problems with
#' a minimum set objective and a single zone.
#' It will throw an error for other types of problems that do not meet
#' this specification.
#'
#' @inheritSection eval_cost_summary Solution format
#'
#' @return A `matrix`, [tibble::tibble()],
#'   [`RasterLayer-class`], or
#'   [`Spatial-class`] object containing the scores for each
#'   planning unit selected in the solution.
#'   Specifically, the returned object is in the
#'   same format (except if the planning units are a `numeric` vector) as the
#'   planning unit data in the argument to `x`.
#'
#' @references
#' Ferrier S, Pressey RL, and Barrett TW (2000) A new predictor of the
#' irreplaceability of areas for achieving a conservation goal, its application
#' to real-world planning, and a research agenda for further refinement.
#' *Biological Conservation*, 93: 303--325.
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
#' \dontrun{
#' # solve problem
#' s1 <- solve(p1)
#'
#' # print solution
#' print(s1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE, box = FALSE)
#'
#' # calculate importance scores using Ferrier et al. 2000 method
#' fs1 <- eval_ferrier_importance(p1, s1)
#'
#' # print importance scores,
#' # each planning unit has an importance score for each feature
#' # (as indicated by the column names) and each planning unit also
#' # has an overall total importance score (in the "total" column)
#' print(fs1)
#'
#' # plot total importance scores
#' plot(fs1, axes = FALSE, box = FALSE)
#' }
#'
#' @inherit eval_replacement_importance seealso
#'
#' @aliases eval_ferrier_importance,ConservationProblem,numeric-method eval_ferrier_importance,ConservationProblem,matrix-method eval_ferrier_importance,ConservationProblem,data.frame-method eval_ferrier_importance,ConservationProblem,Spatial-method eval_ferrier_importance,ConservationProblem,Raster-method
#'
#' @name eval_ferrier_importance
#'
#' @rdname eval_ferrier_importance
#'
#' @exportMethod eval_ferrier_importance
methods::setGeneric("eval_ferrier_importance",
  function(x, solution) {
  standardGeneric("eval_ferrier_importance")
})

#' @name eval_ferrier_importance
#' @usage \S4method{eval_ferrier_importance}{ConservationProblem,numeric}(x, solution)
#' @rdname eval_ferrier_importance
methods::setMethod("eval_ferrier_importance",
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
    v <- internal_eval_ferrier_importance(x, indices)
    # return scores
    out <- matrix(NA_real_, nrow = x$number_of_total_units(),
                  ncol = x$number_of_features() + 1,
                  dimnames = list(NULL, c(x$feature_names(), "total")))
    out[pos, ] <- 0
    out[pos[indices], ] <- c(v)
    out
})

#' @name eval_ferrier_importance
#' @usage \S4method{eval_ferrier_importance}{ConservationProblem,matrix}(x, solution)
#' @rdname eval_ferrier_importance
methods::setMethod("eval_ferrier_importance",
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
    v <- internal_eval_ferrier_importance(x, indices)
    # return scores
    out <- matrix(NA_real_, nrow = x$number_of_total_units(),
                  ncol = x$number_of_features() + 1,
                  dimnames = list(NULL, c(x$feature_names(), "total")))
    out[pos, ] <- 0
    out[pos[indices], ] <- c(v)
    out
})

#' @name eval_ferrier_importance
#' @usage \S4method{eval_ferrier_importance}{ConservationProblem,data.frame}(x, solution)
#' @rdname eval_ferrier_importance
methods::setMethod("eval_ferrier_importance",
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
    v <- internal_eval_ferrier_importance(x, indices)
    # return scores
    out <- matrix(NA_real_, nrow = x$number_of_total_units(),
                  ncol = x$number_of_features() + 1,
                  dimnames = list(NULL, c(x$feature_names(), "total")))
    out[pos, ] <- 0
    out[pos[indices], ] <- c(v)
    tibble::as_tibble(out)
})

#' @name eval_ferrier_importance
#' @usage \S4method{eval_ferrier_importance}{ConservationProblem,Spatial}(x, solution)
#' @rdname eval_ferrier_importance
methods::setMethod("eval_ferrier_importance",
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
    v <- internal_eval_ferrier_importance(x, indices)
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

#' @name eval_ferrier_importance
#' @usage \S4method{eval_ferrier_importance}{ConservationProblem,Raster}(x, solution)
#' @rdname eval_ferrier_importance
methods::setMethod("eval_ferrier_importance",
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
    v <- internal_eval_ferrier_importance(x, indices)
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

internal_eval_ferrier_importance <- function(x, indices) {
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
