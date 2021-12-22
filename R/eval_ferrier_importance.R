#' @include internal.R
NULL

#' Evaluate solution importance using Ferrier scores
#'
#' Calculate importance scores for planning units selected in
#' a solution following Ferrier *et al.* (2000).
#'
#' @inheritParams eval_replacement_importance
#'
#' @details
#' Importance scores are reported separately for each feature within
#' each planning unit. Additionally, a total importance score is also
#' calculated as the sum of the scores for each feature.
#' Note that this function only works for problems with
#' a minimum set objective and a single zone.
#' It will throw an error for problems that do not meet this criteria.
#'
#' @section Notes:
#' In previous versions, the documentation for this function had a warning
#' indicating that the mathematical formulation for this function required
#' verification. The mathematical formulation for this function has since
#' been corrected and verified, so now this function is recommended
#' for general use.
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
#' @seealso
#' See [importance] for an overview of all functions for evaluating
#' the importance of planning units selected in a solution.
#.
#' @family importances
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
#' plot(fs1, main = "Ferrier scores", axes = FALSE, box = FALSE)
#'
#' # create minimal problem with polygon (sf) planning units
#' p2 <- problem(sim_pu_sf, sim_features, cost_column = "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.05) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # print solution
#' print(s2)
#'
#' # plot solution
#' plot(s2[, "solution_1"], main = "solution")
#'
#' # calculate importance scores
#' fs2 <- eval_ferrier_importance(p2, s2[, "solution_1"])
#'
#' # plot importance scores
#' plot(fs2, main = "Ferrier scores")
#'
#' }
#'
#' @aliases eval_ferrier_importance,ConservationProblem,numeric-method eval_ferrier_importance,ConservationProblem,matrix-method eval_ferrier_importance,ConservationProblem,data.frame-method eval_ferrier_importance,ConservationProblem,Spatial-method eval_ferrier_importance,ConservationProblem,sf-method eval_ferrier_importance,ConservationProblem,Raster-method
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
      is.numeric(solution))
    # extract planning unit solution status
    status <- planning_unit_solution_status(x, solution)
    # extract indices
    idx <- x$planning_unit_indices()
    pos <- which(status > 1e-10)
    # calculate scores
    v <- internal_eval_ferrier_importance(x, pos)
    # return scores
    out <- matrix(NA_real_, nrow = x$number_of_total_units(),
                  ncol = x$number_of_features() + 1,
                  dimnames = list(NULL, c(x$feature_names(), "total")))
    out[idx, ] <- 0
    out[idx[pos], ] <- c(v)
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
      number_of_zones(x) == 1)
    # extract planning unit solution status
    status <- planning_unit_solution_status(x, solution)
    # extract indices
    idx <- x$planning_unit_indices()
    pos <- which(status > 1e-10)
    # calculate scores
    v <- internal_eval_ferrier_importance(x, pos)
    # return scores
    out <- matrix(NA_real_, nrow = x$number_of_total_units(),
                  ncol = x$number_of_features() + 1,
                  dimnames = list(NULL, c(x$feature_names(), "total")))
    out[idx, ] <- 0
    out[idx[pos], ] <- c(v)
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
      number_of_zones(x) == 1)
    # extract planning unit solution status
    status <- planning_unit_solution_status(x, solution)
    # extract indices
    idx <- x$planning_unit_indices()
    pos <- which(status > 1e-10)
    # calculate scores
    v <- internal_eval_ferrier_importance(x, pos)
    # return scores
    out <- matrix(NA_real_, nrow = x$number_of_total_units(),
                  ncol = x$number_of_features() + 1,
                  dimnames = list(NULL, c(x$feature_names(), "total")))
    out[idx, ] <- 0
    out[idx[pos], ] <- c(v)
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
      number_of_zones(x) == 1)
    # extract planning unit solution status
    status <- planning_unit_solution_status(x, solution)
    # extract indices
    idx <- x$planning_unit_indices()
    pos <- which(status > 1e-10)
    # calculate scores
    v <- internal_eval_ferrier_importance(x, pos)
    # return scores
    out <- matrix(NA_real_, nrow = x$number_of_total_units(),
                  ncol = x$number_of_features() + 1,
                  dimnames = list(NULL, c(x$feature_names(), "total")))
    out[idx, ] <- 0
    out[idx[pos], ] <- c(v)
    out <- as.data.frame(out)
    rownames(out) <- rownames(solution@data)
    solution@data <- out
    solution
})

#' @name eval_ferrier_importance
#' @usage \S4method{eval_ferrier_importance}{ConservationProblem,sf}(x, solution)
#' @rdname eval_ferrier_importance
methods::setMethod("eval_ferrier_importance",
  methods::signature("ConservationProblem", "sf"),
  function(x, solution) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(solution, "sf"),
      inherits(x$data$cost, "sf"),
      number_of_zones(x) == 1)
    # extract planning unit solution status
    status <- planning_unit_solution_status(x, solution)
    # extract indices
    idx <- x$planning_unit_indices()
    pos <- which(status > 1e-10)
    # calculate scores
    v <- internal_eval_ferrier_importance(x, pos)
    # return scores
    out <- matrix(NA_real_, nrow = x$number_of_total_units(),
                  ncol = x$number_of_features() + 1,
                  dimnames = list(NULL, c(x$feature_names(), "total")))
    out[idx, ] <- 0
    out[idx[pos], ] <- c(v)
    out <- tibble::as_tibble(as.data.frame(out))
    sf::st_as_sf(
      out, geometry = sf::st_geometry(x$data$cost),
      crs = sf::st_crs(x$data$cost))
})

#' @name eval_ferrier_importance
#' @usage \S4method{eval_ferrier_importance}{ConservationProblem,Raster}(x, solution)
#' @rdname eval_ferrier_importance
methods::setMethod("eval_ferrier_importance",
  methods::signature("ConservationProblem", "Raster"),
  function(x, solution) {
    assertthat::assert_that(
      inherits(solution, "Raster"),
      number_of_zones(x) == 1)
    # extract planning unit solution status
    status <- planning_unit_solution_status(x, solution)
    # extract indices
    idx <- x$planning_unit_indices()
    pos <- which(status > 1e-10)
    # calculate scores
    v <- internal_eval_ferrier_importance(x, pos)
    # return scores
    out <- list(x$data$cost[[1]])[rep(1, ncol(v))]
    for (i in seq_along(out)) {
      out[[i]][idx] <- 0
      out[[i]][idx[pos]] <- v[, i]
    }
    out <- raster::stack(out)
    names(out) <- c(x$feature_names(), "total")
    out
})

internal_eval_ferrier_importance <- function(x, indices) {
  assertthat::assert_that(
    inherits(x, "ConservationProblem"),
    number_of_zones(x) == 1,
    is.integer(indices), length(indices) > 0)
  assertthat::assert_that(
    !any(x$feature_names() == "total"),
    msg = paste("argument to x has a feature named \"total\" and this",
                "is not permitted"))
  # extract data
  rij <- x$data$rij_matrix[[1]]
  targets <- x$feature_targets()
  # validate data
  assertthat::assert_that(
    all(targets$sense == ">="),
    msg = "all targets must have a >= sense")
  assertthat::assert_that(
    all(targets$value >= 0),
    msg = "all targets must be greater than or equal to zero")
  assertthat::assert_that(
    all(rij@x >= 0),
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
