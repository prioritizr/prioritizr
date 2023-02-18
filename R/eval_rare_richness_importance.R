#' @include internal.R ConservationProblem-class.R OptimizationProblem-class.R problem.R
NULL

#' Evaluate solution importance using rarity weighted richness scores
#'
#' Calculate importance scores for planning units selected in a solution
#' using rarity weighted richness scores (based on Williams *et al.* 1996).
#' This method is only recommended for large-scaled conservation
#' planning exercises (i.e., more than 100,000 planning units) where
#' importance scores cannot be calculated using other methods in a feasible
#' period of time. This is because rarity weighted richness scores cannot (i)
#' account for the cost of different planning units, (ii) account for multiple
#' management zones, and (iii) identify truly irreplaceable planning units ---
#' unlike the replacement cost metric which does not suffer any of these
#' limitations.
#'
#' @inheritParams eval_replacement_importance
#'
#' @details Rarity weighted richness scores are calculated using the following
#'   terms. Let \eqn{I} denote the set of planning units (indexed by
#'   \eqn{i}), let \eqn{J} denote the set of conservation features (indexed by
#'   \eqn{j}), let \eqn{r_{ij}} denote the amount of feature \eqn{j}
#'   associated with planning unit \eqn{i}, and let \eqn{m_j} denote the
#'   maximum value of feature \eqn{j} in \eqn{r_{ij}} in all planning units
#'   \eqn{i \in I}. To calculate the rarity weighted richness (*RWR*) for
#'   planning unit \eqn{k}:
#'
#'   \deqn{
#'   \mathit{RWR}_{k} = \sum_{j}^{J} \frac{ \frac{r_{ik}}{m_j} }{
#'                                           \sum_{i}^{I}r_{ij}}
#'   }{
#'   RWRk = sum_j^J ( (rik / mj) / sum_i^I rij)
#'   }
#'
#' @inheritSection eval_cost_summary Solution format
#'
#' @inherit eval_replacement_importance return
#'
#' @seealso
#' See [importance] for an overview of all functions for evaluating
#' the importance of planning units selected in a solution.
#.
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
#'
#' # create minimal problem with raster planning units
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
#' # calculate importance scores
#' rwr1 <- eval_rare_richness_importance(p1, s1)
#'
#' # print importance scores
#' print(rwr1)
#'
#' # plot importance scores
#' plot(rwr1, main = "rarity weighted richness", axes = FALSE)
#'
#' # create minimal problem with polygon planning units
#' p2 <-
#'   problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.05) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
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
#' rwr2 <- eval_rare_richness_importance(p2, s2[, "solution_1"])
#'
#' # plot importance scores
#' plot(rwr2, main = "rarity weighted richness")
#' }
#'
#' @references
#' Williams P, Gibbons D, Margules C, Rebelo A, Humphries C, and Pressey RL
#' (1996) A comparison of richness hotspots, rarity hotspots and complementary
#' areas for conserving diversity using British birds.
#' *Conservation Biology*, 10: 155--174.
#'
#' @aliases eval_rare_richness_importance,ConservationProblem,numeric-method eval_rare_richness_importance,ConservationProblem,matrix-method eval_rare_richness_importance,ConservationProblem,data.frame-method eval_rare_richness_importance,ConservationProblem,Spatial-method eval_rare_richness_importance,ConservationProblem,sf-method eval_rare_richness_importance,ConservationProblem,Raster-method eval_rare_richness_importance,ConservationProblem,SpatRaster-method
#'
#' @name eval_rare_richness_importance
#'
#' @rdname eval_rare_richness_importance
#'
#' @exportMethod eval_rare_richness_importance
methods::setGeneric("eval_rare_richness_importance",
  function(x, solution, ...) {
    rlang::check_required(x)
    rlang::check_required(solution)
    assert(
      is_conservation_problem(x),
      is_inherits(
        solution,
        c(
          "numeric", "data.frame",  "matrix", "sf", "SpatRaster",
          "Raster", "Spatial"
        )
      )
    )
    standardGeneric("eval_rare_richness_importance")
  }
)

#' @name eval_rare_richness_importance
#' @usage \S4method{eval_rare_richness_importance}{ConservationProblem,numeric}(x, solution, rescale, ...)
#' @rdname eval_rare_richness_importance
methods::setMethod("eval_rare_richness_importance",
  methods::signature("ConservationProblem", "numeric"),
  function(x, solution, rescale = TRUE, ...) {
    # assert valid arguments
    assert(is.numeric(solution))
    assert_dots_empty()
    # extract planning unit solution status
    status <- planning_unit_solution_status(x, solution)
    # extract indices
    idx <- x$planning_unit_indices()
    pos <- which(status > 1e-10)
    # calculate scores
    v <- internal_eval_rare_richness_importance(x, pos, rescale)
    # return scores
    out <- rep(NA_real_, x$number_of_total_units())
    out[idx] <- 0
    out[idx[pos]] <- c(v)
    out
  }
)

#' @name eval_rare_richness_importance
#' @usage \S4method{eval_rare_richness_importance}{ConservationProblem,matrix}(x, solution, rescale, ...)
#' @rdname eval_rare_richness_importance
methods::setMethod("eval_rare_richness_importance",
  methods::signature("ConservationProblem", "matrix"),
  function(x, solution, rescale = TRUE, ...) {
    # assert valid arguments
    assert(
      is.matrix(solution),
      is.numeric(solution)
    )
    assert_dots_empty()
    # extract planning unit solution status
    status <- planning_unit_solution_status(x, solution)
    # extract indices
    idx <- x$planning_unit_indices()
    pos <- which(status > 1e-10)
    # calculate scores
    v <- internal_eval_rare_richness_importance(x, pos, rescale)
    # return scores
    out <- matrix(
      NA_real_,
      nrow = x$number_of_total_units(),
      ncol = 1,
      dimnames = list(NULL, "rwr")
    )
    out[idx, ] <- 0
    out[idx[pos], ] <- c(v)
    out
  }
)

#' @name eval_rare_richness_importance
#' @usage \S4method{eval_rare_richness_importance}{ConservationProblem,data.frame}(x, solution, rescale, ...)
#' @rdname eval_rare_richness_importance
methods::setMethod("eval_rare_richness_importance",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, solution, rescale = TRUE, ...) {
    # assert valid arguments
    assert(is.data.frame(solution))
    assert_dots_empty()
    # extract planning unit solution status
    status <- planning_unit_solution_status(x, solution)
    # extract indices
    idx <- x$planning_unit_indices()
    pos <- which(status > 1e-10)
    # calculate scores
    v <- internal_eval_rare_richness_importance(x, pos, rescale)
    # return scores
    out <- matrix(
      NA_real_,
      nrow = x$number_of_total_units(),
      ncol = 1,
      dimnames = list(NULL, "rwr")
    )
    out[idx, ] <- 0
    out[idx[pos], ] <- c(v)
    tibble::as_tibble(out)
  }
)

#' @name eval_rare_richness_importance
#' @usage \S4method{eval_rare_richness_importance}{ConservationProblem,Spatial}(x, solution, rescale, ...)
#' @rdname eval_rare_richness_importance
methods::setMethod("eval_rare_richness_importance",
  methods::signature("ConservationProblem", "Spatial"),
  function(x, solution, rescale = TRUE, ...) {
    # assert valid arguments
    assert(
      inherits(
        solution,
        c(
          "SpatialPointsDataFrame", "SpatialLinesDataFrame",
          "SpatialPolygonsDataFrame"
        )
      )
    )
    assert_dots_empty()
    # extract planning unit solution status
    status <- planning_unit_solution_status(x, solution)
    # extract indices
    idx <- x$planning_unit_indices()
    pos <- which(status > 1e-10)
    # calculate scores
    v <- internal_eval_rare_richness_importance(x, pos, rescale)
    # return scores
    out <- matrix(
      NA_real_,
      nrow = x$number_of_total_units(),
      ncol = 1,
      dimnames = list(NULL, "rwr")
    )
    out[idx, ] <- 0
    out[idx[pos], ] <- c(v)
    out <- as.data.frame(out)
    rownames(out) <- rownames(solution@data)
    solution@data <- out
    solution
  }
)

#' @name eval_rare_richness_importance
#' @usage \S4method{eval_rare_richness_importance}{ConservationProblem,sf}(x, solution, rescale, ...)
#' @rdname eval_rare_richness_importance
methods::setMethod("eval_rare_richness_importance",
  methods::signature("ConservationProblem", "sf"),
  function(x, solution, rescale = TRUE, ...) {
    # assert valid arguments
    assert(inherits(solution, "sf"))
    assert_dots_empty()
    # extract planning unit solution status
    status <- planning_unit_solution_status(x, solution)
    # extract indices
    idx <- x$planning_unit_indices()
    pos <- which(status > 1e-10)
    # calculate scores
    v <- internal_eval_rare_richness_importance(x, pos, rescale)
    # return scores
    out <- matrix(
      NA_real_,
      nrow = x$number_of_total_units(),
      ncol = 1,
      dimnames = list(NULL, "rwr")
    )
    out[idx, ] <- 0
    out[idx[pos], ] <- c(v)
    out <- tibble::as_tibble(as.data.frame(out))
    out$geometry <- sf::st_geometry(x$data$cost)
    sf::st_sf(out, crs = sf::st_crs(x$data$cost))
  }
)

#' @name eval_rare_richness_importance
#' @usage \S4method{eval_rare_richness_importance}{ConservationProblem,Raster}(x, solution, rescale, ...)
#' @rdname eval_rare_richness_importance
methods::setMethod("eval_rare_richness_importance",
  methods::signature("ConservationProblem", "Raster"),
  function(x, solution, rescale = TRUE, ...) {
    assert(inherits(solution, "Raster"))
    assert_dots_empty()
    # extract planning unit solution status
    status <- planning_unit_solution_status(x, solution)
    # extract indices
    idx <- x$planning_unit_indices()
    pos <- which(status > 1e-10)
    # calculate scores
    v <- internal_eval_rare_richness_importance(x, pos, rescale)
    # return scores
    out <- x$data$cost[[1]]
    out[idx] <- 0
    out[idx[pos]] <- c(v)
    names(out) <- "rwr"
    out
  }
)

#' @name eval_rare_richness_importance
#' @usage \S4method{eval_rare_richness_importance}{ConservationProblem,SpatRaster}(x, solution, rescale, ...)
#' @rdname eval_rare_richness_importance
methods::setMethod("eval_rare_richness_importance",
  methods::signature("ConservationProblem", "SpatRaster"),
  function(x, solution, rescale = TRUE, ...) {
    assert(inherits(solution, "SpatRaster"))
    assert_dots_empty()
    # extract planning unit solution status
    status <- planning_unit_solution_status(x, solution)
    # extract indices
    idx <- x$planning_unit_indices()
    pos <- which(status > 1e-10)
    # calculate scores
    v <- internal_eval_rare_richness_importance(x, pos, rescale)
    # return scores
    out <- x$data$cost[[1]]
    out[idx] <- 0
    out[idx[pos]] <- c(v)
    names(out) <- "rwr"
    out
})

internal_eval_rare_richness_importance <- function(x, indices, rescale,
                                                   call = fn_caller_env()) {
  assert(
    is_conservation_problem(x),
    assertthat::is.flag(rescale),
    assertthat::noNA(rescale),
    call = call
  )
  assert(
    is.integer(indices),
    length(indices) > 0,
    .internal = TRUE
  )
  assert(
    number_of_zones(x) == 1,
    msg = c(
      "This function requires that {.arg x} must have a single zone.",
      "i" = paste(
        "This is because the calculations only work",
        "for a limited range of problem formulations."
      ),
      "i" = "Try using {.fn eval_replacement_importance} instead."
    ),
    call = call
  )
  # calculate rarity weighted richness for each selected planning unit
  rs <- x$feature_abundances_in_total_units()
  m <- matrix(
    apply(x$data$rij_matrix[[1]], 1, max, na.rm = TRUE),
    nrow = nrow(rs),
    ncol = length(indices),
    byrow = FALSE
  )
  out <- x$data$rij_matrix[[1]][, indices, drop = FALSE]
  ## account for divide by zero issues result in NaNs
  out <- (out / m)
  out[!is.finite(out)] <- 0
  ## account for divide by zero issues result in NaNs
  out <- out / rs[, rep.int(1, ncol(out)), drop = FALSE]
  out[!is.finite(out)] <- 0
  out <- Matrix::colSums(out)
  # rescale values if specified
  if (rescale) {
    rescale_ind <- is.finite(out) & (abs(out) > 1e-10)
    out[rescale_ind] <- rescale(out[rescale_ind], to = c(0.01, 1))
  }
  # return result
  out
}
