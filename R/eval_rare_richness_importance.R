#' @include internal.R pproto.R  ConservationProblem-proto.R OptimizationProblem-proto.R problem.R
NULL

#' Evaluate solution importance using rarity weighted richness scores
#'
#' Calculate importance scores for planning units selected in a solution
#' using rarity weighted richness scores (based on Williams *et al.* 1996).
#' This method is only recommended for large-scaled conservation
#' planning exercises (i.e. more than 100,000 planning units) where
#' importance scores cannot be calculated using other methods in a feasible
#' period of time. This is because rarity weighted richness scores cannot (i)
#' account for the cost of different planning units, (ii) account for multiple
#' management zones, and (iii) identify truly irreplaceable planning units---
#' unlike the replacement cost metric which does not suffer any of these
#' limitations.
#'
#' @inheritParams eval_replacement_importance
#'
#' @details Rarity weighted richness scores are calculated using the following
#'   terms. Let \eqn{I} denote the set of planning units (indexed by
#'   \eqn{i}), let \eqn{J} denote the set of conservation features (indexed by
#'   \eqn{j}), let \eqn{r_{ij}} denote the amount of feature \eqn{j}
#'   associated with planning unit \eqn{i}, and let \eqn{M_j} denote the
#'   maximum value of feature \eqn{j} in \eqn{r_{ij}} in all planning units
#'   \eqn{i \in I}. To calculate the rarity weighted richness (*RWR*) for
#'   planning unit \eqn{k}:
#'
#'   \deqn{
#'   \mathit{RWR}_{k} = \sum_{j}^{J} \frac{ \frac{r_{kj}}{M_j} }{
#'                                           \sum_{i}^{I} r_{ij}}
#'   }{
#'   RWRk = sum_j^J ( (rkj / Mj) / sum_i^I rij)
#'   }
#'
#' @inheritSection eval_cost_summary Solution format
#'
#' @inherit eval_replacement_importance return
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
#' # calculate importance scores
#' rwr1 <- eval_rare_richness_importance(p1, s1)
#'
#' # print importance scores
#' print(rwr1)
#'
#' # plot importance scores
#' plot(rwr1, main = "rarity weighted richness", axes = FALSE, box = FALSE)
#' }
#'
#' @references
#' Williams P, Gibbons D, Margules C, Rebelo A, Humphries C, and Pressey RL
#' (1996) A comparison of richness hotspots, rarity hotspots and complementary
#' areas for conserving diversity using British birds.
#' *Conservation Biology*, 10: 155--174.
#'
#' @inherit eval_replacement_importance seealso
#'
#' @aliases eval_rare_richness_importance,ConservationProblem,numeric-method eval_rare_richness_importance,ConservationProblem,matrix-method eval_rare_richness_importance,ConservationProblem,data.frame-method eval_rare_richness_importance,ConservationProblem,Spatial-method eval_rare_richness_importance,ConservationProblem,sf-method eval_rare_richness_importance,ConservationProblem,Raster-method
#'
#' @name eval_rare_richness_importance
#'
#' @rdname eval_rare_richness_importance
#'
#' @exportMethod eval_rare_richness_importance
methods::setGeneric("eval_rare_richness_importance",
  function(x, solution, ...) {
  standardGeneric("eval_rare_richness_importance")
})

#' @name eval_rare_richness_importance
#' @usage \S4method{eval_rare_richness_importance}{ConservationProblem,numeric}(x, solution, rescale, ...)
#' @rdname eval_rare_richness_importance
methods::setMethod("eval_rare_richness_importance",
  methods::signature("ConservationProblem", "numeric"),
  function(x, solution, rescale = TRUE, ...) {
    # assert valid arguments
    assertthat::assert_that(
      is.numeric(solution), sum(solution, na.rm = TRUE) > 1e-10,
      is.numeric(x$data$cost), is.matrix(x$data$cost),
      number_of_total_units(x) == length(solution),
      number_of_zones(x) == 1,
      min(solution, na.rm = TRUE) >= 0,
      max(solution, na.rm = TRUE) <= 1,
      no_extra_arguments(...))
    # subset planning units with finite cost values
    pos <- x$planning_unit_indices()
    pos2 <- which(!is.na(solution))
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA allocations in the",
           " solution")
    # calculate replacement costs
    indices <- which(solution[pos] > 1e-10)
    rc <- internal_eval_rare_richness_importance(x, indices, rescale)
    # return replacement costs
    out <- rep(NA_real_, x$number_of_total_units())
    out[pos] <- 0
    out[pos[indices]] <- c(rc)
    out
})

#' @name eval_rare_richness_importance
#' @usage \S4method{eval_rare_richness_importance}{ConservationProblem,matrix}(x, solution, rescale, ...)
#' @rdname eval_rare_richness_importance
methods::setMethod("eval_rare_richness_importance",
  methods::signature("ConservationProblem", "matrix"),
  function(x, solution, rescale = TRUE, ...) {
    # assert valid arguments
    assertthat::assert_that(
      is.matrix(solution), is.numeric(solution),
      is.matrix(x$data$cost), is.numeric(x$data$cost),
      sum(solution, na.rm = TRUE) > 1e-10,
      number_of_total_units(x) == nrow(solution),
      number_of_zones(x) == 1,
      min(solution, na.rm = TRUE) >= 0,
      max(solution, na.rm = TRUE) <= 1,
      no_extra_arguments(...))
    # subset planning units with finite cost values
    pos <- x$planning_unit_indices()
    pos2 <- which(rowSums(is.na(solution)) != ncol(solution))
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA allocations in the",
           " solution")
    solution_pu <- solution[pos, , drop = FALSE]
    if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(solution_pu))))
     stop("planning units with NA cost data must have NA allocations in the",
          " solution")
    # calculate replacement costs
    indices <- which(solution_pu > 1e-10)
    rc <- internal_eval_rare_richness_importance(x, indices, rescale)
    # return replacement costs
    out <- matrix(0, nrow = x$number_of_total_units(),
                  ncol = x$number_of_zones())
    if (x$number_of_zones() > 1) {
      colnames(out) <- paste0("rwr_", x$zone_names())
    } else {
      colnames(out) <- "rwr"
    }
    out[which(is.na(as.matrix(x$data$cost)))] <- NA_real_
    out[which(solution > 1e-10)] <- rc
    out
})

#' @name eval_rare_richness_importance
#' @usage \S4method{eval_rare_richness_importance}{ConservationProblem,data.frame}(x, solution, rescale, ...)
#' @rdname eval_rare_richness_importance
methods::setMethod("eval_rare_richness_importance",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, solution, rescale = TRUE, ...) {
    # assert valid arguments
    assertthat::assert_that(
      is.data.frame(solution),
      is.data.frame(x$data$cost),
      sum(as.matrix(solution), na.rm = TRUE) >= 1e-10,
      number_of_zones(x) == 1,
      number_of_total_units(x) == nrow(solution),
      is.numeric(unlist(solution)),
      min(unlist(solution), na.rm = TRUE) >= 0,
      max(unlist(solution), na.rm = TRUE) <= 1,
      no_extra_arguments(...))
    # subset planning units with finite cost values
    solution_matrix <- as.matrix(solution)
    pos <- x$planning_unit_indices()
    pos2 <- which(rowSums(is.na(solution_matrix)) != ncol(solution_matrix))
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA allocations in the",
           " solution")
    solution_pu <- solution_matrix[pos, , drop = FALSE]
    if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(solution_pu))))
     stop("planning units with NA cost data must have NA allocations in the",
          " solution")
    # calculate replacement costs
    indices <- which(solution_pu > 1e-10)
    rc <- internal_eval_rare_richness_importance(x, indices, rescale)
    # return replacement costs
    out <- matrix(0, nrow = x$number_of_total_units(),
                  ncol = x$number_of_zones())
    if (x$number_of_zones() > 1) {
      colnames(out) <- paste0("rwr_", x$zone_names())
    } else {
      colnames(out) <- "rwr"
    }
    pos <- which(is.na(as.matrix(as.data.frame(x$data$cost)[,
      x$data$cost_column, drop = FALSE])))
    out[pos] <- NA_real_
    out[which(solution > 1e-10)] <- rc
    tibble::as_tibble(out)
})

#' @name eval_rare_richness_importance
#' @usage \S4method{eval_rare_richness_importance}{ConservationProblem,Spatial}(x, solution, rescale, ...)
#' @rdname eval_rare_richness_importance
methods::setMethod("eval_rare_richness_importance",
  methods::signature("ConservationProblem", "Spatial"),
  function(x, solution, rescale = TRUE, ...) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(solution, c("SpatialPointsDataFrame", "SpatialLinesDataFrame",
                           "SpatialPolygonsDataFrame")),
      class(x$data$cost)[1] == class(solution)[1],
      number_of_zones(x) == 1,
      number_of_total_units(x) == nrow(solution@data),
      is.numeric(unlist(solution@data)),
      min(unlist(solution@data), na.rm = TRUE) >= 0,
      max(unlist(solution@data), na.rm = TRUE) <= 1,
      no_extra_arguments(...))
    # subset planning units with finite cost values
    solution_matrix <- as.matrix(solution@data)
    pos <- x$planning_unit_indices()
    pos2 <- which(rowSums(is.na(solution_matrix)) != ncol(solution_matrix))
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA allocations in the",
           " solution")
    solution_pu <- solution_matrix[pos, , drop = FALSE]
    if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(solution_pu))))
     stop("planning units with NA cost data must have NA allocations in the",
          " solution")
    # calculate replacement costs
    indices <- which(solution_pu > 1e-10)
    rc <- internal_eval_rare_richness_importance(x, indices, rescale)
    # return replacement costs
    out <- matrix(0, nrow = x$number_of_total_units(),
                  ncol = x$number_of_zones())
    if (x$number_of_zones() > 1) {
      colnames(out) <- paste0("rwr_", x$zone_names())
    } else {
      colnames(out) <- "rwr"
    }
    pos <- which(is.na(as.matrix(as.data.frame(x$data$cost@data)[,
      x$data$cost_column, drop = FALSE])))
    out[pos] <- NA_real_
    out[which(solution_matrix > 1e-10)] <- rc
    out <- as.data.frame(out)
    rownames(out) <- rownames(solution@data)
    solution@data <- out
    solution
})

#' @name eval_rare_richness_importance
#' @usage \S4method{eval_rare_richness_importance}{ConservationProblem,sf}(x, solution, rescale, ...)
#' @rdname eval_rare_richness_importance
methods::setMethod("eval_rare_richness_importance",
  methods::signature("ConservationProblem", "sf"),
  function(x, solution, rescale = TRUE, ...) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(solution, "sf"),
      inherits(x$data$cost, "sf"))
    solution2 <- sf::st_drop_geometry(solution)
    assertthat::assert_that(
      number_of_zones(x) == 1,
      number_of_total_units(x) == nrow(solution2),
      is.numeric(unlist(solution2)),
      min(unlist(solution2), na.rm = TRUE) >= 0,
      max(unlist(solution2), na.rm = TRUE) <= 1,
      no_extra_arguments(...))
    # subset planning units with finite cost values
    solution_matrix <- as.matrix(solution2)
    pos <- x$planning_unit_indices()
    pos2 <- which(rowSums(is.na(solution_matrix)) != ncol(solution_matrix))
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA allocations in the",
           " solution")
    solution_pu <- solution_matrix[pos, , drop = FALSE]
    if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(solution_pu))))
     stop("planning units with NA cost data must have NA allocations in the",
          " solution")
    # calculate replacement costs
    indices <- which(solution_pu > 1e-10)
    rc <- internal_eval_rare_richness_importance(x, indices, rescale)
    # return replacement costs
    out <- matrix(0, nrow = x$number_of_total_units(),
                  ncol = x$number_of_zones())
    if (x$number_of_zones() > 1) {
      colnames(out) <- paste0("rwr_", x$zone_names())
    } else {
      colnames(out) <- "rwr"
    }
    pos <- which(is.na(as.matrix(as.data.frame(x$data$cost)[,
      x$data$cost_column, drop = FALSE])))
    out[pos] <- NA_real_
    out[which(solution_matrix > 1e-10)] <- rc
    out <- as.data.frame(out)
    sf::st_as_sf(sf::st_geometry(x$data$cost), out)
})

#' @name eval_rare_richness_importance
#' @usage \S4method{eval_rare_richness_importance}{ConservationProblem,Raster}(x, solution, rescale, ...)
#' @rdname eval_rare_richness_importance
methods::setMethod("eval_rare_richness_importance",
  methods::signature("ConservationProblem", "Raster"),
  function(x, solution, rescale = TRUE, ...) {
    assertthat::assert_that(
      inherits(solution, "Raster"),
      inherits(x$data$cost, "Raster"),
      number_of_zones(x) == 1,
      sf::st_crs(x$data$cost@crs) == sf::st_crs(solution@crs),
      is_comparable_raster(x$data$cost, solution[[1]]),
      min(raster::cellStats(solution, "min")) >= 0,
      max(raster::cellStats(solution, "max")) <= 1,
      no_extra_arguments(...))
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
    solution_matrix <- solution[pos2]
    if (!is.matrix(solution_matrix))
      solution_matrix <- matrix(solution_matrix, ncol = 1)
    if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(solution_matrix))))
     stop("planning units with NA cost data must have NA allocations in the",
          " solution")
    # calculate replacement costs
    indices <- which(solution_matrix > 1e-10)
    rc <- internal_eval_rare_richness_importance(x, indices, rescale)
    # prepare output
    rc <- split(rc, which(solution_matrix > 1e-10, arr.ind = TRUE)[, 2])
    # return result
    out <- as.list(solution)
    if (x$number_of_zones() > 1) {
      names(out) <- paste0("rwr_", x$zone_names())
    } else {
      names(out) <- "rwr"
    }
    for (i in seq_along(out)) {
      out[[i]][!is.na(out[[i]])] <- 0
      out[[i]][solution[[i]] > 1e-10] <- rc[[i]]
    }
    if (length(out) > 1) {
      out <- raster::stack(out)
    } else {
      out <- out[[1]]
    }
    out
})

internal_eval_rare_richness_importance <- function(x, indices, rescale) {
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          x$number_of_zones() == 1,
                          is.integer(indices), length(indices) > 0,
                          assertthat::is.flag(rescale))
  # calculate rarity weighted richness for each selected planning unit
  rs <- x$feature_abundances_in_total_units()
  m <- matrix(apply(x$data$rij_matrix[[1]], 1, max, na.rm = TRUE),
              nrow = nrow(rs), ncol = length(indices), byrow = FALSE)
  out <- x$data$rij_matrix[[1]][, indices, drop = FALSE]
  ## account for divide by zero issues result in NaNs
  out <- (out / m)
  out[!is.finite(out)] <- 0
  ## account for divide by zero issues result in NaNs
  out <- out / rs[, rep.int(1, ncol(out)), drop = FALSE]
  out[!is.finite(out)] <- 0
  out <- colSums(out)
  # rescale values if specified
  if (rescale) {
    rescale_ind <- is.finite(out) & (abs(out) > 1e-10)
    out[rescale_ind] <- rescale(out[rescale_ind], to = c(0.01, 1))
  }
  # return result
  out
}
