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
#' Note that this function only works for problems
#' that use targets and a single zone.
#' It will throw an error for problems that do not meet these criteria.
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
#'   [terra::rast()], or [sf::st_sf()] object containing the scores for each
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
#'
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
#' # create minimal problem with binary decisions
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
#' plot(fs1, main = names(fs1), axes = FALSE)
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
#' fs2 <- eval_ferrier_importance(p2, s2[, "solution_1"])
#'
#' # plot importance scores
#' plot(fs2)
#'
#' }
#'
#' @export
eval_ferrier_importance <- function(x, solution) {
  # assert valid arguments
  assert_required(x)
  assert_required(solution)
  assert(
    is_conservation_problem(x),
    is_inherits(
      solution,
      c(
        "numeric", "data.frame", "matrix", "sf", "SpatRaster",
        "Spatial", "Raster"
      )
    )
  )
  assert(
    isTRUE(number_of_zones(x) == 1),
    msg = c(
      "This function requires that {.arg x} must have a single zone.",
      "i" = paste(
        "This is because the calculations only work",
        "for a limited range of problem formulations."
      ),
      "i" = "Try using {.fn eval_replacement_importance} instead."
    )
  )
  assert(
    !is.Waiver(x$targets),
    msg = c(
      "This function requires that {.arg x} must have targets.",
      "i" = paste(
        "This is because the calculations only work",
        "for a limited range of problem formulations."
      ),
      "i" = "Try using {.fn eval_replacement_importance} instead."
    )
  )
  assert(
    !inherits(
      x$objective,
      c("MaximumUtilityObjective", "MaximumCoverageObjective")
    ),
    msg = c(
      paste(
        "This function requires that {.arg x} must have an objective",
        "that uses targets."
      ),
      "i" = paste(
        "This is because the calculations only work",
        "for a limited range of problem formulations."
      ),
      "i" = "Try using {.fn eval_replacement_importance} instead."
    )
  )
  assert(
    !any(x$feature_names() == "total"),
    msg = c(
      paste(
        "This function requires that none of the features in",
        "{.arg x} are called {.val total}."
      ),
      "i" = paste(
        "This is because {.fn eval_ferrier_importance} creates a new",
        "column or layer named {.val total} to output results."
      )
    )
  )
  # extract planning unit solution status
  status <- planning_unit_solution_status(x, solution)
  # calculate replacement costs
  v <- internal_eval_ferrier_importance(x, status, rescale)
  # prepare formatted values
  nms <- names(v)
  if (inherits(x$data$cost, "Raster")) {
    out <- stats::setNames(
      raster::stack(planning_unit_solution_format(x, v)),
      nms
    )
  } else if (inherits(x$data$cost, "SpatRaster")) {
    out <- stats::setNames(
      terra::rast(planning_unit_solution_format(x, v)),
      nms
    )
  } else if (inherits(x$data$cost, "matrix")) {
    out <- do.call(cbind, planning_unit_solution_format(x, v))
    colnames(out) <- nms
  } else if (inherits(x$data$cost, "Spatial")) {
    # note that we process this as matrix format to avoid creating
    # many duplicate geometries and save memory usage
    d <- lapply(seq_along(v), function(i) {
      planning_unit_solution_format(x, v[[i]], matrix(1))
    })
    d <- stats::setNames(
      as.data.frame(do.call(cbind, d)),
      nms
    )
    rownames(d) <- rownames(x$data$cost)
    out <- x$data$cost
    out@data <- d
  } else if (inherits(x$data$cost, "sf")) {
    out <- lapply(seq_along(v), function(i) {
      planning_unit_solution_format(x, v[[i]], matrix(1))
    })
    out <- tibble::as_tibble(
      stats::setNames(
        as.data.frame(do.call(cbind, out)),
        nms
      )
    )
    out$geometry <- sf::st_geometry(x$data$cost)
    out <- sf::st_sf(out, crs = sf::st_crs(x$data$cost))
  } else if (inherits(x$data$cost, "data.frame")) {
    out <- planning_unit_solution_format(
      x, v, prefix = seq_along(v), append = FALSE
    )
    out <- tibble::as_tibble(
      stats::setNames(
        as.data.frame(do.call(cbind, out)),
        nms
      )
    )
  } else {
    cli::cli_abort("Data not recognized.", .internal = TRUE) # nocov
  }
  # return result
  out
}

internal_eval_ferrier_importance <- function(x, status,
                                             call = fn_caller_env()) {
  # assert valid arguments
  assert(
    is.numeric(status),
    is.matrix(status),
    call = call,
    .internal = TRUE
  )
  # extract indices for solution
  indices <- which(status > 1e-10)
  # extract data
  rij <- x$data$rij_matrix[[1]]
  targets <- x$feature_targets()
  # validate data
  assert(
    all(targets$sense == ">="),
    msg = c(
      paste(
        "This function requires that all of the feature targets for {.arg x}",
        "have a {.code >=} sense."
      ),
      "i" = paste(
        "This is because the calculations only work",
        "for a limited range of problem formulations."
      ),
      "i" = "Try using {.fn eval_replacement_importance} instead."
    ),
    call = call
  )
  assert(
    all(targets$value >= 0),
    msg = c(
      "This function requires that all of the feature targets for {.arg x}",
      "are {.code >=} 0.",
      "i" = paste(
        "This is because the calculations only work",
        "for a limited range of problem formulations."
      ),
      "i" = "Try using {.fn eval_replacement_importance} instead."
    ),
    call = call
  )
  assert(
    all(rij@x >= 0),
    msg = c(
      "This function requires that all of the feature values for {.arg x}",
      "are {.code >=} 0.",
      "i" = paste(
        "This is because the calculations only work",
        "for a limited range of problem formulations."
      ),
      "i" = "Try using {.fn eval_replacement_importance} instead."
    ),
    call = call
  )
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
  s <- cbind(Matrix::t(s), Matrix::colSums(s))
  as.matrix(s)
  # convert to solution status format
  stats::setNames(
    lapply(seq_len(ncol(s)), function(i) {
      convert_raw_solution_to_solution_status(x, s[, i], indices)
    }),
    c(x$feature_names(), "total")
  )
}
