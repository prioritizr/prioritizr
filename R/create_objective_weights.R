#' Generate objective weight combinations for weighted-sum optimisation
#'
#' Creates combinations of objective weights for use in a
#' weighted-sum multi-objective optimisation. Each row corresponds to
#' one weight configuration, and each column corresponds to an objective.
#'
#' @param n_objectives `character` vector of objective names.
#'
#' @param n_per_objective `integer` number of weight levels
#' to generate per objective.
#'
#' @param include_zero `logical`. Should zero be included as a possible
#' weight value? Defaults to `FALSE`.
#'
#' @return A numeric `matrix` where rows correspond to different
#' weight combinations and columns correspond to objectives.
#' 
#' @examples
#' \dontrun{
#' # In this example we select a set of planning units under a conservation 
#' # budget, aiming to meet representation targets for two species groups:
#' # (1) keystone species (higher ecological priority) and
#' # (2) iconic species (high social or cultural value).
#' 
#' # import data
#' con_cost <- get_sim_pu_raster()
#' keystone_spp <- get_sim_features()[[1:3]]
#' iconic_spp <- get_sim_features()[[4:5]]
#' 
#' # define a total conservation budget (30% of total cost)
#' budget <- terra::global(con_cost, "sum", na.rm = TRUE)[[1]] * 0.3
#' 
#' # define individual problems
#' p1 <-
#'   problem(con_cost, keystone_spp) %>%
#'   add_min_shortfall_objective(budget) %>%
#'   add_relative_targets(0.4) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver()
#' p2 <-
#'   problem(con_cost, iconic_spp) %>%
#'   add_min_shortfall_objective(budget) %>%
#'   add_relative_targets(0.4) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver()
#'   
#'  # create input matrix
#'  weights <- create_objective_weights(n_objectives = 2,
#'  n_per_objective = 5, include_zero = TRUE)
#' 
#' # create multi-objective problem using input matrix
#' mp1 <-
#'   multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
#'   add_weighted_sum_approach(
#'     weights,
#'     verbose = TRUE
#'   ) %>%
#'   add_default_solver(verbose = FALSE)
#' 
#' # solve problem
#' ms1 <- solve(mp1)
#' 
#' # extract objective values and plot approximated pareto front
#' obj_mat <- attributes(ms1)$objective
#' plot(obj_mat, main = "Approximated pareto front", 
#' xlab = "Keystone objective (shortfall)", 
#' ylab = "Iconic objective (shortfall)")
#' }
#'
#' @export
create_objective_weights <- function(n_objectives, n_per_objective, 
                                     include_zero = FALSE){
  
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.count(n_objectives),
    n_objectives >= 1,
    assertthat::noNA(n_objectives),
    assertthat::is.count(n_per_obj),
    assertthat::noNA(n_per_obj),
    is.logical(include_zero),
    length(include_zero) == 1
  )
  
  # create all combinations of values
  if (include_zero) {
    values <- list(seq(0, 1, length.out = n_per_objective + 1))[rep(1, n_objectives)]
  } else {
    values <- list(seq(0, 1, length.out = n_per_objective + 1)[-1])[rep(1, n_objectives)]
  }
  out <- do.call(expand.grid, args = values)
  
  # remove rows where all values are the same
  keep <- apply(out, 1, function(z) length(unique(z)) >= 2)
  out <- out[keep, , drop = FALSE]
  
  # remove rows where all objs are zero except one
  keep <- rowSums(out > 1e-6) >= 2
  out <- out[keep, , drop = FALSE]
  
  # manually add in a row where only one obj is assigned a non-zero value
  single_obj <- as.data.frame(diag(n_objectives))
  names(single_obj) <- names(out)
  
  out <- rbind(single_obj, out)
  
  # manually add in a row where each objective is assigned equal weighting
  equal_weights <- as.data.frame(matrix(1, nrow = 1, ncol = n_objectives))
  names(equal_weights) <- names(out)
  
  out <- rbind(equal_weights, out)
  
  # convert to matrix
  out <- as.matrix(out)
  colnames(out) <- NULL

  out
  
}