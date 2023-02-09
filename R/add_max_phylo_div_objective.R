#' @include internal.R Objective-class.R
NULL

#' Add maximum phylogenetic diversity objective
#'
#' Set the objective of a conservation planning [problem()] to
#' maximize the phylogenetic diversity of the features represented in the
#' solution subject to a budget. This objective is similar to
#' [add_max_features_objective()] except
#' that emphasis is placed on representing a phylogenetically diverse set of
#' species, rather than as many features as possible (subject to weights).
#' This function was inspired by Faith (1992) and Rodrigues *et al.*
#' (2002).
#'
#' @inheritParams add_max_utility_objective
#'
#' @param tree [ape::phylo()] object specifying a phylogenetic tree
#'   for the conservation features.
#'
#' @details
#' The maximum phylogenetic diversity objective finds the set of
#' planning units that meets representation targets for a phylogenetic tree
#' while staying within a fixed budget. If multiple solutions can meet all
#' targets while staying within budget, the cheapest solution is chosen.
#' Note that this objective is similar to the maximum
#' features objective ([add_max_features_objective()]) in that it
#' allows for both a budget and targets to be set for each feature. However,
#' unlike the maximum feature objective, the aim of this objective is to
#' maximize the total phylogenetic diversity of the targets met in the
#' solution, so if multiple targets are provided for a single feature, the
#' problem will only need to meet a single target for that feature
#' for the phylogenetic benefit for that feature to be counted when
#' calculating the phylogenetic diversity of the solution. In other words,
#' for multi-zone problems, this objective does not aim to maximize the
#' phylogenetic diversity in each zone, but rather this objective
#' aims to maximize the phylogenetic diversity of targets that can be met
#' through allocating planning units to any of the different zones in a
#' problem. This can be useful for problems where targets pertain to the total
#' amount held for each feature across multiple zones. For example,
#' each feature might have a non-zero amount of suitable habitat in each
#' planning unit when the planning units are assigned to a (i) not restored,
#' (ii) partially restored, or (iii) completely restored management zone.
#' Here each target corresponds to a single feature and can be met through
#' the total amount of habitat in planning units present to the three
#' zones.
#'
#' @section Mathematical formulation:
#' This objective can be expressed mathematically for a set of planning units
#' (\eqn{I}{I} indexed by \eqn{i}{i}) and a set of features (\eqn{J}{J}
#' indexed by \eqn{j}{j}) as:
#'
#' \deqn{\mathit{Maximize} \space \sum_{i = 1}^{I} -s \space c_i \space x_i +
#' \sum_{j = 1}^{J} m_b l_b \\
#' \mathit{subject \space to} \\
#' \sum_{i = 1}^{I} x_i r_{ij} \geq y_j t_j \forall j \in J \\
#' m_b \leq y_j \forall j \in T(b) \\
#' \sum_{i = 1}^{I} x_i c_i \leq B}{
#' Maximize sum_i^I (-s * ci * xi) + sum_j^J (mb * lb) subject to sum_i^I
#' (xi * rij) >= (yj * tj) for all j in J & mb <= yj for all j in T(b) &
#' sum_i^I (xi * ci) <= B}
#'
#' Here, \eqn{x_i}{xi} is the [decisions] variable (e.g.,
#' specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#' (0)), \eqn{r_{ij}}{rij} is the amount of feature \eqn{j}{j} in planning
#' unit \eqn{i}{i}, \eqn{t_j}{tj} is the representation target for feature
#' \eqn{j}{j}, \eqn{y_j}{yj} indicates if the solution has meet
#' the target \eqn{t_j}{tj} for feature \eqn{j}{j}. Additionally,
#' \eqn{T}{T} represents a phylogenetic tree containing features \eqn{j}{j}
#' and has the branches \eqn{b} associated within lengths \eqn{l_b}{lb}.
#' The binary variable \eqn{m_b}{mb} denotes if
#' at least one feature associated with the branch \eqn{b}{b} has met its
#' representation as indicated by \eqn{y_j}{yj}. For brevity, we denote
#' the features \eqn{j}{j} associated with branch \eqn{b}{b} using
#' \eqn{T(b)}{T(b)}. Finally, \eqn{B}{B} is the budget allocated for the
#' solution, \eqn{c_i}{ci} is the cost of planning unit \eqn{i}{i}, and
#' \eqn{s}{s} is a scaling factor used to shrink the costs so that the problem
#' will return a cheapest solution when there are multiple solutions that
#' represent the same amount of all features within the budget.
#'
#' @section Notes:
#' In early versions, this function was named as the
#' `add_max_phylo_div_objective` function.
#'
#' @seealso
#' See [objectives] for an overview of all functions for adding objectives.
#' Also, see [targets] for an overview of all functions for adding targets, and
#' [add_feature_weights()] to specify weights for different features.
#'
#' @family objectives
#'
#' @inherit add_min_set_objective return
#'
#' @aliases add_max_phylo_objective
#'
#' @references
#' Faith DP (1992) Conservation evaluation and phylogenetic diversity.
#' *Biological Conservation*, 61: 1--10.
#'
#' Rodrigues ASL and Gaston KJ (2002) Maximising phylogenetic diversity in the
#' selection of networks of conservation areas. *Biological Conservation*,
#' 105: 103--111.
#'
#' @examples
#' \dontrun{
#' # load ape package
#' require(ape)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_phylogeny <- get_sim_phylogeny()
#' sim_pu_zones_raster  <- get_sim_zones_pu_raster()
#' sim_features_zones  <- get_sim_zones_features()
#'
#' # plot the simulated phylogeny
#' par(mfrow = c(1, 1))
#' plot(sim_phylogeny, main = "phylogeny")
#'
#' # create problem with a maximum phylogenetic diversity objective,
#' # where each feature needs 10% of its distribution to be secured for
#' # it to be adequately conserved and a total budget of 1900
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_max_phylo_div_objective(1900, sim_phylogeny) %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # find out which features have their targets met
#' r1 <- eval_target_coverage_summary(p1, s1)
#' print(r1, width = Inf)
#'
#' # plot the phylogeny and color the adequately represented features in red
#' plot(
#'   sim_phylogeny, main = "adequately represented features",
#'   tip.color = replace(
#'     rep("black", terra::nlyr(sim_features)),
#'     sim_phylogeny$tip.label %in% r1$feature[r1$met], "red"
#'   )
#' )
#'
#' # rename the features in the example phylogeny for use with the
#' # multi-zone data
#' sim_phylogeny$tip.label <- feature_names(sim_features_zones)
#'
#' # create targets for a multi-zone problem. Here, each feature needs a total
#' # of 10 units of habitat to be conserved among the three zones to be
#' # considered adequately conserved
#' targets <- tibble::tibble(
#'   feature = feature_names(sim_features_zones),
#'   zone = list(zone_names(sim_features_zones))[
#'     rep(1, number_of_features(sim_features_zones))],
#'   type = rep("absolute", number_of_features(sim_features_zones)),
#'   target = rep(10, number_of_features(sim_features_zones))
#' )
#'
#' # create a multi-zone problem with a maximum phylogenetic diversity
#' # objective, where the total expenditure in all zones is 5000.
#' p2 <-
#'   problem(sim_pu_zones_raster, sim_features_zones) %>%
#'   add_max_phylo_div_objective(5000, sim_phylogeny) %>%
#'   add_manual_targets(targets) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(category_layer(s2), main = "solution", axes = FALSE)
#'
#' # find out which features have their targets met
#' r2 <- eval_target_coverage_summary(p2, s2)
#' print(r2, width = Inf)
#'
#' # plot the phylogeny and color the adequately represented features in red
#' plot(
#'   sim_phylogeny, main = "adequately represented features",
#'   tip.color = replace(
#'     rep("black", terra::nlyr(sim_features)), which(r2$met), "red"
#'   )
#' )
#'
#' # create a multi-zone problem with a maximum phylogenetic diversity
#' # objective, where each zone has a separate budget.
#' p3 <-
#'   problem(sim_pu_zones_raster, sim_features_zones) %>%
#'   add_max_phylo_div_objective(c(2500, 500, 2000), sim_phylogeny) %>%
#'   add_manual_targets(targets) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s3 <- solve(p3)
#'
#' # plot solution
#' plot(category_layer(s3), main = "solution", axes = FALSE)
#'
#' # find out which features have their targets met
#' r3 <- eval_target_coverage_summary(p3, s3)
#' print(r3, width = Inf)
#'
#' # plot the phylogeny and color the adequately represented features in red
#' plot(
#'   sim_phylogeny, main = "adequately represented features",
#'   tip.color = replace(
#'     rep("black", terra::nlyr(sim_features)), which(r3$met), "red"
#'   )
#' )
#' }
#' @name add_max_phylo_div_objective
NULL

#' @rdname add_max_phylo_div_objective
#' @export
add_max_phylo_div_objective <- function(x, budget, tree) {
  # assert arguments are valid
  rlang::check_required(x)
  rlang::check_required(budget)
  assert(
    is_conservation_problem(x),
    is.numeric(budget),
    all_finite(budget),
    all_positive(budget),
    inherits(tree, "phylo"),
    length(tree$tip.label) == number_of_features(x),
    all_match_of(tree$tip.label, feature_names(x)),
    is_budget_length(x, budget)
  )
  # add objective to problem
  x$add_objective(
    R6::R6Class(
      "PhylogeneticDiversityObjective",
      inherit = Objective,
      public = list(
        name = "phylogenetic diversity objective",
        data = list(budget = budget, tree = tree),
        apply = function(x, y) {
          # assert arguments valid
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            .internal = TRUE
          )
          # extract data
          tr <- self$get_data("tree")
          el <- tr$edge.length
          # order rows to match order of features in problem
          pos <- match(tr$tip.label, y$feature_names())
          # convert tree into matrix showing which species in which
          # branches and store the result
          bm <- branch_matrix(tr)[pos, , drop = FALSE]
          # apply objective
          invisible(
            rcpp_apply_max_phylo_objective(
              x$ptr,
              y$feature_targets(),
              y$planning_unit_costs(),
              self$get_data("budget"),
              bm,
              tr$edge.length
            )
          )
        }
      )
    )$new()
  )
}
