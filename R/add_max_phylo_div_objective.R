#' @include internal.R pproto.R Objective-proto.R
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
#' @param x [problem()] (i.e. [`ConservationProblem-class`]) object.
#'
#' @param budget `numeric` value specifying the maximum expenditure of
#'   the prioritization. For problems with multiple zones, the argument
#'   to `budget` can be a single `numeric` value to specify a budget
#'   for the entire solution or a `numeric` `vector` to specify
#'   a budget for each each management zone.
#'
#' @param tree [phylo()] object specifying a phylogenetic tree
#'   for the conservation features.
#'
#' @details A problem objective is used to specify the overall goal of the
#'   conservation planning problem. Please note that all conservation
#'   planning problems formulated in the \pkg{prioritizr} package require the
#'   addition of objectives---failing to do so will return an error
#'   message when attempting to solve problem.
#'
#'   The maximum phylogenetic diversity objective finds the set of
#'   planning units that meets representation targets for a phylogenetic tree
#'   while staying within a fixed budget. If multiple solutions can meet all
#'   targets while staying within budget, the cheapest solution is chosen.
#'   Note that this objective is similar to the maximum
#'   features objective ([add_max_features_objective()]) in that it
#'   allows for both a budget and targets to be set for each feature. However,
#'   unlike the maximum feature objective, the aim of this objective is to
#'   maximize the total phylogenetic diversity of the targets met in the
#'   solution, so if multiple targets are provided for a single feature, the
#'   problem will only need to meet a single target for that feature
#'   for the phylogenetic benefit for that feature to be counted when
#'   calculating the phylogenetic diversity of the solution. In other words,
#'   for multi-zone problems, this objective does not aim to maximize the
#'   phylogenetic diversity in each zone, but rather this objective
#'   aims to maximize the phylogenetic diversity of targets that can be met
#'   through allocating planning units to any of the different zones in a
#'   problem. This can be useful for problems where targets pertain to the total
#'   amount held for each feature across multiple zones. For example,
#'   each feature might have a non-zero amount of suitable habitat in each
#'   planning unit when the planning units are assigned to a (i) not restored,
#'   (ii) partially restored, or (iii) completely restored management zone.
#'   Here each target corresponds to a single feature and can be met through
#'   the total amount of habitat in planning units present to the three
#'   zones.
#'
#'   The maximum phylogenetic diversity objective for the reserve design
#'   problem can be expressed mathematically for a set of planning units
#'   (\eqn{I}{I} indexed by \eqn{i}{i}) and a set of features (\eqn{J}{J}
#'   indexed by \eqn{j}{j}) as:
#'
#'   \deqn{\mathit{Maximize} \space \sum_{i = 1}^{I} -s \space c_i \space x_i +
#'   \sum_{j = 1}^{J} m_b l_b \\
#'   \mathit{subject \space to} \\
#'   \sum_{i = 1}^{I} x_i r_{ij} \geq y_j t_j \forall j \in J \\
#'   m_b \leq y_j \forall j \in T(b) \\
#'   \sum_{i = 1}^{I} x_i c_i \leq B}{
#'   Maximize sum_i^I (-s * ci * xi) + sum_j^J (mb * lb) subject to sum_i^I
#'   (xi * rij) >= (yj * tj) for all j in J & mb <= yj for all j in T(b) &
#'   sum_i^I (xi * ci) <= B}
#'
#'   Here, \eqn{x_i}{xi} is the [decisions] variable (e.g.
#'   specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#'   (0)), \eqn{r_{ij}}{rij} is the amount of feature \eqn{j}{j} in planning
#'   unit \eqn{i}{i}, \eqn{t_j}{tj} is the representation target for feature
#'   \eqn{j}{j}, \eqn{y_j}{yj} indicates if the solution has meet
#'   the target \eqn{t_j}{tj} for feature \eqn{j}{j}. Additionally,
#'   \eqn{T}{T} represents a phylogenetic tree containing features \eqn{j}{j}
#'   and has the branches \eqn{b} associated within lengths \eqn{l_b}{lb}.
#'   The binary variable \eqn{m_b}{mb} denotes if
#'   at least one feature associated with the branch \eqn{b}{b} has met its
#'   representation as indicated by \eqn{y_j}{yj}. For brevity, we denote
#'   the features \eqn{j}{j} associated with branch \eqn{b}{b} using
#'   \eqn{T(b)}{T(b)}. Finally, \eqn{B}{B} is the budget allocated for the
#'   solution, \eqn{c_i}{ci} is the cost of planning unit \eqn{i}{i}, and
#'   \eqn{s}{s} is a scaling factor used to shrink the costs so that the problem
#'   will return a cheapest solution when there are multiple solutions that
#'   represent the same amount of all features within the budget.
#'
#' @section Notes:
#' In early versions, this function was named as the
#' `add_max_phylo_div_objective` function.
#'
#' @seealso [objectives], [branch_matrix()].
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
#' # load ape package
#' require(ape)
#'
#' # load data
#' data(sim_pu_raster, sim_features, sim_phylogeny, sim_pu_zones_stack,
#'      sim_features_zones)
#'
#' # plot the simulated phylogeny
#' \donttest{
#' par(mfrow = c(1, 1))
#' plot(sim_phylogeny, main = "phylogeny")
#' }
#' # create problem with a maximum phylogenetic diversity objective,
#' # where each feature needs 10 % of its distribution to be secured for
#' # it to be adequately conserved and a total budget of 1900
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_max_phylo_div_objective(1900, sim_phylogeny) %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE, box = FALSE)
#'
#' # find which features have their targets met
#' r1 <- feature_representation(p1, s1)
#' r1$target_met <- r1$relative_held > 0.1
#' print(r1)
#'
#' # plot the phylogeny and color the adequately represented features in red
#' plot(sim_phylogeny, main = "adequately represented features",
#'      tip.color = replace(
#'        rep("black", nlayers(sim_features)),
#'        sim_phylogeny$tip.label %in% r1$feature[r1$target_met],
#'        "red"))
#' }
#' # rename the features in the example phylogeny for use with the
#' # multi-zone data
#' sim_phylogeny$tip.label <- feature_names(sim_features_zones)
#'
#' # create targets for a multi-zone problem. Here, each feature needs a total
#' # of 10 units of habitat to be conserved among the three zones to be
#' # considered adequately conserved
#' targets <- tibble::tibble(
#'   feature = feature_names(sim_features_zones),
#'   zone = list(zone_names(sim_features_zones))[rep(1,
#'           number_of_features(sim_features_zones))],
#'   type = rep("absolute", number_of_features(sim_features_zones)),
#'   target = rep(10, number_of_features(sim_features_zones)))
#'
#' # create a multi-zone problem with a maximum phylogenetic diversity
#' # objective, where the total expenditure in all zones is 5000.
#' p2 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_max_phylo_div_objective(5000, sim_phylogeny) %>%
#'       add_manual_targets(targets) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(category_layer(s2), main = "solution", axes = FALSE, box = FALSE)
#'
#' # calculate total amount of habitat conserved for each feature among
#' # all three management zones
#' amount_held2 <- numeric(number_of_features(sim_features_zones))
#' for (z in seq_len(number_of_zones(sim_features_zones)))
#'   amount_held2 <- amount_held2 +
#'                   cellStats(sim_features_zones[[z]] * s2[[z]], "sum")
#'
#' # find which features have their targets met
#' targets_met2 <- amount_held2 >= targets$target
#' print(targets_met2)
#'
#' # plot the phylogeny and color the adequately represented features in red
#' plot(sim_phylogeny, main = "adequately represented features",
#'      tip.color = replace(rep("black", nlayers(sim_features)),
#'                          which(targets_met2), "red"))
#' }
#' # create a multi-zone problem with a maximum phylogenetic diversity
#' # objective, where each zone has a separate budget.
#' p3 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_max_phylo_div_objective(c(2500, 500, 2000), sim_phylogeny) %>%
#'       add_manual_targets(targets) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s3 <- solve(p3)
#'
#' # plot solution
#' plot(category_layer(s3), main = "solution", axes = FALSE, box = FALSE)
#'
#' # calculate total amount of habitat conserved for each feature among
#' # all three management zones
#' amount_held3 <- numeric(number_of_features(sim_features_zones))
#' for (z in seq_len(number_of_zones(sim_features_zones)))
#'   amount_held3 <- amount_held3 +
#'                   cellStats(sim_features_zones[[z]] * s3[[z]], "sum")
#'
#' # find which features have their targets met
#' targets_met3 <- amount_held3 >= targets$target
#' print(targets_met3)
#'
#' # plot the phylogeny and color the adequately represented features in red
#' plot(sim_phylogeny, main = "adequately represented features",
#'      tip.color = replace(rep("black", nlayers(sim_features)),
#'                          which(targets_met3), "red"))
#' }
#' @name add_max_phylo_div_objective
NULL

#' @rdname add_max_phylo_div_objective
#' @export
add_max_phylo_div_objective <- function(x, budget, tree) {
  # check that dependencies are installed
  if (!requireNamespace("ape"))
    stop("the \"ape\" package needs to be installed to use phylogenetic data")
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          is.numeric(budget),
                          all(is.finite(budget)),
                          all(budget >= 0.0),
                          isTRUE(min(budget) > 0),
                          length(budget) == 1 ||
                            length(budget) == number_of_zones(x),
                          inherits(tree, "phylo"),
                          length(tree$tip.label) == number_of_features(x),
                          setequal(tree$tip.label, feature_names(x)))
  # make parameter
  if (length(budget) == 1) {
    p <- numeric_parameter("budget", budget, lower_limit = 0,
                           upper_limit = sum(x$planning_unit_costs(),
                                             na.rm = TRUE))
  } else {
    p <- numeric_parameter_array("budget", budget, x$zone_names(),
                                 lower_limit = 0,
                                 upper_limit = colSums(x$planning_unit_costs(),
                                                       na.rm = TRUE))
  }
  # add objective to problem
  x$add_objective(pproto(
    "PhylogeneticDiversityObjective",
    Objective,
    name = "Phylogenetic diversity objective",
    parameters = parameters(p),
    data = list(tree = tree),
    calculate = function(self, x) {
      assertthat::assert_that(inherits(x, "ConservationProblem"))
      # get tree
      tr <- self$get_data("tree")
      # order rows to match order of features in problem
      pos <- match(tr$tip.label, x$feature_names())
      # convert tree into matrix showing which species in which
      # branches and store the result
      self$set_data("branch_matrix", branch_matrix(tr)[pos, ])
      invisible(TRUE)
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
                              inherits(y, "ConservationProblem"))
      invisible(rcpp_apply_max_phylo_objective(x$ptr,
        y$feature_targets(), y$planning_unit_costs(),
        self$parameters$get("budget")[[1]],
        self$get_data("branch_matrix"),
        self$get_data("tree")$edge.length
      ))
    }))
}
