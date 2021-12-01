#' @include internal.R pproto.R ConservationProblem-proto.R  MiscParameter-proto.R
NULL

#' Add feature weights
#'
#' Conservation planning problems that aim to maximize the
#' representation of features given a budget often will not able to conserve
#' all of the features unless the budget is very high. In such budget-limited
#' problems, it may be desirable to prefer the representation of some features
#' over other features. This information can be incorporated into the problem
#' using weights. Weights can be applied to a problem to favor the
#' representation of some features over other features when making decisions
#' about how the budget should be allocated.
#'
#' @param x [problem()] (i.e., [`ConservationProblem-class`]) object.
#'
#' @param weights `numeric` or `matrix` of weights.
#'   See the Weights format section for more information.
#'
#' @details
#' Weights can only be applied to problems that have an objective
#' that is budget limited (e.g., [add_max_cover_objective()]).
#' They can be applied to problems that aim to maximize phylogenetic
#' representation ([add_max_phylo_div_objective()]) to favor the
#' representation of specific features over the representation of
#' some phylogenetic branches. Weights cannot be negative values
#' and must have values that are equal to or larger than zero.
#' **Note that planning unit costs are scaled to 0.01 to identify
#' the cheapest solution among multiple optimal solutions. This means
#' that the optimization process will favor cheaper solutions over solutions
#' that meet feature targets (or occurrences) when feature weights are
#' lower than 0.01.**
#'
#' @section Weights format:
#'
#' The argument to `weights` can be specified using the following formats.
#'
#' \describe{
#'
#' \item{`weights` as a `numeric` vector}{containing weights for each feature.
#'   Note that this format cannot be used to specify weights for problems with
#'   multiple zones.}
#'
#' \item{`weights` as a `matrix` object}{containing weights
#'   for each feature in each zone.
#'   Here, each row corresponds to a different feature in argument to
#'   `x`, each column corresponds to a different zone in argument to
#'   `x`, and each cell contains the weight value for a given feature
#'   that the solution can to secure in a given zone. Note that
#'   if the problem contains targets created using
#'   [add_manual_targets()] then a `matrix` should be
#'   supplied containing a single column that indicates that weight for
#'   fulfilling each target.}
#'
#'   }
#'
#' @return Object (i.e., [`ConservationProblem-class`]) with the weights
#'  added to it.
#'
#' @seealso
#' See [penalties] for an overview of all functions for adding penalties.
#'
#' @family penalties
#'
#' @examples
#' # load ape package
#' require(ape)
#'
#' # load data
#' data(sim_pu_raster, sim_features, sim_phylogeny, sim_pu_zones_stack,
#'      sim_features_zones)
#'
#' # create minimal problem that aims to maximize the number of features
#' # adequately conserved given a total budget of 3800. Here, each feature
#' # needs 20% of its habitat for it to be considered adequately conserved
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_max_features_objective(budget = 3800) %>%
#'       add_relative_targets(0.2) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#'
#' # create weights that assign higher importance to features with less
#' # suitable habitat in the study area
#' (w2 <- exp((1 / cellStats(sim_features, "sum")) * 200))
#'
#' # create problem using rarity weights
#' p2 <- p1 %>% add_feature_weights(w2)
#'
#' # create manually specified weights that assign higher importance to
#' # certain features. These weights could be based on a pre-calculated index
#' # (e.g., an index measuring extinction risk where higher values
#' # denote higher extinction risk)
#' w3 <- c(0, 0, 0, 100, 200)
#' p3 <- p1 %>% add_feature_weights(w3)
#' \dontrun{
#' # solve problems
#' s1 <- stack(solve(p1), solve(p2), solve(p3))
#'
#' # plot solutions
#' plot(s1, main = c("equal weights", "rarity weights", "manual weights"),
#'      axes = FALSE, box = FALSE)
#' }
#'
#' # plot the example phylogeny
#' \dontrun{
#' par(mfrow = c(1, 1))
#' plot(sim_phylogeny, main = "simulated phylogeny")
#' }
#' # create problem with a maximum phylogenetic diversity objective,
#' # where each feature needs 10% of its distribution to be secured for
#' # it to be adequately conserved and a total budget of 1900
#' p4 <- problem(sim_pu_raster, sim_features) %>%
#'       add_max_phylo_div_objective(1900, sim_phylogeny) %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#' \dontrun{
#' # solve problem
#' s4 <- solve(p4)
#'
#' # plot solution
#' plot(s4, main = "solution", axes = FALSE, box = FALSE)
#'
#' # find out which features have their targets met
#' r4 <- eval_target_coverage_summary(p4, s4)
#' print(r4, width = Inf)
#'
#' # plot the example phylogeny and color the represented features in red
#' plot(sim_phylogeny, main = "represented features",
#'      tip.color = replace(rep("black", nlayers(sim_features)),
#'                          which(r4$met), "red"))
#' }
#' # we can see here that the third feature ("layer.3", i.e.,
#' # sim_features[[3]]) is not represented in the solution. Let us pretend
#' # that it is absolutely critical this feature is adequately conserved
#' # in the solution. For example, this feature could represent a species
#' # that plays important role in the ecosystem, or a species that is
#' # important commercial activities (e.g., eco-tourism). So, to generate
#' # a solution that conserves the third feature whilst also aiming to
#' # maximize phylogenetic diversity, we will create a set of weights that
#' # assign a particularly high weighting to the third feature
#' w5 <- c(0, 0, 1000, 0, 0)
#'
#' # we can see that this weighting (i.e., w5[3]) has a much higher value than
#' # the branch lengths in the phylogeny so solutions that represent this
#' # feature be much closer to optimality
#' print(sim_phylogeny$edge.length)
#' \dontrun{
#' # create problem with high weighting for the third feature and solve it
#' s5 <- p4 %>% add_feature_weights(w5) %>% solve()
#'
#' # plot solution
#' plot(s5, main = "solution", axes = FALSE, box = FALSE)
#'
#' # find which features have their targets met
#' r5 <- eval_target_coverage_summary(p4, s5)
#' print(r5, width = Inf)
#'
#' # plot the example phylogeny and color the represented features in red
#' # here we can see that this solution only adequately conserves the
#' # third feature. This means that, given the budget, we are faced with the
#' # trade-off of conserving either the third feature, or a phylogenetically
#' # diverse set of three different features.
#' plot(sim_phylogeny, main = "represented features",
#'      tip.color = replace(rep("black", nlayers(sim_features)),
#'                          which(r5$met), "red"))
#' }
#' # create multi-zone problem with maximum features objective,
#' # with 10% representation targets for each feature, and set
#' # a budget such that the total maximum expenditure in all zones
#' # cannot exceed 3000
#' p6 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_max_features_objective(3000) %>%
#'       add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#'
#' # create weights that assign equal weighting for the representation
#' # of each feature in each zone except that it does not matter if
#' # feature 1 is represented in zone 1 and it really important
#' # that feature 3 is really in zone 1
#' w7 <- matrix(1, ncol = 3, nrow = 5)
#' w7[1, 1] <- 0
#' w7[3, 1] <- 100
#'
#' # create problem with weights
#' p7 <- p6 %>% add_feature_weights(w7)
#' \dontrun{
#' # solve problems
#' s6 <- solve(p6)
#' s7 <- solve(p7)
#'
#' # plot solutions
#' plot(stack(category_layer(s6), category_layer(s7)),
#'      main = c("equal weights", "manual weights"), axes = FALSE, box = FALSE)
#' }
#' # create minimal problem to show the correct method for setting
#' # weights for problems with manual targets
#' p8 <- problem(sim_pu_raster, sim_features) %>%
#'       add_max_features_objective(budget = 3000) %>%
#'       add_manual_targets(data.frame(feature = c("layer.1", "layer.4"),
#'                                     type = "relative",
#'                                     target = 0.1)) %>%
#'       add_feature_weights(matrix(c(1, 200), ncol = 1)) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#' \dontrun{
#' # solve problem
#' s8 <- solve(p8)
#'
#' # plot solution
#' plot(s8, main = "solution", axes = FALSE, box = FALSE)
#' }
#' @name add_feature_weights
#'
#' @exportMethod add_feature_weights
#'
#' @aliases add_feature_weights,ConservationProblem,numeric-method add_feature_weights,ConservationProblem,matrix-method
NULL

#' @export
methods::setGeneric("add_feature_weights",
                    signature = methods::signature("x", "weights"),
                    function(x, weights)
                      standardGeneric("add_feature_weights"))

#' @name add_feature_weights
#' @usage \S4method{add_feature_weights}{ConservationProblem,numeric}(x, weights)
#' @rdname add_feature_weights
methods::setMethod("add_feature_weights",
  methods::signature("ConservationProblem", "numeric"),
  function(x, weights) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "ConservationProblem"),
      is.numeric(weights), isTRUE(all(is.finite(weights))),
      all(weights >= 0), isTRUE(length(weights) >= 0),
      length(weights) == x$number_of_features())
    assertthat::assert_that(x$number_of_zones() == 1,
      msg = paste("weights must be supplied as a matrix for problems",
                  "containing multiple zones"))
    # add weights
    add_feature_weights(x, matrix(weights, ncol = 1))
})

#' @name add_feature_weights
#' @usage \S4method{add_feature_weights}{ConservationProblem,matrix}(x, weights)
#' @rdname add_feature_weights
methods::setMethod("add_feature_weights",
  methods::signature("ConservationProblem", "matrix"),
  function(x, weights) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "ConservationProblem"),
      is.matrix(weights), isTRUE(all(is.finite(weights))),
      all(weights >= 0), ncol(weights) > 0, nrow(weights) > 0)
    if (ncol(weights) > 1)
      assertthat::assert_that(ncol(weights) == x$number_of_zones(),
                              nrow(weights) == x$number_of_features())
    # make parameters
    if (ncol(weights) == 1) {
      p <- numeric_parameter_array("weights", weights[, 1],
                                   as.character(seq_len(nrow(weights))),
                                   lower_limit = 0)
    } else {
      weights <- as.data.frame(weights)
      colnames(weights) <- x$zone_names()
      rownames(weights) <- x$feature_names()
      nc <- ncol(weights)
      nr <- nrow(weights)
      vfun <- function(x)
        assertthat::see_if(ncol(x) == nc, nrow(x) == nr,
                           all(is.finite(as.matrix(x))),
                           all(as.matrix(x) >= 0))
      p <- misc_parameter("weights", weights, vfun)
    }
    # add weights to problem
    x$add_penalty(pproto(
      "FeatureWeights",
      Penalty,
      name = "Feature weights",
      parameters = parameters(p),
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, "OptimizationProblem"),
          inherits(y, "ConservationProblem"))
          weights <- c(as.matrix(self$parameters$get("weights")))
          if (!is.Waiver(y$targets))
            assertthat::assert_that(length(weights) == nrow(y$targets$output()),
              msg = paste0("the number of feature weights must correspond to ",
                           "the number of targets in the problem"))
        invisible(rcpp_apply_feature_weights(x$ptr, weights))
      }))
})
