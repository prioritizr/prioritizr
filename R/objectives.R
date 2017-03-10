#' @include internal.R pproto.R Objective-proto.R
NULL

#' Problem objective
#'
#' Conservation planning problems involve minimizing or maximizing an objective.
#' For instance, the planner may require a solution that conserves
#' enough habitat for each species while minimizing the overall cost of the
#' reserve network. Alternatively, the planner may require a solution
#' that maximizes the number of conserved species while ensuring that the cost
#' of the reserve network does not exceed the budget. The problem objective
#' specifies the overall goal of the problem.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param budget \code{numeric} value specifying the maximum expenditure of
#'   the prioritization.
#'
#' @param tree \code{\link[ape]{phylo}} object specifying a phylogenetic tree
#    for the conservation features.
#'
#' @details
#' All conservation planning problems require an objective in order to
#' be solved. While some users may feel that explicitly defining an objective
#' for a conservation problem adds some element of arbitrariness or
#' subjectivity to the decision making process, we remind them that "canned"
#' decision support tools (such as Marxan or Zonation) also have objectives. The
#' key difference here is that instead of choosing between different software
#' programs here the user is explicitly choosing their objective within the
#' single environment. See below for a list of the objectives that can be
#' added to a conservation problem.
#'
#' \describe{
#'
#'   \item{\code{add_default_objective}}{This objective is used when no objective
#'     has been explicitly specified by the user. This will result in an error
#'     because there is no sensible default.}
#'
#'   \item{\code{add_min_set_objective}}{The objective here is to find the
#'     the solution that fulfills all the targets and constraints for
#'     the smallest cost. This objective is similar to that used in Marxan.}
#'
#'   \item{\code{add_max_cover_objective}}{The objective here is to
#'     find the solution that secures as much of each feature as possible
#'     whilst not exceeding the budget.}
#'
#'   \item{\code{add_max_features_objective}}{The objective here is
#'     to find the solution that fulfills as many targets as possible while
#'     ensuring that the cost of the solution does not exceed budget and that
#'     all constraints are met. This objective was inspired by the conservation
#'     problem defined in Cabeza et al. XXXX.}
#'
#'   \item{\code{add_max_phylo_objective}}{This objective is
#'     similar to \code{add_max_features_objective} except that emphasis is
#'     placed on preserving as much of a representative sample a phylogenetic
#'     tree as possible given a budget. This objective requires the
#'     "ape" R package to be installed.}
#'
#'  }
#'
#' @seealso \code{\link{constraints}}, \code{\link{problem}},
#'   \code{\link{targets}}.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features, sim_phylogeny)
#'
#' # create base problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'  add_relative_targets(0.1)
#'
#  # create problem with added minimum set objective
#' p1 <- p %>% add_min_set_objective()
#'
#' # create problem with added maximum coverage objective
#' # note that this objective does not use targets
#' p2 <- p %>% add_max_cover_objective(5000)
#'
#' # create problem with added maximum feature representation objective
#' p3 <- p %>% add_max_features_objective(5000)
#'
#' # create problem with added maximum phylogenetic representation objective
#' p4 <- p %>% add_max_phylo_objective(5000, sim_phylogeny)
#'
#' \donttest{
#' # solve problems
#' s <- stack(solve(p1), solve(p2), solve(p3), solve(p4))
#'
#' # plot solutions
#' plot(s, main=c("minimum set", "maximum coverage", "maximum representation",
#'                "phylogenetic representation"))
#' }
#'
#' @name objectives
NULL

#' @rdname objectives
#' @export
add_default_objective <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ConservationProblem"))
  # throw error because objectives must be explicitly defined
  stop("problem is missing an objective and this must be explicitly defined")
}

#' @rdname objectives
#' @export
add_min_set_objective <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ConservationProblem"))
  # add objective to problem
  x$add_objective(pproto(
    "MinimumSetObjective",
    Objective,
    name = "Minimum set objective",
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
        inherits(y, "ConservationProblem"))
      invisible(rcpp_apply_min_set_objective(x$ptr, y$feature_targets(),
        y$planning_unit_costs()))
    }))
}

#' @rdname objectives
#' @export
add_max_cover_objective <- function(x, budget) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
    isTRUE(all(is.finite(budget))), assertthat::is.scalar(budget),
    isTRUE(budget > 0.0))
  # make parameter
  p <- numeric_parameter("budget", budget, lower_limit = 0,
    upper_limit = sum(x$planning_unit_costs()))
  # add objective to problem
  x$add_objective(pproto(
    "MaximumCoverageObjective",
    Objective,
    name = "Maximum coverage objective",
    parameters = parameters(p),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
        inherits(y, "ConservationProblem"))
      invisible(rcpp_apply_max_cover_objective(x$ptr,
        unname(y$feature_abundances_in_planning_units()),
        y$planning_unit_costs(), self$parameters$get("budget")))
    }))
}

#' @rdname objectives
#' @export
add_max_features_objective <- function(x, budget) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
    isTRUE(all(is.finite(budget))), assertthat::is.scalar(budget),
    isTRUE(budget > 0.0))
  # make parameter
  p <- numeric_parameter("budget", budget, lower_limit = 0,
    upper_limit = sum(x$planning_unit_costs()))
  # add objective to problem
  x$add_objective(pproto(
    "MaximumRepresentationObjective",
    Objective,
    name = "Maximum representation objective",
    parameters = parameters(p),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
        inherits(y, "ConservationProblem"))
      invisible(rcpp_apply_max_features_objective(x$ptr,
        y$feature_targets(), y$planning_unit_costs(),
        self$parameters$get("budget")))
    }))
}

#' @rdname objectives
#' @export
add_max_phylo_objective <- function(x, budget, tree) {
  # check that dependencies are installed
  if (!requireNamespace("ape"))
    stop("the \"ape\" package needs to be installed to use phylogenetic data")
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
    isTRUE(all(is.finite(budget))), assertthat::is.scalar(budget),
    isTRUE(budget > 0.0), inherits(tree, "phylo"),
    length(tree$tip.label) == x$number_of_features(),
    setequal(tree$tip.label, x$feature_names()))
  # make parameter
  p <- numeric_parameter("budget", budget, lower_limit = 0,
    upper_limit = sum(x$planning_unit_costs()))
  # add objective to problem
  x$add_objective(pproto(
    "PhylogeneticRepresentationObjective",
    Objective,
    name = "Phylogenetic representation objective",
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
        self$parameters$get("budget"), self$get_data("branch_matrix"),
        self$get_data("tree")$edge.length
        ))
    }))
}
