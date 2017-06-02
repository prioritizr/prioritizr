#' @include internal.R pproto.R Objective-proto.R
NULL

#' Problem objective
#'
#' @description
#' A problem objective is used to specify the overall goal of the problem.
#' All conservation planning problems involve minimizing or maximizing some
#' kind of objective. For instance, the planner may require a solution that
#' conserves enough habitat for each species while minimizing the overall cost
#' of the reserve network. Alternatively, the planner may require a solution
#' that maximizes the number of conserved species while ensuring that the cost
#' of the reserve network does not exceed the budget.
#'
#' While some users may feel that explicitly defining an objective for a
#' conservation problem adds some element of arbitrariness or subjectivity to
#' the decision making process, we remind them that "canned" decision support
#' tools (such as Marxan or Zonation) also have objectives. The key difference
#' here is that instead of choosing between different software programs here
#' the user is explicitly choosing their objective within the single
#' environment.
#'
#' \strong{Please note that failing to specify an objective will return a default
#' error message when solving.}
#'
#' @details
#' The following objectives can be added to a conservation planning \code{\link{problem}}:
#'
#' \describe{
#'
#'   \item{\code{\link{add_min_set_objective}}}{Set an objective to find the
#'   solution that fulfills all the targets and constraints for the smallest
#'   cost. This objective is similar to that used in Marxan.}
#'
#'   \item{\code{\link{add_max_cover_objective}}}{Set an objective to find the
#'   solution that secures as much of each feature as possible without
#'   exceeding the budget.}
#'
#'   \item{\code{\link{add_max_features_objective}}}{Set an objective to find to find
#'   the solution that fulfills as many targets as possible while ensuring that
#'   the cost of the solution does not exceed budget and that all constraints
#'   are met. This objective was inspired by the conservation problem defined
#'   in Cabeza and Moilanen (2001).}
#'
#'   \item{\code{\link{add_max_phylo_objective}}}{Set an objective to find the
#'   solution that fulfills as much of a representative sample a phylogenetic
#'   tree as possible given a budget. This objective is similar
#'   to \code{add_max_features_objective} except that emphasis is placed
#'   on phylogenetic representation rather than target representation. This
#'   objective requires the "ape" R package to be installed.}
#'
#'  }
#'
#' @seealso \code{\link{constraints}}, \code{\link{problem}},
#'   \code{\link{targets}}.
#'
#'
#' @name objectives
NULL
