#' @include internal.R Parameters-proto.R Decision-proto.R
NULL

#' Add Proportion Decisions
#'
#' Add a proportion decision to a conservation planning \code{\link{problem}}.
#' This is a relaxed decision where a part of a planning unit can be
#' prioritized,  as opposed to the entire planning unit, which is the default
#' function (see \code{\link{add_binary_decisions}}).
#' Typically, this this decision has the assumed action of buying a fraction
#' of a planning unit to include in a protected area network.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @details
#' Conservation planning problems involve making decisions on planning units.
#' These decisions are then associated with actions (e.g. turning a planning
#' unit into a protected area). If no decision is explicitly added to a problem,
#' then the binary decision class will be used by default.Only a single decision
#' should be added to a
#' \code{ConservationProblem} object. \strong{If multiple decisions are added
#' to a problem object, then the last one to be added will be used.}
#'
#' @return \code{\link{Decision-class}} object.
#'
#' @seealso \code{\link{decisions}}.
#'
#' @examples
#' # create basic problem and using the default decision (binary)
#' p <- problem(sim_pu_raster, sim_features) %>%
#'        add_min_set_objective() %>%
#'        add_relative_targets(0.1)
#'
#' # manually specify a binary decision type
#' p2 <- p %>% add_binary_decisions()
#'
#' # specify a proportion decision type
#' p3 <- p %>% add_proportion_decisions()
#'
#' # specify a semicontinuous decision type
#' p4 <- p %>% add_semicontinuous_decisions(upper_limit=0.5)
#'
#' \donttest{
#' # solve problem
#' s <- stack(solve(p), solve(p2), solve(p3), solve(p4))
#'
#' # plot solutions
#' plot(s, main = c("default (binary)", "binary", "proportion",
#'                  "semicontinuous (upper=0.5)"))
#' }
#'
#' @name add_proportion_decisions
NULL

#' @rdname add_proportion_decisions
#' @export
add_proportion_decisions <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ConservationProblem"))
  # add decision
  x$add_decisions(pproto("ProportionDecision", Decision,
                         name = "Proportion decision",
                         apply = function(self, x) {
                           assertthat::assert_that(inherits(x,
                                                   "OptimizationProblem"))
                           invisible(rcpp_apply_proportion_decisions(x$ptr))
                         }
  ))
}
