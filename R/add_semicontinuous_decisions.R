#' @include internal.R Parameters-proto.R Decision-proto.R
NULL

#' Add semi-continuous decisions
#'
#' Add a semi-continuous decision to a conservation planning \code{\link{problem}}.
#' This is a relaxed decision where a part of a planning unit can be prioritized, 
#' as opposed to the entire planning unit, which is the default function 
#' (see \code{\link{add_binary_decisions}}). 
#' This decision is similar to the \code{\link{add_proportion_decision}} function 
#' except that it has an upper bound parameter. By default, the decision can 
#' range from prioritizing none (0 \%) to all (100 \%) of a planning unit. 
#' However, a upperbound can be specified to ensure that at most only a fraction (eg. 80 \%) 
#' of a planning unit can be preserved. This type of decision may be useful 
#' when it is not practical to conserve the entire planning unit.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#' 
#' @param upper_limit \code{numeric} value specifying the maximum proportion
#'   of a planning unit that can be reserved (eg. set to 0.8 for 80 \%).
#'
#' @details
#' Conservation planning problems involve making decisions on planning units.
#' These decisions are then associated with actions (eg. turning a planning
#' unit into a protected area). If no decision is explicitly added to a problem, 
#' then the binary decision class will be used by default.Only a single decision 
#' should be added to a
#' \code{ConservationProblem} object. \strong{If multiple decisions are added
#' to a problem object, then the last one to be added will be used.}
#'
#' @return \code{\link{Decision-class}} object.
#'
#' @seealso \code{\link{decisions}}, \code{\link{add_binary_decisions}}, 
#' \code{\link{add_proportion_decisions}}, \code{\link{constraints}}, 
#' \code{\link{problem}}, \code{\link{targets}}, \code{\link{objectives}}
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
#' @name add_semicontinuous_decisions
NULL

#' @rdname add_semicontinuous_decisions
#' @export
add_semicontinuous_decisions <- function(x, upper_limit) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(upper_limit))), assertthat::is.scalar(upper_limit),
                          isTRUE(upper_limit <= 1), isTRUE(upper_limit >= 0))
  # add decision to problem
  x$add_decisions(pproto("SemiContinuousDecision", Decision,
                         name = "Semicontinuous decision",
                         parameters = parameters(proportion_parameter("upper limit", upper_limit)),
                         apply = function(self, x) {
                           assertthat::assert_that(inherits(x, "OptimizationProblem"))
                           invisible(rcpp_apply_semicontinuous_decisions(x$ptr,
                                                                         self$parameters$get("upper limit")))
                         }))
}
