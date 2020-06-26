#' @include internal.R Parameters-proto.R Decision-proto.R
NULL

#' Add semi-continuous decisions
#'
#' Add a semi-continuous decision to a conservation planning
#' \code{\link{problem}}. This is a relaxed decision where a part of a planning
#' unit can be prioritized, as opposed to the entire planning unit, which is
#' the default function (see \code{\link{add_binary_decisions}}).
#' This decision is similar to the \code{\link{add_proportion_decisions}}
#' function except that it has an upper bound parameter. By default, the
#' decision can range from prioritizing none (0%) to all (100%) of a
#' planning unit. However, an upper bound can be specified to ensure that at
#' most only a fraction (e.g. 80%) of a planning unit can be preserved. This
#' type of decision may be useful when it is not practical to conserve entire
#' planning units.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param upper_limit `numeric` value specifying the maximum proportion
#'   of a planning unit that can be reserved (e.g. set to 0.8 for 80%).
#'
#' @inherit add_binary_decisions details return seealso
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_raster, sim_features, sim_pu_zones_stack, sim_features_zones)
#'
#' # create minimal problem with semi-continuous decisions
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'      add_min_set_objective() %>%
#'      add_relative_targets(0.1) %>%
#'      add_semicontinuous_decisions(0.5)
#' \donttest{
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solutions
#' plot(s1, main = "solution")
#' }
#' # build multi-zone conservation problem with semi-continuous decisions
#' p2 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_semicontinuous_decisions(0.5)
#' \donttest{
#' # solve the problem
#' s2 <- solve(p2)
#'
#' # print solution
#' print(s2)
#'
#' # plot solution
#' # panels show the proportion of each planning unit allocated to each zone
#' plot(s2, axes = FALSE, box = FALSE)
#' }
#' @name add_semicontinuous_decisions
NULL

#' @rdname add_semicontinuous_decisions
#' @export
add_semicontinuous_decisions <- function(x, upper_limit) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(upper_limit))),
                          assertthat::is.scalar(upper_limit),
                          isTRUE(upper_limit <= 1), isTRUE(upper_limit >= 0))
  # add decision to problem
  x$add_decisions(
    pproto("SemiContinuousDecision",
           Decision,
           name = "Semicontinuous decision",
           parameters = parameters(
             proportion_parameter("upper limit", upper_limit)),
           apply = function(self, x) {
             assertthat::assert_that(inherits(x, "OptimizationProblem"))
             invisible(rcpp_apply_decisions(x$ptr, "C", 0,
                                            self$parameters$get("upper limit")))
           }))
}
