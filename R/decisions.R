#' @include internal.R Parameters-proto.R Decision-proto.R
NULL

#' Specify the type of decisions
#'
#' Conservation planning problems involve making decisions on planning units.
#' These decisions are then associated with actions (eg. turning a planning
#' unit into a protected area). If no decision is explicitly added to a problem,
#' then the binary decision class will be used by default.
#'
#' @details Only a single decision should be added to a
#' \code{ConservationProblem} object. \strong{If multiple decisions are added
#' to a problem object, then the last one to be added will be used.} Below is a
#' list of decisions that can be added to a conservation planning problem.
#'
#' \describe{
#'
#'   \item{\code{\link{add_binary_decisions}}}{Add a binary decision to a conservation
#'   planning \code{problem}. This is the classic decision of either
#'   prioritizing or not prioritizing a planning unit. Typically, this
#'   decision has the assumed action of buying the planning unit to include
#'   in a protected area network. If no decision is added to a problem object
#'   then this decision class will be used by default.}
#'
#'   \item{\code{\link{add_proportion_decisions}}}{Add a proportion decision to a
#'   conservation planning \code{problem}. This is a relaxed decision
#'   where a part of a planning unit can be prioritized, as opposed to the
#'   default of the entire planning unit. Typically, this this decision
#'   has the assumed action of buying a fraction of a planning unit to
#'   include in a protected area network.}
#'
#'   \item{\code{\link{add_semicontinuous_decisions}}}{Add a semi-continuous
#'   decision to a conservation planning \code{problem}. This decision is similar to
#'     \code{add_proportion_decision} except that it has an upper bound
#'     parameter. By default, the decision can range from prioritizing
#'     none (0 \%) to all (100 \%) of a planning unit. However, a upper
#'     bound can be specified to ensure that at most only a fraction
#'     (eg. 80 \%) of a planning unit can be preserved. This type of
#'     decision may be useful when it is not practical to conserve the
#'     entire planning unit.}
#'
#'  }
#'
#' @seealso \code{\link{constraints}}, \code{\link{problem}},
#' \code{\link{targets}}, \code{\link{objectives}}
#'
#' @name decisions
NULL
