#' @include internal.R Collection-proto.R
NULL

#' Solution portfolio methods
#'
#' Conservation planners often desire a portfolio of solutions
#' to present to decision makers. This is because conservation planners
#' often do not have access to "perfect" information, such as cost data that
#' accurately reflects stakeholder preferences, and so having multiple
#' near-optimal solutions can be a useful.
#'
#' @details All methods for generating portfolios will return solutions that
#'   are within the specified optimality gap. Below are the portfolio methods
#'   that can be added to a \code{ConservationProblem} object.
#'
#'   \describe{
#'   \item{\code{add_default_portfolio}}{Generate a single solution.}
#'
#'   \item{\code{\link{add_cuts_portfolio}}}{
#'     Generate a portfolio of solutions using Bender's cuts.}
#'
#'   \item{\code{\link{add_shuffle_portfolio}}}{
#'     Generate a portfolio of solutions by randomly reordering the data
#'     prior to attempting to solve the problem.}
#'
#'   }
#'
#' @name portfolios
#'
#' @seealso \code{\link{constraints}},  \code{\link{decisions}},
#'  \code{\link{objectives}} \code{\link{penalties}}, \code{\link{problem}},
#'  \code{\link{solvers}}, \code{\link{targets}}.
#'
NULL
