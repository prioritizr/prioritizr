# @include internal.R

#' Marxan conservation problem
#'
#' Create a Marxan conservation problem. Although users are encouraged to 
#' build and tailor conservation planning problems to suit their own needs, 
#' sometimes it is easier to just use a more familiar "canned" approach. To
#' address this need, this function provides a convenient wrapper for generating
#' and solving Marxan-style conservation problems. If users already have
#' their conservation planning data in the Marxan input format, this function
#' can be used to read such files and solve the Marxan problems using 
#' exact algorithm solvers.
#'
#' @param x \code{character} file path for Marxan input file (typically
#'   called \emph{input.dat}) or a \code{\link[sp]{Spatial-class}} or 
#'   \code{\link[raster]{Raster-class}} object containing planning unit data.
#'
#' @param features \code{\link[raster]{Raster-class}} object containing
#'   data on the distribution of features across the study area.
#'
#' @param targets \code{numeric} amount of each feature to be represented
#'   in the solution.
#'
#' @param targets_type \code{character} name indicating if the \code{targets}
#'   are expressed as "relative" (eg. 0.2 meaning that 20 \% of a feature 
#'   needs to be conserved), or "absolute" (eg. 100 meaning that 100 units of a 
#'   feature need to be conserved) amounts. 
#'
#' @inheritParams add_boundary_constraint
#'
#' @inheritParams add_locked_in_constraint
#'
#' @inheritParams add_locked_out_constraint
#'
#' @param ... not used
#'
#' @return \code{link{ConservationProblem}} or \code{logical} \code{vector}
#'   indicating if which planning units were prioritized.
#'
#' @examples
#' # create Marxan problem using spatial data
#' data(sim_pu_raster, sim_features)
#' p <- marxan_problem(sim_pu_raster, features=sim_features, targets=0.2, 
#'                     targets_type='relative', penalty=1, edge_factor=0.5)
#'
#' # solve problem
#' s <- solve(p)
#'
#' # show solution
#' plot(s)
#'
#' \dontrun{
#' # create and solve Marxan problem using Marxan input files
#' file <- system.file('extdata/input.dat', package='prioritizr')
#' s <- marxan_problem(file)
#'
#' # count number of selected planning units
#' print(sum(s))
#' }
#'
#' @name marxan_problem
#'
#' @export
marxan_problem <- function(x, ...) UseMethod('marxan_problem')

#' @rdname marxan_problem
#' @export
marxan_problem.default <- function(x, features, targets, 
                                  targets_type=c('relative', 'absolute'), 
                                  locked_in = waiver(), locked_out = waiver(),
                                  penalty=0, edge_factor=0.5, ...) {
  # create problem
  p <- problem(x, features) %>%
    add_minimum_set_objective() %>%
    add_boundary_constraint(penalty, edge_factor)
  if (targets_type=='relative')
    p <- p %>% add_relative_targets(targets)
  if (targets_type=='absolute')
    p <- p %>% add_absolute_targets(targets)
  if (!is.Waiver(locked_in))
    p <- p %>% add_locked_in_constraint(locked_in)
  if (!is.Waiver(locked_out))
    p <- p %>% add_locked_out_constraint(locked_out)
  # return problem
  return(p)
}

#' @rdname marxan_problem
#' @export
marxan_problem.character <- function(x, ...) {
  stop('TODO: implement this')
}

