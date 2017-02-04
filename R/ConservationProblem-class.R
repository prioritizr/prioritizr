#' @include internal.R Target-class.R Objective-class.R Constraints-class.R Decision-class.R
NULL

#' @export
methods::setOldClass('ConservationProblem')

#' Conservation problem class
#'
#' This class is used to represent conservation planning problems. A 
#' conservation planning problem has spatially explicit planning units.
#' A prioritisation involves making a decision on each planning unit (eg. is 
#' the planning unit going to be turned into a protected area?). Each 
#' planning unit is associated with a cost that represents the cost incurred
#' by applying the decision to the planning unit. The problem also has a set 
#' of representation targets for each feature. Further, it also has 
#' constraints used to ensure that the solution meets additional
#' objectives (eg. certain areas are locked into the solution). Finally,
#' a conservation planning problem--unlike an optimization problem--also
#' requires a method to solve the problem. \strong{This class represents a
#' planning problem, to actually build and then solve a planning problem,
#' use the \code{\link{problem}} function. Only experts should use this
#' class directly.}
#'
#' @section Fields:
#' \itemize{
#' \item{$data}{\code{list} object containing data.}
#' \item{$objective}{\code{\link{Objective}} object used to represent how
#'   the targets relate to the solution.}
#' \item{$decision}{\code{\link{Decision}} object used to represent the type
#'   of decision made on planning units.}
#' \item{$targets}{\code{\link{Target}} object used to represent
#'   representation targets for features.}
#' \item{$constraints}{\code{\link{Constraints}} object used to represent
#'   additional \link{Constraint}s that the problem is subject to.}
#' \item{$solver}{\code{\link{Solver}} object used to solve the problem.}
#' }
#'
#' @section Usage:
#' \preformatted{x <- ConservationProblem$new(cost, features)}
#'
#' \code{x$print()}
#' \code{x$show()}
#' \code{x$compile()}
#' \code{x$solve()}
#'
#' \code{x$get_constraint_parameter(id)} 
#' \code{x$set_constraint_parameter(id, value)} 
#' \code{x$render_constraint_parameter(id)} 
#' \code{x$render_all_constraint_parameters()}
#'
#' \code{x$get_objective_parameter(id)} 
#' \code{x$set_objective_parameter(id, value)} 
#' \code{x$render_objective_parameter(id)} 
#' \code{x$render_all_objective_parameters()}
#'
#' \code{x$get_solver_parameter(id)} 
#' \code{x$set_solver_parameter(id, value)} 
#' \code{x$render_solver_parameter(id)} 
#' \code{x$render_all_solver_parameters()}
#'
#' @section Arguments:
#' \describe{
#' \item{cost}{\code{\link[raster]{RasterLayer-class}}, 
#'   \code{\link{SpatialPolygonsDataFrame}}, or 
#'   \code{\link{SpatialLinesDataFrame}} object showing spatial representation
#'   of the planning units and their cost.}
#' \item{features}{\code{\link[raster]{RasterStack-class}} object showing
#'   distribution of features.}
#' \item{id}{\code{\link{id}} object that refers to a specific parameter.}
#' \item{value}{object that the parameter value should become.}
#' }
#'
#' @section Details:
#' \describe{
#' \item{print}{print the object.}
#' \item{show}{show the object.}
#' \item{solve}{compile the object and into an 
#'   \code{\link{OptimizationProblem}} and then solve it.}
#'
#' \item{get_constraint_parameter}{get the value of a parameter (specified by
#'   argument \code{id}) used in one of the constraints in the object.}
#' \item{set_constraint_parameter}{set the value of a parameter (specified by 
#'   argument \code{id}) used in one of the constraints in the object to
#'  \code{value}.}
#' \item{render_constraint_parameter}{generate a \emph{shiny} widget to modify the
#'   the value of a parameter (specified by argument \code{id}).}
#' \item{render_all_constraint_parameters}{generate a \emph{shiny} \code{div}
#'   containing all the parameters' widgets.}
#'
#' \item{get_objective_parameter}{get the value of a parameter (specified by
#'   argument \code{id}) used in one of the objectives in the object.}
#' \item{set_objective_parameter}{set the value of a parameter (specified by 
#'   argument \code{id}) used in one of the objectives in the object to
#'  \code{value}.}
#' \item{render_objective_parameter}{generate a \emph{shiny} widget to modify the
#'   the value of a parameter (specified by argument \code{id}).}
#' \item{render_all_objective_parameters}{generate a \emph{shiny} \code{div}
#'   containing all the parameters' widgets.}
#'
#' \item{get_solver_parameter}{get the value of a parameter (specified by
#'   argument \code{id}) used in one of the solvers in the object.}
#' \item{set_solver_parameter}{set the value of a parameter (specified by 
#'   argument \code{id}) used in one of the solvers in the object to
#'  \code{value}.}
#' \item{render_solver_parameter}{generate a \emph{shiny} widget to modify the
#'   the value of a parameter (specified by argument \code{id}).}
#' \item{render_all_solver_parameters}{generate a \emph{shiny} \code{div}
#'   containing all the parameters' widgets.}
#' }
#' @name ConservationProblem
NULL

#' @export
ConservationProblem <- R6::R6Class('ConservationProblem',
  public = list(
    data = NULL,
    objective = NULL,
    decision = NULL,
    targets = NULL,
    constraints = NULL,
    solver  = NULL,
    initialize = function(cost, features) {
      assertthat::assert_that(
        inherits(cost, 'RasterLayer') || inherits(features, 'Spatial'),
        inherits(features, 'Raster'))
      self$data <- list(cost = cost, features=features)
      self$objective <- waiver()
      self$decision <- waiver()
      self$targets <- waiver()
      self$constraints <- Constraints$new()
      self$solver <- waiver()
    },
    print = function() {
      message(paste0('ConservationProblem object',
'\n  objective:   ',repr(self$objective),
'\n  decision:   ',repr(self$decision),
'\n  cost:        ',round(raster::cellStats(self$cost, 'min'), 5), ', ',
                    round(raster::cellStats(self$cost, 'min'), 5),
                    '(min, max)',
'\n  features:    ',raster::nlayers(self$features),
'\n  targets:   ',repr(self$targets),
'\n  constraints: ',repr(self$constraints),
'\n  solver: ',repr(self$solver)))
    },
    show = function() {
      self$show()
    },
    get_constraint_parameter = function(id) {
      self$constraints$get_parameter(id)
    },
    set_constraint_parameter = function(id, value) {
      self$constraints$set_parameter(id, value)
    },
    render_constraint_parameter = function(id) {
      self$constraints$render_parameter(id)
    },
    render_all_constraint_parameters = function() {
      self$constraints$render_all_parameter()
    },
    get_objective_parameter = function(id) {
      self$objective$get_parameter(id)
    },
    set_objective_parameter = function(id, value) {
      self$objective$set_parameter(id, value)
    },
    render_objective_parameter = function(id) {
      self$objective$render_parameter(id)
    },
    render_all_objective_parameters = function() {
      self$objective$render_all_parameter()
    },
    get_solver_parameter = function(id) {
      self$solver$get_parameter(id)
    },
    set_solver_parameter = function(id, value) {
      self$solver$set_parameter(id, value)
    },
    render_solver_parameter = function(id) {
      self$solver$render_parameter(id)
    },
    render_all_solver_parameters = function() {
      self$solver$render_all_parameter()
    }
  )
)
