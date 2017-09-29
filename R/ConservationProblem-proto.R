#' @include internal.R waiver.R pproto.R Collection-proto.R
NULL

#' @export
methods::setOldClass("ConservationProblem")

#' Conservation problem class
#'
#' This class is used to represent conservation planning problems. A
#' conservation planning problem has spatially explicit planning units.
#' A prioritization involves making a decision on each planning unit (eg. is
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
#' \describe{
#'
#' \item{$data}{\code{list} object containing data.}
#'
#' \item{$objective}{\code{\link{Objective-class}} object used to represent how
#'   the targets relate to the solution.}
#'
#' \item{$decisions}{\code{\link{Decision-class}} object used to represent the
#'   type of decision made on planning units.}
#'
#' \item{$targets}{\code{\link{Target-class}} object used to represent
#'   representation targets for features.}
#'
#' \item{$penalties}{\code{\link{Collection-class}} object used to represent
#'   additional \code{\link{penalties}} that the problem is subject to.}
#'
#' \item{$constraints}{\code{\link{Collection-class}} object used to represent
#'   additional \code{\link{constraints}} that the problem is subject to.}
#'
#' \item{$solver}{\code{\link{Solver-class}} object used to solve the problem.}
#'
#' }
#'
#' @section Usage:
#'
#' \code{x$print()}
#'
#' \code{x$show()}
#'
#' \code{x$repr()}
#'
#' \code{x$get_data(name)}
#'
#' \code{x$set_data(name, value)}
#'
#' \code{x$number_of_total_units()}
#'
#' \code{x$number_of_planning_units()}
#'
#' \code{x$planning_unit_costs()}
#'
#' \code{x$number_of_features()}
#'
#' \code{x$feature_names()}
#'
#' \code{x$feature_abundances_in_planning_units()}
#'
#' \code{x$feature_targets()}
#'
#' \code{x$add_objective(obj)}
#'
#' \code{x$add_decisions(dec)}
#'
#' \code{x$add_solver(sol)}
#'
#' \code{x$add_constraint(con)}
#'
#' \code{x$add_targets(targ)}
#'
#' \code{x$get_constraint_parameter(id)}
#'
#' \code{x$set_constraint_parameter(id, value)}
#'
#' \code{x$render_constraint_parameter(id)}
#'
#' \code{x$render_all_constraint_parameters()}
#'
#' \code{x$get_objective_parameter(id)}
#'
#' \code{x$set_objective_parameter(id, value)}
#'
#' \code{x$render_objective_parameter(id)}
#'
#' \code{x$render_all_objective_parameters()}
#'
#' \code{x$get_solver_parameter(id)}
#'
#' \code{x$set_solver_parameter(id, value)}
#'
#' \code{x$render_solver_parameter(id)}
#'
#' \code{x$render_all_solver_parameters()}
#'
#' @section Arguments:
#'
#' \describe{
#'
#' \item{name}{\code{character} name for object.}
#'
#' \item{value}{an object.}
#'
#' \item{obj}{\code{\link{Objective-class}} object.}
#'
#' \item{dec}{\code{\link{Decision-class}} object.}
#'
#' \item{con}{\code{\link{Constraint-class}} object.}
#'
#' \item{sol}{\code{\link{Solver-class}} object.}
#'
#' \item{targ}{\code{\link{Target-class}} object.}
#'
#' \item{cost}{\code{\link[raster]{RasterLayer-class}},
#'   \code{\link[sp]{SpatialPolygonsDataFrame-class}}, or
#'   \code{\link[sp]{SpatialLinesDataFrame-class}} object showing spatial
#'   representation of the planning units and their cost.}
#'
#' \item{features}{\code{\link[raster]{RasterStack-class}} object showing
#'   distribution of features.}
#'
#' \item{id}{\code{Id} object that refers to a specific parameter.}
#'
#' \item{value}{object that the parameter value should become.}
#'
#' }
#'
#' @section Details:
#' \describe{
#' \item{print}{print the object.}
#'
#' \item{show}{show the object.}
#'
#' \item{repr}{return \code{character} representation of the object.}
#'
#' \item{get_data}{return an object stored in the \code{data} field with
#'   the corresponding \code{name}. If the object is not present in the
#'   \code{data} field, a \code{waiver} object is returned.}
#'
#' \item{set_data}{store an object stored in the \code{data} field with
#'   the corresponding name. If an object with that name already
#'   exists then the object is overwritten.}
#'
#' \item{number_of_planning_units}{\code{integer} number of planning units.}
#'
#' \item{number_of_total_units}{\code{integer} number of units in the cost
#'   data including units that have NA values for cost.}
#'
#' \item{planning_unit_costs}{\code{numeric} costs of each planning unit.}
#'
#' \item{number_of_features}{\code{integer} number of features.}
#'
#' \item{feature_names}{\code{character} names of features in problem.}
#'
#' \item{feature_abundances_in_planning_units}{\code{numeric} get total
#'   abundance of each feature in all the planning units.}
#'
#' \item{feature_targets}{\code{numeric} targets for each feature.}
#'
#' \item{add_objective}{return a new  \code{\link{ConservationProblem-class}}
#'   with the objective added to it.}
#'
#' \item{add_decisions}{return a new \code{\link{ConservationProblem-class}}
#'   object with the decision added to it.}
#'
#' \item{add_solver}{return a new \code{\link{ConservationProblem-class}} object
#'   with the solver added to it.}
#'
#' \item{add_constraint}{return a new \code{\link{ConservationProblem-class}}
#'   object with the constraint added to it.}
#'
#' \item{add_targets}{return a copy with the targets to the problem.}
#'
#' \item{get_constraint_parameter}{get the value of a parameter (specified by
#'   argument \code{id}) used in one of the constraints in the object.}
#'
#' \item{set_constraint_parameter}{set the value of a parameter (specified by
#'   argument \code{id}) used in one of the constraints in the object to
#'   \code{value}.}
#'
#' \item{render_constraint_parameter}{generate a \emph{shiny} widget to modify
#'  the value of a parameter (specified by argument \code{id}).}
#'
#' \item{render_all_constraint_parameters}{generate a \emph{shiny} \code{div}
#'   containing all the parameters" widgets.}
#'
#' \item{get_objective_parameter}{get the value of a parameter (specified by
#'   argument \code{id}) used in one of the objectives in the object.}
#'
#' \item{set_objective_parameter}{set the value of a parameter (specified by
#'   argument \code{id}) used in one of the objectives in the object to
#'  \code{value}.}
#'
#' \item{render_objective_parameter}{generate a \emph{shiny} widget to modify
#'   the value of a parameter (specified by argument \code{id}).}
#'
#' \item{render_all_objective_parameters}{generate a \emph{shiny} \code{div}
#'   containing all the parameters" widgets.}
#'
#' \item{get_solver_parameter}{get the value of a parameter (specified by
#'   argument \code{id}) used in one of the solvers in the object.}
#'
#' \item{set_solver_parameter}{set the value of a parameter (specified by
#'   argument \code{id}) used in one of the solvers in the object to
#'  \code{value}.}
#'
#' \item{render_solver_parameter}{generate a \emph{shiny} widget to modify the
#'   the value of a parameter (specified by argument \code{id}).}
#'
#' \item{render_all_solver_parameters}{generate a \emph{shiny} \code{div}
#'   containing all the parameters" widgets.}
#'
#' }
#' @name ConservationProblem-class
#'
#' @aliases ConservationProblem
NULL

#' @export
ConservationProblem <- pproto(
  "ConservationProblem",
  data = list(),
  objective = new_waiver(),
  decisions = new_waiver(),
  targets = new_waiver(),
  constraints = pproto(NULL, Collection),
  penalties = pproto(NULL, Collection),
  solver  = new_waiver(),
  print = function(self) {
    r <- vapply(list(self$objective, self$targets), function(x) {
      if (is.Waiver(x))
        return("none specified")
      return(x$repr())
    }, character(1))
    d <- vapply(list(self$solver, self$decisions), function(x) {
      if (is.Waiver(x))
        return("default")
      return(x$repr())
    }, character(1))
    cs <- round(range(self$planning_unit_costs()), 5)
    f <- self$feature_names()
    nf <- length(f)
    if (length(f) <= 4) {
      f <- f[seq_len(min(length(f), 4))]
    } else {
      f <- c(f[seq_len(min(length(f), 3))], "...")
    }
    f <- paste0(paste(f, collapse = ", "), " (", nf, " features)")
    message(paste0("Conservation Problem",
    "\n  planning units: ", class(self$data$cost)[1], " (",
      self$number_of_planning_units(), " units)",
    "\n  cost:           min: ", cs[1], ", max: ", cs[2],
    "\n  features:       ", f,
    "\n  objective:      ", r[1],
    "\n  targets:        ", r[2],
    "\n  decisions:      ", d[2],
    "\n  constraints:    ", self$constraints$repr(),
    "\n  penalties:      ", self$penalties$repr(),
    "\n  solver:         ", d[1]))
  },
  show = function(self) {
    self$print()
  },
  repr = function(self) {
    "ConservationProblem object"
  },
  get_data = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x))
    if (!x %in% names(self$data))
      return(new_waiver())
    return(self$data[[x]])
  },
  set_data = function(self, x, value) {
    assertthat::assert_that(assertthat::is.string(x))
    self$data[[x]] <- value
    invisible()
  },
  number_of_planning_units = function(self) {
    if (inherits(self$data$cost, "Raster")) {
      return(raster::cellStats(raster::Which(!is.na(self$data$cost)), "sum"))
    } else if (inherits(self$data$cost, c("data.frame", "Spatial"))) {
      return(nrow(self$data$cost))
    } else if (is.numeric(self$data$cost)) {
      return(length(self$data$cost))
    } else {
      stop("cost is of unknown class")
    }
  },
  number_of_total_units = function(self) {
    if (inherits(self$data$cost, "Raster")) {
      return(raster::ncell(self$data$cost))
    } else if (inherits(self$data$cost, c("data.frame", "Spatial"))) {
      return(nrow(self$data$cost))
    } else if (is.numeric(self$data$cost)) {
      return(length(self$data$cost))
    } else {
      stop("cost is of unknown class")
    }
  },
  planning_unit_costs = function(self) {
    if (inherits(self$data$cost, "Raster")) {
      return(self$data$cost[raster::Which(!is.na(self$data$cost))])
    } else if (inherits(self$data$cost, c("data.frame",
               "SpatialPolygonsDataFrame", "SpatialLinesDataFrame",
               "SpatialPointsDataFrame"))) {
      return(self$data$cost[[self$data$cost_column]])
    } else if (is.numeric(self$data$cost)) {
      return(self$data$cost)
    } else {
      stop("cost is of unknown class")
    }
  },
  number_of_features = function(self) {
    if (inherits(self$data$features, "Raster")) {
      return(raster::nlayers(self$data$features))
    } else if (inherits(self$data$features, "data.frame")) {
      return(nrow(self$data$features))
    } else {
      stop("feature data is of an unrecognized class")
    }
  },
  feature_names = function(self) {
    if (inherits(self$data$features, "Raster")) {
      return(names(self$data$features))
    } else if (inherits(self$data$features, "data.frame")) {
      return(as.character(self$data$features$name))
    } else {
      stop("feature data is of an unrecognized class")
    }
  },
  feature_abundances_in_planning_units = function(self) {
    Matrix::rowSums(self$data$rij_matrix)
  },
  feature_targets = function(self) {
    if (is.Waiver(self$targets))
      stop("problem is missing targets")
    self$targets$output()
  },
  add_solver = function(self, x) {
    assertthat::assert_that(inherits(x, "Solver"))
    if (!is.Waiver(self$solver))
      warning("overwriting previously defined solver")
    pproto(NULL, self, solver = x)
    },
  add_targets = function(self, x) {
    assertthat::assert_that(inherits(x, "Target"))
    if (!is.Waiver(self$targets))
      warning("overwriting previously defined targets")
    pproto(NULL, self, targets = x)
  },
  add_objective = function(self, x) {
    assertthat::assert_that(inherits(x, "Objective"))
    if (!is.Waiver(self$objective))
      warning("overwriting previously defined objective")
    pproto(NULL, self, objective = x)
  },
  add_decisions = function(self, x) {
    assertthat::assert_that(inherits(x, "Decision"))
    if (!is.Waiver(self$decisions))
      warning("overwriting previously defined decision")
    pproto(NULL, self, decisions = x)
  },
  add_constraint = function(self, x) {
    assertthat::assert_that(inherits(x, "Constraint"))
    p <- pproto(NULL, self)
    p$constraints$add(x)
    return(p)
  },
  add_penalty = function(self, x) {
    assertthat::assert_that(inherits(x, "Penalty"))
    p <- pproto(NULL, self)
    p$penalties$add(x)
    return(p)
  },
  get_constraint_parameter = function(self, id) {
    self$constraints$get_parameter(id)
  },
  set_constraint_parameter = function(self, id, value) {
    self$constraints$set_parameter(id, value)
  },
  render_constraint_parameter = function(self, id) {
    self$constraints$render_parameter(id)
  },
  render_all_constraint_parameters = function(self) {
    self$constraints$render_all_parameter()
  },
  get_objective_parameter = function(self, id) {
    self$objective$get_parameter(id)
  },
  set_objective_parameter = function(self, id, value) {
    self$objective$set_parameter(id, value)
  },
  render_objective_parameter = function(self, id) {
    self$objective$render_parameter(id)
  },
  render_all_objective_parameters = function(self) {
    self$objective$render_all_parameter()
  },
  get_solver_parameter = function(self, id) {
    self$solver$get_parameter(id)
  },
  set_solver_parameter = function(self, id, value) {
    self$solver$set_parameter(id, value)
  },
  render_solver_parameter = function(self, id) {
    self$solver$render_parameter(id)
  },
  render_all_solver_parameters = function(self) {
    self$solver$render_all_parameter()
  },
  get_penalty_parameter = function(self, id) {
    self$penalty$get_parameter(id)
  },
  set_penalty_parameter = function(self, id, value) {
    self$penalty$set_parameter(id, value)
  },
  render_penalty_parameter = function(self, id) {
    self$penalty$render_parameter(id)
  },
  render_all_penalty_parameters = function(self) {
    self$penalty$render_all_parameter()
  })
