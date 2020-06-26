#' @include internal.R waiver.R pproto.R Collection-proto.R binary_stack.R category_layer.R category_vector.R
NULL

#' @export
if (!methods::isClass("ConservationProblem")) methods::setOldClass("ConservationProblem")
NULL

#' Conservation problem class
#'
#' This class is used to represent conservation planning problems. A
#' conservation planning problem has spatially explicit planning units.
#' A prioritization involves making a decision on each planning unit (e.g. is
#' the planning unit going to be turned into a protected area?). Each
#' planning unit is associated with a cost that represents the cost incurred
#' by applying the decision to the planning unit. The problem also has a set
#' of representation targets for each feature. Further, it also has
#' constraints used to ensure that the solution meets additional
#' objectives (e.g. certain areas are locked into the solution). Finally,
#' a conservation planning problem---unlike an optimization problem---also
#' requires a method to solve the problem. **This class represents a
#' planning problem, to actually build and then solve a planning problem,
#' use the [problem()] function. Only experts should use this
#' class directly.**
#'
#' @section Fields:
#' \describe{
#'
#' \item{$data}{`list` object containing data.}
#'
#' \item{$objective}{[Objective-class()] object used to represent how
#'   the targets relate to the solution.}
#'
#' \item{$decisions}{[Decision-class()] object used to represent the
#'   type of decision made on planning units.}
#'
#' \item{$targets}{[Target-class()] object used to represent
#'   representation targets for features.}
#'
#' \item{$penalties}{[Collection-class()] object used to represent
#'   additional [penalties] that the problem is subject to.}
#'
#' \item{$constraints}{[Collection-class()] object used to represent
#'   additional [constraints] that the problem is subject to.}
#'
#' \item{$portfolio}{[Portfolio-class()] object used to represent
#'   the method for generating a portfolio of solutions.}
#'
#' \item{$solver}{[Solver-class()] object used to solve the problem.}
#'
#' }
#'
#' @section Usage:
#'
#' `x$print()`
#'
#' `x$show()`
#'
#' `x$repr()`
#'
#' `x$get_data(name)`
#'
#' `x$set_data(name, value)`
#'
#' `x$number_of_total_units()`
#'
#' `x$number_of_planning_units()`
#'
#' `x$planning_unit_indices()`
#'
#' `x$planning_unit_indices_with_finite_costs()`
#'
#' `x$planning_unit_costs()`
#'
#' `x$number_of_features()`
#'
#' `x$feature_names()`
#'
#' `x$feature_abundances_in_planning_units()`
#'
#' `x$feature_abundances_in_total_units()`
#'
#' `x$feature_targets()`
#'
#' `x$number_of_zones()`
#'
#' `x$zone_names()`
#'
#' `x$add_objective(obj)`
#'
#' `x$add_decisions(dec)`
#'
#' `x$add_portfolio(pol)`
#'
#' `x$add_solver(sol)`
#'
#' `x$add_constraint(con)`
#'
#' `x$add_targets(targ)`
#'
#' `x$get_constraint_parameter(id)`
#'
#' `x$set_constraint_parameter(id, value)`
#'
#' `x$render_constraint_parameter(id)`
#'
#' `x$render_all_constraint_parameters()`
#'
#' `x$get_objective_parameter(id)`
#'
#' `x$set_objective_parameter(id, value)`
#'
#' `x$render_objective_parameter(id)`
#'
#' `x$render_all_objective_parameters()`
#'
#' `x$get_solver_parameter(id)`
#'
#' `x$set_solver_parameter(id, value)`
#'
#' `x$render_solver_parameter(id)`
#'
#' `x$render_all_solver_parameters()`
#'
#' `x$get_portfolio_parameter(id)`
#'
#' `x$set_portfolio_parameter(id, value)`
#'
#' `x$render_portfolio_parameter(id)`
#'
#' `x$render_all_portfolio_parameters()`
#'
#' `x$get_penalty_parameter(id)`
#'
#' `x$set_penalty_parameter(id, value)`
#'
#' `x$render_penalty_parameter(id)`
#'
#' `x$render_all_penalty_parameters()`
#'
#' @section Arguments:
#'
#' \describe{
#'
#' \item{name}{`character` name for object.}
#'
#' \item{value}{an object.}
#'
#' \item{obj}{[Objective-class()] object.}
#'
#' \item{dec}{[Decision-class()] object.}
#'
#' \item{con}{[Constraint-class()] object.}
#'
#' \item{pol}{[Portfolio-class()] object.}
#'
#' \item{sol}{[Solver-class()] object.}
#'
#' \item{targ}{[Target-class()] object.}
#'
#' \item{cost}{[raster::RasterLayer-class()],
#'   [sp::SpatialPolygonsDataFrame-class()], or
#'   [sp::SpatialLinesDataFrame-class()] object showing spatial
#'   representation of the planning units and their cost.}
#'
#' \item{features}{[Zones-class()] or `data.frame` object
#'   containing feature data.}
#'
#' \item{id}{`Id` object that refers to a specific parameter.}
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
#' \item{repr}{return `character` representation of the object.}
#'
#' \item{get_data}{return an object stored in the `data` field with
#'   the corresponding `name`. If the object is not present in the
#'   `data` field, a `waiver` object is returned.}
#'
#' \item{set_data}{store an object stored in the `data` field with
#'   the corresponding name. If an object with that name already
#'   exists then the object is overwritten.}
#'
#' \item{number_of_planning_units}{`integer` number of planning units.}
#'
#' \item{planning_unit_indices}{`integer` indices of the planning units in
#'   the planning unit data.}
#'
#' \item{planning_unit_indices_with_finite_costs}{`list` of `integer`
#'   indices of planning units in each zone that have finite cost data.}
#'
#' \item{number_of_total_units}{`integer` number of units in the cost
#'   data including units that have `N` cost data.}
#'
#' \item{planning_unit_costs}{`matrix` cost of allocating each planning
#'   unit to each zone. Each column corresponds to a different zone and
#'   each row corresponds to a different planning unit.}
#'
#' \item{number_of_features}{`integer` number of features.}
#'
#' \item{feature_names}{`character` names of features in problem.}
#'
#' \item{feature_abundances_in_planning_units}{`matrix` total
#'   abundance of each feature in planning units available in each zone. Each
#'   column corresponds to a different zone and each row corresponds to a
#'   different feature.}
#'
#' \item{feature_abundances_in_total_units}{`matrix` total
#'   abundance of each feature in each zone. Each column corresponds to a
#'   different zone and each row corresponds to a different feature.}
#'
#' \item{feature_targets}{[tibble::tibble()] with feature targets.}
#'
#' \item{number_of_zones}{`integer` number of zones.}
#'
#' \item{zone_names}{`character` names of zones in problem.}
#'
#' \item{add_objective}{return a new  [ConservationProblem-class()]
#'   with the objective added to it.}
#'
#' \item{add_decisions}{return a new [ConservationProblem-class()]
#'   object with the decision added to it.}
#'
#' \item{add_portfolio}{return a new [ConservationProblem-class()]
#'   object with the portfolio method added to it.}
#'
#' \item{add_solver}{return a new [ConservationProblem-class()] object
#'   with the solver added to it.}
#'
#' \item{add_constraint}{return a new [ConservationProblem-class()]
#'   object with the constraint added to it.}
#'
#' \item{add_targets}{return a copy with the targets added to the problem.}
#'
#' \item{get_constraint_parameter}{get the value of a parameter (specified by
#'   argument `id`) used in one of the constraints in the object.}
#'
#' \item{set_constraint_parameter}{set the value of a parameter (specified by
#'   argument `id`) used in one of the constraints in the object to
#'   `value`.}
#'
#' \item{render_constraint_parameter}{generate a *shiny* widget to modify
#'  the value of a parameter (specified by argument `id`).}
#'
#' \item{render_all_constraint_parameters}{generate a *shiny* `div`
#'   containing all the parameters' widgets.}
#'
#' \item{get_objective_parameter}{get the value of a parameter (specified by
#'   argument `id`) used in the object's objective.}
#'
#' \item{set_objective_parameter}{set the value of a parameter (specified by
#'   argument `id`) used in the object's objective to `value`.}
#'
#' \item{render_objective_parameter}{generate a *shiny* widget to modify
#'   the value of a parameter (specified by argument `id`).}
#'
#' \item{render_all_objective_parameters}{generate a *shiny* `div`
#'   containing all the parameters' widgets.}
#'
#' \item{get_solver_parameter}{get the value of a parameter (specified by
#'   argument `id`) used in the object's solver.}
#'
#' \item{set_solver_parameter}{set the value of a parameter (specified by
#'   argument `id`) used in the object's solver to `value`.}
#'
#' \item{render_solver_parameter}{generate a *shiny* widget to modify
#'   the value of a parameter (specified by argument `id`).}
#'
#' \item{render_all_solver_parameters}{generate a *shiny* `div`
#'   containing all the parameters' widgets.}
#'
#' \item{get_portfolio_parameter}{get the value of a parameter (specified by
#'   argument `id`) used in the object's portfolio.}
#'
#' \item{set_portfolio_parameter}{set the value of a parameter (specified by
#'   argument `id`) used in objects' solver to `value`.}
#'
#' \item{render_portfolio_parameter}{generate a *shiny* widget to modify
#'   the value of a parameter (specified by argument `id`).}
#'
#' \item{render_all_portfolio_parameters}{generate a *shiny* `div`
#'   containing all the parameters' widgets.}
#'
#' }
#'
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
  portfolio  = new_waiver(),
  solver  = new_waiver(),
  print = function(self) {
    r <- vapply(list(self$objective, self$targets), function(x) {
      if (is.Waiver(x))
        return("none")
      return(x$repr())
    }, character(1))
    d <- vapply(list(self$solver, self$decisions, self$portfolio), function(x) {
      if (is.Waiver(x))
        return("default")
      return(x$repr())
    }, character(1))
    cs <- round(range(self$planning_unit_costs(), na.rm = TRUE), 5)
    message(paste0("Conservation Problem",
    ifelse(self$number_of_zones() > 1, paste0(
      "\n  zones:          ", repr_atomic(self$zone_names(), "zones")),
      ""),
    "\n  planning units: ", class(self$data$cost)[1], " (",
      self$number_of_planning_units(), " units)",
    "\n  cost:           min: ", cs[1], ", max: ", cs[2],
    "\n  features:       ", repr_atomic(self$feature_names(), "features"),
    "\n  objective:      ", r[1],
    "\n  targets:        ", r[2],
    "\n  decisions:      ", d[2],
    "\n  constraints:    ", align_text(self$constraints$repr(), 19),
    "\n  penalties:      ", align_text(self$penalties$repr(), 19),
    "\n  portfolio:      ", d[3],
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
      if (raster::nlayers(self$data$cost) == 1) {
        return(raster::cellStats(raster::Which(!is.na(self$data$cost)), "sum"))
      } else {
        return(raster::cellStats(max(!is.na(self$data$cost)), "sum"))
      }
    } else if (inherits(self$data$cost, c("data.frame", "Spatial"))) {
      return(sum(rowSums(!is.na(as.matrix(
        as.data.frame(self$data$cost)[, self$data$cost_column,
                      drop = FALSE]))) > 0))
    } else if (inherits(self$data$cost, "sf")) {
      return(sum(rowSums(!is.na(as.matrix(
        sf::st_drop_geometry(self$data$cost)[, self$data$cost_column,
                                             drop = FALSE]))) > 0))
    } else if (is.matrix(self$data$cost)) {
      return(sum(rowSums(!is.na(self$data$cost)) > 0))
    } else {
      stop("cost is of unknown class")
    }
  },
  planning_unit_indices = function(self) {
    if (inherits(self$data$cost, "Raster")) {
      if (raster::nlayers(self$data$cost) == 1) {
        return(raster::Which(!is.na(self$data$cost), cells = TRUE))
      } else {
        return(raster::Which(max(!is.na(self$data$cost)) > 0, cells = TRUE))
      }
    } else if (inherits(self$data$cost, c("data.frame", "Spatial"))) {
      return(unname(which(rowSums(!is.na(as.matrix(
             as.data.frame(self$data$cost)[, self$data$cost_column,
              drop = FALSE]))) > 0)))
    } else if (inherits(self$data$cost, "sf")) {
      return(sum(rowSums(!is.na(as.matrix(
        sf::st_drop_geometry(self$data$cost)[, self$data$cost_column,
                                             drop = FALSE]))) > 0))
    } else if (is.matrix(self$data$cost)) {
      return(unname(which(rowSums(!is.na(self$data$cost)) > 0)))
    } else {
      stop("cost is of unknown class")
    }
  },
  planning_unit_indices_with_finite_costs = function(self) {
    if (inherits(self$data$cost, "Raster")) {
      if (raster::nlayers(self$data$cost) == 1) {
        out <- list(raster::Which(!is.na(self$data$cost), cells = TRUE))
      } else {
        out <- lapply(seq_len(raster::nlayers(self$data$cost)),
                      function(i) raster::Which(!is.na(self$data$cost[[i]]),
                                                cells = TRUE))
      }
    } else if (inherits(self$data$cost, c("data.frame", "Spatial", "sf"))) {
      out <- lapply(self$data$cost_column,
                    function(i) which(!is.na(self$data$cost[[i]])))
    } else if (is.matrix(self$data$cost)) {
      out <- lapply(seq_len(ncol(self$data$cost)),
                    function(i) which(!is.na(self$data$cost[, i])))
    } else {
      stop("cost is of unknown class")
    }
    names(out) <- self$zone_names()
    out
  },
  number_of_total_units = function(self) {
    if (inherits(self$data$cost, "Raster")) {
      return(raster::ncell(self$data$cost))
    } else if (inherits(self$data$cost, c("data.frame", "Spatial", "sf"))) {
      return(nrow(self$data$cost))
    } else if (is.matrix(self$data$cost)) {
      return(nrow(self$data$cost))
    } else {
      stop("cost is of unknown class")
    }
  },
  planning_unit_costs = function(self) {
    if (inherits(self$data$cost, "Raster")) {
      if (raster::nlayers(self$data$cost) == 1) {
        m <- matrix(self$data$cost[raster::Which(!is.na(self$data$cost))],
                    ncol = 1)
      } else {
        cells <- raster::Which(max(!is.na(self$data$cost)) == 1)
        m <- self$data$cost[cells]
      }
    } else if (inherits(self$data$cost,
                        c("SpatialPolygonsDataFrame",
                          "SpatialLinesDataFrame",
                          "SpatialPointsDataFrame",
                          "data.frame"))) {
      m <- as.matrix(as.data.frame(self$data$cost)[, self$data$cost_column,
                                                     drop = FALSE])
    } else if (inherits(self$data$cost, "sf")) {
      m <-
        as.matrix(sf::st_drop_geometry(self$data$cost)[, self$data$cost_column,
                                                       drop = FALSE])
    } else if (is.matrix(self$data$cost)) {
      m <- self$data$cost
    } else {
      stop("cost is of unknown class")
    }
    colnames(m) <- self$zone_names()
    return(m[rowSums(!is.na(m)) > 0, , drop = FALSE])
  },
  number_of_features = function(self) {
    if (inherits(self$data$features, "ZonesCharacter")) {
      return(length(self$data$features[[1]]))
    } else if (inherits(self$data$features, "ZonesRaster")) {
      return(raster::nlayers(self$data$features[[1]]))
    } else if (inherits(self$data$features, "data.frame")) {
      return(nrow(self$data$features))
    } else {
      stop("feature data is of an unrecognized class")
    }
  },
  feature_names = function(self) {
    if (inherits(self$data$features, "Zones")) {
      return(attr(self$data$features, "feature_names"))
    } else if (inherits(self$data$features, "data.frame")) {
      return(as.character(self$data$features$name))
    } else {
      stop("feature data is of an unrecognized class")
    }
  },
  feature_abundances_in_planning_units = function(self) {
    pu_indices <- self$planning_unit_indices()
    ind <- self$planning_unit_indices_with_finite_costs()
    ind <- lapply(ind, function(x) match(x, pu_indices))
    out <- vapply(seq_along(self$data$rij_matrix),
      function(i) Matrix::rowSums(self$data$rij_matrix[[i]][, ind[[i]],
        drop = FALSE], na.rm = TRUE),
      numeric(nrow(self$data$rij_matrix[[1]])))
    if (!is.matrix(out))
      out <- matrix(out, ncol = self$number_of_zones())
    colnames(out) <- self$zone_names()
    out
  },
  feature_abundances_in_total_units = function(self) {
    self$data$feature_abundances_in_total_units
  },
  feature_targets = function(self) {
    if (is.Waiver(self$targets))
      stop("problem is missing targets")
    self$targets$output()
  },
  number_of_zones = function(self) {
    if (inherits(self$data$features, "Zones")) {
      return(length(self$data$features))
    } else if (inherits(self$data$features, "data.frame")) {
      return(length(self$data$rij_matrix))
    } else {
      stop("feature is of an unrecognized class")
    }
  },
  zone_names = function(self) {
    if (inherits(self$data$features, "Zones")) {
      return(attr(self$data$features, "zone_names"))
    } else if (inherits(self$data$features, "data.frame")) {
      return(names(self$data$rij_matrix))
    } else {
      stop("feature is of an unrecognized class")
    }
  },
  add_portfolio = function(self, x) {
    assertthat::assert_that(inherits(x, "Portfolio"))
    if (!is.Waiver(self$portfolio))
      warning("overwriting previously defined portfolio")
    pproto(NULL, self, portfolio = x)
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
    self$constraints$render_all_parameters()
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
    self$objective$render_all_parameters()
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
    self$solver$render_all_parameters()
  },
  get_portfolio_parameter = function(self, id) {
    self$portfolio$get_parameter(id)
  },
  set_portfolio_parameter = function(self, id, value) {
    self$portfolio$set_parameter(id, value)
  },
  render_portfolio_parameter = function(self, id) {
    self$portfolio$render_parameter(id)
  },
  render_all_portfolio_parameters = function(self) {
    self$portfolio$render_all_parameters()
  },
  get_penalty_parameter = function(self, id) {
    self$penalties$get_parameter(id)
  },
  set_penalty_parameter = function(self, id, value) {
    self$penalties$set_parameter(id, value)
  },
  render_penalty_parameter = function(self, id) {
    self$penalties$render_parameter(id)
  },
  render_all_penalty_parameters = function(self) {
    self$penalties$render_all_parameters()
  })
