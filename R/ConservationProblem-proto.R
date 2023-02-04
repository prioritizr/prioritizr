#' @include internal.R waiver.R pproto.R binary_stack.R category_layer.R category_vector.R
NULL

#' @export
if (!methods::isClass("ConservationProblem")) methods::setOldClass("ConservationProblem")
NULL

#' Conservation problem class
#'
#' This class is used to represent conservation planning problems. A
#' conservation planning problem has spatially explicit planning units.
#' A prioritization involves making a decision on each planning unit (e.g., is
#' the planning unit going to be turned into a protected area?). Each
#' planning unit is associated with a cost that represents the cost incurred
#' by applying the decision to the planning unit. The problem also has a set
#' of representation targets for each feature. Further, it also has
#' constraints used to ensure that the solution meets additional
#' objectives (e.g., certain areas are locked into the solution). Finally,
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
#' \item{$objective}{[`Objective-class`] object used to represent how
#'   the targets relate to the solution.}
#'
#' \item{$decisions}{[`Decision-class`] object used to represent the
#'   type of decision made on planning units.}
#'
#' \item{$targets}{[`Target-class`] object used to represent
#'   representation targets for features.}
#'
#' \item{$penalties}{`list` containing [`Penalty-class`] objects.}
#'
#' \item{$constraints}{`list` containing [`Constraint-class`] objects.}
#'
#' \item{$portfolio}{[`Portfolio-class`] object used to represent
#'   the method for generating a portfolio of solutions.}
#'
#' \item{$solver}{[`Solver-class`] object used to solve the problem.}
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
#' `x$set_planning_unit_indices()`
#'
#' `x$planning_unit_indices_with_finite_costs()`
#'
#' `x$set_planning_unit_indices_with_finite_costs()`
#'
#' `x$planning_unit_costs()`
#'
#' `x$set_planning_unit_costs()`
#'
#' `x$number_of_features()`
#'
#' `x$feature_names()`
#'
#' `x$feature_abundances_in_planning_units()`
#'
#' `x$set_feature_abundances_in_planning_units()`
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
#' @section Arguments:
#'
#' \describe{
#'
#' \item{name}{`character` name for object.}
#'
#' \item{value}{an object.}
#'
#' \item{obj}{[`Objective-class`] object.}
#'
#' \item{dec}{[`Decision-class`] object.}
#'
#' \item{con}{[`Constraint-class`] object.}
#'
#' \item{pol}{[`Portfolio-class`] object.}
#'
#' \item{sol}{[`Solver-class`] object.}
#'
#' \item{targ}{[`Target-class`] object.}
#'
#' \item{cost}{`matrix`, `data.frame`, [terra::rast()], or [sf::st_sf()] object
#'   containing the planning unit cost data.}
#'
#' \item{features}{[`Zones-class`] or `data.frame` object
#'   containing feature data.}
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
#' \item{set_planning_unit_indices}{calculate the indices of the planning
#'   units.}
#'
#' \item{planning_unit_indices_with_finite_costs}{`list` of `integer`
#'   indices of planning units in each zone that have finite cost data.}
#'
#' \item{set_planning_unit_indices_with_finite_costs}{calculate the planning
#'   unit indices with finite costs.}
#'
#' \item{number_of_total_units}{`integer` number of units in the cost
#'   data including units that have `N` cost data.}
#'
#' \item{planning_unit_costs}{`matrix` cost of allocating each planning
#'   unit to each zone. Each column corresponds to a different zone and
#'   each row corresponds to a different planning unit.}
#'
#' \item{set_planning_unit_costs}{calculate the planning unit costs.}
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
#' \item{set_feature_abundances_in_planning_units}{calculate the
#'   feature abundances in planning units.}
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
#' \item{add_objective}{return a new  [`ConservationProblem-class`]
#'   with the objective added to it.}
#'
#' \item{add_decisions}{return a new [`ConservationProblem-class`]
#'   object with the decision added to it.}
#'
#' \item{add_portfolio}{return a new [`ConservationProblem-class`]
#'   object with the portfolio method added to it.}
#'
#' \item{add_solver}{return a new [`ConservationProblem-class`] object
#'   with the solver added to it.}
#'
#' \item{add_constraint}{return a new [`ConservationProblem-class`]
#'   object with the constraint added to it.}
#'
#' \item{add_targets}{return a copy with the targets added to the problem.}
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
  constraints = list(),
  penalties = list(),
  portfolio = new_waiver(),
  solver = new_waiver(),
  summary = function(self) {
    # define characters
    ch <- box_chars()

    # create container
    div_id <- cli::cli_div(theme = conservation_problem_theme())

    # create header
    cli::cli_text(
      "A conservation problem ({.cls ConservationProblem})"
    )

    # pre-compute values for data section
    if (is_spatially_explicit(self$data$cost)) {
      crs_text <- repr.crs(get_crs(self$data$cost))
      extent_text <- repr.bbox(sf::st_bbox(self$data$cost))
    } else {
      crs_text <- "{.gray NA}"
      extent_text <- "{.gray NA}"
    }
    cost_range <- range(self$planning_unit_costs() , na.rm = TRUE)
    cost_text <- ifelse(
      all_binary(self$planning_unit_costs()),
      paste(
        "binary values ({.val 0} and {.val 1})"
      ),
      paste(
        "continuous values (between {.val {cost_range}})"
      )
    )

    ############# here

    # print data section
    cli::cli_text("{ch$j}{ch$b}{.bg data}:")
    if (self$number_of_zones() > 1) {
      cli_vtext(
        "{ch$v}{ch$j}{ch$b}{.g zones}:     ",
        repr.character(self$zone_names())
      )
    }
    cli_vtext(
      "{ch$v}{ch$j}{ch$b}{.g features}:     ",
      repr.character(self$feature_names())
    )
    cli_vtext("{ch$v}{ch$l}{ch$b}{.g planning units}:")
    cli_vtext(
      "{ch$v} {ch$j}{ch$b}{.g data}:        ",
      "{.cls ",
      class(self$data$cost), "} (",
      self$number_of_planning_units(),
      " total)"
    )
    cli_vtext(
      "{ch$v} {ch$j}{ch$b}{.g costs}:       ",
      cost_text
    )
    cli_vtext(
      "{ch$v} {ch$j}{ch$b}{.g extent}:      ",
      extent_text
    )
    cli_vtext(
      "{ch$v} {ch$l}{ch$b}{.g CRS}:         ",
      crs_text
    )

    # pre-compute values for formulation section
    ## missing text
    missing_text <- "{.gray none specified}"
    ## objective
    objective_text <- missing_text
    if (!is.Waiver(self$objective)) {
      objective_text <- self$objective$repr(compact = FALSE)
    }
    ## targets
    targets_text <- missing_text
    if (!is.Waiver(self$targets)) {
      targets_text <- self$targets$repr(compact = FALSE)
    }
    ## decisions
    decisions_text <- missing_text
    if (!is.Waiver(self$decisions)) {
      decisions_text <- self$decisions$repr(compact = FALSE)
    }
    ## portfolio
    portfolio_text <- missing_text
    if (!is.Waiver(self$portfolio)) {
      portfolio_text <- self$portfolio$repr(compact = FALSE)
    }
    ## solver
    solver_text <- missing_text
    if (!is.Waiver(self$solver)) {
      solver_text <- self$solver$repr(compact = FALSE)
    }
    ## constraints
    constraints_text <- missing_text
    if (length(self$constraints) > 0) {
      constraints_text <- lapply(
        self$constraints,
        function(w) w$repr(compact = FALSE)
      )
    }
    ## penalties
    penalties_text <- missing_text
    if (length(self$penalties) > 0) {
      penalties_text <- lapply(
        self$penalties,
        function(w) w$repr(compact = FALSE)
      )
    }

    # print formulation section
    ## header
    cli::cli_text("{ch$l}{ch$b}{.bg formulation}:")
    ## objective
    cli_tree_component(
      objective_text,
      header = " {ch$j}{ch$b}{.g objective}:    ",
      subheader = " {ch$v}",
      width = 15
    )
    ## penalties
    if (length(self$penalties) > 0) {
      cli_vtext(" {ch$j}{ch$b}{.g penalties}:")
      for (i in seq_along(penalties_text)) {
        if (i < length(penalties_text)) {
          cli_tree_component(
            penalties_text[[i]],
            header = "{ch$v}{ch$j}{ch$b}             ",
            subheader = "{ch$v}{ch$v}",
            padding = " ",
            width = 14
          )
        } else {
          cli_tree_component(
            penalties_text[[i]],
            header = "{ch$v}{ch$l}{ch$b}             ",
            subheader = "{ch$v} ",
            padding = " ",
            width = 14
          )
        }
      }
    } else {
      cli_vtext(
        " {ch$j}{ch$b}{.g penalties}:    ",
        penalties_text
      )
    }
    ## targets
    cli_vtext(
     " {ch$j}{ch$b}{.g targets}:      ",
      targets_text
    )
    ## constraints
    if (length(self$constraints) > 0) {
      cli_vtext(" {ch$j}{ch$b}{.g constraints}: ")
      for (i in seq_along(constraints_text)) {
        if (i < length(constraints_text)) {
          cli_tree_component(
            constraints_text[[i]],
            header = "{ch$v}{ch$j}{ch$b}             ",
            subheader = "{ch$v}{ch$v}",
            padding = " ",
            width = 14
          )
        } else {
          cli_tree_component(
            constraints_text[[i]],
            header = "{ch$v}{ch$l}{ch$b}             ",
            subheader = "{ch$v} ",
            padding = " ",
            width = 14
          )
        }
      }
    } else {
      cli_vtext(
        " {ch$j}{ch$b}{.g constraints}:  ",
        constraints_text
      )
    }
    ## decisions
    cli_tree_component(
      decisions_text,
      header = " {ch$j}{ch$b}{.g decisions}:    ",
      subheader = " {ch$v}",
      width = 15
    )
    cli_tree_component(
      portfolio_text,
      header = " {ch$j}{ch$b}{.g portfolio}:    ",
      subheader = " {ch$v}",
      width = 15
    )
    cli_tree_component(
      solver_text,
      header = " {ch$l}{ch$b}{.g solver}:       ",
      subheader = "  ",
      width = 15
    )

    # end container
    cli::cli_end(div_id)

    # return success
    invisible(TRUE)
  },
  print = function(self) {
    # define characters
    ch <- box_chars()

    # create container
    div_id <- cli::cli_div(theme = conservation_problem_theme())

    # create header
    cli::cli_text(
      "A conservation problem ({.cls ConservationProblem})"
    )

    # pre-compute values for data section
    if (is_spatially_explicit(self$data$cost)) {
      crs_text <- repr.crs(get_crs(self$data$cost))
      extent_text <- repr.bbox(sf::st_bbox(self$data$cost))
    } else {
      crs_text <- "{.gray NA}"
      extent_text <- "{.gray NA}"
    }
    cost_range <- range(self$planning_unit_costs() , na.rm = TRUE)
    cost_text <- ifelse(
      all_binary(self$planning_unit_costs()),
      paste(
        "binary values ({.val 0} and {.val 1})"
      ),
      paste(
        "continuous values (between {.val {cost_range}})"
      )
    )

    # print data section
    cli::cli_text("{ch$j}{ch$b}{.bg data}:")
    if (self$number_of_zones() > 1) {
      cli_vtext(
        "{ch$v}{ch$j}{.g zones}:     ",
        repr.character(self$zone_names())
      )
    }
    cli_vtext(
      "{ch$v}{ch$j}{ch$b}{.g features}:    ",
      repr.character(self$feature_names())
    )
    cli_vtext("{ch$v}{ch$l}{ch$b}{.g planning units}:")
    cli_vtext(
      "{ch$v} {ch$j}{ch$b}{.g data}:       ",
      "{.cls ",
      class(self$data$cost), "} (",
      self$number_of_planning_units(),
      " total)"
    )
    cli_vtext(
      "{ch$v} {ch$j}{ch$b}{.g costs}:      ",
      cost_text
    )
    cli_vtext(
      "{ch$v} {ch$j}{ch$b}{.g extent}:     ",
      extent_text
    )
    cli_vtext(
      "{ch$v} {ch$l}{ch$b}{.g CRS}:        ",
      crs_text
    )

    # pre-compute values for formulation section
    ## missing text
    missing_text <- "{.gray none specified}"
    ## objective
    objective_text <- missing_text
    if (!is.Waiver(self$objective)) {
      objective_text <- self$objective$repr()
    }
    ## targets
    targets_text <- missing_text
    if (!is.Waiver(self$targets)) {
      targets_text <- self$targets$repr()
    }
    ## decisions
    decisions_text <- missing_text
    if (!is.Waiver(self$decisions)) {
      decisions_text <- self$decisions$repr()
    }
    ## portfolio
    portfolio_text <- missing_text
    if (!is.Waiver(self$portfolio)) {
      portfolio_text <- self$portfolio$repr()
    }
    ## solver
    solver_text <- missing_text
    if (!is.Waiver(self$solver)) {
      solver_text <- self$solver$repr()
    }
    ## constraints
    constraints_text <- missing_text
    if (length(self$constraints) > 0) {
      constraints_text <- vapply(
        self$constraints,
        function(w) w$repr(),
        character(1)
      )
    }
    ## penalties
    penalties_text <- missing_text
    if (length(self$penalties) > 0) {
      penalties_text <- vapply(
        self$penalties,
        function(w) w$repr(),
        character(1)
      )
    }

    # print formulation section
    ## header
    cli::cli_text("{ch$l}{ch$b}{.bg formulation}:")
    ## objective
    cli_vtext(
      " {ch$j}{ch$b}{.g objective}:   ",
      objective_text
    )
    ## penalties
    if (length(self$penalties) > 0) {
      cli_vtext(" {ch$j}{ch$b}{.g penalties}: ")
      for (i in seq_along(penalties_text)) {
        if (i < length(penalties_text)) {
          cli_vtext(
            " {ch$v}{ch$j}{ch$b}", i, ":",
            paste(rep(" ", max(0, 11 - nchar(i))), collapse = ""),
            penalties_text[[i]]
          )
        } else {
          cli_vtext(
            " {ch$v}{ch$l}{ch$b}", i, ":",
            paste(rep(" ", max(0, 11 - nchar(i))), collapse = ""),
            penalties_text[[i]]
          )
        }
      }
    } else {
      cli_vtext(
        " {ch$j}{ch$b}{.g penalties}:   ",
        penalties_text
      )
    }
    ## targets
    cli_vtext(
      " {ch$j}{ch$b}{.g targets}:     ",
      targets_text
    )
    ## constraints
    if (length(self$constraints) > 0) {
      cli_vtext(" {ch$j}{ch$b}{.g constraints}: ")
      for (i in seq_along(constraints_text)) {
        if (i < length(constraints_text)) {
          cli_vtext(
            " {ch$v}{ch$j}{ch$b}", i, ":",
            paste(rep(" ", max(0, 11 - nchar(i))), collapse = ""),
            constraints_text[[i]]
          )
        } else {
          cli_vtext(
            " {ch$v}{ch$l}{ch$b}", i, ":",
            paste(rep(" ", max(0, 11 - nchar(i))), collapse = ""),
            constraints_text[[i]]
          )
        }
      }
    } else {
      cli_vtext(
        " {ch$j}{ch$b}{.g constraints}: ",
        constraints_text
      )
    }
    ## decisions
    cli_vtext(
      " {ch$j}{ch$b}{.g decisions}:   ",
      decisions_text
    )
    cli_vtext(
      " {ch$j}{ch$b}{.g portfolio}:   ",
      portfolio_text
    )
    cli_vtext(
      " {ch$l}{ch$b}{.g solver}:      ",
      solver_text
    )

    # add footer
    cli::cli_text(
      cli::col_grey(
        "# {cli::symbol$info} Use {.code summary(...)}",
        " to see complete formulation."
      )
    )

    # end container
    cli::cli_end(div_id)

    # return success
    invisible(TRUE)
  },
  show = function(self) {
    self$print()
  },
  repr = function(self) {
    "{.cls ConservationProblem} object"
  },
  get_data = function(self, x) {
    assert(assertthat::is.string(x))
    if (!x %in% names(self$data)) return(new_waiver())
    self$data[[x]]
  },
  set_data = function(self, x, value) {
    assert(assertthat::is.string(x))
    self$data[[x]] <- value
    invisible()
  },
  number_of_planning_units = function(self) {
    length(self$planning_unit_indices())
  },
  planning_unit_indices = function(self) {
    i <- self$get_data("planning_unit_indices")
    if (!is.Waiver(i)) return(i)
    self$set_planning_unit_indices()
    self$get_data("planning_unit_indices")
  },
  set_planning_unit_indices = function(self) {
    if (inherits(self$data$cost, "Raster")) {
      if (raster::nlayers(self$data$cost) == 1) {
        x <- raster::Which(!is.na(self$data$cost), cells = TRUE)
      } else {
        x <- raster::Which(max(!is.na(self$data$cost)) > 0, cells = TRUE)
      }
    } else if (inherits(self$data$cost, "SpatRaster")) {
      x <- terra::cells(terra::allNA(self$data$cost), 0)[[1]]
    } else if (inherits(self$data$cost, c("data.frame", "Spatial"))) {
      x <- as.data.frame(self$data$cost)[, self$data$cost_column, drop = FALSE]
      x <- unname(which(rowSums(!is.na(as.matrix(x))) > 0))
    } else if (inherits(self$data$cost, "sf")) {
      x <- sf::st_drop_geometry(self$data$cost)
      x <- x[, self$data$cost_column, drop = FALSE]
      x <- sum(rowSums(!is.na(as.matrix(x))) > 0)
    } else if (is.matrix(self$data$cost)) {
      x <- unname(which(rowSums(!is.na(self$data$cost)) > 0))
    } else {
      cli::cli_abort("$data$cost is of unknown class", .internal = TRUE)
    }
    self$set_data("planning_unit_indices", x)
    invisible()
  },
  planning_unit_indices_with_finite_costs = function(self) {
    i <- self$get_data("planning_unit_indices_with_finite_costs")
    if (!is.Waiver(i)) return(i)
    self$set_planning_unit_indices_with_finite_costs()
    self$get_data("planning_unit_indices_with_finite_costs")
  },
  set_planning_unit_indices_with_finite_costs = function(self) {
    if (inherits(self$data$cost, "Raster")) {
      if (raster::nlayers(self$data$cost) == 1) {
        x <- list(raster::Which(!is.na(self$data$cost), cells = TRUE))
      } else {
        x <- lapply(
          seq_len(raster::nlayers(self$data$cost)),
          function(i) raster::Which(!is.na(self$data$cost[[i]]), cells = TRUE)
        )
      }
    } else if (inherits(self$data$cost, "SpatRaster")) {
      x <- unname(terra::cells(is.na(self$data$cost), 0))
    } else if (inherits(self$data$cost, c("data.frame", "Spatial", "sf"))) {
      x <- lapply(
        self$data$cost_column,
        function(i) which(!is.na(self$data$cost[[i]]))
      )
    } else if (is.matrix(self$data$cost)) {
      x <- lapply(
        seq_len(ncol(self$data$cost)),
        function(i) which(!is.na(self$data$cost[, i]))
      )
    } else {
      cli::cli_abort("$data$cost is of unknown class", .internal = TRUE)
    }
    names(x) <- self$zone_names()
    self$set_data("planning_unit_indices_with_finite_costs", x)
    invisible()
  },
  number_of_total_units = function(self) {
    if (inherits(self$data$cost, "Raster")) {
      return(raster::ncell(self$data$cost))
    } else if (inherits(self$data$cost, "SpatRaster")) {
      return(terra::ncell(self$data$cost))
    } else if (inherits(self$data$cost, c("data.frame", "Spatial", "sf"))) {
      return(nrow(self$data$cost))
    } else if (is.matrix(self$data$cost)) {
      return(nrow(self$data$cost))
    } else {
      cli::cli_abort("$data$cost is of unknown class", .internal = TRUE)
    }
  },
  planning_unit_costs = function(self) {
    i <- self$get_data("planning_unit_costs")
    if (!is.Waiver(i)) return(i)
    self$set_planning_unit_costs()
    self$get_data("planning_unit_costs")
  },
  set_planning_unit_costs = function(self) {
    idx <- self$planning_unit_indices()
    if (inherits(self$data$cost, "Raster")) {
      if (raster::nlayers(self$data$cost) == 1) {
        x <- matrix(self$data$cost[idx], ncol = 1)
      } else {
        x <- self$data$cost[idx]
      }
    } else if (inherits(self$data$cost, "SpatRaster")) {
      x <- as.matrix(self$data$cost[idx])
    } else if (inherits(self$data$cost, c("Spatial", "data.frame"))) {
      x <- as.matrix(
        as.data.frame(self$data$cost)[idx, self$data$cost_column, drop = FALSE]
      )
    } else if (inherits(self$data$cost, "sf")) {
      x <- sf::st_drop_geometry(self$data$cost)
      x <- as.matrix(x[idx, self$data$cost_column, drop = FALSE])
    } else if (is.matrix(self$data$cost)) {
      x <- self$data$cost[idx, , drop = FALSE]
    } else {
      cli::cli_abort("$data$cost is of unknown class", .internal = TRUE)
    }
    colnames(x) <- self$zone_names()
    self$set_data("planning_unit_costs", x)
    invisible()
  },
  number_of_features = function(self) {
    if (inherits(self$data$features, "ZonesCharacter")) {
      return(length(self$data$features[[1]]))
    } else if (inherits(self$data$features, "ZonesRaster")) {
      return(raster::nlayers(self$data$features[[1]]))
    } else if (inherits(self$data$features, "ZonesSpatRaster")) {
      return(terra::nlyr(self$data$features[[1]]))
    } else if (inherits(self$data$features, "data.frame")) {
      return(nrow(self$data$features))
    } else {
      cli::cli_abort("$features is of an unrecognized class", .internal = TRUE)
    }
  },
  feature_names = function(self) {
    if (inherits(self$data$features, "Zones")) {
      return(attr(self$data$features, "feature_names"))
    } else if (inherits(self$data$features, "data.frame")) {
      return(as.character(self$data$features$name))
    } else {
      cli::cli_abort("$features is of an unrecognized class", .internal = TRUE)
    }
  },
  feature_abundances_in_planning_units = function(self) {
    x <- self$get_data("feature_abundances_in_planning_units")
    if (!is.Waiver(x)) return(x)
    self$set_feature_abundances_in_planning_units()
    self$get_data("feature_abundances_in_planning_units")
  },
  set_feature_abundances_in_planning_units = function(self) {
    pu_indices <- self$planning_unit_indices()
    ind <- self$planning_unit_indices_with_finite_costs()
    ind <- lapply(ind, function(x) match(x, pu_indices))
    out <- vapply(
      seq_along(self$data$rij_matrix),
      FUN.VALUE = numeric(nrow(self$data$rij_matrix[[1]])),
      function(i) {
        Matrix::rowSums(
          self$data$rij_matrix[[i]][, ind[[i]], drop = FALSE],
          na.rm = TRUE
        )
      }
    )
    if (!is.matrix(out))
      out <- matrix(out, ncol = self$number_of_zones())
    colnames(out) <- self$zone_names()
    rownames(out) <- self$feature_names()
    self$set_data("feature_abundances_in_planning_units", out)
    invisible()
  },
  feature_abundances_in_total_units = function(self) {
    self$data$feature_abundances_in_total_units
  },
  feature_targets = function(self) {
    if (is.Waiver(self$targets))
      cli::cli_abort("Targets have not been specified.")
    self$targets$output()
  },
  number_of_zones = function(self) {
    if (inherits(self$data$features, "Zones")) {
      return(length(self$data$features))
    } else if (inherits(self$data$features, "data.frame")) {
      return(length(self$data$rij_matrix))
    } else {
      cli::cli_abort("$features is of an unrecognized class.", .internal = TRUE)
    }
  },
  zone_names = function(self) {
    if (inherits(self$data$features, "Zones")) {
      return(attr(self$data$features, "zone_names"))
    } else if (inherits(self$data$features, "data.frame")) {
      return(names(self$data$rij_matrix))
    } else {
      cli::cli_abort("$features is of an unrecognized class.", .internal = TRUE)
    }
  },
  add_portfolio = function(self, x) {
    assert(inherits(x, "Portfolio"))
    if (!is.Waiver(self$portfolio))
      cli::cli_warn("overwriting previously defined portfolio")
    pproto(NULL, self, portfolio = x)
  },
  add_solver = function(self, x) {
    assert(inherits(x, "Solver"))
    if (!is.Waiver(self$solver))
      cli::cli_warn("overwriting previously defined solver")
    pproto(NULL, self, solver = x)
  },
  add_targets = function(self, x) {
    assert(inherits(x, "Target"))
    if (!is.Waiver(self$targets))
      cli::cli_warn("overwriting previously defined targets")
    pproto(NULL, self, targets = x)
  },
  add_objective = function(self, x) {
    assert(inherits(x, "Objective"))
    if (!is.Waiver(self$objective))
      cli::cli_warn("overwriting previously defined objective")
    pproto(NULL, self, objective = x)
  },
  add_decisions = function(self, x) {
    assert(inherits(x, "Decision"))
    if (!is.Waiver(self$decisions))
      cli::cli_warn("overwriting previously defined decision")
    pproto(NULL, self, decisions = x)
  },
  add_constraint = function(self, x) {
    assert(inherits(x, "Constraint"))
    p <- pproto(NULL, self)
    p$constraints[[length(p$constraints) + 1]] <- x
    return(p)
  },
  add_penalty = function(self, x) {
    assert(inherits(x, "Penalty"))
    p <- pproto(NULL, self)
    p$penalties[[length(p$penalties) + 1]] <- x
    return(p)
  }
)

# internal cli functions
box_chars <- function() {
  if (cli::is_utf8_output()) {
    list(
      "h" = "\u2500",                   # horizontal
      "v" = "\u2502",                   # vertical
      "l" = "\u2514",                   # leaf
      "j" = "\u251C",                   # junction
      "b" = "\u2022"                   # bullet
    )
  } else {
    list(
      "h" = "-",                        # horizontal
      "v" = "|",                        # vertical
      "l" = "\\",                       # leaf
      "j" = "+",                        # junction
      "b" = "@"                         # bullet
    )
  }
}

cli_vtext <- function(..., .envir = parent.frame()) {
  cli::cli_verbatim(
    cli::format_inline(..., keep_whitespace = TRUE, .envir = .envir)
  )
}

cli_tree_component <- function(x,
                               header = "",
                               subheader = "",
                               padding = "",
                               width = 0,
                               .envir = parent.frame()) {
  ch <- box_chars()
  out <- paste0(
    padding, header, x[1]
  )
  if (length(x) > 1) {
    if (length(x) > 2) {
      out <- c(
        out,
        paste0(
          padding, subheader, ch$j, ch$b,
          paste(rep(" ", width), collapse = ""),
          x[c(-1, -length(x))]
        )
      )
    }
    out <- c(
      out,
      paste0(
        padding, subheader, ch$l, ch$b,
        paste(rep(" ", width), collapse = ""),
        x[length(x)]
      )
    )
  }
  for (x in out) cli_vtext(x, .envir = .envir)
  invisible(TRUE)
}

conservation_problem_theme <- function() {
  theme = list(
    .gray = list(color = "gray"),
    .val = list(digits = 4),
    .bg = list("font-weight" = "bold", color = "br_green"),
    .g = list(color = "br_green")
  )
}
