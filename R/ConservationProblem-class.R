#' @include internal.R waiver.R binary_stack.R category_layer.R category_vector.R
NULL

#' @export
if (!methods::isClass("ConservationProblem")) methods::setOldClass("ConservationProblem")
NULL

#' Conservation problem class
#'
#' @description
#' This class is used to represent conservation planning problems.
#' It stores the data (e.g., planning units, and features) and
#' mathematical formulation (e.g., the objective, constraints,
#' and other design criteria) needed to generate prioritizations.
#' Most users should use [problem()] to generate new conservation problem
#' objects, and the functions distributed with the package to interact
#' with them (e.g., [number_of_features()], [number_of_planning_units()]).
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name ConservationProblem-class
#'
#' @family classes
#'
#' @export
ConservationProblem <- R6::R6Class(
  "ConservationProblem",
  public = list(

    #' @field data `list` containing data (e.g., planning units, costs).
    data = list(),

    #' @field defaults `list` indicating if other fields contain defaults.
    defaults = list(
      objective = TRUE,
      decisions = TRUE,
      targets = TRUE,
      constraints = TRUE,
      penalties = TRUE,
      solver = TRUE
    ),

    #' @field objective [`Objective-class`] object specifying the objective
    #' function for the problem formulation.
    objective = new_waiver(),

    #' @field decisions [`Decision-class`] object specifying the decision types
    #' for the problem formulation.
    decisions = new_waiver(),

    #' @field targets [`Target-class`] object specifying the representation
    #' targets for the problem formulation.
    targets = new_waiver(),

    #' @field constraints `list` containing [`Constraint-class`] objects that
    #' specify constraints for the problem formulation.
    constraints = list(),

    #' @field penalties `list` containing [`Penalty-class`] objects that specify
    #' penalties for the problem formulation.
    penalties = list(),

    #' @field portfolio [`Portfolio-class`] object specifying the approach for
    #' generating multiple solutions.
    portfolio = new_waiver(),

    #' @field solver [`Solver-class`] object specifying the solver for
    #' generating solutions.
    solver = new_waiver(),

    #' @description
    #' Create a new conservation problem object.
    #' @param data `list` containing data
    #' @return A new `ConservationProblem` object.
    initialize = function(data = list()) {
      self$data <- data
    },

    #' @description
    #' Print extended information about the object.
    #' @return Invisible `TRUE`.
    summary = function() {
      # define characters
      ch <- cli_box_chars()

      # create container
      div_id <- cli::cli_div(theme = cli_pkg_theme())

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
      cost_text <- repr_cost(self$planning_unit_costs())

      # print data section
      cli::cli_text("{ch$j}{ch$b}{.h data}")
      if (self$number_of_zones() > 1) {
        cli_vtext(
          "{ch$v}{ch$j}{ch$b}zones:        ",
          repr.character(self$zone_names())
        )
      }
      cli_vtext(
        "{ch$v}{ch$j}{ch$b}features:     ",
        repr.character(self$feature_names())
      )
      cli_vtext("{ch$v}{ch$l}{ch$b}planning units:")
      cli_vtext(
        "{ch$v} {ch$j}{ch$b}data:        ",
        "{.cls ",
        self$planning_unit_class(), "} (",
        self$number_of_planning_units(),
        " total)"
      )
      cli_vtext(
        "{ch$v} {ch$j}{ch$b}costs:       ",
        cost_text
      )
      cli_vtext(
        "{ch$v} {ch$j}{ch$b}extent:      ",
        extent_text
      )
      cli_vtext(
        "{ch$v} {ch$l}{ch$b}CRS:         ",
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
      cli::cli_text("{ch$j}{ch$b}{.h formulation}")
      ## objective
      cli_tree_component(
        objective_text,
        header = "{ch$v}{ch$j}{ch$b}objective:    ",
        subheader = " {ch$v}",
        width = 15
      )
      ## penalties
      if (length(self$penalties) > 0) {
        cli_vtext("{ch$v}{ch$j}{ch$b}penalties:")
        for (i in seq_along(penalties_text)) {
          if (i < length(penalties_text)) {
            cli_tree_component(
              penalties_text[[i]],
              header = "{ch$v}{ch$j}{ch$b}             ",
              subheader = "{ch$v}{ch$v}",
              padding = "{ch$v}",
              width = 14
            )
          } else {
            cli_tree_component(
              penalties_text[[i]],
              header = "{ch$v}{ch$l}{ch$b}             ",
              subheader = "{ch$v} ",
              padding = "{ch$v}",
              width = 14
            )
          }
        }
      } else {
        cli_vtext(
          "{ch$v}{ch$j}{ch$b}penalties:    ",
          penalties_text
        )
      }
      ## targets
      cli_vtext(
       "{ch$v}{ch$j}{ch$b}targets:      ",
        targets_text
      )
      ## constraints
      if (length(self$constraints) > 0) {
        cli_vtext("{ch$v}{ch$j}{ch$b}constraints: ")
        for (i in seq_along(constraints_text)) {
          if (i < length(constraints_text)) {
            cli_tree_component(
              constraints_text[[i]],
              header = "{ch$v}{ch$j}{ch$b}             ",
              subheader = "{ch$v}{ch$v}",
              padding = "{ch$v}",
              width = 14
            )
          } else {
            cli_tree_component(
              constraints_text[[i]],
              header = "{ch$v}{ch$l}{ch$b}             ",
              subheader = "{ch$v} ",
              padding = "{ch$v}",
              width = 14
            )
          }
        }
      } else {
        cli_vtext(
          "{ch$v}{ch$j}{ch$b}constraints:  ",
          constraints_text
        )
      }
      ## decisions
      cli_tree_component(
        decisions_text,
        header = "{ch$v}{ch$l}{ch$b}decisions:    ",
        subheader = " {ch$v}",
        width = 15
      )

      # print optimization section
      ## header
      cli::cli_text("{ch$l}{ch$b}{.h optimization}")
      ## portfolio
      cli_tree_component(
        portfolio_text,
        header = " {ch$j}{ch$b}portfolio:    ",
        subheader = " {ch$v}",
        width = 15
      )
      # solver
      cli_tree_component(
        solver_text,
        header = " {ch$l}{ch$b}solver:       ",
        subheader = "  ",
        width = 15
      )

      # end container
      cli::cli_end(div_id)

      # return success
      invisible(TRUE)
    },

    #' @description
    #' Print concise information about the object.
    #' @return Invisible `TRUE`.
    print = function() {
      # define characters
      ch <- cli_box_chars()

      # create container
      div_id <- cli::cli_div(theme = cli_pkg_theme())

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
      cost_text <- repr_cost(self$planning_unit_costs())

      # print data section
      cli::cli_text("{ch$j}{ch$b}{.h data}")
      if (self$number_of_zones() > 1) {
        cli_vtext(
          "{ch$v}{ch$j}{ch$b}zones:       ",
          repr.character(self$zone_names())
        )
      }
      cli_vtext(
        "{ch$v}{ch$j}{ch$b}features:    ",
        repr.character(self$feature_names())
      )
      cli_vtext("{ch$v}{ch$l}{ch$b}planning units:")
      cli_vtext(
        "{ch$v} {ch$j}{ch$b}data:       ",
        "{.cls ",
        self$planning_unit_class(), "} (",
        self$number_of_planning_units(),
        " total)"
      )
      cli_vtext(
        "{ch$v} {ch$j}{ch$b}costs:      ",
        cost_text
      )
      cli_vtext(
        "{ch$v} {ch$j}{ch$b}extent:     ",
        extent_text
      )
      cli_vtext(
        "{ch$v} {ch$l}{ch$b}CRS:        ",
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
      cli::cli_text("{ch$j}{ch$b}{.h formulation}")
      ## objective
      cli_vtext(
        "{ch$v}{ch$j}{ch$b}objective:   ",
        objective_text
      )
      ## penalties
      if (length(self$penalties) > 0) {
        cli_vtext("{ch$v}{ch$j}{ch$b}penalties: ")
        for (i in seq_along(penalties_text)) {
          if (i < length(penalties_text)) {
            cli_vtext(
              "{ch$v}{ch$v}{ch$j}{ch$b}", i, ":",
              paste(rep(" ", max(0, 11 - nchar(i))), collapse = ""),
              penalties_text[[i]]
            )
          } else {
            cli_vtext(
              "{ch$v}{ch$v}{ch$l}{ch$b}", i, ":",
              paste(rep(" ", max(0, 11 - nchar(i))), collapse = ""),
              penalties_text[[i]]
            )
          }
        }
      } else {
        cli_vtext(
          "{ch$v}{ch$j}{ch$b}penalties:   ",
          penalties_text
        )
      }
      ## targets
      cli_vtext(
        "{ch$v}{ch$j}{ch$b}targets:     ",
        targets_text
      )
      ## constraints
      if (length(self$constraints) > 0) {
        cli_vtext("{ch$v}{ch$j}{ch$b}constraints: ")
        for (i in seq_along(constraints_text)) {
          if (i < length(constraints_text)) {
            cli_vtext(
              "{ch$v}{ch$v}{ch$j}{ch$b}", i, ":",
              paste(rep(" ", max(0, 11 - nchar(i))), collapse = ""),
              constraints_text[[i]]
            )
          } else {
            cli_vtext(
              "{ch$v}{ch$v}{ch$l}{ch$b}", i, ":",
              paste(rep(" ", max(0, 11 - nchar(i))), collapse = ""),
              constraints_text[[i]]
            )
          }
        }
      } else {
        cli_vtext(
          "{ch$v}{ch$j}{ch$b}constraints: ",
          constraints_text
        )
      }
      ## decisions
      cli_vtext(
        "{ch$v}{ch$l}{ch$b}decisions:   ",
        decisions_text
      )

      # print optimization section
      ## header
      cli::cli_text("{ch$l}{ch$b}{.h optimization}")
      ## portfolio
      cli_vtext(
        " {ch$j}{ch$b}portfolio:   ",
        portfolio_text
      )
      ## solver
      cli_vtext(
        " {ch$l}{ch$b}solver:      ",
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

    #' @description
    #' Display concise information about the object.
    #' @return Invisible `TRUE`.
    show = function() {
      self$print()
      invisible(TRUE)
    },


    #' @description
    #' Generate a character representation of the object.
    #' @return A `character` value.
    repr = function() {
      "{.cls ConservationProblem} object"
    },

    #' @description
    #' Get values stored in the `data` field.
    #' @param x `character` name of data.
    #' @return An object. If the `data` field does not contain an object
    #' associated with the argument to `x`, then a [new_waiver()] object is
    #' returned.
    get_data = function(x) {
      if (!x %in% names(self$data)) return(new_waiver())
      self$data[[x]]
    },

    #' @description
    #' Set values stored in the `data` field. Note that this method will
    #' overwrite existing data.
    #' @param x `character` name of data.
    #' @param value Object to store.
    #' @return Invisible `TRUE`.
    set_data = function(x, value) {
      self$data[[x]] <- value
      invisible(TRUE)
    },

    #' @description
    #' Obtain the number of planning units. The planning units correspond to
    #' elements in the cost data
    #' (e.g., indices, rows, geometries, cells) that have finite
    #' values in at least one zone. In other words, planning unit are
    #' elements in the cost data that do not have missing (`NA`) values in
    #' every zone.
    #' @return An `integer` value.
    number_of_planning_units = function() {
      length(self$planning_unit_indices())
    },

    #' @description
    #' Check if planning unit identifiers are equivalent to the planning
    #' unit indices? Only `FALSE` if the planning units are
    #' `data.frame` format.
    #' @return A `logical` value.
    is_ids_equivalent_to_indices = function() {
      self$get_data("is_ids_equivalent_to_indices")
    },

    #' @description
    #' Obtain the planning unit indices.
    #' @return An `integer` vector.
    planning_unit_indices = function() {
      self$get_data("planning_unit_indices")
    },

    #' @description
    #' Obtain the total unit identifiers.
    #' @return An `integer` vector.
    total_unit_ids = function() {
      v <- self$get_data("total_unit_ids")
      assert(
        is.numeric(v),
        msg = "no total unit identifiers defined.",
        .internal = TRUE
      )
      v
    },

    #' @description
    #' Convert total unit identifiers to indices.
    #' @param ids `integer` vector with planning unit identifiers.
    #' @return An `integer` vector.
    convert_total_unit_ids_to_indices = function(ids) {
      # if planning unit identifiers are sequential integers,
      # then just return the ids because they are equivalent to indices
      if (self$is_ids_equivalent_to_indices()) {
        return(ids)
      }
      # extract the identifiers
      total_ids <- self$get_data("total_unit_ids")
      # convert identifiers to indices
      out <- match(ids, total_ids)
      # return result
      out
    },

    #' @description
    #' Obtain the planning unit indices that are associated with
    #' finite cost values.
    #' @return A `list` of `integer` vectors. Each `list` element corresponds to
    #' a different zone.
    planning_unit_indices_with_finite_costs = function() {
      i <- self$get_data("planning_unit_indices_with_finite_costs")
      if (!is.Waiver(i)) return(i)
      self$set_planning_unit_indices_with_finite_costs()
      self$get_data("planning_unit_indices_with_finite_costs")
    },

    #' @description
    #' Perform calculations to cache the planning unit indices that are
    #' associated with finite cost values.
    #' @return Invisible `TRUE`.
    set_planning_unit_indices_with_finite_costs = function() {
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
        # nocov start
        cli::cli_abort("$data$cost is of unknown class", .internal = TRUE)
        # nocov end
      }
      names(x) <- self$zone_names()
      self$set_data("planning_unit_indices_with_finite_costs", x)
      invisible(TRUE)
    },

    #' @description
    #' Obtain the number of total units. The total units include all elements
    #' in the cost data
    #' (e.g., indices, rows, geometries, cells), including those with
    #' missing (`NA`) values.
    #' @return An `integer` value.
    number_of_total_units = function() {
      if (inherits(self$data$cost, "Raster")) {
        return(raster::ncell(self$data$cost))
      } else if (inherits(self$data$cost, "SpatRaster")) {
        return(terra::ncell(self$data$cost))
      } else if (inherits(self$data$cost, c("data.frame", "Spatial", "sf"))) {
        return(nrow(self$data$cost))
      } else if (is.matrix(self$data$cost)) {
        return(nrow(self$data$cost))
      } else {
        # nocov start
        cli::cli_abort("$data$cost is of unknown class", .internal = TRUE)
        # nocov end
      }
    },

    #' @description
    #' Obtain the planning unit costs.
    #' @return A `numeric` matrix.
    planning_unit_costs = function() {
      i <- self$get_data("planning_unit_costs")
      if (!is.Waiver(i)) return(i)
      self$set_planning_unit_costs()
      self$get_data("planning_unit_costs")
    },

    #' @description
    #' Get planning unit class.
    #' @return A `character` value.
    planning_unit_class = function() {
      class(self$data$cost)[[1]]
    },

    #' @description
    #' Perform calculations to cache the planning unit costs.
    #' @return Invisible `TRUE`.
    set_planning_unit_costs = function() {
      idx <- self$planning_unit_indices()
      if (inherits(self$data$cost, "Raster")) {
        if (raster::nlayers(self$data$cost) == 1) {
          x <- matrix(self$data$cost[idx], ncol = 1)
        } else {
          x <- self$data$cost[idx]
        }
      } else if (inherits(self$data$cost, "SpatRaster")) {
        x <- as.matrix(self$data$cost[idx])
      } else if (inherits(self$data$cost, "sf")) {
        x <- sf::st_drop_geometry(self$data$cost)
        x <- as.matrix(x[idx, self$data$cost_column, drop = FALSE])
      } else if (inherits(self$data$cost, c("Spatial", "data.frame"))) {
        x <- as.data.frame(self$data$cost)
        x <- as.matrix(x[idx, self$data$cost_column, drop = FALSE])
      } else if (is.matrix(self$data$cost)) {
        x <- self$data$cost[idx, , drop = FALSE]
      } else {
        # nocov start
        cli::cli_abort("$data$cost is of unknown class", .internal = TRUE)
        # nocov end
      }
      colnames(x) <- self$zone_names()
      self$set_data("planning_unit_costs", x)
      invisible()
    },

    #' @description
    #' Obtain the number of features.
    #' @return An `integer` value.
    number_of_features = function() {
      if (inherits(self$data$features, "ZonesCharacter")) {
        return(length(self$data$features[[1]]))
      } else if (inherits(self$data$features, "ZonesRaster")) {
        return(raster::nlayers(self$data$features[[1]]))
      } else if (inherits(self$data$features, "ZonesSpatRaster")) {
        return(terra::nlyr(self$data$features[[1]]))
      } else if (inherits(self$data$features, "data.frame")) {
        return(nrow(self$data$features))
      } else {
        # nocov start
        cli::cli_abort(
          "$features is not a recognized class", .internal = TRUE
        )
        # nocov end
      }
    },

    #' @description
    #' Obtain the names of the features.
    #' @return A `character` vector.
    feature_names = function() {
      if (inherits(self$data$features, "Zones")) {
        return(attr(self$data$features, "feature_names"))
      } else if (inherits(self$data$features, "data.frame")) {
        return(as.character(self$data$features$name))
      } else {
        # nocov start
        cli::cli_abort(
          "$features is not a recognized class", .internal = TRUE
        )
        # nocov end
      }
    },

    #' @description
    #' Obtain the resolution of the features in
    #' \ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}}.
    #' Note that if the features do not have spatial information,
    #' then a missing (`NA`) value is returned.
    #' @return A `numeric` value.
    feature_conversion_factor = function() {
      if (inherits(self$data$features, "ZonesSpatRaster")) {
        prod(terra::res(self$data$features[[1]]))
      } else if (inherits(self$data$features, "SpatRaster")) {
        prod(terra::res(self$data$features))
      } else if (inherits(self$data$features, "ZonesRaster")) {
        prod(raster::res(self$data$features[[1]]))
      } else if (inherits(self$data$features, "Raster")) {
        prod(raster::res(self$data$features))
      } else {
        return(NA_real_)
      }
    },

    #' @description
    #' Obtain the abundance of the features in the planning units.
    #' @return A `numeric` matrix. Each column corresponds to a different zone
    #' and each row corresponds to a different feature.
    feature_abundances_in_planning_units = function() {
      x <- self$get_data("feature_abundances_in_planning_units")
      if (!is.Waiver(x)) return(x)
      self$set_feature_abundances_in_planning_units()
      self$get_data("feature_abundances_in_planning_units")
    },

    #' @description
    #' Perform calculations to cache the abundance of the features in the
    #' planning units.
    #' @return Invisible `TRUE`.
    set_feature_abundances_in_planning_units = function() {
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
      invisible(TRUE)
    },

    #' @description
    #' Obtain the positive abundance of the features in the planning units.
    #' Note that this method, unlike `feature_abundances_in_planning_units`,
    # is only calculated using positive values.
    #' @return A `numeric` matrix. Each column corresponds to a different zone
    #' and each row corresponds to a different feature.
    feature_positive_abundances_in_planning_units = function() {
      x <- self$get_data("feature_positive_abundances_in_planning_units")
      if (!is.Waiver(x)) return(x)
      self$set_feature_positive_abundances_in_planning_units()
      self$get_data("feature_positive_abundances_in_planning_units")
    },

    #' @description
    #' Perform calculations to cache the positive abundance of the features in
    #' the planning units.
    #' @return Invisible `TRUE`.
    set_feature_positive_abundances_in_planning_units = function() {
      pu_indices <- self$planning_unit_indices()
      ind <- self$planning_unit_indices_with_finite_costs()
      ind <- lapply(ind, function(x) match(x, pu_indices))
      out <- vapply(
        seq_along(self$data$rij_matrix),
        FUN.VALUE = numeric(nrow(self$data$rij_matrix[[1]])),
        function(i) {
          m <- self$data$rij_matrix[[i]][, ind[[i]], drop = FALSE]
          m@x <- pmax(m@x, 0)
          Matrix::rowSums(m, na.rm = TRUE)
        }
      )
      if (!is.matrix(out))
        out <- matrix(out, ncol = self$number_of_zones())
      colnames(out) <- self$zone_names()
      rownames(out) <- self$feature_names()
      self$set_data("feature_positive_abundances_in_planning_units", out)
      invisible(TRUE)
    },

    #' @description
    #' Obtain the abundance of the features in the total units.
    #' @return A `numeric` matrix. Each column corresponds to a different zone
    #' and each row corresponds to a different feature.
    feature_abundances_in_total_units = function() {
      self$data$feature_abundances_in_total_units
    },

    #' @description
    #' Obtain the units of the features.
    #' @return A `character` value. Each element corresponds to a different
    #' feature.
    feature_units = function() {
      self$data$feature_units
    },

    #' @description
    #' Obtain the abundance of the features in area-based units of
    #' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
    #' @details Note that if a feature has missing (`NA`) units
    #' then then missing values are returned.
    #' @return A `numeric` matrix. Each column corresponds to a different zone
    #' and each row corresponds to a different feature.
    feature_abundances_km2_in_total_units = function() {
      x <- self$get_data("feature_abundances_km2_in_total_units")
      if (!is.Waiver(x)) return(x)
      self$set_feature_abundances_km2_in_total_units()
      self$get_data("feature_abundances_km2_in_total_units")
    },

    #' @description
    #' Perform calculations to cache the abundance of the features in
    #' area-based units of \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
    #' @return Invisible `TRUE`.
    set_feature_abundances_km2_in_total_units = function() {
      # get total units
      fa <- self$feature_abundances_in_total_units()
      # process data depending on feature data format
      ft <- self$get_data("features")
      if (inherits(ft, c("ZonesRaster", "ZonesSpatRaster"))) {
        ## if raster data...
        if (inherits(ft, "ZonesSpatRaster")) {
          ### extract cell res
          cell_res <- terra::res(ft[[1]])
        } else {
          ### extract cell res
          cell_res <- raster::res(ft[[1]])
        }
        ### extract cell unit
        cell_unit <- units::deparse_unit(get_crs(ft)$ud_unit)
        ### if cell unit is degree, then throw error
        assert(
          !identical(cell_unit, units::deparse_unit(sf::st_crs(4326)$ud_unit)),
          msg = "{.arg features} has a geodetic coordinate reference system.",
          .internal = TRUE,
          call = NULL
        )
        ### if cell unit is NA or CRS is NA, then throw error
        assert(
          assertthat::noNA(cell_unit),
          get_crs(ft) != sf::st_crs(NA),
          get_crs(ft) != sf::st_crs(na_crs),
          msg = "{.arg features} has missing coordinate reference system.",
          .internal = TRUE,
          call = NULL
        )
        ### calculate conversion factor
        cell_factor <- as.numeric(
          units::set_units(
            units::set_units(cell_res[[1]], cell_unit, mode = "standard") *
            units::set_units(cell_res[[2]], cell_unit, mode = "standard"),
          "km^2"
          )
        )
        ### convert values
        out <- fa * cell_factor
      } else {
        ## if not raster data..
        ### get feature units
        fu <- self$feature_units()
        ## convert values
        out <- fa * NA_real_
        for (i in seq_len(nrow(out))) {
          # if fu[[i]] is not NA, then convert it
          if (!is.na(fu[[i]])) {
            out[i, ] <- as_km2(fa[i, ], fu[[i]])
          }
        }
      }
      # store result
      self$set_data("feature_abundances_km2_in_total_units", out)
      invisible(TRUE)
    },

    #' @description
    #' Obtain the representation targets for the features.
    #' @return A [tibble::tibble()] data frame.
    feature_targets = function() {
      if (is.Waiver(self$targets))
        cli::cli_abort("Targets have not been specified.", call = NULL)
      self$targets$output(self)
    },

    #' @description
    #' See if the feature data contain any negative values.
    #' @return A `logical` value.
    has_negative_feature_data = function() {
      isTRUE(
        min(
          vapply(
            self$data$rij_matrix,
            FUN.VALUE = numeric(1),
            function(x) min(x@x, na.rm = TRUE)
          )
        ) < 0
      )
    },

    #' @description
    #' Obtain the number of zones.
    #' @return An `integer` value.
    number_of_zones = function() {
      if (inherits(self$data$features, "Zones")) {
        return(length(self$data$features))
      } else if (inherits(self$data$features, "data.frame")) {
        return(length(self$data$rij_matrix))
      } else {
        # nocov start
        cli::cli_abort(
          "$features is not a recognized class", .internal = TRUE
        )
        # nocov end
      }
    },

    #' @description
    #' Obtain the zone names.
    #' @return A `character` vector.
    zone_names = function() {
      if (inherits(self$data$features, "Zones")) {
        return(attr(self$data$features, "zone_names"))
      } else if (inherits(self$data$features, "data.frame")) {
        return(names(self$data$rij_matrix))
      } else {
        # nocov start
        cli::cli_abort(
          "$features is not a recognized class", .internal = TRUE
        )
        # nocov end
      }
    },

    #' @description
    #' Create a new object with a portfolio added to the problem formulation.
    #' @param x [Portfolio-class] object.
    #' @return An updated `ConservationProblem` object.
    add_portfolio = function(x) {
      assert(inherits(x, "Portfolio"))
      p <- self$clone(deep = TRUE)
      if (!isTRUE(p$defaults$portfolio)) {
        cli_warning("Overwriting previously defined portfolio.", call = NULL)
      } else {
        p$defaults$portfolio <- FALSE
      }
      p$portfolio <- x
      p
    },

    #' @description
    #' Create a new object with a solver added to the problem formulation.
    #' @param x [Solver-class] object.
    #' @return An updated `ConservationProblem` object.
    add_solver = function(x) {
      assert(inherits(x, "Solver"))
      p <- self$clone(deep = TRUE)
      if (!isTRUE(p$defaults$solver)) {
        cli_warning("Overwriting previously defined solver.", call = NULL)
      } else {
        p$defaults$solver <- FALSE
      }
      p$solver <- x
      p
    },

    #' @description
    #' Create a new object with targets added to the problem formulation.
    #' @param x [Target-class] object.
    #' @return An updated `ConservationProblem` object.
    add_targets = function(x) {
      assert(inherits(x, "Target"))
      p <- self$clone(deep = TRUE)
      if (!isTRUE(p$defaults$targets)) {
        cli_warning("Overwriting previously defined targets.", call = NULL)
      } else {
        p$defaults$targets <- FALSE
      }
      p$targets <- x
      p
    },

    #' @description
    #' Create a new object with an objective added to the problem formulation.
    #' @param x [Objective-class] object.
    #' @return An updated `ConservationProblem` object.
    add_objective = function(x) {
      assert(inherits(x, "Objective"))
      p <- self$clone(deep = TRUE)
      if (!isTRUE(p$defaults$objective)) {
        cli_warning("Overwriting previously defined objective.", call = NULL)
      } else {
        p$defaults$objective <- FALSE
      }
      p$objective <- x
      p
    },

    #' @description
    #' Create a new object with decisions added to the problem formulation.
    #' @param x [Decision-class] object.
    #' @return An updated `ConservationProblem` object.
    add_decisions = function(x) {
      assert(inherits(x, "Decision"))
      p <- self$clone(deep = TRUE)
      if (!isTRUE(p$defaults$decisions)) {
        cli_warning("Overwriting previously defined decision.", call = NULL)
      } else {
        p$defaults$decisions <- FALSE
      }
      p$decisions <- x
      p
    },

    #' @description
    #' Create a new object with a constraint added to the problem formulation.
    #' @param x [Constraint-class] object.
    #' @return An updated `ConservationProblem` object.
    add_constraint = function(x) {
      assert(inherits(x, "Constraint"))
      p <- self$clone(deep = TRUE)
      p$constraints[[length(p$constraints) + 1]] <- x
      p
    },

    #' @description
    #' Create a new object with a penalty added to the problem formulation.
    #' @param x [Penalty-class] object.
    #' @return An updated `ConservationProblem` object.
    add_penalty = function(x) {
      assert(inherits(x, "Penalty"))
      p <- self$clone(deep = TRUE)
      p$penalties[[length(p$penalties) + 1]] <- x
      p
    },

    #' @description
    #' Create a new object without any penalties.
    #' @param retain `character` vector of classes to retain. Defaults to
    #' `NULL` such that all penalties are excluded.
    #' @return An updated `ConservationProblem` object.
    remove_all_penalties = function(retain = NULL) {
      p <- self$clone(deep = TRUE)
      if (!is.null(retain)) {
        assert(is.character(retain))
        idx <- which(vapply(p$penalties, inherits, logical(1), retain))
        p$penalties <- p$penalties[idx]
      } else {
        p$penalties <- list()
      }
      p
    }
  )
)

#' New conservation problem
#'
#' Create a new conservation problem with defaults.
#'
#' @param data `list` with data. Defaults to empty `list`.
#'
#' @return A [`ConservationProblem-class`] object.
#'
#' @noRd
conservation_problem <- function(data = list()) {
  # assert valid arguments
  assert(is.list(data))

  # create new problem
  p <- ConservationProblem$new(data = data)

  # add defaults
  p <- suppressWarnings(add_default_portfolio(p))
  p <- suppressWarnings(add_binary_decisions(p))
  p <- suppressWarnings(add_default_solver(p))

  # enforce defaults
  p$defaults$portfolio <- TRUE
  p$defaults$decisions <- TRUE
  p$defaults$solver <- TRUE

  # return result
  p
}
