#' @include internal.R waiver.R binary_stack.R category_layer.R category_vector.R
NULL

#' @export
if (!methods::isClass("MultiObjConservationProblem")) {
  methods::setOldClass("MultiObjConservationProblem")
}
NULL

#' Multi-objective conservation problem class
#'
#' @description
#' This class is used to represent multi-objective conservation planning
#' problems. It stores the data (e.g., planning units, and features) and
#' mathematical formulation (e.g., the objective, constraints,
#' and other design criteria) needed to generate prioritizations.
#' Most users should use [multi_problem()] to generate new
#' multi-objective conservation problem objects, and the functions distributed
#' with the package to interact
#' with them (e.g., [number_of_features()], [number_of_planning_units()]).
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name MultiObjConservationProblem-class
#'
#' @family classes
#'
#' @export
MultiObjConservationProblem <- R6::R6Class(
  "MultiObjConservationProblem",
  public = list(

    #' @field problems `list` containing [`ConservationProblem-class`] objects.
    problems = list(),

    #' @field defaults `list` indicating if other fields contain defaults.
    defaults = list(
      approach = TRUE,
      solver = TRUE
    ),

    #' @field approach [`MultiObjApproach-class`] object
    #' for specifying the multi-objective optimization appraoch.
    approach = new_waiver(),

    #' @field solver [`Solver-class`] object specifying the solver for
    #' generating solutions.
    solver = new_waiver(),

    #' @description
    #' Create a new multi-objective conservation problem object.
    #' @param problems `list` containing [`ConservationProblem-class`] objects.
    #' @return A new `MultiObjConservationProblem` object.
    initialize = function(problems) {
      self$problems <- problems
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
        "A multi-objective conservation problem ({.cls MultiObjConservationProblem})"
      )
      
      # problem names
      problem_names <- self$problem_names()
      
      # pre-compute values for data section
      if (is_spatially_explicit(self$problems[[problem_names[1]]]$data$cost)) {
        crs_text <- repr.crs(
          get_crs(self$problems[[problem_names[1]]]$data$cost))
        extent_text <- repr.bbox(
          sf::st_bbox(self$problems[[problem_names[1]]]$data$cost))
      } else {
        crs_text <- "{.gray NA}"
        extent_text <- "{.gray NA}"
      }
      
      # print data section
      cli::cli_text("{ch$j}{ch$b}{.h data}")
      if (self$number_of_zones() > 1) {
        cli_vtext(
          "{ch$v}{ch$j}{ch$b}zones:       ",
          repr.character(self$zone_names())
        )
      }
      cli_vtext("{ch$v}{ch$l}{ch$b}planning units:")
      cli_vtext(
        "{ch$v} {ch$j}{ch$b}data:       ",
        "{.cls ",
        self$planning_unit_class(), "} (",
        self$number_of_planning_units(),
        " total)"
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
      ## solver
      solver_text <- missing_text
      if (!is.Waiver(self$solver)) {
        solver_text <- self$solver$repr()
      }
      ## approach
      approach_text <- missing_text
      if (!is.null(self$approach) && !is.Waiver(self$approach)) {
        approach_text <- self$approach$repr()
      }
      ## decisions
      decisions_text <- missing_text
      if (!is.Waiver(self$problems[[problem_names[1]]]$decisions)) {
        decisions_text <- self$problems[[problem_names[1]]]$decisions$repr()
      }
      ## per-problem values
      problem_data <- lapply(problem_names, function(pname) {
        prob <- self$problems[[pname]]
        ## cost
        cost_text <- repr_cost(prob$planning_unit_costs())
        ## objective
        obj_text <- missing_text
        if (!is.Waiver(prob$objective)) {
          obj_text <- prob$objective$repr()
        }
        ## penalties
        penalties_text <- missing_text
        if (length(prob$penalties) > 0) {
          penalties_text <- vapply(
            prob$penalties,
            function(w) w$repr(),
            character(1)
          )
        }
        ## features
        feature_names <- prob$feature_names()
        feature_text <- if (length(feature_names) == 0) {
          missing_text
        } else if (length(feature_names) == 1) {
          paste0("\"", feature_names, "\"")
        } else {
          paste0("\"", feature_names[1], "\", \u2026 (", length(feature_names), " total)")
        }
        ## targets
        tgt_text <- missing_text
        if (!is.null(prob$targets) && !is.Waiver(prob$targets)) {
          tgt_text <- prob$targets$repr()
        }
        ## weights
        wgt_text <- missing_text
        if (!is.null(prob$weights) && !is.Waiver(prob$weights)) {
          wgt_text <- prob$weights$repr()
        }
        ## constraints
        constraints_text <- missing_text
        if (length(prob$constraints) > 0) {
          constraints_text <- vapply(
            prob$constraints,
            function(w) w$repr(),
            character(1)
          )
        }
        list(
          cost_text        = cost_text,
          obj_text         = obj_text,
          penalties_text   = penalties_text,
          feature_text     = feature_text,
          tgt_text         = tgt_text,
          wgt_text         = wgt_text,
          constraints_text = constraints_text
        )
      })
      names(problem_data) <- problem_names
      
      # print formulation section
      cli::cli_text("{ch$j}{ch$b}{.h formulation}")
      ## one block per problem
      for (pname in problem_names) {
        pd <- problem_data[[pname]]
        cli_vtext(
          "{ch$v}{ch$j}{ch$b}name:        ",
          pname
        )
        ## cost
        cli_vtext(
          "{ch$v}{ch$v}{ch$j}{ch$b}cost:       ",
          pd$cost_text
        )
        ## objective
        cli_vtext(
          "{ch$v}{ch$v}{ch$j}{ch$b}objective:  ",
          pd$obj_text
        )
        ## penalties
        if (length(pd$penalties_text) > 1 || pd$penalties_text[1] != missing_text) {
          cli_vtext("{ch$v}{ch$v}{ch$j}{ch$b}penalties:")
          for (i in seq_along(pd$penalties_text)) {
            if (i < length(pd$penalties_text)) {
              cli_vtext(
                "{ch$v}{ch$v}{ch$v}{ch$j}{ch$b}", i, ":         ",
                pd$penalties_text[[i]]
              )
            } else {
              cli_vtext(
                "{ch$v}{ch$v}{ch$v}{ch$l}{ch$b}", i, ":         ",
                pd$penalties_text[[i]]
              )
            }
          }
        } else {
          cli_vtext(
            "{ch$v}{ch$v}{ch$j}{ch$b}penalties:  ",
            pd$penalties_text
          )
        }
        ## features
        cli_vtext("{ch$v}{ch$v}{ch$j}{ch$b}features:")
        cli_vtext(
          "{ch$v}{ch$v}{ch$v}{ch$j}{ch$b}names:     ",
          pd$feature_text
        )
        cli_vtext(
          "{ch$v}{ch$v}{ch$v}{ch$j}{ch$b}targets:   ",
          pd$tgt_text
        )
        cli_vtext(
          "{ch$v}{ch$v}{ch$v}{ch$l}{ch$b}weights:   ",
          pd$wgt_text
        )
        ## note: features uses ch$j (not ch$l) above because constraints follows
        ## constraints
        if (length(pd$constraints_text) > 1 || pd$constraints_text[1] != missing_text) {
          cli_vtext("{ch$v}{ch$v}{ch$l}{ch$b}constraints:")
          for (i in seq_along(pd$constraints_text)) {
            if (i < length(pd$constraints_text)) {
              cli_vtext(
                "{ch$v}{ch$v} {ch$j}{ch$b}", i, ":         ",
                pd$constraints_text[[i]]
              )
            } else {
              cli_vtext(
                "{ch$v}{ch$v} {ch$l}{ch$b}", i, ":         ",
                pd$constraints_text[[i]]
              )
            }
          }
        } else {
          cli_vtext(
            "{ch$v}{ch$v}{ch$l}{ch$b}constraints:",
            pd$constraints_text
          )
        }
      }
      ## decisions
      cli_vtext(
        "{ch$v}{ch$l}{ch$b}decisions:   ",
        decisions_text
      )
      
      # print optimization section
      cli::cli_text("{ch$l}{ch$b}{.h optimization}")
      cli_vtext(
        " {ch$j}{ch$b}approach:    ",
        approach_text
      )
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
    #' Print concise information about the object.
    #' @return Invisible `TRUE`.
    print = function() {
      # define characters
      ch <- cli_box_chars()
      
      # create container
      div_id <- cli::cli_div(theme = cli_pkg_theme())
      
      # create header
      cli::cli_text(
        "A multi-objective conservation problem ({.cls MultiObjConservationProblem})"
      )
      
      # problem names
      problem_names <- self$problem_names()
      
      # pre-compute values for data section
      if (is_spatially_explicit(self$problems[[problem_names[1]]]$data$cost)) {
        crs_text <- repr.crs(
          get_crs(self$problems[[problem_names[1]]]$data$cost))
        extent_text <- repr.bbox(
          sf::st_bbox(self$problems[[problem_names[1]]]$data$cost))
      } else {
        crs_text <- "{.gray NA}"
        extent_text <- "{.gray NA}"
      }
      
      # print data section
      cli::cli_text("{ch$j}{ch$b}{.h data}")
      if (self$number_of_zones() > 1) {
        cli_vtext(
          "{ch$v}{ch$j}{ch$b}zones:       ",
          repr.character(self$zone_names())
        )
      }
      cli_vtext("{ch$v}{ch$l}{ch$b}planning units:")
      cli_vtext(
        "{ch$v} {ch$j}{ch$b}data:       ",
        "{.cls ",
        self$planning_unit_class(), "} (",
        self$number_of_planning_units(),
        " total)"
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
      ## solver
      solver_text <- missing_text
      if (!is.Waiver(self$solver)) {
        solver_text <- self$solver$repr()
      }
      ## approach
      approach_text <- missing_text
      if (!is.null(self$approach) && !is.Waiver(self$approach)) {
        approach_text <- self$approach$repr()
      }
      ## decisions
      decisions_text <- missing_text
      if (!is.Waiver(self$problems[[problem_names[1]]]$decisions)) {
        decisions_text <- self$problems[[problem_names[1]]]$decisions$repr()
      }
      ## per-problem values
      problem_data <- lapply(problem_names, function(pname) {
        prob <- self$problems[[pname]]
        ## objective
        obj_text <- missing_text
        if (!is.Waiver(prob$objective)) {
          obj_text <- prob$objective$repr()
        }
        ## penalties
        penalties_text <- missing_text
        if (length(prob$penalties) > 0) {
          penalties_text <- vapply(
            prob$penalties,
            function(w) w$repr(),
            character(1)
          )
        }
        ## features
        feature_names <- prob$feature_names()
        feature_text <- if (length(feature_names) == 0) {
          missing_text
        } else if (length(feature_names) == 1) {
          paste0("\"", feature_names, "\"")
        } else {
          paste0("\"", feature_names[1], "\", \u2026 (", length(feature_names), " total)")
        }
        ## targets
        tgt_text <- missing_text
        if (!is.null(prob$targets) && !is.Waiver(prob$targets)) {
          tgt_text <- prob$targets$repr()
        }
        ## weights
        wgt_text <- missing_text
        if (!is.null(prob$weights) && !is.Waiver(prob$weights)) {
          wgt_text <- prob$weights$repr()
        }
        list(
          obj_text       = obj_text,
          penalties_text = penalties_text,
          feature_text   = feature_text,
          tgt_text       = tgt_text,
          wgt_text       = wgt_text
        )
      })
      names(problem_data) <- problem_names
      ## constraints (collected across all problems)
      all_constraints <- unlist(
        lapply(self$problems, function(p) p$constraints),
        recursive = FALSE
      )
      constraints_text <- missing_text
      if (length(all_constraints) > 0) {
        constraints_text <- vapply(
          all_constraints,
          function(x) x$repr(),
          character(1)
        )
      }
      
      # print formulation section
      cli::cli_text("{ch$j}{ch$b}{.h formulation}")
      ## one block per problem
      for (pname in problem_names) {
        pd <- problem_data[[pname]]
        cli_vtext(
          "{ch$v}{ch$j}{ch$b}name:        ",
          pname
        )
        cli_vtext(
          "{ch$v}{ch$v}{ch$j}{ch$b}objective:  ",
          pd$obj_text
        )
        ## penalties
        if (length(pd$penalties_text) > 1 || pd$penalties_text[1] != missing_text) {
          cli_vtext("{ch$v}{ch$v}{ch$j}{ch$b}penalties:")
          for (i in seq_along(pd$penalties_text)) {
            if (i < length(pd$penalties_text)) {
              cli_vtext(
                "{ch$v}{ch$v}{ch$v}{ch$j}{ch$b}", i, ":         ",
                pd$penalties_text[[i]]
              )
            } else {
              cli_vtext(
                "{ch$v}{ch$v}{ch$v}{ch$l}{ch$b}", i, ":         ",
                pd$penalties_text[[i]]
              )
            }
          }
        } else {
          cli_vtext(
            "{ch$v}{ch$v}{ch$j}{ch$b}penalties:  ",
            pd$penalties_text
          )
        }
        ## features
        cli_vtext("{ch$v}{ch$v}{ch$l}{ch$b}features:")
        cli_vtext(
          "{ch$v}{ch$v} {ch$j}{ch$b}names:     ",
          pd$feature_text
        )
        cli_vtext(
          "{ch$v}{ch$v} {ch$j}{ch$b}targets:   ",
          pd$tgt_text
        )
        cli_vtext(
          "{ch$v}{ch$v} {ch$l}{ch$b}weights:   ",
          pd$wgt_text
        )
      }
      ## constraints
      if (length(all_constraints) > 0) {
        cli_vtext("{ch$v}{ch$j}{ch$b}constraints:")
        for (i in seq_along(constraints_text)) {
          if (i < length(constraints_text)) {
            cli_vtext(
              "{ch$v}{ch$v}{ch$j}{ch$b}", i, ": ",
              constraints_text[[i]]
            )
          } else {
            cli_vtext(
              "{ch$v}{ch$v}{ch$l}{ch$b}", i, ": ",
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
      cli::cli_text("{ch$l}{ch$b}{.h optimization}")
      cli_vtext(
        " {ch$j}{ch$b}approach:    ",
        approach_text
      )
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
      "{.cls MultiObjConservationProblem} object"
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
      self$problems[[1]]$number_of_planning_units()
    },

    #' @description
    #' Check if planning unit identifiers are equivalent to the planning
    #' unit indices? Only `FALSE` if the planning units are
    #' `data.frame` format.
    #' @return A `logical` value.
    is_ids_equivalent_to_indices = function() {
      self$problems[[1]]$is_ids_equivalent_to_indices()
    },

    #' @description
    #' Obtain the planning unit indices.
    #' @return An `integer` vector.
    planning_unit_indices = function() {
      self$problems[[1]]$planning_unit_indices()
    },

    #' @description
    #' Obtain the total unit identifiers.
    #' @return An `integer` vector.
    total_unit_ids = function() {
      self$problems[[1]]$total_unit_ids()
    },

    #' @description
    #' Convert total unit identifiers to indices.
    #' @param ids `integer` vector with planning unit identifiers.
    #' @return An `integer` vector.
    convert_total_unit_ids_to_indices = function(ids) {
      self$problems[[1]]$convert_total_unit_ids_to_indices(ids)
    },

    #' @description
    #' Obtain the planning unit indices that are associated with
    #' finite cost values.
    #' @return A `list` of `integer` vectors. Each `list` element corresponds to
    #' a different zone.
    planning_unit_indices_with_finite_costs = function() {
      self$problems[[1]]$planning_unit_indices_with_finite_costs()
    },

    #' @description
    #' Obtain the number of total units. The total units include all elements
    #' in the cost data
    #' (e.g., indices, rows, geometries, cells), including those with
    #' missing (`NA`) values.
    #' @return An `integer` value.
    number_of_total_units = function() {
      self$problems[[1]]$number_of_total_units()
    },

    #' @description
    #' Get planning unit class.
    #' @return A `character` value.
    planning_unit_class = function() {
      self$problems[[1]]$planning_unit_class()
    },

    #' @description
    #' Obtain the number of features.
    #' @return An `integer` value.
    number_of_features = function() {
      sum(
        vapply(
          self$problems,
          FUN.VALUE = numeric(1),
          function(x) x$number_of_features()
        )
      )
    },

    #' @description
    #' Obtain the names of the features.
    #' @return A `list` of `character` vectors.
    feature_names = function() {
      stats::setNames(
        lapply(
          self$problems,
          function(x) x$feature_names()
        ),
        self$problem_names()
      )
    },

    #' @description
    #' Obtain the number of problems.
    #' @return An `integer` value.
    number_of_problems = function() {
      length(self$problems)
    },

    #' @description
    #' Obtain the names of the problems.
    #' @return A `character` vector.
    problem_names = function() {
      names(self$problems)
    },

    #' @description
    #' Obtain the number of zones.
    #' @return An `integer` value.
    number_of_zones = function() {
      self$problems[[1]]$number_of_zones()
    },

    #' @description
    #' Obtain the zone names.
    #' @return A `character` vector.
    zone_names = function() {
      self$problems[[1]]$zone_names()
    },

    #' @description
    #' Create a new object with an approach added to the problem formulation.
    #' @param x [MultiObjApproach-class] object.
    #' @return An updated `MultiObjConservationProblem` object.
    add_approach = function(x) {
      assert(inherits(x, "MultiObjApproach"))
      p <- self$clone(deep = TRUE)
      if (!isTRUE(p$defaults$approach)) {
        cli_warning("Overwriting previously defined approach.")
      } else {
        p$defaults$portfolio <- FALSE
      }
      p$approach <- x
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
        cli_warning("Overwriting previously defined solver.")
      } else {
        p$defaults$solver <- FALSE
      }
      p$solver <- x
      p
    }

  )
)

#' New multi-objective conservation problem
#'
#' Create a new multi-objective conservation problem with defaults.
#'
#' @param problems `list` of [`ConservationProblem-class`] objects.ks
#'
#' @return A [`MultiObjConservationProblem-class`] object.
#'
#' @noRd
new_multi_obj_conservation_problem <- function(problems) {
  # assert valid arguments
  assert_required(problems)
  assert(
    is.list(problems),
    all_elements_inherit(problems, "ConservationProblem"),
    length(problems) >= 2,
    .internal = TRUE
  )

  # if needed, set default problem names
  if (is.null(names(problems))) {
    names(problems) <- paste("Objective", seq_along(problems)) # nocov
  }

  # create new multi objective conservation problem
  p <- MultiObjConservationProblem$new(problems = problems)

  # add defaults
  p <- suppressWarnings(add_default_solver(p))

  # enforce defaults
  p$defaults$approach <- TRUE
  p$defaults$solver <- TRUE

  # return result
  p
}
