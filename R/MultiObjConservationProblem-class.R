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
      
      # helper for alignment 
      pad <- function(x, width = 13) {
        paste0(x, strrep(" ", max(0, width - nchar(x))))
      }
      
      # problem names
      problem_names <- self$problem_names()
      
      # create header
      cli::cli_text(
        "A multi-objective conservation problem 
        ({.cls MultiObjConservationProblem})"
      )
      
      # pre-compute values for data section
      if (is_spatially_explicit(self$problems[[problem_names[1]]]$data$cost)) {
        crs_text <- repr.crs(get_crs(
          self$problems[[problem_names[1]]]$data$cost))
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
          "{ch$v}{ch$j}{ch$b}zones:        ",
          repr.character(self$zone_names())
        )
      }
      cli_vtext("{ch$v}{ch$l}{ch$b}planning units:")
      cli_vtext(
        "{ch$v} {ch$j}{ch$b}data:        ",
        "{.cls ",
        self$planning_unit_class(), "} (",
        self$number_of_planning_units(),
        " total)"
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
      
      # print problems section
      ## header
      cli::cli_text("{ch$j}{ch$b}{.h formulation}")
      ## objectives header 
      cli_vtext("{ch$v}{ch$j}{ch$b}", "objectives:")
      
      ## problems
      if (length(problem_names) > 0) {
        for (i in seq_along(problem_names)) {
          pname   <- problem_names[i]
          prob    <- self$problems[[pname]]
          is_last <- (i == length(problem_names))
          
          # first-level branch
          branch1 <- if (is_last) ch$l else ch$j
          
          # print name 
          cli_vtext(
            "{ch$v}{branch1}{ch$b}",
            pad("name:"),
            pname
          )
          
          # second-level prefix
          prefix_lvl2 <- paste0(ch$v, if (is_last) " " else ch$v)
          
          ## cost
          prefix_lvl3 <- paste0(prefix_lvl2, ch$v)
          
          cost_range <- range(prob$planning_unit_costs(), na.rm = TRUE)
          cost_text <- repr_cost(prob$planning_unit_costs())
          
          cli_vtext(
            "{prefix_lvl2}{ch$j}{ch$b}",
            pad("cost:"),
            cost_text
          )
          
          ## objective
          obj_text <- prob$objective$repr()
          
          cli_vtext(
            "{prefix_lvl2}{ch$j}{ch$b}",
            pad("objective:"),
            obj_text
          )
          
          ## penalties
          penalties_text <- missing_text
          if (length(prob$penalties) > 0) {
            penalties_text <- vapply(
              prob$penalties,
              function(w) w$repr(),
              character(1)
            )
          }
          
          if (length(prob$penalties) > 0) {
            cli_vtext("{prefix_lvl2}{ch$j}{ch$b}", pad("penalties:"))
            for (j in seq_along(penalties_text)) {
              branch <- if (j < length(penalties_text)) ch$j else ch$l
              cli_vtext(
                paste0(prefix_lvl2, ch$v, branch, ch$b),
                j, ":",
                paste(rep(" ", max(0, 11 - nchar(j))), collapse = ""),
                penalties_text[[j]]
              )
            }
          } else {
            cli_vtext(
              "{prefix_lvl2}{ch$j}{ch$b}",
              pad("penalties:"),
              penalties_text
            )
          }
          
          ## features 
          feature_names <- prob$feature_names()
          feature_text <- if (length(feature_names) == 0) {
            missing_text
          } else if (length(feature_names) == 1) {
            paste0("\"", feature_names, "\"")
          } else {
            paste0("\"", feature_names[1], "\", … (", length(feature_names), " total)")
          }
          
          cli_vtext(
            "{prefix_lvl2}{ch$j}{ch$b}",
            pad("features:"),
            feature_text
          )
          
          ## targets
          tgt_text <- "{.gray none specified}"
          if (!is.null(prob$targets) && !is.Waiver(prob$targets)) {
            tgt_text <- prob$targets$repr()
          }
          
          cli_vtext(
            "{prefix_lvl2}{ch$j}{ch$b}",
            pad("targets:"),
            tgt_text
          )
          
          ## weights
          wgt_text <- "{.gray none specified}"
          if (!is.null(prob$weights) && !is.Waiver(prob$weights)) {
            wgt_text <- prob$weights$repr()
          }
          
          cli_vtext(
            "{prefix_lvl2}{ch$j}{ch$b}",
            pad("weights:"),
            wgt_text
          )
          
          ## constraints 
          constraints_text <- missing_text
          if (length(prob$constraints) > 0) {
            constraints_text <- vapply(
              prob$constraints,
              function(w) w$repr(),
              character(1)
            )
          }
          
          if (length(prob$constraints) > 0) {
            cli_vtext("{prefix_lvl2}{ch$j}{ch$b}", pad("constraints:"))
            for (j in seq_along(constraints_text)) {
              branch <- if (j < length(constraints_text)) ch$j else ch$l
              cli_vtext(
                paste0(prefix_lvl2, ch$v, branch, ch$b),
                j, ":",
                paste(rep(" ", max(0, 11 - nchar(j))), collapse = ""),
                constraints_text[[j]]
              )
            }
          } else {
            cli_vtext(
              "{prefix_lvl2}{ch$j}{ch$b}",
              pad("constraints:"),
              constraints_text
            )
          }
        }
        
      } else {
        cli_vtext(
          "{ch$v}{ch$l}{ch$b}",
          "objectives:  ",
          missing_text
        )
      }
      
      ## decisions
      cli_vtext(
        "{ch$v}{ch$l}{ch$b}",
        pad("decisions:"),
        decisions_text
      )
      
      # print optimization section
      ## header 
      cli::cli_text("{ch$l}{ch$b}{.h optimization}")
      
      ## approach 
      cli_vtext(
        "{ch$v}{ch$j}{ch$b}",
        pad("approach:"),
        approach_text
      )
      
      ## solver
      cli_vtext(
        "{ch$v}{ch$l}{ch$b}",
        pad("solver:"),
        solver_text
      )
      
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
      
      # helper for alignment 
      pad <- function(x, width = 13) {
        paste0(x, strrep(" ", max(0, width - nchar(x))))
      }
      
      # problem names
      problem_names <- self$problem_names()
      
      # create header
      cli::cli_text(
        "A multi-objective conservation problem 
  ({.cls MultiObjConservationProblem})"
      )
      
      # pre-compute values for data section
      if (is_spatially_explicit(self$problems[[problem_names[1]]]$data$cost)) {
        crs_text <- repr.crs(get_crs(
          self$problems[[problem_names[1]]]$data$cost))
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
          "{ch$v}{ch$j}{ch$b}zones:        ",
          repr.character(self$zone_names())
        )
      }
      cli_vtext("{ch$v}{ch$l}{ch$b}planning units:")
      cli_vtext(
        "{ch$v} {ch$j}{ch$b}data:        ",
        "{.cls ",
        self$planning_unit_class(), "} (",
        self$number_of_planning_units(),
        " total)"
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
      
      # print problems section
      ## header
      cli::cli_text("{ch$j}{ch$b}{.h formulation}")
      ## objectives header 
      cli_vtext("{ch$v}{ch$j}{ch$b}", "objectives:")
      
      ## problems
      if (length(problem_names) > 0) {
        for (i in seq_along(problem_names)) {
          pname   <- problem_names[i]
          prob    <- self$problems[[pname]]
          is_last <- (i == length(problem_names))
          
          # first-level branch
          branch1 <- if (is_last) ch$l else ch$j
          
          # print name 
          cli_vtext(
            "{ch$v}{branch1}{ch$b}",
            pad("name:"),
            pname
          )
          
          # second-level prefix
          prefix_lvl2 <- paste0(ch$v, if (is_last) " " else ch$v)
          
          ## objective
          obj_text <- prob$objective$repr()
          
          cli_vtext(
            "{prefix_lvl2}{ch$j}{ch$b}",
            pad("objective:"),
            obj_text
          )
          
          ## penalties
          penalties_text <- missing_text
          if (length(prob$penalties) > 0) {
            penalties_text <- vapply(
              prob$penalties,
              function(w) w$repr(),
              character(1)
            )
          }
          
          if (length(prob$penalties) > 0) {
            cli_vtext("{prefix_lvl2}{ch$j}{ch$b}", pad("penalties:"))
            for (j in seq_along(penalties_text)) {
              branch <- if (j < length(penalties_text)) ch$j else ch$l
              cli_vtext(
                paste0(prefix_lvl2, ch$v, branch, ch$b),
                j, ":",
                paste(rep(" ", max(0, 11 - nchar(j))), collapse = ""),
                penalties_text[[j]]
              )
            }
          } else {
            cli_vtext(
              "{prefix_lvl2}{ch$j}{ch$b}",
              pad("penalties:"),
              penalties_text
            )
          }
          
          ## features 
          feature_names <- prob$feature_names()
          feature_text <- if (length(feature_names) == 0) {
            missing_text
          } else if (length(feature_names) == 1) {
            paste0("\"", feature_names, "\"")
          } else {
            paste0("\"", feature_names[1], "\", … (", length(feature_names), " total)")
          }
          
          cli_vtext(
            "{prefix_lvl2}{ch$j}{ch$b}",
            pad("features:"),
            feature_text
          )
          
          ## targets
          tgt_text <- "{.gray none specified}"
          if (!is.null(prob$targets) && !is.Waiver(prob$targets)) {
            tgt_text <- prob$targets$repr()
          }
          
          cli_vtext(
            "{prefix_lvl2}{ch$j}{ch$b}",
            pad("targets:"),
            tgt_text
          )
          
          ## weights
          wgt_text <- "{.gray none specified}"
          if (!is.null(prob$weights) && !is.Waiver(prob$weights)) {
            wgt_text <- prob$weights$repr()
          }
          
          cli_vtext(
            "{prefix_lvl2}{ch$l}{ch$b}",
            pad("weights:"),
            wgt_text
          )
        }
        
      } else {
        cli_vtext(
          "{ch$v}{ch$l}{ch$b}",
          "objectives:  ",
          missing_text
        )
      }
      
      ## constraints section 
      all_constraints <- unlist(
        lapply(self$problems, function(p) p$constraints),
        recursive = FALSE
      )
      
      constraints_text <- if (length(all_constraints) > 0) {
        vapply(all_constraints, function(x) x$repr(), character(1))
      } else {
        character(0)
      }
      
      cli_vtext("{ch$v}{ch$j}{ch$b}", "constraints:")
      
      if (length(constraints_text) > 0) {
        for (i in seq_along(constraints_text)) {
          branch <- if (i < length(constraints_text)) ch$j else ch$l
          cli_vtext(
            "{ch$v}{ch$v}{branch}{ch$b}",
            i, ":",
            paste(rep(" ", max(0, 11 - nchar(i))), collapse = ""),
            constraints_text[[i]]
          )
        }
      } else {
        cli_vtext(
          "{ch$v}{ch$v}{ch$l}{ch$b}",
          missing_text
        )
      }
      
      ## decisions
      cli_vtext(
        "{ch$v}{ch$l}{ch$b}",
        pad("decisions:"),
        decisions_text
      )
      
      # print optimization section
      ## header 
      cli::cli_text("{ch$l}{ch$b}{.h optimization}")
      
      ## approach 
      cli_vtext(
        "{ch$v}{ch$j}{ch$b}",
        pad("approach:"),
        approach_text
      )
      
      ## solver
      cli_vtext(
        "{ch$v}{ch$l}{ch$b}",
        pad("solver:"),
        solver_text
      )
      
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
