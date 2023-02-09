#' @include internal.R ConservationProblem-class.R OptimizationProblem-class.R compile.R
NULL

#' Presolve check
#'
#' Check a conservation planning [problem()] for potential issues
#' before trying to solve it. Specifically, problems are checked for (i) values
#' that are likely to result in "strange" solutions and (ii) values that are
#' likely to cause numerical instability issues and lead to unreasonably long
#' run times when solving it. Although these checks are provided to help
#' diagnose potential issues, please be aware that some detected issues may be
#' false positives. Please note that these checks will not be able to
#' verify if a problem has a feasible solution or not.
#'
#' @param x [problem()] or [optimization_problem()] object.
#'
#' @param warn `logical` should a warning be thrown if the presolve checks fail?
#'  Defaults to `TRUE`.
#'
#' @details This function checks for issues that are likely to result in
#'   "strange" solutions. Specifically, it checks if (i) all planning units are
#'   locked in, (ii) all planning units are locked out, and (iii) all
#'   planning units have negative cost values (after applying penalties if any
#'   were specified). Although such conservation planning problems
#'   are mathematically valid, they are generally the result of a coding mistake
#'   when building the problem (e.g., using an absurdly high
#'   penalty value or using the wrong dataset to lock in planning units).
#'   Thus such issues, if they are indeed issues and not false positives, can
#'   be fixed by carefully checking the code, data, and parameters used to build
#'   the conservation planning problem.
#'
#'   This function then checks for values that may lead to numerical instability
#'   issues when solving the problem. Specifically, it checks if the range of
#'   values in certain components of the optimization problem are over a
#'   certain threshold (i.e., \eqn{1 \times 10 ^9}{1e+9}) or if the values
#'   themselves exceed a certain threshold
#'   (i.e., \eqn{1 \times 10^{10}}{1e+10}).
#'   In most cases, such issues will simply cause an exact
#'   algorithm solver to take a very long time to generate a solution. In rare
#'   cases, such issues can cause incorrect calculations which can lead
#'   to exact algorithm solvers returning infeasible solutions
#'   (e.g., a solution to the minimum set problem where not all targets are met)
#'   or solutions that exceed the specified optimality gap (e.g., a suboptimal
#'   solution when a zero optimality gap is specified).
#'
#'   What can you do if a conservation planning problem fails to pass these
#'   checks? Well, this function will have thrown some warning messages
#'   describing the source of these issues, so read them carefully. For
#'   instance, a common issue is when a relatively large penalty value is
#'   specified for boundary ([add_boundary_penalties()]) or
#'   connectivity penalties ([add_connectivity_penalties()]). This
#'   can be fixed by trying a smaller penalty value. In such cases, the
#'   original penalty value supplied was so high that the optimal solution
#'   would just have selected every single planning unit in the solution---and
#'   this may not be especially helpful anyway (see below for example). Another
#'   common issue is that the
#'   planning unit cost values are too large. For example, if you express the
#'   costs of the planning units in terms of USD then you might have
#'   some planning units that cost over one billion dollars in large-scale
#'   planning exercises. This can be fixed by rescaling the values so that they
#'   are smaller (e.g., multiplying the values by a number smaller than one, or
#'   expressing them as a fraction of the maximum cost). Let's consider another
#'   common issue, let's pretend that you used habitat suitability models to
#'   predict the amount of suitable habitat
#'   in each planning unit for each feature. If you calculated the amount of
#'   suitable habitat in each planning unit in square meters then this
#'   could lead to very large numbers. You could fix this by converting
#'   the units from square meters to square kilometers or thousands of square
#'   kilometers. Alternatively, you could calculate the percentage of each
#'   planning unit that is occupied by suitable habitat, which will yield
#'   values between zero and one hundred.
#'
#'   But what can you do if you can't fix these issues by simply changing
#'   the penalty values or rescaling data? You will need to apply some creative
#'   thinking. Let's run through a couple of scenarios.
#'   Let's pretend that you have a few planning units that
#'   cost a billion times more than any other planning
#'   unit so you can't fix this by rescaling the cost values. In this case, it's
#'   extremely unlikely that these planning units will
#'   be selected in the optimal solution so just set the costs to zero and lock
#'   them out. If this procedure yields a problem with no feasible solution,
#'   because one (or several) of the planning units that you manually locked out
#'   contains critical habitat for a feature, then find out which planning
#'   unit(s) is causing this infeasibility and set its cost to zero. After
#'   solving the problem, you will need to manually recalculate the cost
#'   of the solutions but at least now you can be confident that you have the
#'   optimal solution. Now let's pretend that you are using the maximum features
#'   objective (i.e., [add_max_features_objective()]) and assigned some
#'   really high weights to the targets for some features to ensure that their
#'   targets were met in the optimal solution. If you set the weights for
#'   these features to one billion then you will probably run into numerical
#'   instability issues. Instead, you can calculate minimum weight needed to
#'   guarantee that these features will be represented in the optimal solution
#'   and use this value instead of one billion. This minimum weight value
#'   can be calculated as the sum of the weight values for the other features
#'   and adding a small number to it (e.g., 1). Finally, if you're running out
#'   of ideas for addressing numerical stability issues you have one remaining
#'   option: you can use the `numeric_focus` argument in the
#'   [add_gurobi_solver()] function to tell the solver to pay extra
#'   attention to numerical instability issues. This is not a free lunch,
#'   however, because telling the solver to pay extra attention to numerical
#'   issues can substantially increase run time. So, if you have problems that
#'   are already taking an unreasonable time to solve, then this will not help
#'   at all.
#'
#' @return A `logical` value indicating if all checks passed successfully.
#'
#' @seealso [problem()], [solve()], <https://www.gurobi.com/documentation/9.5/refman/numerics_gurobi_guidelines.html>.
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create minimal problem with no issues
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions()
#'
#' # run presolve checks
#' # note that no warning is thrown which suggests that we should not
#' # encounter any numerical stability issues when trying to solve the problem
#' print(presolve_check(p1))
#'
#' # create a minimal problem, containing cost values that are really
#' # high so that they could cause numerical instability issues when trying
#' # to solve it
#' sim_pu_raster2 <- sim_pu_raster
#' sim_pu_raster2[1] <- 1e+15
#' p2 <-
#'   problem(sim_pu_raster2, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions()
#'
#' # run presolve checks
#' # note that a warning is thrown which suggests that we might encounter
#' # some issues, such as long solve time or suboptimal solutions, when
#' # trying to solve the problem
#' print(presolve_check(p2))
#'
#' # create a minimal problem with connectivity penalties values that have
#' # a really high penalty value that is likely to cause numerical instability
#' # issues when trying to solve the it
#' cm <- adjacency_matrix(sim_pu_raster)
#' p3 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_connectivity_penalties(1e+15, data = cm) %>%
#'   add_binary_decisions()
#'
#' # run presolve checks
#' # note that a warning is thrown which suggests that we might encounter
#' # some numerical instability issues when trying to solve the problem
#' print(presolve_check(p3))
#'
#' # let's forcibly solve the problem using Gurobi and tell it to
#' # be extra careful about numerical instability problems
#' s3 <-
#'    p3 %>%
#'    add_gurobi_solver(numeric_focus = TRUE) %>%
#'    solve(force = TRUE)
#'
#' # plot solution
#' # we can see that all planning units were selected because the connectivity
#' # penalty is so high that cost becomes irrelevant, so we should try using
#' # a much lower penalty value
#' plot(s3, main = "solution", axes = FALSE)
#' }
#' @export
presolve_check <- function(x, warn = TRUE) {
  rlang::check_required(x)
  rlang::check_required(warn)
  UseMethod("presolve_check")
}

#' @rdname presolve_check
#' @method presolve_check ConservationProblem
#' @export
presolve_check.ConservationProblem <- function(x, warn = TRUE) {
  assert(is_conservation_problem(x))
  presolve_check(compile(x), warn = warn)
}

#' @rdname presolve_check
#' @method presolve_check OptimizationProblem
#' @export
presolve_check.OptimizationProblem <- function(x, warn = TRUE) {
  result <- internal_presolve_check(x)
  if (!isTRUE(result$pass) && isTRUE(warn)) {
    cli_warning(result$msg)
  }
  result$pass
}

internal_presolve_check <- function(x) {
  # assert argument is valid
  rlang::check_required(x)
  assert(inherits(x, "OptimizationProblem"), .internal = TRUE)

  # set thresholds
  upper_value <- 1e+6
  lower_value <- 1e-6

  # initialize output values
  pass <- TRUE
  msg1 <- c()
  msg2 <- c()

  # presolve checks
  ## check for non-standard input data
  ### check if all planning units locked out
  n_pu_vars <- x$number_of_planning_units() * x$number_of_zones()
  if (all(x$ub()[seq_len(n_pu_vars)] < 1e-5)) {
    pass <- FALSE
    msg2 <- c(
      msg2,
      c(
        "All planning units must not be locked out.",
        paste(
          "v" = "Maybe you made a mistake when using",
          "{.fn add_locked_out_constraints}?"
        ),
        ""
      )
    )
  }
  ### check if all planning units locked in
  if (all(x$lb()[seq_len(n_pu_vars)] > 0.9999)) {
    pass <- FALSE
    msg2 <- c(
      msg2,
      c(
        "x" = "All planning units must not be locked in.",
        ">" = paste(
          "Maybe you made a mistake when using",
          "{.fn add_locked_in_constraints}?"
        ),
        ""
      )
    )
  }

  ## check if budget exceeds total of planning unit costs
  r1 <- which(x$row_ids() == "budget")
  if (length(r1) > 0) {
    result <- vapply(r1, FUN.VALUE = logical(1), function(i) {
      sum(x$A()[i, ]) <= x$rhs()[i]
    })
    if (!all(r1)) {
      if (length(r1) == 1) {
        msg2 <- c(
          msg2,
          c(
            "x" = paste(
              "Budget is greater than the total cost of selecting",
              "all planning units."
            ),
            ">" = paste(
              "Maybe you made a mistake when setting the {.arg budget}",
              "in the objective function?"
            ),
            ""
          )
        )
      } else {
        msg2 <- c(
          msg2,
          c(
            "x" = paste(
              "One or more of the budget values is greater than the total cost",
              "of all planning units in a zone."
            ),
            ">" = cli::format_inline(
              paste(
                "Maybe you made a mistake when setting the {.arg budget}",
                "in the objective function?"
              )
            ),
            ""
          )
        )
      }
    }
  }

  ## check objective function
  #### check upper threshold
  r1 <- which(x$obj() > upper_value)
  r2 <- which(abs(x$obj()) > upper_value)
  if ((length(r1) > 0) || (length(r2) > 0)) {
    ### find names of decision variables in the problem which exceed thresholds
    pass <- FALSE
    n1 <- x$col_ids()[r1]
    n2 <- x$col_ids()[r2]
    ### throw warnings
    if (("pu" %in% n1) && (!"ac" %in% n2) && (!"b" %in% n2) && (!"c" %in% n2))
      msg1 <- c(
        msg1,
        c(
          "x" = paste(
            "Planning units must not have cost values that are too high",
            "(> 1e6)."
          ),
          ">" = paste(
            "Try re-scaling cost values",
            "(e.g., convert units from USD to millions of USD)."
          ),
          ""
        )
      )
    if ("spp_met" %in% n1)
      msg1 <- c(
        msg1,
        c(
          "x" = paste(
            "Features must not have target weight values that are too high",
            "(> 1e6)."
          ),
          ">" = "Try using lower values in {.fn add_feature_weights}.",
          ""
        )
      )
    if ("amount" %in% n1)
      msg1 <- c(
        msg1,
        c(
          "x" = paste(
            "Features must not have weight values that are too high",
            "(> 1e6)."
          ),
          ">" = "Try using lower values in {.fn add_feature_weights}."
        )
      )
    if ("branch_met" %in% n1)
      msg1 <- c(
        msg1,
        c(
          "x" = paste(
            "Features must not have branch lengths that are too high",
            "(> 1e6)."
          ),
          ">" = paste(
            "Try rescaling the phylogenetic tree data",
            "(e.g., convert units from years to millions of years)."
          ),
          ""
        )
      )
    if ("b" %in% n2)
      msg1 <- c(
        msg1,
        c(
          "x" = paste(
            "Multiplying the boundary length data by {.arg penalty}",
            "must not produce values that are too high",
            "(> 1e6)."
          ),
          ">" = paste(
            "Try using a smaller {.arg penalty} in",
            "{.fn add_boundary_penalties}."
          ),
          ""
        )
      )
    if ("c" %in% n2)
      msg1 <- c(
        msg1,
        c(
          "x" = paste(
            "Multiplying the connectivity data by {.arg penalty}",
            "must not produce values that are too high",
            "(> 1e6)."
          ),
          ">" = paste(
            "Try using a smaller {.arg penalty} in",
            "{.fn add_connectivity_penalties}."
          ),
          ""
        )
      )
    if ("ac" %in% n2)
      msg1 <- c(
        msg1,
        c(
          "x" = paste(
            "Multiplying the asymmetric connectivity data by {.arg penalty}",
            "must not produce values that are too high",
            "(> 1e6)."
          ),
          ">" = paste(
            "Try using a smaller {.arg penalty} in",
            "{.fn add_asym_connectivity_penalties}."
          ),
          ""
        )
      )
  }

  ## check rhs
  ### check upper threshold
  r <- which(x$rhs() > upper_value)
  if (length(r) > 0) {
    #### find names of constraints in the problem which exceed thresholds
    pass <- FALSE
    n <- x$row_ids()[r]
    #### throw warnings
    if ("budget" %in% n)
      msg1 <- c(
        msg1,
        c(
          "x" = "{.arg budget} must not be too high (> 1e6).",
          ">" = paste0(
            "Try re-scaling cost values",
            "(e.g., convert cost units from USD to millions of USD)."
          ),
          ""
        )
      )
    if ("spp_target" %in% n)
      msg1 <- c(
        msg1,
        c(
          "x" = paste(
            "Features must not have target values that are too high",
            "(> 1e6)."
          ),
          ">" = paste(
            "Try re-scaling the feature data",
            "(e.g., convert units from m{cli::symbol$sup_2} to",
            "km{cli::symbol$sup_2})."
          ),
          ""
        )
      )
  }
  ### check lower threshold
  r <- which((x$rhs() < lower_value) & (x$rhs() > 1e-300))
  if (length(r) > 0) {
    #### find names of constraints in the problem which exceed thresholds
    pass <- FALSE
    n <- x$row_ids()[r]
    ### throw warnings
    if ("budget" %in% n)
      msg2 <- c(
        msg2,
        c(
          "x" = "{.arg budget} is effectively {.val {0}} (due to rounding).",
          ">" = "This might prevent any planning units from being selected.",
          ""
        )
      )
    if ("spp_target" %in% n)
      msg2 <- c(
        msg2,
        c(
          "x" = paste(
            "Some features have targets that are effectively {.val {0}}",
            "(due to rounding)."
          ),
          ">" = paste(
            "This might cause the features to be",
            "under-represented by solutions."
          ),
          ""
        )
      )
  }

  ## check constraint matrix
  y <- as_Matrix(x$A(), "dgTMatrix")
  rownames(y) <- x$row_ids()
  colnames(y) <- x$col_ids()
  ### check upper threshold
  r1 <- which(y@x > upper_value)
  r2 <- which(abs(y@x) > upper_value)
  if ((length(r1) > 0) || ((length(r2) > 0))) {
    #### find names of constraints in the problem which exceed thresholds
    pass <- FALSE
    rn1 <- rownames(y)[y@i + 1][r1]
    rn2 <- rownames(y)[y@i + 1][r2]
    #### throw warnings
    if ("budget" %in% rn1)
      msg1 <- c(
        msg1,
        c(
          "x" = paste(
            "Planning units must not have cost values that are too high",
            "(> 1e6)."
          ),
          ">" = paste(
            "Try re-scaling cost data to different units",
            "(e.g., convert units from USD to millions of USD)"
          ),
          ""
        )
      )
    if ("n" %in% rn2)
      msg1 <- c(
        msg1,
        c(
          "x" = paste(
            "{.arg neighbors} must not be too high",
            "(> 1e6)."
          ),
          ">" = c(
            "Try setting a smaller number in {.fn add_neighbor_constraints}."
          ),
          ""
        )
      )
  }

  ## check feature data
  rij_cn_ids <- c("pu_ijz", "pu")
  rij_rn_ids <- c("spp_amount", "spp_target", "spp_present", "pu_ijz")
  rij <- y[
    which(rownames(y) %in% rij_rn_ids),
    which(colnames(y) %in% rij_cn_ids),
    drop = FALSE]
  ### check upper threshold
  if (any(rij@x > upper_value)) {
    pass <- FALSE
    msg1 <- c(
      msg1,
      c(
        "x" = paste(
          "Feature data in {.arg x} (specified via",
          "({.arg feature}, {.arg rij}, or {.arg rij_matrix}) must not",
          "be too high (> 1e6)."
        ),
        ">" = paste(
          "Try re-scaling them",
          "(e.g., convert units from m{cli::symbol$sup_2} to",
          "km{cli::symbol$sup_2})."
        ),
        ""
      )
    )
  }
  ### check lower threshold
  if (mean(Matrix::colSums(rij) <= lower_value) >= 0.5) {
    pass <- FALSE
    msg2 <- c(
      msg2,
      c(
        "x" = paste(
          "Most of the planning units do not have a single",
          "feature inside them."
        ),
        ">" = paste(
          "This indicates that more features are needed."
        ),
        ""
      )
    )
  }

  # prepare output message
  if (isTRUE(pass)) {
    msg <- c("v" = "Problem passed presolve checks.")
  } else {
    # construct message
    msg <- c("Problem failed presolve checks.", "")
    if (length(msg2) > 0) {
      msg <- c(
        msg,
        paste(
          "These checks indicate that solutions",
          "might not identify meaningful priority areas:"
        ),
        ""
      )
      msg <- c(msg, msg2)
    }
    if (length(msg1) > 0) {
      msg <- c(
        msg,
        paste(
          "These failures indicate that numerical",
          "issues could stall optimizer or produce incorrect results:"
        ),
        ""
      )
      msg <- c(msg, msg1)
    }
    msg = c(
      msg,
      "i" = "For more information, see {.fun presolve_check}."
    )
  }

  # return results
  list(pass = pass, msg = msg)
}
