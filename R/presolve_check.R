#' @include internal.R ConservationProblem-proto.R OptimizationProblem-proto.R compile.R
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
#' verify if  a problem has a feasible solution or not.
#'
#' @param x [problem()] (i.e. [`ConservationProblem-class`]) or
#'   [`OptimizationProblem-class`] object.
#'
#' @details This function checks for issues that are likely to result in
#'   "strange" solutions. Specifically, it checks if (i) all planning units are
#'   locked in, (ii) all planning units are locked out, and (iii) all
#'   planning units have negative cost values (after applying penalties if any
#'   were specified). Although such conservation planning problems
#'   are mathematically valid, they are generally the result of a coding mistake
#'   when building the problem (e.g. using an absurdly high
#'   penalty value or using the wrong dataset to lock in planning units).
#'   Thus such issues, if they are indeed issues and not false positives, can
#'   be fixed by carefully checking the code, data, and parameters used to build
#'   the conservation planning problem.
#'
#'   This function then checks for values that may lead to numerical instability
#'   issues when solving the problem. Specifically, it checks if the range of
#'   values in certain components of the optimization problem are over a
#'   certain threshold (i.e. \eqn{1 \times 10 ^9}{1e+9}) or if the values
#'   themselves exceed a certain threshold
#'   (i.e. \eqn{1 \times 10^{10}}{1e+10}).
#'   In most cases, such issues will simply cause an exact
#'   algorithm solver to take a very long time to generate a solution. In rare
#'   cases, such issues can cause incorrect calculations which can lead
#'   to exact algorithm solvers returning infeasible solutions
#'   (e.g. a solution to the minimum set problem where not all targets are met)
#'   or solutions that exceed the specified optimality gap (e.g. a suboptimal
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
#'   are smaller (e.g. multiplying the values by a number smaller than one, or
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
#'   objective (i.e. [add_max_features_objective()]) and assigned some
#'   really high weights to the targets for some features to ensure that their
#'   targets were met in the optimal solution. If you set the weights for
#'   these features to one billion then you will probably run into numerical
#'   instability issues. Instead, you can calculate minimum weight needed to
#'   guarantee that these features will be represented in the optimal solution
#'   and use this value instead of one billion. This minimum weight value
#'   can be calculated as the sum of the weight values for the other features
#'   and adding a small number to it (e.g. 1). Finally, if you're running out
#'   of ideas for addressing numerical stability issues you have one remaining
#'   option: you can use the `numeric_focus` argument in the
#'   [add_gurobi_solver()] function to tell the solver to pay extra
#'   attention to numerical instability issues. This is not a free lunch,
#'   however, because telling the solver to pay extra attention to numerical
#'   issues can substantially increase run time. So, if you have problems that
#'   are already taking an unreasonable time to solve, then this will not help
#'   at all.
#'
#' @return `logical` value indicating if all checks are passed
#'   successfully.
#'
#' @seealso [problem()], [solve()], <https://www.gurobi.com/documentation/9.1/refman/numerics_gurobi_guidelines.html>.
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create minimal problem with no issues
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions()
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
#' p2 <- problem(sim_pu_raster2, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions()
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
#' p3 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_connectivity_penalties(1e+15, data = cm) %>%
#'       add_binary_decisions()
#'
#' # run presolve checks
#' # note that a warning is thrown which suggests that we might encounter
#' # some numerical instability issues when trying to solve the problem
#' print(presolve_check(p3))
#' \dontrun{
#' # let's forcibly solve the problem using Gurobi and tell it to
#' # be extra careful about numerical instability problems
#' s3 <- p3 %>%
#'       add_gurobi_solver(numeric_focus = TRUE) %>%
#'       solve(force = TRUE)
#'
#' # plot solution
#' # we can see that all planning units were selected because the connectivity
#' # penalty is so high that cost becomes irrelevant, so we should try using
#' # a much lower penalty value
#' plot(s3, main = "solution", axes = FALSE, box = FALSE)
#' }
#' @export
presolve_check <- function(x) UseMethod("presolve_check")

#' @rdname presolve_check
#' @method presolve_check ConservationProblem
#' @export
presolve_check.ConservationProblem <- function(x) {
  assertthat::assert_that(inherits(x, "ConservationProblem"))
  presolve_check(compile(x))
}

#' @rdname presolve_check
#' @method presolve_check OptimizationProblem
#' @export
presolve_check.OptimizationProblem <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "OptimizationProblem"))
  # define helper functions
  ratio <- function(x) {
    x <- abs(x)
    x / min(x[x != 0])
  }
  # set thresholds
  threshold_ratio <- 1e+9
  threshold_value <- 1e+10
  # set output value
  out <- TRUE
  # check if all planning units locked out
  n_pu_vars <- x$number_of_planning_units() * x$number_of_zones()
  if (all(x$ub()[seq_len(n_pu_vars)] < 1e-5)) {
    out <- FALSE
    warning("all planning units locked out", immediate. = TRUE)
  }
  # check if all planning units locked in
  if (all(x$lb()[seq_len(n_pu_vars)] > 0.9999)) {
    out <- FALSE
    warning("all planning units locked in", immediate. = TRUE)
  }
  # check if all planning units have negative costs
  if (((x$modelsense() == "min") && all(x$obj()[seq_len(n_pu_vars)] < 0)) ||
      ((x$modelsense() == "max") && all(x$obj()[seq_len(n_pu_vars)] > 0))) {
    out <- FALSE
    warning(paste("all planning units have negative values, may be due to",
                  "(relatively) high penalty values"),
            immediate. = TRUE)
  }
  # scan for values with really large ratio to smallest value
  ## objective function
  r <- which((ratio(x$obj()) > threshold_ratio)  |
              (abs(x$obj()) > threshold_value))
  if (length(r) > 0) {
    ### find names of decision variables in the problem which exceed thresholds
    out <- FALSE
    n <- x$col_ids()[r]
    ### throw warnings
    if (("pu" %in% n))
      warning(paste("planning units with very high (> 1e+6) or very low",
                    "(< 1e-6) non-zero cost values note this may be a false",
                    "positive"),
              immediate. = TRUE)
    if ("spp_met" %in% n)
      warning(paste0("feature targets with very high target weights (> 1e+6)"),
              immediate. = TRUE)
    if ("amount" %in% n)
      warning("features with (relatively) very high weights (> 1e+6)",
              immediate. = TRUE)
    if ("branch_met" %in% n)
      warning("features with (relatively) very large branch lengths (> 1e+6)",
              immediate. = TRUE)
    if ("b" %in% n)
      warning("penalty multiplied boundary lengths are very high",
              immediate. = TRUE)
    if ("c" %in% n)
      warning(paste("penalty multiplied connectivity values are very high"),
              immediate. = TRUE)
  }
  ## rhs
  r <- which((ratio(x$rhs()) > threshold_ratio) |
             (abs(x$rhs()) > threshold_value))
  if (length(r) > 0) {
    ### find names of constraints in the problem which exceed thresholds
    out <- FALSE
    n <- x$row_ids()[r]
    ### throw warnings
    if ("budget" %in% n)
      warning("budget is very high (> 1e+6)",
              immediate. = TRUE)
    if ("spp_target" %in% n)
      warning(paste0("features targets are very high (> 1e+6) or ",
                     "very low non-zero (< 1e-6) values"),
              immediate. = TRUE)
  }
  ## constraint matrix
  r <- which((ratio(x$A()@x) > threshold_ratio) |
              (abs(x$A()@x) > threshold_value))
  if (length(r) > 0) {
    ### find names of constraints in the problem which exceed thresholds
    out <- FALSE
    y <- methods::as(x$A(), "dgTMatrix")
    rn <- x$row_ids()[y@i + 1]
    cn <- x$col_ids()[y@j + 1]
    rm(y)
    cn <- cn[r]
    rn <- rn[r]
    ### throw warnings
    if ("n" %in% rn) {
      warning("number of neighbors required is very high (> 1e+6)",
                immediate. = TRUE)
      cn <- cn[rn != "n"]
      rn <- rn[rn != "n"]
    }
    if ("budget" %in% rn)
      warning(paste("planning units with very high (> 1e+6) or very low",
                    "(< 1e-6) non-zero values"),
              immediate. = TRUE)
    if (("pu_ijz" %in% cn) | ("pu" %in% cn))
      warning(paste("feature amounts in planning units have",
                    "very high (> 1e+6) or very low (< 1e-6) non-zero values"),
              immediate. = TRUE)
  }
  # return check
  out
}
