#' @include internal.R
NULL

#' Run presolve check
#'
#' This internal function is used to perform the presolve checks.
#'
#' @param x [`OptimizationProblem-class`] object.
#'
#' @return
#' A `list` with containing a (`$msg`) `character` vector with information on
#' the presolve checks and (`$pass`) `logical` value indicating if the
#' checks were passed.
#'
#' @noRd
run_presolve_check <- function(x) {
  # assert argument is valid
  assert_required(x)
  assert(inherits(x, "OptimizationProblem"), .internal = TRUE)

  # set thresholds
  upper_value <- 1e+6
  lower_value <- 1e-6

  # initialize output values
  pass <- TRUE
  msg1 <- c()
  msg2 <- c()
  msg3 <- c()

  # import constraint matrix
  y <- as_Matrix(x$A(), "dgTMatrix")
  rownames(y) <- x$row_ids()
  colnames(y) <- x$col_ids()

  # presolve checks
  ## check for non-standard input data
  ### check if all planning units locked out
  n_pu_vars <- x$number_of_planning_units() * x$number_of_zones()
  if (all(x$ub()[seq_len(n_pu_vars)] < 1e-5)) {
    pass <- FALSE
    msg2 <- c(
      msg2,
      c(
        "x" = "All planning units must not be locked out.",
        ">" = paste(
          "Maybe you made a mistake when using",
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
  ### check if only a single feature
  if (x$number_of_features() == 1) {
    pass <- FALSE
    msg2 <- c(
      msg2,
      c(
        "x" = "The problem only contains a single feature.",
        ">" = paste(
          "Conservation planning generally requires multiple features",
          "(e.g., species, ecosystem types) to identify meaningful",
          "priority areas."
        ),
        ""
      )
    )
  }
  ### check if budget exceeds total of planning unit costs
  r1 <- which(x$row_ids() == "budget")
  if (length(r1) > 0) {
    result <- Matrix::rowSums(y[r1, , drop = FALSE]) <= x$rhs()[r1]
    if (any(result)) {
      pass <- FALSE
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
    if (("pu" %in% n1) && (!any("ac", "b", "b1", "c") %in% n2)) {
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
    }
    
    if ("spp_met" %in% n1){
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
    }
    if ("amount" %in% n1){
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
  }
    if ("branch_met" %in% n1) {
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
}
    if (any("b", "b1") %in% n2) {
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
}
    if ("c" %in% n2) {
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
    }
    if ("ac" %in% n2) {
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
  if (mean(Matrix::colSums(abs(rij)) <= lower_value) >= 0.5) {
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

  ## check decision variable bounds
  n_fail_pu <- sum(x$ub()[seq_len(n_pu_vars)] < x$lb()[seq_len(n_pu_vars)])
  if (isTRUE(n_fail_pu > 0)) {
    pass <- FALSE
    msg3 <- c(
      msg3,
      c(
        "x" = paste0(
          "The same {.val {", n_fail_pu, "}} planning unit{?s} ",
          "{?has/have} been specified to be both locked in and locked out."
        ),
        ">" = paste(
          "Maybe you made a mistake when specifying which planning units",
          "should be locked in or out?"
        ),
        ""
      )
    )
  }
  n_fail_extra <- sum(x$ub()[-seq_len(n_pu_vars)] < x$lb()[-seq_len(n_pu_vars)])
  if (isTRUE(n_fail_extra > 0)) {
    pass <- FALSE
    msg3 <- c(
      msg3,
      c(
        "x" = paste0(
          "The {cli::qty(", n_fail_extra, ")} ",
          "lower bound{?s} for {.val {", n_fail_extra, "}} decision ",
          "variable{?s} {?has a greater/have greater} value{?s} than ",
          "{?its/their} upper bound{?s}."
        ),
        ">" = paste(
          "Maybe you made a mistake when specifying some",
          "constraints or penalties?"
        ),
        ""
      )
    )
  }

  # prepare output message
  msg <- c()
  if (!isTRUE(pass)) {
    ## construct message
    if (length(msg2) > 0) {
      msg <- c(
        msg,
        cli::cli_fmt(cli::cli_h2("Data limitation issues")),
        "i" = paste(
          "These following issues indicate that solutions",
          "might not identify meaningful priority areas:"
        ),
        ""
      )
      msg2 <- msg2[seq_len(length(msg2) - 1)]
      msg <- c(msg, msg2)
    }
    if (length(msg3) > 0) {
      msg <- c(
        msg,
        cli::cli_fmt(cli::cli_h2("Infeasibility issues")),
        "i" = paste(
          "These failures indicate that infeasibility",
          "issues could prevent the optimizer from finding a solution:"
        ),
        ""
      )
      msg <- c(msg, msg3)
    }
    if (length(msg1) > 0) {
      msg <- c(
        msg,
        cli::cli_fmt(cli::cli_h2("Numerical issues")),
        "i" = paste(
          "The following issues could stall",
          "optimization or produce incorrect solutions:"
        ),
        ""
      )
      msg1 <- msg1[seq_len(length(msg1) - 1)]
      msg <- c(msg, msg1)
    }
  }

  # return results
  list(pass = pass, msg = vapply(msg, cli::format_inline, character(1)))
}
