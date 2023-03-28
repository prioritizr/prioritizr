#' Ferrier's irreplaceability metric (R implementation)
#'
#' Calculate Ferrier's irreplaceability metric as per Ferrier et al. 2000
#'
#' @param rij \code{numeric} \code{matrix} containing the amount of each
#'   feature associated within each planning unit (e.g. presence/absence data
#'   denoted with zeros and ones). Here, rows correspond to features and
#'   columns correspond
#'
#' @param targets \code{numeric} \code{vector} of absolute targets for each
#'   feature.
#'
#' @param solution \code{numeric} \code{vector} of planing unit statuses
#'   in the solution.
#'
#' @return \code{\link[Matrix]{dgCMatrix-class}} with irreplceability values for
#'   each feature in each planning unit. Similar to the \code{rij} parameter,
#'   rows correspond to features and columns correspond to planning units.
#'
#' @details
#' This implementation was developed by Brandon Edwards.
#'
#' @noRd
ferrier_scores_r <- function(rij, targets, solution) {
  # assert that arguments are valid
  if (inherits(rij, c("matrix", "Matrix")) && !inherits(rij, "dgTMatrix")) {
    rij <- as_Matrix(rij, "dgTMatrix")
  }
  assertthat::assert_that(
    inherits(rij, "dgTMatrix"),
    assertthat::noNA(c(rij@x)), all(rij@x >= 0), nrow(rij) > 0,
    ncol(rij) > 0,
    is.numeric(targets), assertthat::noNA(c(targets)), all(targets >= 0),
    length(targets) == nrow(rij),
    is.numeric(solution),
    length(solution) == ncol(rij))

  # exclude planning units with NA values in solution
  rij_idx <- which(!is.na(solution))
  rij2 <- rij[, rij_idx, drop = FALSE]

  # initialize variables
  portfolio_size <- sum(solution, na.rm = TRUE)
  n_planning_units <- ncol(rij2)
  n_features <- nrow(rij2)
  mult <- n_planning_units / (n_planning_units - 1)
  wt_include <- portfolio_size / n_planning_units
  wt_exclude <- 1 - wt_include
  sum_feature_amount <- Matrix::rowSums(rij2)
  sum_sq_feature_amount <- Matrix::rowSums(rij2 ^ 2)
  target_shortfall_amount <- targets

  # main calculations
  out <- rij2
  planning_units <- rij2@j + 1
  features <- rij2@i + 1
  out@x <- vapply(
    seq_along(planning_units),
    FUN.VALUE = numeric(1),
    function(i) {
      internal_calculate_irrep_value(
        n_planning_units,
        portfolio_size,
        mult,
        wt_include,
        wt_exclude,
        rij2[features[i], planning_units[i]],
        targets[features[i]],
        sum_feature_amount[features[i]],
        sum_sq_feature_amount[features[i]]
      )
    }
  )

  # prepare for output
  out2 <- Matrix::drop0(rij * NA_real_)
  for (i in seq_along(rij_idx)) {
    out2[, rij_idx[i]] <- out[, i]
  }
  out2[, which(solution < 0.5)] <- 0

  # prepare output
  out2 <- as_Matrix(Matrix::t(out2), "dgCMatrix")
  out2 <- cbind(out2, Matrix::rowSums(out2))
  colnames(out2) <- c(rownames(rij), "total")

  # return result
  out2
}

internal_calculate_irrep_value <- function(n_planning_units, portfolio_size,
                                           mult, wt_include,
                                           wt_exclude, feature_amount,
                                           feature_target, sum_feature_amount,
                                           sum_sq_feature_amount) {
  feature_amount_sq <- feature_amount ^ 2
  sum_feature_amount <- (sum_feature_amount - feature_amount) * mult
  sum_sq_feature_amount <- (sum_sq_feature_amount - feature_amount_sq) * mult
  mean_feature_amount_per_pu <- sum_feature_amount / n_planning_units

  stdev <- internal_calculate_standard_dev(
    sum_feature_amount, sum_sq_feature_amount, n_planning_units)

  rx_removed <- internal_calculate_rx_removed(
    n_planning_units, portfolio_size, stdev, feature_amount,
    feature_target, mean_feature_amount_per_pu, sum_feature_amount)

  rx_included <- internal_calculate_rx_included(
    n_planning_units, portfolio_size, stdev, feature_amount,
    feature_target, mean_feature_amount_per_pu)

  rx_excluded <- internal_calculate_rx_excluded(
    n_planning_units, portfolio_size, stdev, feature_amount,
    feature_target, sum_feature_amount, mean_feature_amount_per_pu)

  # Calculate the irreplaceability value
  if ((rx_included + rx_excluded) == 0) {
    irrep_value <- 0
  } else {
    if ((rx_included == 0) & (feature_amount > 0)) {
      rx_included <- 1
    }
    if ((rx_included + rx_excluded) == 0) {
      irrep_value <- 0
    } else {
      irrep_value <- ((rx_included - rx_removed) * wt_include) /
        (rx_included * wt_include + rx_excluded * wt_exclude)
    }
  }
  irrep_value
}

internal_calculate_standard_dev <- function(sum_feature_amount,
                                            sum_sq_feature_amount,
                                            n_planning_units) {
  sqrt(
    sum_sq_feature_amount -
    ((sum_feature_amount ^ 2) / n_planning_units) / n_planning_units)
}

internal_calculate_rx_removed <- function(n_planning_units, portfolio_size,
                                          stdev, feature_amount,
                                          feature_target,
                                          mean_feature_amount_per_pu,
                                          sum_feature_amount) {
  mean_target_per_portfolio_size <- feature_target / (portfolio_size - 1)
  adj_sd <- stdev * calculate_adjusted_portfolio_size(
    n_planning_units - 1, portfolio_size - 1)

  if ((sum_feature_amount - feature_amount) < feature_target) {
    rx_removed <- 0
  } else {
    if (adj_sd < 0.00000000001) {
      if (mean_feature_amount_per_pu < mean_target_per_portfolio_size) {
        rx_removed <- 0
      } else {
        rx_removed <- 1
      }
    } else {
      z <- (mean_target_per_portfolio_size - mean_feature_amount_per_pu) /
           adj_sd
      rx_removed <- 1 - pnorm(z) #area under the right tail
    }
  }
  rx_removed
}

internal_calculate_rx_included <- function(n_planning_units, portfolio_size,
                                           stdev, feature_amount,
                                           feature_target,
                                           mean_feature_amount_per_pu) {
  mean_target_per_portfolio_size <-
    (feature_target - feature_amount) / (portfolio_size - 1)
  adj_sd <- stdev * calculate_adjusted_portfolio_size(
    n_planning_units - 1, portfolio_size - 1)
  if (feature_amount >= feature_target) {
    rx_included <- 1
  } else {
    if (adj_sd < 0.00000000001) {
      if (mean_feature_amount_per_pu < mean_target_per_portfolio_size) {
        rx_included <- 0
      } else {
        rx_included <- 1
      }
    } else {
      z <- (mean_target_per_portfolio_size - mean_feature_amount_per_pu) /
        adj_sd
      rx_included <- 1 - pnorm(z) # area on the right tail
    }
  }
  rx_included
}

internal_calculate_rx_excluded <- function(n_planning_units, portfolio_size,
                                           stdev, feature_amount,
                                           feature_target, sum_feature_amount,
                                           mean_feature_amount_per_pu) {
  mean_target_per_portfolio_size <- feature_target / portfolio_size

  adj_sd <- stdev *
    calculate_adjusted_portfolio_size(n_planning_units - 1, portfolio_size)

  if ((sum_feature_amount - feature_amount) < feature_target) {
    rx_excluded <- 0
  } else {
    if (adj_sd < 0.00000000001) {
      if (mean_feature_amount_per_pu < mean_target_per_portfolio_size) {
        rx_excluded <- 0
      } else {
        rx_excluded <- 1
      }
    } else {
      z <- (mean_target_per_portfolio_size - mean_feature_amount_per_pu) /
        adj_sd
      rx_excluded <- 1 - pnorm(z) # area under the right tail
    }
  }
  rx_excluded
}

calculate_adjusted_portfolio_size <- function(n_pu, portfolio_size) {
  if (portfolio_size > (n_pu / 2)) {
    adjusted_portfolio_size <- sqrt(n_pu - portfolio_size) / portfolio_size
  } else {
    adjusted_portfolio_size <- sqrt(portfolio_size) / portfolio_size
  }
  adjusted_portfolio_size
}
