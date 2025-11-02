#' @include internal.R

#' Format targets for optimization format
#'
#' Format a data frame containing targets for optimization.
#'
#' @param x [problem()] object.
#'
#' @param targets `data.frame` containing the targets. Here each row
#' corresponds to a different target and columns contain information about
#' the targets. It must contain the following columns:
#' `"feature"` with `character` values denoting the features names,
#' `"target"` with `numeric` values denoting target thresholds,
#' and `"type"` with `character` values denoting if the target thresholds
#' are `"relative"` or `"absolute"`.
#'
#' @details
#' This function is used to convert a `data.frame` with target information
#' into a standard format for subsequent use with `rcpp_add_rij_data()`.
#' In particular, it performs the following steps:
#' 1. If `targets` does not have a `"zone"` column indicating the zone(s)
#' associated with each target, then a `"zone"` column is added with all
#' values set according to the first zone in `x`.
#' This functionality present because target setting functions designed for
#' single zone problems may not specify a `"zones"` column.
#' 2. If the `"zone"` column in `targets` is a `character` vector,
#' then it is converted a `list` column. Since the `"zone"` column may
#' contain a `character` vector or `list` column, this functionality is used
#' to standardize the `"zone"` column to a consistent format.
#' 3. If `targets` does not have a `"sense"` column indicating the
#' constraint senses for the targets, then a `"sense"` column is added
#' with all values set to `">="`.
#' 4. The `"feature"` column is processed to convert the `character` names
#' of features into `integer` indices. For example, if `x` has the features
#' `"A"`, `"B"`, and `"C"`, and a row of the `targets$feature` column contains
#' the `"B", then it will contain the value 2 after completing this processing.
#' 5. The `"zone"` column is processed to convert the `character` names
#' of zones into `integer` indices. For example, if `x` has the zones
#' `"Z1"`, `"Z2"`, and `"Z3"`, and a row of the `targets$zone` column contains
#' the value `"Z3", then it will contain the value 3 after completing this
#' processing.
#' 6. A new `"value"` column is added to `targets`. This column contains
#' the target thresholds expressed as absolute units. If there are any
#' relative targets (per `targets$type`), then these are converted to
#' absolute units based on `x$feature_abundances_in_total_units()`.
#' 7. The `targets` data frame is processed to select the following columns:
#' 8. The `targets` data frame is returned.
#' `"feature"`, `"zone"`, `"sense"`, and `"value"`.
#'
#' @return
#' A `data.frame` with the columns `"feature"`, `"zone"`, `"sense"`, and
#' `"value"`.
#'
#' @noRd
target_optimization_format <- function(x, targets) {
  # assert valid arguments
  assert(
    is_conservation_problem(x),
    is.data.frame(targets),
    assertthat::has_name(targets, "feature"),
    is_inherits(targets$feature, c("character", "factor")),
    assertthat::has_name(targets, "type"),
    is_inherits(targets$type, c("character", "factor")),
    all_match_of(targets$type, c("relative", "absolute")),
    assertthat::has_name(targets, "target"),
    .internal = TRUE
  )

  # coerce to character
  targets$feature <- as.character(targets$feature)
  targets$type <- as.character(targets$type)

  # get data
  abundances <- x$feature_abundances_in_total_units()

  # add zone column if missing
  if (!assertthat::has_name(targets, "zone")) {
    targets$zone <- colnames(abundances)[[1]]
  }

  # convert zone column to list of characters if needed
  if (!inherits(targets$zone, "list")) {
    targets$zone <- as.list(targets$zone)
  }

  # add sense column if missing
  if (!assertthat::has_name(targets, "sense")) {
    targets$sense <- ">="
  }
  targets$sense <- as.character(targets$sense)

  # convert feature names to indices
  targets$feature <- match(
    as.character(targets$feature), rownames(abundances)
  )
  assert(
    assertthat::noNA(targets$feature),
    msg = "All features in {.arg targets} must be present in {.arg x}.",
    .internal = TRUE
  )

  # convert zone names to indices
  for (i in seq_len(nrow(targets))) {
    targets$zone[[i]] <- match(
      targets$zone[[i]], colnames(abundances)
    )
    assert(
      assertthat::noNA(targets$zone[[i]]),
      msg = "All zones in {.arg targets} must be present in {.arg x}.",
      .internal = TRUE
    )
  }

  # add compute relative targets as absolute targets and assign zone ids
  targets$value <- as.numeric(targets$target)
  relative_rows <- which(targets$type == "relative")
  for (i in seq_along(relative_rows)) {
    zone_id <- targets$zone[[relative_rows[[i]]]]
    feature_id <- targets$feature[[relative_rows[[i]]]]
    abund_mtx <- as.matrix(data.frame(feature_id, zone_id))
    targets$value[relative_rows[i]] <-
      sum(abundances[abund_mtx]) * targets$target[relative_rows[i]]
   }

  # return tibble
  targets[, c("feature", "zone", "sense", "value")]
}
