#' @include internal.R
NULL

#' Validate *Marxan* planning unit data
#'
#' Validate *Marxan* data containing information on each planning unit.
#'
#' @param pu `data.frame` object.
#'
#' @param prefix `character` value with prefix for error message.
#'
#' @inheritParams assert
#'
#' @details
#' This function is designed to validate `pu.dat` files.
#'
#' @return A `logical` indicating success.
#'
#' @noRd
validate_marxan_pu_data <- function(
  pu, prefix = NULL, call = fn_caller_env()
) {
  assert(
    is.data.frame(pu),
    assertthat::has_name(pu, "id"),
    assertthat::has_name(pu, "cost"),
    is_count_vector(pu$id),
    no_duplicates(pu$id),
    assertthat::noNA(pu$id),
    assertthat::has_name(pu, "cost"),
    is.numeric(pu$cost),
    assertthat::noNA(pu$cost),
    call = call
  )
  if (assertthat::has_name(pu, "status")) {
    assert(
      is.numeric(pu$status),
      assertthat::noNA(pu$status),
      all_match_of(pu$status, seq(0, 3)),
      call = call
    )
  }
  invisible(TRUE)
}

#' Validate *Marxan* planning unit vs. species data
#'
#' Validate *Marxan* data containing information on the amount of each
#' species within each planning unit.
#'
#' @param puvspr `data.frame` object.
#'
#' @details
#' This function is designed to validate `puvspr.dat` files.
#'
#' @inherit validate_marxan_pu_data return
#'
#' @noRd
validate_marxan_puvspr_data <- function(puvspr, call = fn_caller_env()) {
  assert(
    is.data.frame(puvspr),
    assertthat::has_name(puvspr, "pu"),
    assertthat::has_name(puvspr, "species"),
    assertthat::has_name(puvspr, "amount"),
    is_count_vector(puvspr$pu),
    is_count_vector(puvspr$species),
    is.numeric(puvspr$amount),
    all_finite(puvspr$pu),
    all_finite(puvspr$species),
    all_finite(puvspr$amount),
    call = call
  )
  invisible(TRUE)
}

#' Validate *Marxan* planning unit and planning unit vs. species data
#'
#' Validate consistency between *Marxan* data that contain the planning units #' and also the amount of each species within each planning unit.
#'
#' @param puvspr `data.frame` object.
#' @param pu `data.frame` object.
#'
#' @details
#' This function is designed to validate compatibility of `puvspr.dat` files
#' with `pu.dat` files.
#'
#' @inherit validate_marxan_pu_data return
#'
#' @noRd
validate_marxan_puvspr_pu_data <- function(puvspr, pu, call = fn_caller_env()) {
  assert(
    is.data.frame(puvspr),
    is.data.frame(pu),
    assertthat::has_name(puvspr, "pu"),
    assertthat::has_name(pu, "id"),
    all_match_of(puvspr$pu, pu$id),
    call = call
  )
  invisible(TRUE)
}

#' Validate *Marxan* species and planning unit vs. species data
#'
#' Validate consistency between *Marxan* data that contain the species
#' and also the amount of each species within each planning unit.
#'
#' @param puvspr `data.frame` object.
#' @param spec `data.frame` object.
#'
#' @details
#' This function is designed to validate compatibility of `puvspr.dat` files
#' with `spec.dat` files.
#'
#' @inherit validate_marxan_pu_data return
#'
#' @noRd
validate_marxan_puvspr_spec_data <- function(
  puvspr, spec, call = fn_caller_env()
) {
  assert(
    is.data.frame(puvspr),
    is.data.frame(spec),
    assertthat::has_name(puvspr, "species"),
    assertthat::has_name(spec, "id"),
    all_match_of(puvspr$species, spec$id),
    call = call
  )
  invisible(TRUE)
}

#' Validate *Marxan* species data
#'
#' Validate *Marxan* data containing information on the species.
#'
#' @param spec `data.frame` object.
#'
#' @details
#' This function is designed to validate `spec.dat` files.
#'
#' @inherit validate_marxan_pu_data return
#'
#' @noRd
validate_marxan_spec_data <- function(spec, call = fn_caller_env()) {
  assert(
    is.data.frame(spec),
    assertthat::has_name(spec, "id"),
    is_count_vector(spec$id),
    no_duplicates(spec$id),
    assertthat::noNA(spec$id),
    call = call
  )
  if (assertthat::has_name(spec, "name")) {
    assert(
      is_inherits(spec$name, c("character", "factor")),
      no_duplicates(as.character(spec$name)),
      assertthat::noNA(spec$name),
      call = call
    )
  }
  assert(
    do.call(xor, as.list(c("prop", "amount") %in% names(spec))),
    msg = paste(
      "{.arg spec} must specify either column {.col prop} or {.col amount},",
      "and not both."
    )
  )
  if (assertthat::has_name(spec, "prop")) {
    assert(
      is.numeric(spec$prop),
      all_finite(spec$prop),
      all_proportion(spec$prop),
      call = call
    )
  }
  if (assertthat::has_name(spec, "amount")) {
    assert(
      is.numeric(spec$amount),
      all_finite(spec$amount),
      call = call
    )
  }
  invisible(TRUE)
}

#' Validate *Marxan* boundary data
#'
#' Validate *Marxan* data containing information on the planning unit
#' boundaries.
#'
#' @param bound `data.frame` object.
#'
#' @details
#' This function is designed to validate `bound.dat` files.
#'
#' @inherit validate_marxan_pu_data return
#'
#' @noRd
validate_marxan_bound_data <- function(bound, call = fn_caller_env()) {
  assert(
    assertthat::has_name(bound, "id1"),
    assertthat::has_name(bound, "id2"),
    assertthat::has_name(bound, "boundary"),
    is_count_vector(bound$id1),
    assertthat::noNA(bound$id1),
    is_count_vector(bound$id2),
    assertthat::noNA(bound$id2),
    is.numeric(bound$boundary),
    assertthat::noNA(bound$boundary),
    call = call
  )
  invisible(TRUE)
}

#' Validate *Marxan* boundary and planning unit data
#'
#' Validate *Marxan* data containing information on the planning unit
#' boundaries and the planning units.
#'
#' @param bound `data.frame` object.
#' @param pu `data.frame` object.
#'
#' @details
#' This function is designed to validate compatibility of `bound.dat` and
#' `pu.dat` files.
#'
#' @inherit validate_marxan_pu_data return
#'
#' @noRd
validate_marxan_bound_pu_data <- function(bound, pu, call = fn_caller_env()) {
  assert(
    is.data.frame(bound),
    is.data.frame(pu),
    assertthat::has_name(bound, "id1"),
    assertthat::has_name(bound, "id2"),
    assertthat::has_name(pu, "id"),
    all_match_of(bound$id1, pu$id),
    all_match_of(bound$id2, pu$id),
    call = call
  )
  invisible(TRUE)
}
