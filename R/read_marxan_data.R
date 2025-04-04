#' @include internal.R
NULL

#' Read *Marxan* planning unit data
#'
#' Read *Marxan* data containing information on each planning unit.
#'
#' @param x `character` file path.
#'
#' @inheritParams assert
#'
#' @details
#' This function is designed to import `pu.dat` files.
#'
#' @return A [tibble::tibble()] data frame.
#'
#' @noRd
read_marxan_pu_data <- function(x, call = fn_caller_env()) {
  assert(
    assertthat::is.string(x),
    assertthat::noNA(x),
    assertthat::is.readable(x),
    is_installed("vroom"),
    call = call,
    .internal = TRUE
  )
  suppressWarnings(
    vroom::vroom(
      x,
      delim = marxan_data_delimiter(x),
      col_types = vroom::cols(
        id = vroom::col_double(),
        cost = vroom::col_double(),
        status = vroom::col_double(),
        xloc = vroom::col_double(),
        yloc = vroom::col_double()
      )
    )
  )
}

#' Read *Marxan* planning unit vs. species data
#'
#' Read *Marxan* data containing information on the amount of each
#' species within each planning unit.
#'
#' @inheritParams read_marxan_pu_data
#'
#' @details
#' This function is designed to import `puvspr.dat` files.
#'
#' @inherit read_marxan_pu_data return
#'
#' @noRd
read_marxan_puvspr_data <- function(x, call = fn_caller_env()) {
  assert(
    assertthat::is.string(x),
    assertthat::noNA(x),
    assertthat::is.readable(x),
    is_installed("vroom"),
    call = call,
    .internal = TRUE
  )
  suppressWarnings(
    vroom::vroom(
      x,
      delim = marxan_data_delimiter(x),
      col_types = vroom::cols(
        species = vroom::col_double(),
        pu = vroom::col_double(),
        amount = vroom::col_double()
      )
    )
  )
}

#' Read *Marxan* species data
#'
#' Read *Marxan* data containing information on the species.
#'
#' @inheritParams read_marxan_pu_data
#'
#' @details
#' This function is designed to import `spec.dat` files.
#'
#' @inherit read_marxan_pu_data return
#'
#' @noRd
read_marxan_spec_data <- function(x, call = fn_caller_env()) {
  assert(
    assertthat::is.string(x),
    assertthat::noNA(x),
    assertthat::is.readable(x),
    is_installed("vroom"),
    call = call,
    .internal = TRUE
  )
  suppressWarnings(
    vroom::vroom(
      x,
      delim = marxan_data_delimiter(x),
      col_types = vroom::cols(
        id = vroom::col_double(),
        name = vroom::col_character(),
        prop = vroom::col_double(),
        amount = vroom::col_double(),
        spf = vroom::col_character(),
      )
    )
  )
}

#' Read *Marxan* boundary data
#'
#' Read *Marxan* data containing information on the planning unit boundaries.
#'
#' @inheritParams read_marxan_pu_data
#'
#' @details
#' This function is designed to import `bound.dat` files.
#'
#' @inherit read_marxan_pu_data return
#'
#' @noRd
read_marxan_bound_data <- function(x, call = fn_caller_env()) {
  assert(
    assertthat::is.string(x),
    assertthat::noNA(x),
    assertthat::is.readable(x),
    is_installed("vroom"),
    call = call,
    .internal = TRUE
  )
  suppressWarnings(
    vroom::vroom(
      x,
      delim = marxan_data_delimiter(x),
      col_types = vroom::cols(
        id1 = vroom::col_double(),
        id2 = vroom::col_double(),
        boundary = vroom::col_double()
      )
    )
  )
}

#' File delimiter for a *Marxan* file
#'
#' @param x `character` flile path.
#'
#' @details
#' This function determines the file delimiter for a *Marxan* data file.
#' To achieve this, it will load in the first line of the data file and check
#' if it as a tab symbol (i.e, `"\t"`). If a tab symbol is detected, then
#' it will output a the tab symbol. Otherwise, it will assume that the
#' data file is comma delimited and output the comma symbol.
#'
#' @section Notes:
#' This function is needed because the latest version of the \pkg{vroom}
#' fails to recognize the correct delimiter for *Marxan* files that contian
#' two columns. If the \pkg{vroom} is updated in the future to address
#' this limitation, this function may be safely deprecated.
#'
#' @return A `character` value.
#'
#' @noRd
marxan_data_delimiter <- function(x) {
  has_tabs <- any(grepl("\t", readLines(x, n = 1)))
  ifelse(isTRUE(has_tabs), "\t", ",")
}
