#' Get target method
#'
#' Get a target setting method based on its name.
#'
#' @param method `character` value name of target setting method.
#'
#' @inheritParams assert
#'
#' @details
#' ```{r child = "man/rmd/target_method_character.md"}
#' ```
#'
#' @return A [`Method-class`] object.
#'
#' @noRd
get_target_method <- function(method, call = fn_caller_env()) {
  # assert valid arguments
  assert(
    assertthat::is.string(method),
    assertthat::noNA(method),
    call = call,
    .internal = TRUE
  )

  # identify targets
  out <- NULL
  if (identical(method, "jung")) {
    out <- spec_jung_targets()
  } else if (identical(method, "rodrigues")) {
    out <- spec_rodrigues_targets()
  } else if (identical(method, "polak")) {
    out <- spec_polak_targets()
  } else if (identical(method, "ward")) {
    out <- spec_ward_targets()
  } else if (identical(method, "watson")) {
    out <- spec_watson_targets()
  } else if (startsWith(method, "rl_species")) {
    x <- strsplit(method, "_", fixed = TRUE)[[1]]
    if (identical(length(x), 5L)) {
      out <- try(
        spec_rl_species_targets(x[[3]], x[[4]], x[[5]]),
        silent = TRUE
      )
    }
  } else if (startsWith(method, "rl_ecosystem")) {
    x <- strsplit(method, "_", fixed = TRUE)[[1]]
    if (identical(length(x), 5L)) {
      out <- try(
        spec_rl_ecosystem_targets(x[[3]], x[[4]], x[[5]]),
        silent = TRUE
      )
    }
  }

  # determine if method refers to a target setting method
  method_name <- paste0("spec_", method, "_targets")
  msg <- ifelse(
    !isTRUE(method_name %in% utils::ls.str("package:prioritizr")),
    "{.val {method}} is not the name of a recognized method.",
    paste(
      "{.val {method}} has non-optional parameters and",
      "must be specified with {.fn {method_name}}."
    )
  )

  # if couldn't identify method, throw error
  assert(
    !is.null(out),
    inherits(out, "Method"),
    msg = c(
      "!" = "{.arg method} must refer to a target setting method.",
      "x" = msg
    ),
    call = call
  )

  # return result
  out
}
