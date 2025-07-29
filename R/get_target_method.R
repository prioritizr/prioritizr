#' Get target method
#'
#' Get a target setting method based on its name.
#'
#' @param method `character` value name of target setting method.
#'
#' @inheritParams assert
#'
#' @details
#' ```{r child = "man/common/target_method_character.md"}
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
    out <- jung_targets()
  } else if (identical(method, "rodrigues")) {
    out = rodrigues_targets()
  } else if (identical(method, "polak")) {
    out = polak_targets()
  } else if (identical(method, "ward")) {
    out = ward_targets()
  } else if (identical(method, "watson")) {
    out = watson_targets()
  } else if (startsWith(method, "rl_species")) {
    x <- strsplit(method, "_", fixed = TRUE)[[1]]
    if (identical(length(y), 5L)) {
      out <- try(rl_spp_targets(x[[3]], x[[4]], x[[5]]), silent = TRUE)
    }
  } else if (startsWith(method, "rl_ecosystem")) {
    x <- strsplit(method, "_", fixed = TRUE)[[1]]
    if (identical(length(y), 5L)) {
      out <- try(rl_eco_targets(x[[3]], x[[4]], x[[5]]), silent = TRUE)
    }
  }

  # if couldn't identify method, throw error
  assert(
    !is.null(out),
    inherits(out, "Method"),
    msg = c(
      "!" = "{.arg method} must refer to a target setting method.",
      "x" = "{.val {method}} is not the name of a recognized method."
    ),
    call = call
  )

  # return result
  out
}
