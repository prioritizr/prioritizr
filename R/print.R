#' @include internal.R cli.R
NULL

#' @method print ConservationProblem
#'
#' @export
print.ConservationProblem <- function(x, ...) {
  cli::cli({x$print()})
}

#' @method print ConservationModifier
#'
#' @export
print.ConservationModifier <- function(x, ...) {
  cli::cli({x$print()})
}

#' @method print OptimizationProblem
#'
#' @export
print.OptimizationProblem <- function(x, ...) {
  cli::cli({x$print()})
}

#' @method print Solver
#'
#' @export
print.Solver <- function(x, ...) {
  cli::cli({x$print()})
}

#' @method print Zones
#'
#' @export
print.Zones <- function(x, ...) {
  cli::cli({
    div_id <- cli::cli_div(theme = cli_pkg_theme())
    ch <- cli_box_chars()
    cli::cli_text("A zones object {.cls {class(x)}}")
    cli_vtext("{ch$b} zones:    ", repr(zone_names(x)))
    cli_vtext("{ch$b} features: ", repr(feature_names(x)))
    cli::cli_end(div_id)
  })
}
