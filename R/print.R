#' @include internal.R cli.R
NULL

#' @method print ConservationProblem
#'
#' @export
print.ConservationProblem <- function(x, ...) x$print()

#' @method print ConservationModifier
#'
#' @export
print.ConservationModifier <- function(x, ...) x$print()

#' @method print OptimizationProblem
#'
#' @export
print.OptimizationProblem <- function(x, ...) x$print()

#' @method print Solver
#'
#' @export
print.Solver <- function(x, ...) x$print()

#' @method print Zones
#'
#' @export
print.Zones <- function(x, ...) {
  div_id <- cli::cli_div(theme = cli_pkg_theme())
  ch <- cli_box_chars()
  cli::cli_text("Zones {.cls {class(x)}}")
  cli_vtext("{ch$b} {.g zones}:    ", repr(zone_names(x)))
  cli_vtext("{ch$b} {.g features}: ", repr(feature_names(x)))
  cli::cli_end(div_id)
}
