#' @include internal.R
NULL

#' Modify a conservation planning problem
#'
#' Open an interactive application in the browser to modify all of the 
#' parameters in a pre-specified conservartion planning problem. This
#' is useful for understanding how different constraints and targets
#' affect the solution.
#' 
#' @param x \code{\link{ConservationProblem}} object.
#' 
#' @param overwrite \code{logical} should the parameters in the input
#'   conservation planning problem be overwritten with the modified
#'   parameters when the application is closed?
#'
#' @details This function requires the \code{\link[shiny]{shiny}} R package to be installed.
#'
#' @return \code{\link{ConservationProblem}} object.
#' 
#' @seealso \code{\link{construct}} to create conservation planning problems.
#'
#' @export
modify <- function(x, overwrite=TRUE) {
  # check that web-app dependencies are installed
  if (!'shiny' %in% rownames(installed.packages()))
    stop('the "shiny" package needs to be installed to make web apps')
  if (!'rhandsontable' %in% rownames(installed.packages()))
    stop('the "rhandsontable" package needs to be installed to make web apps')
  assertthat::assert_that(inherits(x, 'ConservationProblem'))
  
  stop('TODO: shiny magic')
}


