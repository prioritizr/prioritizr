#' @include internal.R
NULL

#' Construct a conservation planning problem
#'
#' Open an interactive application in the browser to construct a conservation 
#' planning problem.
#'
#' @details This function requires the \code{\link[shiny]{shiny}} package to
#'   be installed.
#'
#' @return \code{ConservationProblem} object.
#' 
#' @seealso \code{\link{modify}} to modify the parameters in a conservation
#'   planning problem.
#'
#' @export
construct <- function() {
  # check that web-app dependencies are installed
  if (!'shiny' %in% rownames(installed.packages()))
    stop('the "shiny" package needs to be installed to make web apps')
  assertthat::assert_that(inherits(x, 'ConservationProblem'))  
  stop('TODO: shiny magic')
}
