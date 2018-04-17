#' @include internal.R
NULL

#' @import raster
#' @import sp
#' @import proto
#' @useDynLib prioritizr, .registration = TRUE
NULL

#' \pkg{prioritizr}
#'
#' The \pkg{prioritizr} package uses integer linear programming (ILP)
#' techniques for defining and solving systematic conservation prioritization
#' problems (Rodrigues \emph{et al.} 2000; Billionnet 2013). This package
#' offers a flexible interface for creating and customizing conservation
#' problems. It supports a broad of objectives,
#' constraints, and penalties that can be used to create a conservation
#' problems that are custom-tailored to the specific needs of a conservation
#' planning exercise. Once built, conservation problems can be solved using a
#' variety of commercial and open-source exact algorithm solvers. In contrast
#' to the algorithms conventionally used to solve conservation problems, such
#' as heuristics or simulated annealing [@3], the exact algorithms used here
#' are guaranteed to find optimal solutions. Furthermore, conservation problems
#' can be constructed to optimize the spatial allocation of different
#' management actions or zones, meaning that conservation practitioners can
#' identify solutions that benefit multiple stakeholders. Finally, this package
#' has the functionality to read input data formatted for the _Marxan_
#' conservation planning program (Ball \emph{et al.} 2009), and find much
#' cheaper solutions in a much shorter period of time than _Marxan_ (Beyer
#' \emph{et al.} 2016). See the
#' \href{https://github.com/prioritizr/prioritizr}{online code repository}
#' for more information.
#'
#' @details This package contains several vignettes that are designed to
#'   showcase its functionality. To view them, type of the command
#'   \code{vignette("name", package = "prioritizr")} where \code{"name"} is the
#'   name of the desired vignette (e.g. \code{"gurobi_installation"}.
#'
#'   \describe{
#'
#'   \item{prioritizr}{provides background
#'     information on systematic conservation planning and a comprehensive
#'     overview of the package and its usage.}
#'
#'   \item{gurobi_installation}{contains
#'     detailed instructions for installing and setting up the \emph{Gurobi}
#'     software suite for use with the package.}
#'
#'   \item{publication_record}{lists of
#'     scientific publications that have used the package for developing
#'     prioritizations.}
#'
#'   \item{zones}{describes how problems can be constructed with multiple
#'     management actions or zones.}
#'
#'   }
#'
#'   For more worked examples using the \pkg{prioritizr} package, check out
#'   the supplemental \pkg{prioritizrdata} package.
#'
#' @references
#' Ball IR, Possingham HP, and Watts M (2009) \emph{Marxan and relatives:
#' Software for spatial conservation prioritisation} in Spatial conservation
#' prioritisation: Quantitative methods and computational tools. Eds Moilanen
#' A, Wilson KA, and Possingham HP. Oxford University Press, Oxford, UK.
#'
#' Beyer HL, Dujardin Y, Watts ME, and Possingham HP (2016) Solving
#' conservation planning problems with integer linear programming.
#' \emph{Ecological Modelling}, 228: 14--22.
#'
#' Billionnet A (2013) Mathematical optimization ideas for biodiversity
#' conservation. \emph{European Journal of Operational Research}, 231:
#' 514--534.
#'
#' Rodrigues AS, Cerdeira OJ, and Gaston KJ (2000) Flexibility,
#' efficiency, and accountability: adapting reserve selection algorithms to
#' more complex conservation problems. \emph{Ecography}, 23: 565--574.
#'
#' @name prioritizr
#' @docType package
NULL
