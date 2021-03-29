#' @include internal.R
NULL

#' @import raster
#' @import sp
#' @import proto
#' @import sf
#' @useDynLib prioritizr, .registration = TRUE
NULL

#' prioritizr: Systematic Conservation Prioritization in R
#'
#' The \pkg{prioritizr R} package uses mixed integer linear programming (MILP)
#' techniques to provide a flexible interface for building and solving
#' conservation planning problems (Rodrigues *et al.* 2000; Billionnet
#' 2013). It supports a broad range of objectives, constraints, and penalties
#' that can be used to custom-tailor conservation planning problems to the
#' specific needs of a conservation planning exercise. Once built, conservation
#' planning problems can be solved using a variety of commercial and
#' open-source exact algorithm solvers. In contrast to the algorithms
#' conventionally used to solve conservation problems, such as heuristics or
#' simulated annealing (Ball *et al.* 2009), the exact algorithms used
#' here are guaranteed to find optimal solutions. Furthermore, conservation
#' problems can be constructed to optimize the spatial allocation of different
#' management actions or zones, meaning that conservation practitioners can
#' identify solutions that benefit multiple stakeholders. Finally, this package
#' has the functionality to read input data formatted for the *Marxan*
#' conservation planning program (Ball *et al.* 2009), and find much
#' cheaper solutions in a much shorter period of time than *Marxan* (Beyer
#' *et al.* 2016). See the
#' [online code repository](https://github.com/prioritizr/prioritizr)
#' for more information.
#'
#' @details This package contains several vignettes that are designed to
#'   showcase its functionality. To view them, please use the code
#'   `vignette("name", package = "prioritizr")` where `"name"` is the
#'   name of the desired vignette (e.g. `"gurobi_installation"`).
#'
#'   \describe{
#'
#'   \item{prioritizr}{provides background
#'     information on systematic conservation planning and a comprehensive
#'     overview of the package and its usage.}
#'
#'   \item{gurobi_installation}{contains
#'     detailed instructions for installing and setting up the *Gurobi*
#'     software suite for use with the package.}
#'
#'   \item{publication_record}{lists of
#'     scientific publications that have used the package for developing
#'     prioritizations.}
#'
#'   \item{zones}{describes how problems can be constructed with multiple
#'     management actions or zones.}
#'
#'   \item{tasmania}{provides a tutorial using Tasmania, Australia
#'     as a case-study. This tutorial uses vector-based planning unit data and
#'     is written for individuals familiar with the *Marxan* decision
#'     support tool.}
#'
#'   \item{saltspring}{provides a tutorial using Salt Spring Island, Canada as a
#'     case-study. This tutorial uses raster-based planning unit data.}
#'   }
#'
#' @references
#' Ball IR, Possingham HP, and Watts M (2009) *Marxan and relatives:
#' Software for spatial conservation prioritisation* in Spatial conservation
#' prioritisation: Quantitative methods and computational tools. Eds Moilanen
#' A, Wilson KA, and Possingham HP. Oxford University Press, Oxford, UK.
#'
#' Beyer HL, Dujardin Y, Watts ME, and Possingham HP (2016) Solving
#' conservation planning problems with integer linear programming.
#' *Ecological Modelling*, 228: 14--22.
#'
#' Billionnet A (2013) Mathematical optimization ideas for biodiversity
#' conservation. *European Journal of Operational Research*, 231:
#' 514--534.
#'
#' Rodrigues AS, Cerdeira OJ, and Gaston KJ (2000) Flexibility,
#' efficiency, and accountability: adapting reserve selection algorithms to
#' more complex conservation problems. *Ecography*, 23: 565--574.
#'
#' @name prioritizr
#' @docType package
NULL
