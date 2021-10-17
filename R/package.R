#' @include internal.R
NULL

#' @import raster
#' @import sp
#' @import proto
#' @import sf
#' @useDynLib prioritizr, .registration = TRUE
NULL

#' @importMethodsFrom fasterize raster
#' @export
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
#'   \item{prioritizr}{Background information on systematic conservation
#'     planning and a comprehensive overview of the package and its usage.}
#'
#'   \item{tasmania}{Tutorial using Tasmania, Australia
#'     as a case-study. This tutorial uses vector-based planning unit data and
#'     is written for individuals familiar with the *Marxan* decision
#'     support tool.}
#'
#'   \item{saltspring}{Tutorial using Salt Spring Island, Canada as a
#'     case-study. This tutorial uses raster-based planning unit data.}
#'
#'   \item{zones}{Tutorial on using multiple management actions or zones
#'     to create detailed prioritizations.}
#'
#'   \item{gurobi_installation}{Instructions for installing and setting up
#'     the *Gurobi* optimization software for use with the package.}
#'
#'   \item{solver_benchmark}{Reports run times for solving
#'     conservation planning problems of varying size and complexity
#'     using different solvers.}
#'
#'   \item{publication_record}{List of publications that have cited the
#'     package.}
#'
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

# avoid false positive NOTES
f <- exactextractr::exact_extract
