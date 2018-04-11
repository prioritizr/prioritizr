
#' Generic methods
#'
#' These functions are used to access data from various classes defined in the
#' \pkg{prioritizr} package.
#'
#' @param x \code{\link{ConservationProblem-class}},
#'   \code{\link{OptimizationProblem-class}}, or \code{\link{Zones-class}}
#'   object.
#'
#' @return \code{\link[Matrix]{dgCMatrix-class}}, \code{numeric} vector,
#'   \code{character} vector, or scalar \code{integer} depending on the method.
#'
#' @details For more information on the methods available for a specific class,
#'   please see the relevant help page for that class listed in the
#'   See Also section below.
#'
#' @seealso \code{\link{ConservationProblem-methods}},
#'   \code{\link{OptimizationProblem-methods}}, \code{\link{Zones-class}}.
#'
#' @name generics
#'
#' @aliases nrow ncol ncell modelsense vtype obj A rhs sense lb ub number_of_features number_of_planning_units number_of_total_units number_of_zones number_of_items col_ids row_ids compressed_formulation zone_names feature_names
NULL

#' @name generics
#'
#' @rdname generics
#'
#' @importFrom raster nrow
#'
#' @exportMethod nrow
NULL

#' @name generics
#'
#' @rdname generics
#'
#' @importFrom raster ncol
#'
#' @exportMethod ncol
#'
NULL

#' @name generics
#'
#' @rdname generics
#'
#' @importFrom raster ncell
#'
#' @exportMethod ncell
#'
NULL

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod modelsense
#'
#' @usage modelsense(x)
#'
methods::setGeneric("modelsense", function(x) standardGeneric("modelsense"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod number_of_items
#'
#' @usage number_of_items(x)
#'
methods::setGeneric("number_of_items", function(x)
  standardGeneric("number_of_items"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod vtype
#'
#' @usage vtype(x)
#'
methods::setGeneric("vtype", function(x) standardGeneric("vtype"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod obj
#'
#' @usage obj(x)
#'
methods::setGeneric("obj", function(x) standardGeneric("obj"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod A
#'
#' @usage A(x)
#'
methods::setGeneric("A", function(x) standardGeneric("A"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod rhs
#'
#' @usage rhs(x)
#'
methods::setGeneric("rhs", function(x) standardGeneric("rhs"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod sense
#'
#' @usage sense(x)
#'
methods::setGeneric("sense", function(x) standardGeneric("sense"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod lb
#'
#' @usage lb(x)
#'
methods::setGeneric("lb", function(x) standardGeneric("lb"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod ub
#'
#' @usage ub(x)
#'
methods::setGeneric("ub", function(x) standardGeneric("ub"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod number_of_features
#'
#' @usage number_of_features(x)
#'
methods::setGeneric("number_of_features",
  function(x) standardGeneric("number_of_features"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod number_of_zones
#'
#' @usage number_of_zones(x)
#'
methods::setGeneric("number_of_zones",
  function(x) standardGeneric("number_of_zones"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod number_of_planning_units
#'
#' @usage number_of_planning_units(x)
#'
methods::setGeneric("number_of_planning_units",
  function(x) standardGeneric("number_of_planning_units"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod col_ids
#'
#' @usage col_ids(x)
#'
methods::setGeneric("col_ids", function(x) standardGeneric("col_ids"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod row_ids
#'
#' @usage row_ids(x)
#'
methods::setGeneric("row_ids", function(x) standardGeneric("row_ids"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod compressed_formulation
#'
#' @usage compressed_formulation(x)
#'
methods::setGeneric("compressed_formulation",
                    function(x) standardGeneric("compressed_formulation"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod feature_names
#'
#' @usage feature_names(x)
#'
methods::setGeneric("feature_names",
                    function(x) standardGeneric("feature_names"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod zone_names
#'
#' @usage zone_names(x)
#'
methods::setGeneric("zone_names",
                    function(x) standardGeneric("zone_names"))

#' @name generics
#'
#' @rdname generics
#'
#' @exportMethod number_of_total_units
#'
#' @usage number_of_total_units(x)
#'
methods::setGeneric("number_of_total_units",
                    function(x) standardGeneric("number_of_total_units"))
