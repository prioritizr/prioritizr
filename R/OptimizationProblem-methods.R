#' @include internal.R OptimizationProblem-class.R reexports.R
NULL

#' Optimization problem methods
#'
#' These functions are used to query and update a [optimization_problem()].
#'
#' @param x [optimization_problem()] object.
#'
#' @param obj `numeric` vector containing a new linear coefficient for
#'  each decision variable in the problem.
#'
#' @param lb `numeric` vector containing a new lower bound for each
#'   decision variable in the problem.
#'
#' @param ub `numeric` vector containing a new upper bound for each
#'   decision variable in the problem.
#'
#' @param A [Matrix::sparseMatrix()] matrix with coefficients for new
#'   constraints.
#'
#' @param sense `character` vector with senses for new constraints
#'   (i.e., `">="`, `"<="`, or "`=`" values).
#'
#' @param rhs `numeric` vector with the right-hand-side values for new
#'   constraints.
#'
#' @param row_ids `character` vector with identifiers for new constraints.
#'
#' @details
#' The following functions are used to query data.
#'
#' \describe{
#'
#' \item{`nrow(x)`}{`integer` number of rows (constraints).}
#'
#' \item{`ncol(x)`}{`integer` number of columns (decision variables).}
#'
#' \item{`ncell(x)`}{`integer` number of cells.}
#'
#' \item{`modelsense(x)`}{`character` describing if the problem is to be
#'   maximized (`"max"`) or minimized (`"min"`).}
#'
#' \item{`vtype(x)`}{`character` describing the type of each decision variable:
#'   binary (`"B"`), semi-continuous (`"S"`), or continuous
#'   (`"C"`)}
#'
#' \item{`obj(x)`}{`numeric` vector specifying the objective function.}
#'
#' \item{`A(x)`}{[`Matrix::dgCMatrix-class`] matrix object defining the
#'   problem matrix.}
#'
#' \item{`rhs(x)`}{`numeric` vector with right-hand-side linear constraints}
#'
#' \item{`sense(x)`}{`character` vector with the senses of the linear
#'   constraints (`"<="`, `">="`, `"="`).}
#'
#' \item{`lb(x)`}{`numeric` lower bound for each decision variable. Missing data
#'   values (`NA`) indicate no lower bound for a given variable.}
#'
#' \item{`ub(x)`}{`numeric` upper bounds for each decision variable. Missing
#'   data values (`NA`) indicate no upper bound for a given variable.}
#'
#' \item{`number_of_planning_units(x)`}{`integer` number of planning units in
#'   the problem.}
#'
#' \item{`number_of_features(x)`}{`integer` number of features
#'   the problem.}
#' }
#'
#' The following functions are used to update data. Note that these
#' functions return an invisible `TRUE` indicating success.
#'
#' \describe{
#' \item{`set_obj(x, obj)`}{override the objective in the problem.
#'  Here, `obj` is a `numeric` vector containing a new linear coefficient for
#'  each decision variable in the problem.}
#'
#' \item{`set_lb(x, lb)`}{override the variable lower bounds in the problem.
#'  Here, `lb` is a `numeric` vector containing a new lower bound.for each
#'  decision variable in the problem.}
#'
#' \item{`set_ub(x, ub)`}{override the variable upper bounds in the problem.
#'  Here, `ub` is a `numeric` vector containing a new upper bound.for each
#'  decision variable in the problem.}
#'
#' \item{`remove_last_linear_constraint()`}{remove the last linear constraint
#'  added to a problem.}
#'
#' \item{`append_linear_constraints(x, A, sense, rhs, row_ids)`}{
#'  add an additional linear constraints to a problem. Here,
#'  `A` is a [Matrix::sparseMatrix()] matrix, `sense` is a
#'  `character` vector with constraint senses (i.e., `">="`, `"<="`, or "`=`"
#'  values), `rhs` is a `numeric` vector with the right-hand-side values,
#'  and `row_ids` is a `character` vector with identifiers.}
#'
#' }
#'
#' @return A [`Matrix::dgCMatrix-class`], `numeric` vector,
#'   `numeric` vector, or scalar `integer` depending on the method
#'   used.
#'
#' @name OptimizationProblem-methods
#'
#' @aliases modelsense vtype obj A rhs sense lb ub col_ids row_ids compressed_formulation remove_last_linear_constraint set_obj set_lb set_ub append_linear_constraints ncell,OptimizationProblem-method A,OptimizationProblem-method col_ids,OptimizationProblem-method lb,OptimizationProblem-method modelsense,OptimizationProblem-method ncol,OptimizationProblem-method nrow,OptimizationProblem-method obj,OptimizationProblem-method rhs,OptimizationProblem-method row_ids,OptimizationProblem-method sense,OptimizationProblem-method ub,OptimizationProblem-method vtype,OptimizationProblem-method compressed_formulation,OptimizationProblem-method set_obj,OptimizationProblem,ANY-method set_obj,OptimizationProblem-method set_lb,OptimizationProblem,ANY-method set_lb,OptimizationProblem-method set_ub,OptimizationProblem,ANY-method set_ub,OptimizationProblem-method append_linear_constraints,OptimizationProblem,ANY,ANY,ANY,ANY-method append_linear_constraints,OptimizationProblem-method remove_last_linear_constraint,OptimizationProblem-method
NULL

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{nrow}{OptimizationProblem}(x)
methods::setMethod("nrow", "OptimizationProblem", function(x) x$nrow())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{ncol}{OptimizationProblem}(x)
methods::setMethod("ncol", "OptimizationProblem", function(x) x$ncol())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{ncell}{OptimizationProblem}(x)
methods::setMethod("ncell", "OptimizationProblem", function(x) x$ncell())

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @exportMethod modelsense
#' @usage modelsense(x)
methods::setGeneric(
  "modelsense",
  function(x) {
    assert_required(x)
    standardGeneric("modelsense")
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @usage \S4method{modelsense}{OptimizationProblem}(x)
methods::setMethod(
  "modelsense",
  "OptimizationProblem",
  function(x) x$modelsense()
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @exportMethod vtype
#' @usage vtype(x)
methods::setGeneric(
  "vtype",
  function(x) {
    assert_required(x)
    standardGeneric("vtype")
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @usage \S4method{vtype}{OptimizationProblem}(x)
methods::setMethod("vtype", "OptimizationProblem", function(x) x$vtype())

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @exportMethod obj
#' @usage obj(x)
methods::setGeneric(
  "obj",
  function(x) {
    assert_required(x)
    standardGeneric("obj")
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @usage \S4method{obj}{OptimizationProblem}(x)
methods::setMethod("obj", "OptimizationProblem", function(x) x$obj())

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @exportMethod A
#' @usage A(x)
methods::setGeneric(
  "A",
  function(x) {
    assert_required(x)
    standardGeneric("A")
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @usage \S4method{A}{OptimizationProblem}(x)
methods::setMethod("A", "OptimizationProblem", function(x) x$A())

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @exportMethod rhs
#' @usage rhs(x)
methods::setGeneric(
  "rhs",
  function(x) {
    assert_required(x)
    standardGeneric("rhs")
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @usage \S4method{rhs}{OptimizationProblem}(x)
methods::setMethod("rhs", "OptimizationProblem", function(x) x$rhs())

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @exportMethod sense
#' @usage sense(x)
methods::setGeneric(
  "sense",
  function(x) {
    assert_required(x)
    standardGeneric("sense")
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @usage \S4method{sense}{OptimizationProblem}(x)
methods::setMethod("sense", "OptimizationProblem", function(x) x$sense())

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @exportMethod lb
#' @usage lb(x)
methods::setGeneric(
  "lb",
  function(x) {
    assert_required(x)
    standardGeneric("lb")
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @usage \S4method{lb}{OptimizationProblem}(x)
methods::setMethod("lb", "OptimizationProblem", function(x) x$lb())

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @exportMethod ub
#' @usage ub(x)
methods::setGeneric(
  "ub",
  function(x) {
    assert_required(x)
    standardGeneric("ub")
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @usage \S4method{ub}{OptimizationProblem}(x)
methods::setMethod("ub", "OptimizationProblem", function(x) x$ub())

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @exportMethod col_ids
#' @usage col_ids(x)
methods::setGeneric(
  "col_ids",
  function(x) {
    assert_required(x)
    standardGeneric("col_ids")
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @usage \S4method{col_ids}{OptimizationProblem}(x)
methods::setMethod(
  "col_ids",
  "OptimizationProblem",
  function(x) x$col_ids()
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @exportMethod row_ids
#' @usage row_ids(x)
methods::setGeneric(
  "row_ids",
  function(x) {
    assert_required(x)
    standardGeneric("row_ids")
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @usage \S4method{row_ids}{OptimizationProblem}(x)
methods::setMethod("row_ids", "OptimizationProblem", function(x) x$row_ids())

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @exportMethod compressed_formulation
#' @usage compressed_formulation(x)
methods::setGeneric(
  "compressed_formulation",
  function(x) {
    assert_required(x)
    standardGeneric("compressed_formulation")
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @usage \S4method{compressed_formulation}{OptimizationProblem}(x)
methods::setMethod(
  "compressed_formulation",
  "OptimizationProblem",
  function(x) x$compressed_formulation()
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @exportMethod set_obj
#' @usage set_obj(x, obj)
methods::setGeneric(
  "set_obj",
  signature = methods::signature("x", "obj"),
  function(x, obj) {
    assert_required(x)
    assert_required(obj)
    standardGeneric("set_obj")
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @usage \S4method{set_obj}{OptimizationProblem,ANY}(x, obj)
methods::setMethod(
  "set_obj",
  methods::signature("OptimizationProblem", "ANY"),
  function(x, obj) x$set_obj(obj)
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @exportMethod set_lb
#' @usage set_lb(x, lb)
methods::setGeneric(
  "set_lb",
  signature = methods::signature("x", "lb"),
  function(x, lb) {
    assert_required(x)
    assert_required(lb)
    standardGeneric("set_lb")
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @usage \S4method{set_lb}{OptimizationProblem,ANY}(x, lb)
methods::setMethod(
  "set_lb",
  methods::signature("OptimizationProblem", "ANY"),
  function(x, lb) x$set_lb(lb)
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @exportMethod set_ub
#' @usage set_ub(x, ub)
methods::setGeneric(
  "set_ub",
  signature = methods::signature("x", "ub"),
  function(x, ub) {
    assert_required(x)
    assert_required(ub)
    standardGeneric("set_ub")
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @usage \S4method{set_ub}{OptimizationProblem,ANY}(x, ub)
methods::setMethod(
  "set_ub",
  methods::signature("OptimizationProblem", "ANY"),
  function(x, ub) x$set_ub(ub)
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @exportMethod append_linear_constraints
#' @usage append_linear_constraints(x, rhs, sense, A, row_ids)
methods::setGeneric(
  "append_linear_constraints",
  function(x, rhs, sense, A, row_ids) {
    assert_required(x)
    assert_required(rhs)
    assert_required(sense)
    assert_required(A)
    assert_required(row_ids)
    standardGeneric("append_linear_constraints")
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @usage \S4method{append_linear_constraints}{OptimizationProblem,ANY,ANY,ANY,ANY}(x, rhs, sense, A, row_ids)
methods::setMethod(
  "append_linear_constraints",
  methods::signature("OptimizationProblem", "ANY", "ANY", "ANY", "ANY"),
  function(x, rhs, sense, A, row_ids) {
    x$append_linear_constraints(rhs, sense, A, row_ids)
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @exportMethod remove_last_linear_constraint
#' @usage remove_last_linear_constraint(x)
methods::setGeneric(
  "remove_last_linear_constraint",
  signature = methods::signature("x"),
  function(x) {
    assert_required(x)
    standardGeneric("remove_last_linear_constraint")
  }
)

#' @name OptimizationProblem-methods
#' @rdname OptimizationProblem-methods
#' @usage \S4method{remove_last_linear_constraint}{OptimizationProblem}(x)
methods::setMethod(
  "remove_last_linear_constraint",
  methods::signature("OptimizationProblem"),
  function(x) {
    x$remove_last_linear_constraint()
  }
)
