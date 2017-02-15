#' @include internal.R Solver-proto.R ConservationProblem-proto.R 
#'   Constraint-proto.R Objective-proto.R Target-proto.R
NULL

#' Operators
#' 
#' This help page contains all the operators used in this R package.
#'
#' @param e1 Object.
#' @param e2 Object.
#'
#' @return Object.
#'
#' @name operators
NULL

#' @rdname operators
#' @export
setMethod('+', signature('Solver', 'ConservationProblem'), function(e1,e2) {
  stop('A conservation problem cannot be added to a solver object. Try reordering your code.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Solver', 'Constraint'), function(e1,e2) {
  stop('A constraint cannot be added to a solver. Try reordering code and ensure you have specified a problem.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Solver', 'Solver'), function(e1,e2) {
  stop('A solver cannot be added to a solver. You should only specify one solver.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Solver', 'Objective'), function(e1,e2) {
  stop('An objective cannot be added to a solver. Try reordering code and ensure you have specified a problem.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Solver', 'Target'), function(e1,e2) {
  stop('A target cannot be added to an solver. Try reordering code and ensure you have specified a problem.')
})

#' @rdname operators
#' @export
setMethod('+', signature('ConservationProblem', 'Solver'), function(e1,e2) {
  if (inherits(e1$solver_options, 'Solver'))
    warning('overwriting previously specified solver options')
  e1$solver <- e2
  e1
})

#' @rdname operators
#' @export
setMethod('+', signature('ConservationProblem', 'Constraint'), function(e1,e2) {
  check(e2$prevalidate(e1))
  e2$synchronize(e1)
  check(e2$postvalidate(e1))
  e1$constraints$add(e2)
  e1
})

#' @rdname operators
#' @export
setMethod('+', signature('ConservationProblem', 'Objective'), function(e1,e2) {
  if (inherits(e1$solver, 'Objective'))
    warning('overwriting previously specified objective')
  check(e2$prevalidate(e1))
  e2$synchronize(e1)
  check(e2$postvalidate(e1))
  e1$objective <- e2
  e1
})

#' @rdname operators
#' @export
setMethod('+', signature('ConservationProblem', 'ConservationProblem'), function(e1,e2) {
  stop('A problem cannot be added to a problem. You should only specify one problem.')
})

#' @rdname operators
#' @export
setMethod('+', signature('ConservationProblem', 'Target'), function(e1,e2) {
  if (inherits(e1$target, 'Target'))
    warning('overwriting previously specified targets')
  check(e2$prevalidate(e1))
  e2$synchronize(e1)
  check(e2$postvalidate(e1))
  e1$target <- e2
  e1
})

#' @rdname operators
#' @export
setMethod('+', signature('Constraint', 'ConservationProblem'), function(e1,e2) {
  stop('A problem cannot be added to a constraint. Try reordering code.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Constraint', 'Solver'), function(e1,e2) {
  stop('A solver cannot be added to a constraint. Try reordering code and ensure you have specified a problem.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Constraint', 'Constraint'), function(e1,e2) {
  stop('A constraint cannot be added to constraint. Ensure that you have specified a problem.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Constraint', 'Objective'), function(e1,e2) {
  stop('An objective cannot be added to constraint. Ensure that you have specified a problem.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Constraint', 'Target'), function(e1,e2) {
  stop('An target cannot be added to constraint. Ensure that you have specified a problem.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Objective', 'ConservationProblem'), function(e1,e2) {
  stop('A problem cannot be added to an objective. Try reordering code.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Objective', 'Solver'), function(e1,e2) {
  stop('A solver cannot be added to an objective. Try reordering code and ensure you have specified a problem.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Objective', 'Constraint'), function(e1,e2) {
  stop('A constraint cannot be added to an objective. Ensure that you have specified a problem.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Objective', 'Objective'), function(e1,e2) {
  stop('An objective cannot be added to an objective. Only one objective should be specified.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Objective', 'Target'), function(e1,e2) {
  stop('A target cannot be added to an objective. Only one objective should be specified.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Target', 'ConservationProblem'), function(e1,e2) {
  stop('A problem cannot be added to a target. Try reordering code.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Target', 'Solver'), function(e1,e2) {
  stop('A solver cannot be added to a target. Try reordering code and ensure you have specified a problem.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Target', 'Constraint'), function(e1,e2) {
  stop('A constraint cannot be added to a target. Ensure that you have specified a problem.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Target', 'Objective'), function(e1,e2) {
  stop('An objective cannot be added to a target. Only one objective should be specified.')
})

#' @rdname operators
#' @export
setMethod('+', signature('Target', 'Target'), function(e1,e2) {
  stop('A target cannot be added to a target. Only one objective should be specified.')
})

