#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _prioritizr_rcpp_add_rij_data(SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_asymmetric_boundary_constraints(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_binary_decisions(SEXP);
extern SEXP _prioritizr_rcpp_apply_connected_constraints(SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_corridor_constraints(SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_feature_weights(SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_locked_in_constraints(SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_locked_out_constraints(SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_max_cover_objective(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_max_features_objective(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_max_phylo_objective(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_min_set_objective(SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_neighbor_constraints(SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_proportion_decisions(SEXP);
extern SEXP _prioritizr_rcpp_apply_semicontinuous_decisions(SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_symmetric_boundary_constraints(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_boundary_data(SEXP, SEXP);
extern SEXP _prioritizr_rcpp_branch_matrix(SEXP);
extern SEXP _prioritizr_rcpp_forbid_solution(SEXP, SEXP);
extern SEXP _prioritizr_rcpp_get_optimization_problem_A(SEXP);
extern SEXP _prioritizr_rcpp_get_optimization_problem_col_ids(SEXP);
extern SEXP _prioritizr_rcpp_get_optimization_problem_compressed_formulation(SEXP);
extern SEXP _prioritizr_rcpp_get_optimization_problem_lb(SEXP);
extern SEXP _prioritizr_rcpp_get_optimization_problem_modelsense(SEXP);
extern SEXP _prioritizr_rcpp_get_optimization_problem_ncell(SEXP);
extern SEXP _prioritizr_rcpp_get_optimization_problem_ncol(SEXP);
extern SEXP _prioritizr_rcpp_get_optimization_problem_nrow(SEXP);
extern SEXP _prioritizr_rcpp_get_optimization_problem_number_of_features(SEXP);
extern SEXP _prioritizr_rcpp_get_optimization_problem_number_of_planning_units(SEXP);
extern SEXP _prioritizr_rcpp_get_optimization_problem_obj(SEXP);
extern SEXP _prioritizr_rcpp_get_optimization_problem_rhs(SEXP);
extern SEXP _prioritizr_rcpp_get_optimization_problem_row_ids(SEXP);
extern SEXP _prioritizr_rcpp_get_optimization_problem_sense(SEXP);
extern SEXP _prioritizr_rcpp_get_optimization_problem_ub(SEXP);
extern SEXP _prioritizr_rcpp_get_optimization_problem_vtype(SEXP);
extern SEXP _prioritizr_rcpp_list_to_matrix_indices(SEXP, SEXP);
extern SEXP _prioritizr_rcpp_new_optimization_problem(SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_optimization_problem_as_list(SEXP);
extern SEXP _prioritizr_rcpp_predefined_optimization_problem(SEXP);
extern SEXP _prioritizr_rcpp_set_optimization_problem_shuffled(SEXP);
extern SEXP _prioritizr_rcpp_sp_to_polyset(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_prioritizr_rcpp_add_rij_data",                                      (DL_FUNC) &_prioritizr_rcpp_add_rij_data,                                      3},
    {"_prioritizr_rcpp_apply_asymmetric_boundary_constraints",             (DL_FUNC) &_prioritizr_rcpp_apply_asymmetric_boundary_constraints,             4},
    {"_prioritizr_rcpp_apply_binary_decisions",                            (DL_FUNC) &_prioritizr_rcpp_apply_binary_decisions,                            1},
    {"_prioritizr_rcpp_apply_connected_constraints",                       (DL_FUNC) &_prioritizr_rcpp_apply_connected_constraints,                       2},
    {"_prioritizr_rcpp_apply_corridor_constraints",                        (DL_FUNC) &_prioritizr_rcpp_apply_corridor_constraints,                        3},
    {"_prioritizr_rcpp_apply_feature_weights",                             (DL_FUNC) &_prioritizr_rcpp_apply_feature_weights,                             2},
    {"_prioritizr_rcpp_apply_locked_in_constraints",                       (DL_FUNC) &_prioritizr_rcpp_apply_locked_in_constraints,                       2},
    {"_prioritizr_rcpp_apply_locked_out_constraints",                      (DL_FUNC) &_prioritizr_rcpp_apply_locked_out_constraints,                      2},
    {"_prioritizr_rcpp_apply_max_cover_objective",                         (DL_FUNC) &_prioritizr_rcpp_apply_max_cover_objective,                         4},
    {"_prioritizr_rcpp_apply_max_features_objective",                      (DL_FUNC) &_prioritizr_rcpp_apply_max_features_objective,                      4},
    {"_prioritizr_rcpp_apply_max_phylo_objective",                         (DL_FUNC) &_prioritizr_rcpp_apply_max_phylo_objective,                         6},
    {"_prioritizr_rcpp_apply_min_set_objective",                           (DL_FUNC) &_prioritizr_rcpp_apply_min_set_objective,                           3},
    {"_prioritizr_rcpp_apply_neighbor_constraints",                        (DL_FUNC) &_prioritizr_rcpp_apply_neighbor_constraints,                        3},
    {"_prioritizr_rcpp_apply_proportion_decisions",                        (DL_FUNC) &_prioritizr_rcpp_apply_proportion_decisions,                        1},
    {"_prioritizr_rcpp_apply_semicontinuous_decisions",                    (DL_FUNC) &_prioritizr_rcpp_apply_semicontinuous_decisions,                    2},
    {"_prioritizr_rcpp_apply_symmetric_boundary_constraints",              (DL_FUNC) &_prioritizr_rcpp_apply_symmetric_boundary_constraints,              4},
    {"_prioritizr_rcpp_boundary_data",                                     (DL_FUNC) &_prioritizr_rcpp_boundary_data,                                     2},
    {"_prioritizr_rcpp_branch_matrix",                                     (DL_FUNC) &_prioritizr_rcpp_branch_matrix,                                     1},
    {"_prioritizr_rcpp_forbid_solution",                                   (DL_FUNC) &_prioritizr_rcpp_forbid_solution,                                   2},
    {"_prioritizr_rcpp_get_optimization_problem_A",                        (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_A,                        1},
    {"_prioritizr_rcpp_get_optimization_problem_col_ids",                  (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_col_ids,                  1},
    {"_prioritizr_rcpp_get_optimization_problem_compressed_formulation",   (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_compressed_formulation,   1},
    {"_prioritizr_rcpp_get_optimization_problem_lb",                       (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_lb,                       1},
    {"_prioritizr_rcpp_get_optimization_problem_modelsense",               (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_modelsense,               1},
    {"_prioritizr_rcpp_get_optimization_problem_ncell",                    (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_ncell,                    1},
    {"_prioritizr_rcpp_get_optimization_problem_ncol",                     (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_ncol,                     1},
    {"_prioritizr_rcpp_get_optimization_problem_nrow",                     (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_nrow,                     1},
    {"_prioritizr_rcpp_get_optimization_problem_number_of_features",       (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_number_of_features,       1},
    {"_prioritizr_rcpp_get_optimization_problem_number_of_planning_units", (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_number_of_planning_units, 1},
    {"_prioritizr_rcpp_get_optimization_problem_obj",                      (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_obj,                      1},
    {"_prioritizr_rcpp_get_optimization_problem_rhs",                      (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_rhs,                      1},
    {"_prioritizr_rcpp_get_optimization_problem_row_ids",                  (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_row_ids,                  1},
    {"_prioritizr_rcpp_get_optimization_problem_sense",                    (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_sense,                    1},
    {"_prioritizr_rcpp_get_optimization_problem_ub",                       (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_ub,                       1},
    {"_prioritizr_rcpp_get_optimization_problem_vtype",                    (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_vtype,                    1},
    {"_prioritizr_rcpp_list_to_matrix_indices",                            (DL_FUNC) &_prioritizr_rcpp_list_to_matrix_indices,                            2},
    {"_prioritizr_rcpp_new_optimization_problem",                          (DL_FUNC) &_prioritizr_rcpp_new_optimization_problem,                          3},
    {"_prioritizr_rcpp_optimization_problem_as_list",                      (DL_FUNC) &_prioritizr_rcpp_optimization_problem_as_list,                      1},
    {"_prioritizr_rcpp_predefined_optimization_problem",                   (DL_FUNC) &_prioritizr_rcpp_predefined_optimization_problem,                   1},
    {"_prioritizr_rcpp_set_optimization_problem_shuffled",                 (DL_FUNC) &_prioritizr_rcpp_set_optimization_problem_shuffled,                 1},
    {"_prioritizr_rcpp_sp_to_polyset",                                     (DL_FUNC) &_prioritizr_rcpp_sp_to_polyset,                                     3},
    {NULL, NULL, 0}
};

void R_init_prioritizr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
