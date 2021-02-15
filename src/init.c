#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _prioritizr_rcpp_absolute_amount_held_by_solution(SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_add_rij_data(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_add_zones_constraints(SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_boundary_penalties(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_bounded_constraints(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_connectivity_penalties(SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_contiguity_constraints(SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_decisions(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_feature_contiguity_constraints(SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_feature_weights(SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_linear_penalties(SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_locked_constraints(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_max_cover_objective(SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_max_features_objective(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_max_phylo_objective(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_max_utility_objective(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_min_largest_shortfall_objective(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_min_set_objective(SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_min_shortfall_objective(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_apply_neighbor_constraints(SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_boundary(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_boundary_data(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioritizr_rcpp_branch_matrix(SEXP);
extern SEXP _prioritizr_rcpp_connectivity(SEXP, SEXP);
extern SEXP _prioritizr_rcpp_ferrier_score(SEXP, SEXP, SEXP, SEXP);
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
extern SEXP _prioritizr_rcpp_get_optimization_problem_number_of_zones(SEXP);
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
extern SEXP _prioritizr_rcpp_str_tree_to_sparse_matrix(SEXP);
extern SEXP _prioritizr_rcpp_summarize_exactextractr(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_prioritizr_rcpp_absolute_amount_held_by_solution",                  (DL_FUNC) &_prioritizr_rcpp_absolute_amount_held_by_solution,                  3},
    {"_prioritizr_rcpp_add_rij_data",                                      (DL_FUNC) &_prioritizr_rcpp_add_rij_data,                                      4},
    {"_prioritizr_rcpp_add_zones_constraints",                             (DL_FUNC) &_prioritizr_rcpp_add_zones_constraints,                             2},
    {"_prioritizr_rcpp_apply_boundary_penalties",                          (DL_FUNC) &_prioritizr_rcpp_apply_boundary_penalties,                          5},
    {"_prioritizr_rcpp_apply_bounded_constraints",                         (DL_FUNC) &_prioritizr_rcpp_apply_bounded_constraints,                         5},
    {"_prioritizr_rcpp_apply_connectivity_penalties",                      (DL_FUNC) &_prioritizr_rcpp_apply_connectivity_penalties,                      3},
    {"_prioritizr_rcpp_apply_contiguity_constraints",                      (DL_FUNC) &_prioritizr_rcpp_apply_contiguity_constraints,                      3},
    {"_prioritizr_rcpp_apply_decisions",                                   (DL_FUNC) &_prioritizr_rcpp_apply_decisions,                                   4},
    {"_prioritizr_rcpp_apply_feature_contiguity_constraints",              (DL_FUNC) &_prioritizr_rcpp_apply_feature_contiguity_constraints,              3},
    {"_prioritizr_rcpp_apply_feature_weights",                             (DL_FUNC) &_prioritizr_rcpp_apply_feature_weights,                             2},
    {"_prioritizr_rcpp_apply_linear_penalties",                            (DL_FUNC) &_prioritizr_rcpp_apply_linear_penalties,                            3},
    {"_prioritizr_rcpp_apply_locked_constraints",                          (DL_FUNC) &_prioritizr_rcpp_apply_locked_constraints,                          4},
    {"_prioritizr_rcpp_apply_max_cover_objective",                         (DL_FUNC) &_prioritizr_rcpp_apply_max_cover_objective,                         3},
    {"_prioritizr_rcpp_apply_max_features_objective",                      (DL_FUNC) &_prioritizr_rcpp_apply_max_features_objective,                      4},
    {"_prioritizr_rcpp_apply_max_phylo_objective",                         (DL_FUNC) &_prioritizr_rcpp_apply_max_phylo_objective,                         6},
    {"_prioritizr_rcpp_apply_max_utility_objective",                       (DL_FUNC) &_prioritizr_rcpp_apply_max_utility_objective,                       4},
    {"_prioritizr_rcpp_apply_min_largest_shortfall_objective",             (DL_FUNC) &_prioritizr_rcpp_apply_min_largest_shortfall_objective,             4},
    {"_prioritizr_rcpp_apply_min_set_objective",                           (DL_FUNC) &_prioritizr_rcpp_apply_min_set_objective,                           3},
    {"_prioritizr_rcpp_apply_min_shortfall_objective",                     (DL_FUNC) &_prioritizr_rcpp_apply_min_shortfall_objective,                     4},
    {"_prioritizr_rcpp_apply_neighbor_constraints",                        (DL_FUNC) &_prioritizr_rcpp_apply_neighbor_constraints,                        3},
    {"_prioritizr_rcpp_boundary",                                          (DL_FUNC) &_prioritizr_rcpp_boundary,                                          4},
    {"_prioritizr_rcpp_boundary_data",                                     (DL_FUNC) &_prioritizr_rcpp_boundary_data,                                     4},
    {"_prioritizr_rcpp_branch_matrix",                                     (DL_FUNC) &_prioritizr_rcpp_branch_matrix,                                     1},
    {"_prioritizr_rcpp_connectivity",                                      (DL_FUNC) &_prioritizr_rcpp_connectivity,                                      2},
    {"_prioritizr_rcpp_ferrier_score",                                     (DL_FUNC) &_prioritizr_rcpp_ferrier_score,                                     4},
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
    {"_prioritizr_rcpp_get_optimization_problem_number_of_zones",          (DL_FUNC) &_prioritizr_rcpp_get_optimization_problem_number_of_zones,          1},
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
    {"_prioritizr_rcpp_str_tree_to_sparse_matrix",                         (DL_FUNC) &_prioritizr_rcpp_str_tree_to_sparse_matrix,                         1},
    {"_prioritizr_rcpp_summarize_exactextractr",                           (DL_FUNC) &_prioritizr_rcpp_summarize_exactextractr,                           4},
    {NULL, NULL, 0}
};

void R_init_prioritizr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
