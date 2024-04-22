#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _prioritizr_rcpp_absolute_amount_held_by_solution(void *, void *, void *);
extern SEXP _prioritizr_rcpp_add_rij_data(void *, void *, void *, void *);
extern SEXP _prioritizr_rcpp_add_zones_constraints(void *, void *);
extern SEXP _prioritizr_rcpp_apply_asym_connectivity_penalties(void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_boundary_penalties(void *, void *, void *, void *, void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_bounded_constraints(void *, void *, void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_connectivity_penalties(void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_contiguity_constraints(void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_decisions(void *, void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_feature_contiguity_constraints(void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_feature_weights(void *, void *);
extern SEXP _prioritizr_rcpp_apply_linear_constraints(void *, void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_linear_penalties(void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_locked_constraints(void *, void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_max_cover_objective(void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_max_features_objective(void *, void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_max_phylo_objective(void *, void *, void *, void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_max_utility_objective(void *, void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_min_largest_shortfall_objective(void *, void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_min_set_objective(void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_min_shortfall_objective(void *, void *, void *, void *);
extern SEXP _prioritizr_rcpp_apply_neighbor_constraints(void *, void *, void *);
extern SEXP _prioritizr_rcpp_asym_connectivity(void *, void *);
extern SEXP _prioritizr_rcpp_boundary(void *, void *, void *, void *, void *, void *);
extern SEXP _prioritizr_rcpp_branch_matrix(void *);
extern SEXP _prioritizr_rcpp_connectivity(void *, void *);
extern SEXP _prioritizr_rcpp_copy_optimization_problem(void *);
extern SEXP _prioritizr_rcpp_ferrier_score(void *, void *, void *, void *);
extern SEXP _prioritizr_rcpp_forbid_solution(void *, void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_A(void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_col_ids(void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_compressed_formulation(void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_lb(void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_modelsense(void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_ncell(void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_ncol(void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_nrow(void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_number_of_features(void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_number_of_planning_units(void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_number_of_zones(void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_obj(void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_rhs(void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_row_ids(void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_sense(void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_ub(void *);
extern SEXP _prioritizr_rcpp_get_optimization_problem_vtype(void *);
extern SEXP _prioritizr_rcpp_list_to_matrix_indices(void *, void *);
extern SEXP _prioritizr_rcpp_new_optimization_problem(void *, void *, void *);
extern SEXP _prioritizr_rcpp_optimization_problem_as_list(void *);
extern SEXP _prioritizr_rcpp_predefined_optimization_problem(void *);
extern SEXP _prioritizr_rcpp_set_optimization_problem_shuffled(void *, void *);

static const R_CallMethodDef CallEntries[] = {
    {"_prioritizr_rcpp_absolute_amount_held_by_solution",                  (DL_FUNC) &_prioritizr_rcpp_absolute_amount_held_by_solution,                  3},
    {"_prioritizr_rcpp_add_rij_data",                                      (DL_FUNC) &_prioritizr_rcpp_add_rij_data,                                      4},
    {"_prioritizr_rcpp_add_zones_constraints",                             (DL_FUNC) &_prioritizr_rcpp_add_zones_constraints,                             2},
    {"_prioritizr_rcpp_apply_asym_connectivity_penalties",                 (DL_FUNC) &_prioritizr_rcpp_apply_asym_connectivity_penalties,                 3},
    {"_prioritizr_rcpp_apply_boundary_penalties",                          (DL_FUNC) &_prioritizr_rcpp_apply_boundary_penalties,                          7},
    {"_prioritizr_rcpp_apply_bounded_constraints",                         (DL_FUNC) &_prioritizr_rcpp_apply_bounded_constraints,                         5},
    {"_prioritizr_rcpp_apply_connectivity_penalties",                      (DL_FUNC) &_prioritizr_rcpp_apply_connectivity_penalties,                      3},
    {"_prioritizr_rcpp_apply_contiguity_constraints",                      (DL_FUNC) &_prioritizr_rcpp_apply_contiguity_constraints,                      3},
    {"_prioritizr_rcpp_apply_decisions",                                   (DL_FUNC) &_prioritizr_rcpp_apply_decisions,                                   4},
    {"_prioritizr_rcpp_apply_feature_contiguity_constraints",              (DL_FUNC) &_prioritizr_rcpp_apply_feature_contiguity_constraints,              3},
    {"_prioritizr_rcpp_apply_feature_weights",                             (DL_FUNC) &_prioritizr_rcpp_apply_feature_weights,                             2},
    {"_prioritizr_rcpp_apply_linear_constraints",                          (DL_FUNC) &_prioritizr_rcpp_apply_linear_constraints,                          4},
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
    {"_prioritizr_rcpp_asym_connectivity",                                 (DL_FUNC) &_prioritizr_rcpp_asym_connectivity,                                 2},
    {"_prioritizr_rcpp_boundary",                                          (DL_FUNC) &_prioritizr_rcpp_boundary,                                          6},
    {"_prioritizr_rcpp_branch_matrix",                                     (DL_FUNC) &_prioritizr_rcpp_branch_matrix,                                     1},
    {"_prioritizr_rcpp_connectivity",                                      (DL_FUNC) &_prioritizr_rcpp_connectivity,                                      2},
    {"_prioritizr_rcpp_copy_optimization_problem",                         (DL_FUNC) &_prioritizr_rcpp_copy_optimization_problem,                         1},
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
    {"_prioritizr_rcpp_set_optimization_problem_shuffled",                 (DL_FUNC) &_prioritizr_rcpp_set_optimization_problem_shuffled,                 2},
    {NULL, NULL, 0}
};

void R_init_prioritizr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
