# Package index

## Overview

Overview of the package.

- [`prioritizr`](https://prioritizr.net/reference/prioritizr.md)
  [`prioritizr-package`](https://prioritizr.net/reference/prioritizr.md)
  : prioritizr: Systematic Conservation Prioritization in R

## Create and solve problems

Functions for creating new problems and solving them.

- [`problem()`](https://prioritizr.net/reference/problem.md) :
  Conservation planning problem
- [`solve(`*`<ConservationProblem>`*`)`](https://prioritizr.net/reference/solve.md)
  [`solve(`*`<MultiConservationProblem>`*`)`](https://prioritizr.net/reference/solve.md)
  : Solve
- [`zones()`](https://prioritizr.net/reference/zones.md) : Management
  zones

## Data

Simulated datasets distributed with the package.

- [`get_sim_pu_polygons()`](https://prioritizr.net/reference/sim_data.md)
  [`get_sim_zones_pu_polygons()`](https://prioritizr.net/reference/sim_data.md)
  [`get_sim_pu_lines()`](https://prioritizr.net/reference/sim_data.md)
  [`get_sim_pu_points()`](https://prioritizr.net/reference/sim_data.md)
  [`get_sim_pu_raster()`](https://prioritizr.net/reference/sim_data.md)
  [`get_sim_locked_in_raster()`](https://prioritizr.net/reference/sim_data.md)
  [`get_sim_locked_out_raster()`](https://prioritizr.net/reference/sim_data.md)
  [`get_sim_zones_pu_raster()`](https://prioritizr.net/reference/sim_data.md)
  [`get_sim_features()`](https://prioritizr.net/reference/sim_data.md)
  [`get_sim_zones_features()`](https://prioritizr.net/reference/sim_data.md)
  [`get_sim_phylogeny()`](https://prioritizr.net/reference/sim_data.md)
  [`get_sim_complex_pu_raster()`](https://prioritizr.net/reference/sim_data.md)
  [`get_sim_complex_locked_in_raster()`](https://prioritizr.net/reference/sim_data.md)
  [`get_sim_complex_locked_out_raster()`](https://prioritizr.net/reference/sim_data.md)
  [`get_sim_complex_features()`](https://prioritizr.net/reference/sim_data.md)
  [`get_sim_complex_historical_features()`](https://prioritizr.net/reference/sim_data.md)
  : Get simulated conservation planning data

## Objectives

Functions for adding an objective to a problem.

- [`objectives`](https://prioritizr.net/reference/objectives.md) : Add
  an objective
- [`add_max_cover_objective()`](https://prioritizr.net/reference/add_max_cover_objective.md)
  : Add maximum coverage objective
- [`add_max_n_targets_met_objective()`](https://prioritizr.net/reference/add_max_n_targets_met_objective.md)
  : Add maximum number of targets met objective
- [`add_max_phylo_div_objective()`](https://prioritizr.net/reference/add_max_phylo_div_objective.md)
  : Add maximum phylogenetic diversity objective
- [`add_max_phylo_end_objective()`](https://prioritizr.net/reference/add_max_phylo_end_objective.md)
  : Add maximum phylogenetic endemism objective
- [`add_max_wtd_sum_objective()`](https://prioritizr.net/reference/add_max_wtd_sum_objective.md)
  : Add maximum weighted sum objective
- [`add_min_largest_shortfall_objective()`](https://prioritizr.net/reference/add_min_largest_shortfall_objective.md)
  : Add minimum largest shortfall objective
- [`add_min_penalties_objective()`](https://prioritizr.net/reference/add_min_penalties_objective.md)
  : Add minimum penalties objective
- [`add_min_set_objective()`](https://prioritizr.net/reference/add_min_set_objective.md)
  : Add minimum set objective
- [`add_min_shortfall_objective()`](https://prioritizr.net/reference/add_min_shortfall_objective.md)
  : Add minimum shortfall objective

## Targets

Functions for adding targets to a problem.

- [`targets`](https://prioritizr.net/reference/targets.md) : Add
  representation targets

- [`add_absolute_targets()`](https://prioritizr.net/reference/add_absolute_targets.md)
  : Add absolute targets

- [`add_auto_targets(`*`<ConservationProblem>`*`,`*`<character>`*`)`](https://prioritizr.net/reference/add_auto_targets.md)
  [`add_auto_targets(`*`<ConservationProblem>`*`,`*`<list>`*`)`](https://prioritizr.net/reference/add_auto_targets.md)
  [`add_auto_targets(`*`<ConservationProblem>`*`,`*`<TargetMethod>`*`)`](https://prioritizr.net/reference/add_auto_targets.md)
  : Add targets automatically

- [`add_group_targets()`](https://prioritizr.net/reference/add_group_targets.md)
  : Add targets based on feature groups

- [`add_manual_targets()`](https://prioritizr.net/reference/add_manual_targets.md)
  : Add manual targets

- [`add_relative_targets()`](https://prioritizr.net/reference/add_relative_targets.md)
  : Add relative targets

- [`spec_absolute_targets()`](https://prioritizr.net/reference/spec_absolute_targets.md)
  : Specify absolute targets

- [`spec_area_targets()`](https://prioritizr.net/reference/spec_area_targets.md)
  : Specify targets based on area units

- [`spec_duran_targets()`](https://prioritizr.net/reference/spec_duran_targets.md)
  :

  Specify targets following Durán *et al.* (2020)

- [`spec_interp_absolute_targets()`](https://prioritizr.net/reference/spec_interp_absolute_targets.md)
  : Specify targets based on interpolating absolute thresholds

- [`spec_interp_area_targets()`](https://prioritizr.net/reference/spec_interp_area_targets.md)
  : Specify targets based on interpolating area-based thresholds

- [`spec_jung_targets()`](https://prioritizr.net/reference/spec_jung_targets.md)
  :

  Specify targets following Jung *et al.* (2021)

- [`spec_max_targets()`](https://prioritizr.net/reference/spec_max_targets.md)
  : Specify targets based on maxima

- [`spec_min_targets()`](https://prioritizr.net/reference/spec_min_targets.md)
  : Specify targets based on minima

- [`spec_polak_targets()`](https://prioritizr.net/reference/spec_polak_targets.md)
  :

  Specify targets following Polak *et al.* (2015)

- [`spec_pop_size_targets()`](https://prioritizr.net/reference/spec_pop_size_targets.md)
  : Specify targets based on population size

- [`spec_relative_targets()`](https://prioritizr.net/reference/spec_relative_targets.md)
  : Specify relative targets

- [`spec_rl_ecosystem_targets()`](https://prioritizr.net/reference/spec_rl_ecosystem_targets.md)
  : Specify targets based on the IUCN Red List of Ecosystems

- [`spec_rl_species_targets()`](https://prioritizr.net/reference/spec_rl_species_targets.md)
  : Specify targets based on the IUCN Red List of Threatened Species

- [`spec_rodrigues_targets()`](https://prioritizr.net/reference/spec_rodrigues_targets.md)
  :

  Specify targets following Rodrigues *et al.* (2004)

- [`spec_rule_targets()`](https://prioritizr.net/reference/spec_rule_targets.md)
  : Specify targets following a set of rules

- [`spec_sreekar_targets()`](https://prioritizr.net/reference/spec_sreekar_targets.md)
  : Specify targets following Sreekar and Watson (2026)

- [`spec_ward_targets()`](https://prioritizr.net/reference/spec_ward_targets.md)
  :

  Specify targets following Ward *et al.* (2025)

- [`spec_watson_targets()`](https://prioritizr.net/reference/spec_watson_targets.md)
  :

  Specify targets following Watson *et al.* (2010)

- [`spec_wilson_targets()`](https://prioritizr.net/reference/spec_wilson_targets.md)
  :

  Specify targets following Wilson *et al.* (2010)

## Constraints

Functions for adding constraints to a problem.

- [`constraints`](https://prioritizr.net/reference/constraints.md) :
  Conservation problem constraints
- [`add_contiguity_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](https://prioritizr.net/reference/add_contiguity_constraints.md)
  [`add_contiguity_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<data.frame>`*`)`](https://prioritizr.net/reference/add_contiguity_constraints.md)
  [`add_contiguity_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<matrix>`*`)`](https://prioritizr.net/reference/add_contiguity_constraints.md)
  : Add contiguity constraints
- [`add_cost_constraints()`](https://prioritizr.net/reference/add_cost_constraints.md)
  : Add cost constraints
- [`add_feature_contiguity_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<data.frame>`*`)`](https://prioritizr.net/reference/add_feature_contiguity_constraints.md)
  [`add_feature_contiguity_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<matrix>`*`)`](https://prioritizr.net/reference/add_feature_contiguity_constraints.md)
  [`add_feature_contiguity_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](https://prioritizr.net/reference/add_feature_contiguity_constraints.md)
  : Add feature contiguity constraints
- [`add_linear_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<character>`*`)`](https://prioritizr.net/reference/add_linear_constraints.md)
  [`add_linear_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<numeric>`*`)`](https://prioritizr.net/reference/add_linear_constraints.md)
  [`add_linear_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<matrix>`*`)`](https://prioritizr.net/reference/add_linear_constraints.md)
  [`add_linear_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<Matrix>`*`)`](https://prioritizr.net/reference/add_linear_constraints.md)
  [`add_linear_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<Raster>`*`)`](https://prioritizr.net/reference/add_linear_constraints.md)
  [`add_linear_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<SpatRaster>`*`)`](https://prioritizr.net/reference/add_linear_constraints.md)
  [`add_linear_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<dgCMatrix>`*`)`](https://prioritizr.net/reference/add_linear_constraints.md)
  : Add linear constraints
- [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md)
  : Add locked in constraints
- [`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md)
  : Add locked out constraints
- [`add_mandatory_allocation_constraints()`](https://prioritizr.net/reference/add_mandatory_allocation_constraints.md)
  : Add mandatory allocation constraints
- [`add_manual_bounded_constraints()`](https://prioritizr.net/reference/add_manual_bounded_constraints.md)
  : Add manually specified bound constraints
- [`add_manual_locked_constraints()`](https://prioritizr.net/reference/add_manual_locked_constraints.md)
  : Add manually specified locked constraints
- [`add_neighbor_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](https://prioritizr.net/reference/add_neighbor_constraints.md)
  [`add_neighbor_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<data.frame>`*`)`](https://prioritizr.net/reference/add_neighbor_constraints.md)
  [`add_neighbor_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<matrix>`*`)`](https://prioritizr.net/reference/add_neighbor_constraints.md)
  [`add_neighbor_constraints(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<array>`*`)`](https://prioritizr.net/reference/add_neighbor_constraints.md)
  : Add neighbor constraints

## Penalties

Functions for adding penalties to a problem.

- [`penalties`](https://prioritizr.net/reference/penalties.md) : Add a
  penalty
- [`add_asym_connectivity_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<matrix>`*`)`](https://prioritizr.net/reference/add_asym_connectivity_penalties.md)
  [`add_asym_connectivity_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<Matrix>`*`)`](https://prioritizr.net/reference/add_asym_connectivity_penalties.md)
  [`add_asym_connectivity_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<data.frame>`*`)`](https://prioritizr.net/reference/add_asym_connectivity_penalties.md)
  [`add_asym_connectivity_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<dgCMatrix>`*`)`](https://prioritizr.net/reference/add_asym_connectivity_penalties.md)
  [`add_asym_connectivity_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<array>`*`)`](https://prioritizr.net/reference/add_asym_connectivity_penalties.md)
  : Add asymmetric connectivity penalties
- [`add_boundary_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<data.frame>`*`)`](https://prioritizr.net/reference/add_boundary_penalties.md)
  [`add_boundary_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<matrix>`*`)`](https://prioritizr.net/reference/add_boundary_penalties.md)
  [`add_boundary_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](https://prioritizr.net/reference/add_boundary_penalties.md)
  : Add boundary penalties
- [`add_connectivity_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<matrix>`*`)`](https://prioritizr.net/reference/add_connectivity_penalties.md)
  [`add_connectivity_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<Matrix>`*`)`](https://prioritizr.net/reference/add_connectivity_penalties.md)
  [`add_connectivity_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<data.frame>`*`)`](https://prioritizr.net/reference/add_connectivity_penalties.md)
  [`add_connectivity_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<dgCMatrix>`*`)`](https://prioritizr.net/reference/add_connectivity_penalties.md)
  [`add_connectivity_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<array>`*`)`](https://prioritizr.net/reference/add_connectivity_penalties.md)
  : Add connectivity penalties
- [`add_cost_penalties()`](https://prioritizr.net/reference/add_cost_penalties.md)
  : Add cost penalties
- [`add_feature_weights(`*`<ConservationProblem>`*`,`*`<numeric>`*`)`](https://prioritizr.net/reference/add_feature_weights.md)
  [`add_feature_weights(`*`<ConservationProblem>`*`,`*`<matrix>`*`)`](https://prioritizr.net/reference/add_feature_weights.md)
  : Add feature weights
- [`add_linear_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<character>`*`)`](https://prioritizr.net/reference/add_linear_penalties.md)
  [`add_linear_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<numeric>`*`)`](https://prioritizr.net/reference/add_linear_penalties.md)
  [`add_linear_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<matrix>`*`)`](https://prioritizr.net/reference/add_linear_penalties.md)
  [`add_linear_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<Matrix>`*`)`](https://prioritizr.net/reference/add_linear_penalties.md)
  [`add_linear_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<Raster>`*`)`](https://prioritizr.net/reference/add_linear_penalties.md)
  [`add_linear_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<SpatRaster>`*`)`](https://prioritizr.net/reference/add_linear_penalties.md)
  [`add_linear_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<dgCMatrix>`*`)`](https://prioritizr.net/reference/add_linear_penalties.md)
  : Add linear penalties
- [`add_neighbor_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<matrix>`*`)`](https://prioritizr.net/reference/add_neighbor_penalties.md)
  [`add_neighbor_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<data.frame>`*`)`](https://prioritizr.net/reference/add_neighbor_penalties.md)
  [`add_neighbor_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](https://prioritizr.net/reference/add_neighbor_penalties.md)
  [`add_neighbor_penalties(`*`<ConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<array>`*`)`](https://prioritizr.net/reference/add_neighbor_penalties.md)
  : Add neighbor penalties
- [`calibrate_cohon_penalty()`](https://prioritizr.net/reference/calibrate_cohon_penalty.md)
  : Calibrate penalties with Cohon's method

## Decisions

Functions for specifying the type of decisions in a problem.

- [`decisions`](https://prioritizr.net/reference/decisions.md) : Add
  decision types
- [`add_binary_decisions()`](https://prioritizr.net/reference/add_binary_decisions.md)
  : Add binary decisions
- [`add_proportion_decisions()`](https://prioritizr.net/reference/add_proportion_decisions.md)
  : Add proportion decisions
- [`add_semicontinuous_decisions()`](https://prioritizr.net/reference/add_semicontinuous_decisions.md)
  : Add semi-continuous decisions

## Solvers

Functions for specifying how a problem should be solved.

- [`solvers`](https://prioritizr.net/reference/solvers.md) : Add solvers

- [`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md)
  :

  Add a *CBC* solver

- [`add_cplex_solver()`](https://prioritizr.net/reference/add_cplex_solver.md)
  :

  Add a *CPLEX* solver

- [`add_default_solver()`](https://prioritizr.net/reference/add_default_solver.md)
  : Add default solver

- [`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md)
  :

  Add a *Gurobi* solver

- [`add_highs_solver()`](https://prioritizr.net/reference/add_highs_solver.md)
  :

  Add a *HiGHS* solver

- [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)
  :

  Add a *SYMPHONY* solver with *lpsymphony*

- [`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md)
  :

  Add a *SYMPHONY* solver with *Rsymphony*

## Portfolios

Functions for generating a portfolio of solutions.

- [`portfolios`](https://prioritizr.net/reference/portfolios.md) : Add
  portfolios
- [`add_cuts_portfolio()`](https://prioritizr.net/reference/add_cuts_portfolio.md)
  : Add Bender's cuts portfolio
- [`add_default_portfolio()`](https://prioritizr.net/reference/add_default_portfolio.md)
  : Add a default portfolio
- [`add_extra_portfolio()`](https://prioritizr.net/reference/add_extra_portfolio.md)
  : Add an extra portfolio
- [`add_gap_portfolio()`](https://prioritizr.net/reference/add_gap_portfolio.md)
  : Add a gap portfolio
- [`add_shuffle_portfolio()`](https://prioritizr.net/reference/add_shuffle_portfolio.md)
  : Add a shuffle portfolio
- [`add_single_portfolio()`](https://prioritizr.net/reference/add_single_portfolio.md)
  : Add a single portfolio
- [`add_top_portfolio()`](https://prioritizr.net/reference/add_top_portfolio.md)
  : Add a top portfolio

## Summary statistics

Functions for summarizing the performance of solutions.

- [`summaries`](https://prioritizr.net/reference/summaries.md) :
  Evaluate solutions using summary statistics
- [`eval_asym_connectivity_summary(`*`<GenericConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<matrix>`*`)`](https://prioritizr.net/reference/eval_asym_connectivity_summary.md)
  [`eval_asym_connectivity_summary(`*`<GenericConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<Matrix>`*`)`](https://prioritizr.net/reference/eval_asym_connectivity_summary.md)
  [`eval_asym_connectivity_summary(`*`<GenericConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<data.frame>`*`)`](https://prioritizr.net/reference/eval_asym_connectivity_summary.md)
  [`eval_asym_connectivity_summary(`*`<GenericConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<dgCMatrix>`*`)`](https://prioritizr.net/reference/eval_asym_connectivity_summary.md)
  [`eval_asym_connectivity_summary(`*`<GenericConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<array>`*`)`](https://prioritizr.net/reference/eval_asym_connectivity_summary.md)
  : Evaluate asymmetric connectivity of solution
- [`eval_boundary_summary()`](https://prioritizr.net/reference/eval_boundary_summary.md)
  : Evaluate boundary length of solution
- [`eval_connectivity_summary(`*`<GenericConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<matrix>`*`)`](https://prioritizr.net/reference/eval_connectivity_summary.md)
  [`eval_connectivity_summary(`*`<GenericConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<Matrix>`*`)`](https://prioritizr.net/reference/eval_connectivity_summary.md)
  [`eval_connectivity_summary(`*`<GenericConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<data.frame>`*`)`](https://prioritizr.net/reference/eval_connectivity_summary.md)
  [`eval_connectivity_summary(`*`<GenericConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<dgCMatrix>`*`)`](https://prioritizr.net/reference/eval_connectivity_summary.md)
  [`eval_connectivity_summary(`*`<GenericConservationProblem>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<array>`*`)`](https://prioritizr.net/reference/eval_connectivity_summary.md)
  : Evaluate connectivity of solution
- [`eval_cost_summary()`](https://prioritizr.net/reference/eval_cost_summary.md)
  : Evaluate cost of solution
- [`eval_feature_representation_summary()`](https://prioritizr.net/reference/eval_feature_representation_summary.md)
  : Evaluate feature representation by solution
- [`eval_n_summary()`](https://prioritizr.net/reference/eval_n_summary.md)
  : Evaluate number of planning units selected by solution
- [`eval_objective_summary()`](https://prioritizr.net/reference/eval_objective_summary.md)
  : Evaluate objective value of solution
- [`eval_target_coverage_summary()`](https://prioritizr.net/reference/eval_target_coverage_summary.md)
  : Evaluate target coverage by solution

## Importance

Functions for calculating importance scores for a solution.

- [`importance`](https://prioritizr.net/reference/importance.md)
  [`irreplaceability`](https://prioritizr.net/reference/importance.md) :
  Evaluate solution importance
- [`eval_ferrier_importance()`](https://prioritizr.net/reference/eval_ferrier_importance.md)
  : Evaluate solution importance using Ferrier scores
- [`eval_rank_importance()`](https://prioritizr.net/reference/eval_rank_importance.md)
  : Evaluate solution importance using incremental ranks
- [`eval_rare_richness_importance()`](https://prioritizr.net/reference/eval_rare_richness_importance.md)
  : Evaluate solution importance using rarity weighted richness scores
- [`eval_replacement_importance()`](https://prioritizr.net/reference/eval_replacement_importance.md)
  : Evaluate solution importance using replacement cost scores

## Multi-objective optimization

Functions for multi-objective optimization.

- [`multi_problem()`](https://prioritizr.net/reference/multi_problem.md)
  : Multi-objective conservation planning problem
- [`approaches`](https://prioritizr.net/reference/approaches.md) : Add
  an approach
- [`add_hier_approach()`](https://prioritizr.net/reference/add_hier_approach.md)
  : Add a hierarchical approach
- [`add_ref_point_approach()`](https://prioritizr.net/reference/add_ref_point_approach.md)
  : Add a reference point approach
- [`add_wtd_sum_approach()`](https://prioritizr.net/reference/add_wtd_sum_approach.md)
  : Add a weighted sum approach
- [`approach_weights_matrix()`](https://prioritizr.net/reference/approach_weights_matrix.md)
  : Create weight values for a multi-objective approach
- [`approach_rel_tol_matrix()`](https://prioritizr.net/reference/approach_rel_tol_matrix.md)
  : Create relative tolerance values for a multi-objective approach

## Data simulation

Functions for simulating new datasets.

- [`simulate_cost()`](https://prioritizr.net/reference/simulate_cost.md)
  : Simulate cost data
- [`simulate_data()`](https://prioritizr.net/reference/simulate_data.md)
  : Simulate data
- [`simulate_species()`](https://prioritizr.net/reference/simulate_species.md)
  : Simulate species habitat suitability data

## Geoprocessing

Functions for manipulating spatial datasets.

- [`fast_extract()`](https://prioritizr.net/reference/fast_extract.md) :
  Fast extract
- [`intersecting_units()`](https://prioritizr.net/reference/intersecting_units.md)
  : Find intersecting units

## Marxan functions

Functions for importing and converting *Marxan* data.

- [`marxan_problem()`](https://prioritizr.net/reference/marxan_problem.md)
  :

  *Marxan* conservation problem

- [`marxan_boundary_data_to_matrix()`](https://prioritizr.net/reference/marxan_boundary_data_to_matrix.md)
  :

  Convert *Marxan* boundary data to matrix format

- [`marxan_connectivity_data_to_matrix()`](https://prioritizr.net/reference/marxan_connectivity_data_to_matrix.md)
  :

  Convert *Marxan* connectivity data to matrix format

## Matrix functions

Functions for creating matrices that are used in conservation planning
problems.

- [`adjacency_matrix()`](https://prioritizr.net/reference/adjacency_matrix.md)
  : Adjacency matrix
- [`boundary_matrix()`](https://prioritizr.net/reference/boundary_matrix.md)
  : Boundary matrix
- [`branch_matrix()`](https://prioritizr.net/reference/branch_matrix.md)
  : Branch matrix
- [`connectivity_matrix()`](https://prioritizr.net/reference/connectivity_matrix.md)
  : Connectivity matrix
- [`proximity_matrix()`](https://prioritizr.net/reference/proximity_matrix.md)
  : Proximity matrix
- [`rij_matrix()`](https://prioritizr.net/reference/rij_matrix.md) :
  Feature by planning unit matrix
- [`rescale_matrix()`](https://prioritizr.net/reference/rescale_matrix.md)
  : Rescale a matrix

## Processing multi-zone data

Functions for manipulating data that pertain to multiple zones.

- [`category_layer()`](https://prioritizr.net/reference/category_layer.md)
  : Category layer
- [`category_vector()`](https://prioritizr.net/reference/category_vector.md)
  : Category vector
- [`binary_stack()`](https://prioritizr.net/reference/binary_stack.md) :
  Binary stack

## Problem manipulation functions

Functions for working with problems.

- [`compile()`](https://prioritizr.net/reference/compile.md) : Compile a
  problem
- [`feature_abundances()`](https://prioritizr.net/reference/feature_abundances.md)
  : Feature abundances
- [`feature_names()`](https://prioritizr.net/reference/feature_names.md)
  [`problem_names(`*`<MultiConservationProblem>`*`)`](https://prioritizr.net/reference/feature_names.md)
  : Feature names
- [`multi_compile()`](https://prioritizr.net/reference/multi_compile.md)
  : Compile a multi-objective optimization problem
- [`number_of_features()`](https://prioritizr.net/reference/number_of_features.md)
  [`number_of_problems(`*`<MultiConservationProblem>`*`)`](https://prioritizr.net/reference/number_of_features.md)
  : Number of features
- [`number_of_planning_units()`](https://prioritizr.net/reference/number_of_planning_units.md)
  : Number of planning units
- [`number_of_problems()`](https://prioritizr.net/reference/number_of_problems.md)
  : Number of problems
- [`number_of_total_units()`](https://prioritizr.net/reference/number_of_total_units.md)
  : Number of total units
- [`number_of_zones()`](https://prioritizr.net/reference/number_of_zones.md)
  : Number of zones
- [`presolve_check()`](https://prioritizr.net/reference/presolve_check.md)
  : Presolve check
- [`problem_names()`](https://prioritizr.net/reference/problem_names.md)
  : Problem names
- [`run_calculations()`](https://prioritizr.net/reference/run_calculations.md)
  : Run calculations
- [`write_problem()`](https://prioritizr.net/reference/write_problem.md)
  : Write problem
- [`zone_names()`](https://prioritizr.net/reference/zone_names.md) :
  Zone names

## Class definitions and methods

Documentation for internal classes and associated functions.

- [`new_waiver()`](https://prioritizr.net/reference/new_waiver.md) :
  Waiver
- [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
  : Optimization problem
- [`ConservationModifier-class`](https://prioritizr.net/reference/ConservationModifier-class.md)
  [`ConservationModifier`](https://prioritizr.net/reference/ConservationModifier-class.md)
  : Conservation problem modifier class
- [`ConservationProblem-class`](https://prioritizr.net/reference/ConservationProblem-class.md)
  [`ConservationProblem`](https://prioritizr.net/reference/ConservationProblem-class.md)
  : Conservation problem class
- [`Constraint-class`](https://prioritizr.net/reference/Constraint-class.md)
  [`Constraint`](https://prioritizr.net/reference/Constraint-class.md) :
  Constraint class
- [`Decision-class`](https://prioritizr.net/reference/Decision-class.md)
  [`Decision`](https://prioritizr.net/reference/Decision-class.md) :
  Decision class
- [`MultiConservationProblem-class`](https://prioritizr.net/reference/MultiConservationProblem-class.md)
  [`MultiConservationProblem`](https://prioritizr.net/reference/MultiConservationProblem-class.md)
  : Multi-objective conservation problem class
- [`MultiObjApproach-class`](https://prioritizr.net/reference/MultiObjApproach-class.md)
  [`MultiObjApproach`](https://prioritizr.net/reference/MultiObjApproach-class.md)
  : Multi-objective approach class
- [`Objective-class`](https://prioritizr.net/reference/Objective-class.md)
  [`Objective`](https://prioritizr.net/reference/Objective-class.md) :
  Objective class
- [`OptimizationProblem-class`](https://prioritizr.net/reference/OptimizationProblem-class.md)
  [`OptimizationProblem`](https://prioritizr.net/reference/OptimizationProblem-class.md)
  : Optimization problem class
- [`Penalty-class`](https://prioritizr.net/reference/Penalty-class.md)
  [`Penalty`](https://prioritizr.net/reference/Penalty-class.md) :
  Penalty class
- [`Portfolio-class`](https://prioritizr.net/reference/Portfolio-class.md)
  [`Portfolio`](https://prioritizr.net/reference/Portfolio-class.md) :
  Portfolio class
- [`Solver-class`](https://prioritizr.net/reference/Solver-class.md)
  [`Solver`](https://prioritizr.net/reference/Solver-class.md) : Solver
  class
- [`Target-class`](https://prioritizr.net/reference/Target-class.md)
  [`Target`](https://prioritizr.net/reference/Target-class.md) : Target
  class
- [`TargetMethod-class`](https://prioritizr.net/reference/TargetMethod-class.md)
  [`TargetMethod`](https://prioritizr.net/reference/TargetMethod-class.md)
  : Target setting method class
- [`Weight-class`](https://prioritizr.net/reference/Weight-class.md)
  [`Weight`](https://prioritizr.net/reference/Weight-class.md) : Weight
  class
- [`nrow(`*`<tbl_df>`*`)`](https://prioritizr.net/reference/tibble-methods.md)
  [`ncol(`*`<tbl_df>`*`)`](https://prioritizr.net/reference/tibble-methods.md)
  [`as.list(`*`<tbl_df>`*`)`](https://prioritizr.net/reference/tibble-methods.md)
  : Manipulate tibbles

## Miscellaneous functions

Assorted functions distributed with the package.

- [`show(`*`<ConservationModifier>`*`)`](https://prioritizr.net/reference/show.md)
  [`show(`*`<ConservationProblem>`*`)`](https://prioritizr.net/reference/show.md)
  [`show(`*`<OptimizationProblem>`*`)`](https://prioritizr.net/reference/show.md)
  [`show(`*`<Solver>`*`)`](https://prioritizr.net/reference/show.md) :
  Show

- [`linear_interpolation()`](https://prioritizr.net/reference/linear_interpolation.md)
  : Linear interpolation

- [`loglinear_interpolation()`](https://prioritizr.net/reference/loglinear_interpolation.md)
  : Log-linear interpolation

- [`knit_print.ConservationProblem()`](https://prioritizr.net/reference/knit_print.md)
  [`knit_print.MultiConservationProblem()`](https://prioritizr.net/reference/knit_print.md)
  [`knit_print.OptimizationProblem()`](https://prioritizr.net/reference/knit_print.md)
  : Print an object for knitr package.

- [`as_km2()`](https://prioritizr.net/reference/as_km2.md) :

  Standardize unit to km²

- [`as_per_km2()`](https://prioritizr.net/reference/as_per_km2.md) :

  Standardize unit to density per km²

- [`do_run_example()`](https://prioritizr.net/reference/do_run_example.md)
  : Do run example?

## Deprecated functions

Documentation for functions that are no longer available.

- [`add_connected_constraints()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  [`add_corridor_constraints()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  [`set_number_of_threads()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  [`get_number_of_threads()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  [`is.parallel()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  [`add_pool_portfolio()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  [`connected_matrix()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  [`feature_representation()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  [`replacement_cost()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  [`rarity_weighted_richness()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  [`ferrier_score()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  [`distribute_load()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  [`new_optimization_problem()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  [`predefined_optimization_problem()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  [`add_loglinear_targets()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  [`add_max_phylo_objective()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  [`add_max_utility_objective()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  : Deprecation notice
