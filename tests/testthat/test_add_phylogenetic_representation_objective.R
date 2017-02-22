context('add_phylogenetic_representation_objective') 

test_that('compile', {
  # generate optimization problem
  data(sim_pu_raster, sim_features, sim_phylogeny)
  b <- floor(raster::cellStats(sim_pu_raster, 'sum')) * 0.25
  targ <- unname(floor(raster::cellStats(sim_features, 'sum') * 0.25))
  p <- problem(sim_pu_raster, sim_features) %>%
    add_phylogenetic_representation_objective(budget=b, tree=sim_phylogeny) %>%
    add_absolute_targets(targ) %>%
    add_binary_decision()
  o <- compile(p)
  # check that objective has been correctly applied
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_features <- raster::nlayers(sim_features)
  n_branches <- nrow(sim_phylogeny$edge)
  bm <- branch_matrix(sim_phylogeny)
  scaled_costs <- p$planning_unit_costs()
  scaled_costs <- scaled_costs * (1e-10/min(scaled_costs))
  expect_equal(o$modelsense(), 'max')
  expect_equal(o$obj(), c(scaled_costs, rep(1e-10, n_features),
                          sim_phylogeny$edge.length))
  expect_equal(o$sense(), c(rep('>=', n_features), '<=', rep('>=', n_branches)))
  expect_equal(o$rhs(), c(rep(0, n_features), b, rep(0, n_branches)))
  expect_equal(o$col_ids(), c(rep('pu', n_pu), rep('spp_met', n_features),
               rep('branch_met', n_branches)))               
  expect_equal(o$row_ids(), c(rep('spp_target', n_features), 'budget',
               rep('branch_target', n_branches)))
  expect_true(all(o$A()[seq_len(n_features),seq_len(n_pu)]==p$data$rij_matrix))
  expect_equal(o$A()[n_features+1,], c(p$planning_unit_costs(), 
                                     rep(0, n_features), 
                                     rep(0, n_branches)))
  expect_true(all(o$A()[seq_len(n_features),n_pu+seq_len(n_features)] ==
    Matrix::sparseMatrix(i=seq_len(n_features),j=seq_len(n_features),
      x=(-1*targ),giveCsparse=FALSE)))
  expect_true(all(
    o$A()[n_features+1+seq_len(n_branches),n_pu+seq_len(n_features)] ==
    t(bm)
  ))
  expect_true(all({
  a <- o$A()
  a[n_features+1+seq_len(n_branches),n_pu+n_features+seq_len(n_branches)] == 
    Matrix::sparseMatrix(i=seq_len(n_branches),j=seq_len(n_branches),
                         x=rep(-1, n_branches))
  }))
  expect_true(all(o$lb() == 0))
  expect_true(all(o$ub() == 1))      
})

test_that('solution', {
  skip_on_cran()
  # create data
  budget <- 4.23
  cost <- raster::raster(matrix(c(1,2,NA,4), ncol=2))
  locked_in <- 2
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2,1,1,0), ncol=2)),
                            raster::raster(matrix(c(10,10,10,10), ncol=2)),
                            raster::raster(matrix(c(0,0,0,2), ncol=2)))
  tr <- list(tip.label=c('layer.1', 'layer.2', 'layer.3'),
             edge.length=c(1,1,1,100),
             Nnode=2L,
             edge=matrix(c(
               4L,  1L,
               4L,  5L,
               5L,  2L,
               5L,  3L
             ), byrow=TRUE, ncol=2))
  class(tr) <- 'phylo'
  attr(tr, 'order') <- 'cladewise'
  # create problem
  p <- problem(cost, features) %>%
          add_phylogenetic_representation_objective(budget=budget, tree=tr) %>%
          add_absolute_targets(c(2, 12, 2)) %>%
          add_locked_in_constraint(locked_in) %>%
          add_locked_out_constraint(locked_out)
  # solve problem
  s <- solve(p)
  # test for correct solution
  expect_equal(raster::values(s), c(0,NA,0,1))
})

test_that('invalid inputs', {
  # check that invalid arguments result in errors
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
      add_maximum_representation_objective(budget=-5, sim_phylogeny)
  })
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
      add_maximum_representation_objective(budget=0, sim_phylogeny)
  })
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
      add_maximum_representation_objective(budget=NA, sim_phylogeny)
  })
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
      add_maximum_representation_objective(budget=Inf, sim_phylogeny)
  })
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
      add_maximum_representation_objective(budget=5000, 
        ape::drop.tip(sim_phylogeny, 'layer.1'))
    })
})
