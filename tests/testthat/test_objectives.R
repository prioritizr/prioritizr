context('objectives') 

test_that('minimum_set_objective', {
  x <- minimum_set_objective()
  print(x)
  x
})

test_that('maximum_coverage_objective', {
  x <- maximum_coverage_objective(budget=5)
  print(x)
  x
})

test_that('phylogenetic_coverage_objective', {
  data(sim_phylogeny)
  x <- phylogenetic_coverage_objective(budget=5, tree=sim_phylogeny)
  print(x)
  x
})
