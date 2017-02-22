context('branch matrix') 

test_that('phylo input', {
  # create data
  phy <- structure(list(edge = structure(c(6L, 7L, 8L, 8L, 7L, 6L, 9L, 
          9L, 7L, 8L, 1L, 2L, 3L, 9L, 4L, 5L), .Dim = c(8L, 2L)), 
          tip.label = c('t1',  't5', 't4', 't2', 't3'), 
          edge.length = c(0.357997308950871, 0.279826242243871, 
          0.22146653640084, 0.357243377715349, 0.00875525642186403, 
          0.967695261584595, 
          0.723838896490633, 0.0858941006008536), Nnode = 4L, .Names = c('edge', 
          'tip.label', 'edge.length', 'Nnode', 'tip.labels')),
          class = 'phylo', order = 'cladewise')
  # make branch matrix
  m <- branch_matrix(phy)
  # create correct branch matrix
  s <- matrix(0, ncol=nrow(phy$edge), nrow=length(phy$tip.label))
  s[1,c(3,2,1)] <- 1
  s[2,c(4,2,1)] <- 1
  s[3,c(5,1)] <- 1
  s[4,c(7,6)] <- 1
  s[5,c(8,6)] <- 1
  s <- as(s, 'dgCMatrix')
  # tests
  expect_is(m, 'dgCMatrix')
  expect_true(all(m==s))
})

test_that('invalid input', {
  tr <- ape::rtree(3)
  tr$edge[1] <- 0
  expect_error(branch_matrix(tr))
  expect_error(branch_matrix('a'))
})



