context("pproto")

test_that("check that proto objects behave as expected", {
  # initialize
  proto <- proto::proto
  p <- proto(a = 5)
  p2 <- p$proto(b = 5)
  # check that fields are correct
  expect_equal(p$ls(), "a")
  expect_equal(p$a, 5)
  expect_equal(p2$ls(), "b")
  expect_equal(p2$b, 5)
  # update value
  p$a <- 10
  # check that a in both objects have become 10
  expect_equal(p$a, 10)
  expect_equal(p2$a, 10)
})

test_that("create new pproto", {
  p <- pproto(a = 5)
  expect_equal(p$ls(), "a")
  expect_equal(p$a, 5)
})

test_that("create inherited proto with s3 fields", {
  # initialize
  p <- pproto(a = 5)
  p2 <- pproto(NULL, p, b = 5)
  # check that fields are correct
  expect_equal(p$ls(), "a")
  expect_equal(p$a, 5)
  expect_equal(p2$ls(), c("a", "b"))
  expect_equal(p2$a, 5)
  expect_equal(p2$b, 5)
  # update parent proto
  p$a <- 7
  # check that fields are correct
  expect_equal(p$ls(), "a")
  expect_equal(p$a, 7)
  expect_equal(p2$ls(), c("a", "b"))
  expect_equal(p2$a, 5)
  expect_equal(p2$b, 5)
})


test_that("create inherited proto with proto fields", {
  # initialize
  p <- pproto("p", a = 1)
  p2 <- pproto("p2", p, b = 2, c = p)
  p3 <- pproto("p3", p, d = 3, f = p2)
  # check that fields are correct
  expect_equal(class(p), c("p", "pproto", "proto", "environment"))
  expect_equal(p$ls(), "a")
  expect_equal(p$a, 1)

  expect_equal(class(p2), c("p2", "p", "pproto", "proto", "environment"))
  expect_equal(p2$ls(), c("a", "b", "c"))
  expect_equal(p2$a, 1)
  expect_equal(p2$b, 2)
  expect_equal(class(p2$c), c("p", "pproto", "proto", "environment"))
  expect_equal(p2$c$a, 1)

  expect_equal(class(p3), c("p3", "p", "pproto", "proto", "environment"))
  expect_equal(p3$ls(), c("a", "d", "f"))
  expect_equal(p3$a, 1)
  expect_equal(p3$d, 3)
  expect_equal(class(p3$f), c("p2", "p", "pproto", "proto", "environment"))
  expect_equal(p3$f$a, 1)
  expect_equal(p3$f$b, 2)
  expect_equal(class(p3$f$c), c("p", "pproto", "proto", "environment"))
  expect_equal(p3$f$c$a, 1)

  # change p and check that values are correct
  p$a <- 4
  p2$b <- 5

  # check that fields are correct
  expect_equal(class(p), c("p", "pproto", "proto", "environment"))
  expect_equal(p$ls(), "a")
  expect_equal(p$a, 4)

  expect_equal(class(p2), c("p2", "p", "pproto", "proto", "environment"))
  expect_equal(p2$ls(), c("a", "b", "c"))
  expect_equal(p2$a, 1)
  expect_equal(p2$b, 5)
  expect_equal(class(p2$c), c("p", "pproto", "proto", "environment"))
  expect_equal(p2$c$a, 1)

  expect_equal(class(p3), c("p3", "p", "pproto", "proto", "environment"))
  expect_equal(p3$ls(), c("a", "d", "f"))
  expect_equal(p3$a, 1)
  expect_equal(p3$d, 3)
  expect_equal(class(p3$f), c("p2", "p", "pproto", "proto", "environment"))
  expect_equal(p3$f$a, 1)
  expect_equal(p3$f$b, 2)
  expect_equal(class(p3$f$c), c("p", "pproto", "proto", "environment"))
  expect_equal(p3$f$c$a, 1)
})
