context("parameters")

test_that("proportion_parameter", {
  x <- proportion_parameter("test", 0.1)
  # methods
  x$show()
  x$print()
  expect_is(x$repr(), "character")
  expect_equal(x$name, "test")
  expect_true(inherits(x$id, "Id"))
  expect_equal(x$lower_limit, 0)
  expect_equal(x$upper_limit, 1)
  expect_equal(x$class, "numeric")
  expect_is(x$id, "Id")
  expect_equal(x$get(), 0.1)
  expect_false(x$validate(NA_real_))
  expect_false(x$validate(Inf))
  expect_false(x$validate(-5))
  expect_false(x$validate(5))
  expect_true(x$validate(0.5))
  x$set(0.6)
  expect_equal(x$get(), 0.6)
  x$reset()
  expect_equal(x$get(), 0.1)
  expect_true(inherits(x$render(), "shiny.tag"))
  # errors
  expect_error(proportion_parameter("test", NA_real_))
  expect_error(proportion_parameter("test", Inf))
  expect_error(proportion_parameter("test", -5))
  expect_error(proportion_parameter("test", 5))
  expect_error(x$set(NA_real_))
  expect_error(x$set(Inf))
  expect_error(x$set(-5))
  expect_error(x$set(5))
  expect_error(x$set("a"))
})

test_that("integer_parameter", {
  x <- integer_parameter("test", 1L)
  # methods
  x$show()
  x$print()
  expect_is(x$repr(), "character")
  expect_equal(x$name, "test")
  expect_true(inherits(x$id, "Id"))
  expect_equal(x$lower_limit, as.integer(-.Machine$integer.max))
  expect_equal(x$upper_limit, as.integer(.Machine$integer.max))
  expect_equal(x$class, "integer")
  expect_is(x$id, "Id")
  expect_equal(x$get(), 1L)
  expect_false(x$validate(NA_real_))
  expect_false(x$validate(Inf))
  expect_false(x$validate(-5))
  expect_false(x$validate(5))
  expect_true(x$validate(3L))
  x$set(4L)
  expect_equal(x$get(), 4L)
  x$reset()
  expect_equal(x$get(), 1L)
  expect_true(inherits(x$render(), "shiny.tag"))
  # errors
  expect_error(integer_parameter("test", NA_real_))
  expect_error(integer_parameter("test", Inf))
  expect_error(integer_parameter("test", -5.5))
  expect_error(integer_parameter("test", 5.5))
  expect_error(x$set(NA_real_))
  expect_error(x$set(Inf))
  expect_error(x$set(-5.5))
  expect_error(x$set(5.5))
  expect_error(x$set("a"))
})

test_that("numeric_parameter", {
  x <- numeric_parameter("test", 1)
  # methods
  x$show()
  x$print()
  expect_is(x$repr(), "character")
  expect_equal(x$name, "test")
  expect_true(inherits(x$id, "Id"))
  expect_equal(x$lower_limit, .Machine$double.xmin)
  expect_equal(x$upper_limit, .Machine$double.xmax)
  expect_equal(x$class, "numeric")
  expect_is(x$id, "Id")
  expect_equal(x$get(), 1)
  expect_false(x$validate(NA_real_))
  expect_false(x$validate(Inf))
  expect_false(x$validate(3L))
  expect_true(x$validate(3))
  x$set(4)
  expect_equal(x$get(), 4)
  x$reset()
  expect_equal(x$get(), 1)
  expect_true(inherits(x$render(), "shiny.tag"))
  # errors
  expect_error(numeric_parameter("test", NA_real_))
  expect_error(numeric_parameter("test", Inf))
  expect_error(x$set(NA_real_))
  expect_error(x$set(Inf))
  expect_error(x$set("a"))
})

test_that("binary_parameter", {
  x <- binary_parameter("test", 1L)
  # methods
  x$show()
  x$print()
  expect_is(x$repr(), "character")
  expect_equal(x$name, "test")
  expect_true(inherits(x$id, "Id"))
  expect_equal(x$lower_limit, 0L)
  expect_equal(x$upper_limit, 1L)
  expect_equal(x$class, "integer")
  expect_is(x$id, "Id")
  expect_equal(x$get(), 1L)
  expect_false(x$validate(NA_real_))
  expect_false(x$validate(Inf))
  expect_false(x$validate(-1L))
  expect_false(x$validate(0))
  expect_true(x$validate(0L))
  x$set(0L)
  expect_equal(x$get(), 0L)
  x$reset()
  expect_equal(x$get(), 1L)
  expect_true(inherits(x$render(), "shiny.tag"))
  # errors
  expect_error(binary_parameter("test", NA_real_))
  expect_error(binary_parameter("test", Inf))
  expect_error(binary_parameter("test", 5L))
  expect_error(binary_parameter("test", -1L))
  expect_error(x$set(NA_real_))
  expect_error(x$set(Inf))
  expect_error(x$set(0))
  expect_error(x$set(-1L))
  expect_error(x$set("a"))
})

test_that("proportion_parameter_array", {
  x <- proportion_parameter_array("test", c(0.3, 0.1, 0.4), letters[1:3])
  # methods
  x$show()
  x$print()
  expect_is(x$repr(), "character")
  expect_equal(x$name, "test")
  expect_true(inherits(x$id, "Id"))
  expect_true(all(x$lower_limit == rep(0, x$length)))
  expect_true(all(x$upper_limit == rep(1, x$length)))
  expect_equal(x$class, "numeric")
  expect_equal(rownames(x$get()), letters[1:3])
  expect_equal(x$get()[[1]], c(0.3, 0.1, 0.4))
  expect_is(x$render(), "shiny.tag.list")
  expect_false(x$validate(data.frame(value = c(Inf, 0.1, 0.2),
                          row.names = letters[1:3])))
  expect_false(x$validate(data.frame(value = c(NA_real_, 0.1, 0.2),
                          row.names = letters[1:3])))
  expect_false(x$validate(data.frame(value = c(0L, 0L, 0L),
                          row.names = letters[1:3])))
  expect_false(x$validate(data.frame(value = c(5, 0.1, 0.2),
                          row.names = letters[1:3])))
  expect_false(x$validate(data.frame(value = c(0.1, 0.1, 0.2),
                          row.names = c("a", "b", "d"))))
  expect_true(x$validate(data.frame(value = c(0.1, 0.1, 0.2),
                         row.names = letters[1:3])))
  x$set(data.frame(value = c(0.9, 0.8, 0.7), row.names = letters[1:3]))
  expect_equal(rownames(x$get()), letters[1:3])
  expect_equal(x$get()[[1]], c(0.9, 0.8, 0.7))
  x$reset()
  expect_equal(rownames(x$get()), letters[1:3])
  expect_equal(x$get()[[1]], c(0.3, 0.1, 0.4))
  # errors
  expect_error(proportion_parameter_array("test", c(Inf, 0.1, 0.2),
                                          letters[1:3]))
  expect_error(proportion_parameter_array("test",
    data.frame(value = c(NA_real_, 0.1, 0.2), row.names = letters[1:3])))
  expect_error(proportion_parameter_array("test",
    data.frame(value = c(0L, 0L, 0L), row.names = letters[1:3])))
  expect_error(proportion_parameter_array("test",
    data.frame(value = c(5, 0.1, 0.2), row.names = letters[1:3])))
  expect_error(x$set(data.frame(value = c(Inf, 0.1, 0.2),
                                row.names = letters[1:3])))
  expect_error(x$set(data.frame(value = c(NA_real_, 0.1, 0.2),
                                row.names = letters[1:3])))
  expect_error(x$set(data.frame(value = c(0L, 0L, 0L),
                                row.names = letters[1:3])))
  expect_error(x$set(data.frame(value = c(5, 0.1, 0.2),
                                row.names = letters[1:3])))
  expect_error(x$set(data.frame(value = c(0.1, 0.1, 0.2),
                                row.names = c("a", "b", "d"))))
})

test_that("binary_parameter_array", {
  x <- binary_parameter_array("test", c(0, 1, 1), letters[1:3])
  # methods
  x$show()
  x$print()
  expect_is(x$repr(), "character")
  expect_equal(x$name, "test")
  expect_true(inherits(x$id, "Id"))
  expect_true(all(x$lower_limit == rep(0L, x$length)))
  expect_true(all(x$upper_limit == rep(1L, x$length)))
  expect_equal(x$class, "integer")
  expect_equal(rownames(x$get()), letters[1:3])
  expect_equal(x$get()[[1]], c(0L, 1L, 1L))
  expect_is(x$render(), "shiny.tag.list")
  expect_false(x$validate(data.frame(value = c(Inf, 1L, 1L),
    row.names = letters[1:3])))
  expect_false(x$validate(data.frame(value = c(NA_real_, 1L, 1L),
    row.names = letters[1:3])))
  expect_false(x$validate(data.frame(value = c(5, 0L, 1L),
    row.names = letters[1:3])))
  expect_false(x$validate(data.frame(value = c(-5, 1, 0),
    row.names = c("a", "b", "d"))))
  expect_false(x$validate(data.frame(value = c(0.1, 1L, 2L),
    row.names = letters[1:3])))
  x$set(data.frame(value = c(0L, 1L, 0L), row.names = letters[1:3]))
  expect_equal(rownames(x$get()), letters[1:3])
  expect_equal(x$get()[[1]], c(0L, 1L, 0L))
  x$reset()
  expect_equal(rownames(x$get()), letters[1:3])
  expect_equal(x$get()[[1]], c(0L, 1L, 1L))
  # errors
  expect_error(binary_parameter_array("test", c(Inf, 1L, 0L),
    letters[1:3]))
  expect_error(binary_parameter_array("test", value = c(NA_real_, 1L, 0L),
    row.names = letters[1:3]))
  expect_error(binary_parameter_array("test", value = c(0L, 1L, 5L),
    row.names = letters[1:3]))
  expect_error(binary_parameter_array("test", value = c(-4L, 0L, 1L),
    row.names = letters[1:3]))
  expect_error(x$set(data.frame(value = c(Inf, 1L, 0L),
    row.names = letters[1:3])))
  expect_error(x$set(data.frame(value = c(NA_real_, 1L, 0L),
    row.names = letters[1:3])))
  expect_error(x$set(data.frame(value = c(0L, 1L, 5L),
    row.names = letters[1:3])))
  expect_error(x$set(data.frame(value = c(0L, 1L, 5L),
    row.names = letters[1:3])))
  expect_error(x$set(data.frame(value = c(-4L, 0L, 1L),
    row.names = c("a", "b", "d"))))
})

test_that("numeric_parameter_array", {
  x <- numeric_parameter_array("test", c(-8, 0.1, 5), letters[1:3])
  # methods
  x$show()
  x$print()
  expect_is(x$repr(), "character")
  expect_equal(x$name, "test")
  expect_true(inherits(x$id, "Id"))
  expect_true(all(x$lower_limit == rep(.Machine$double.xmin, x$length)))
  expect_true(all(x$upper_limit == rep(.Machine$double.xmax, x$length)))
  expect_equal(x$class, "numeric")
  expect_equal(rownames(x$get()), letters[1:3])
  expect_equal(x$get()[[1]], c(-8, 0.1, 5))
  expect_is(x$render(), "shiny.tag.list")
  expect_false(x$validate(data.frame(
                            value = c(Inf, 0.1, 0.2),
                            row.names = letters[1:3])))
  expect_false(x$validate(data.frame(
                            value = c(NA_real_, 0.1, 0.2),
                            row.names = letters[1:3])))
  expect_false(x$validate(data.frame(
                            value = c(0L, 0L, 0L),
                            row.names = letters[1:3])))
  expect_false(x$validate(data.frame(
                            value = c(-5, 0.1, 0.2),
                            row.names = letters[1:3])))
  expect_false(x$validate(data.frame(
                            value = c(0.1, 5, 0.2),
                            row.names = c("a", "b", "d"))))
  expect_true(x$validate(data.frame(
                            value = c(0.1, 5, 0.2),
                            row.names = letters[1:3])))
  x$set(data.frame(value = c(0.9, 0.8, 10),
                   row.names = letters[1:3]))
  expect_equal(rownames(x$get()), letters[1:3])
  expect_equal(x$get()[[1]], c(0.9, 0.8, 10))
  x$reset()
  expect_equal(rownames(x$get()), letters[1:3])
  expect_equal(x$get()[[1]], c(-8, 0.1, 5))
  # errors
  expect_error(numeric_parameter_array("test",
                            value = c(Inf, 0.1, 0.2),
                            row.names = letters[1:3]))
  expect_error(numeric_parameter_array("test",
                            value = c(NA_real_, 0.1, 0.2),
                            row.names = letters[1:3]))
  expect_error(numeric_parameter_array("test",
                            value = c(0L, 0L, 0L),
                            row.names = letters[1:3]))
  expect_error(numeric_parameter_array("test",
                            value = c(-5, 0.1, 0.2),
                            row.names = letters[1:3]))
  expect_error(x$set(data.frame(value = c(Inf, 0.1, 0.2),
                                row.names = letters[1:3])))
  expect_error(x$set(data.frame(value = c(NA_real_, 0.1, 0.2),
                                row.names = letters[1:3])))
  expect_error(x$set(data.frame(value = c(0L, 0L, 0L),
                                row.names = letters[1:3])))
  expect_error(x$set(data.frame(value = c(-5, 0.1, 0.2),
                                row.names = letters[1:3])))
  expect_error(x$set(data.frame(value = c(0.1, 5, 0.2),
                                row.names = c("a", "b", "d"))))
})

test_that("misc_parameter", {
  # load data
  data(mtcars, iris)
  # create parameter
  x <- misc_parameter("tbl", iris,
                      function(x) all(names(x) %in% names(iris)) &&
                                  all(x[[1]] < 200),
                      function(id, x) structure(id, class = "shiny.tag"))
  # run tests
  x$show()
  x$print()
  expect_is(x$id, "Id")
  expect_equal(x$get(), iris)
  expect_false(x$validate(mtcars))
  # create updated iris data set
  iris2 <- iris
  iris2[1, 1] <- 300
  expect_false(x$validate(iris2))
  iris2[1, 1] <- 50
  x$set(iris2)
  expect_equal(x$get(), iris2)
  expect_is(x$render(), "shiny.tag")
  x$reset()
  expect_equal(x$get(), iris)
  # errors
  iris2[1, 1] <- 300
  expect_error(x$set(iris2))
  expect_error(x$set(mtcars))
})

test_that("numeric_matrix_parameter", {
  # load data
  m <- matrix(runif(9), ncol = 3)
  colnames(m) <- letters[1:3]
  rownames(m) <- letters[1:3]
  x <- numeric_matrix_parameter("m", m)
  # methods
  x$show()
  x$print()
  expect_is(x$id, "Id")
  expect_equal(x$get(), m)
  expect_false(x$validate(m[, -1]))
  expect_false(x$validate(m[-1, ]))
  m2 <- m
  m2[] <- NA
  expect_false(x$validate(m2))
  x$set(m + 1)
  expect_equal(x$get(), m + 1)
  expect_is(x$render(), "shiny.tag.list")
  # errors
  expect_error(x$set(as.data.frame(m)))
  expect_error(x$set(m2))
})

test_that("binary_matrix_parameter", {
  # load data
  m <- matrix(round(runif(9)), ncol = 3)
  colnames(m) <- letters[1:3]
  rownames(m) <- letters[1:3]
  x <- binary_matrix_parameter("m", m)
  # methods
  x$show()
  x$print()
  expect_is(x$id, "Id")
  expect_equal(x$get(), m)
  expect_false(x$validate(m[, -1]))
  expect_false(x$validate(m[-1, ]))
  m2 <- m
  m2[] <- NA
  expect_false(x$validate(m2))
  x$set(abs(m - 1))
  expect_equal(x$get(), abs(m +-1))
  expect_is(x$render(), "shiny.tag.list")
  # errors
  expect_error(x$set(as.data.frame(m)))
  expect_error(x$set(m2))
})

test_that("parameters", {
  p1 <- proportion_parameter("p1", 0.5)
  p2 <- numeric_parameter_array("p2", c(0, 10, 6), letters[1:3])
  x <- parameters(p1, p2)
  # methods
  x$show()
  x$print()
  expect_equal(x$repr(), "[p1 (0.5), p2 (min: 0, max: 10)]")
  expect_equal(unname(sort(x$names())), c(p1$name, p2$name))
  expect_equal(x$get(p1$id), p1$get())
  expect_equal(x$get(p1$name), p1$get())
  expect_equal(x$render(p1$id), p1$render())
  expect_equal(x$render(p1$name), p1$render())
  expect_is(x$render_all(), "shiny.tag")
  x$set(p1$id, 0.9)
  x$set(p2$name, data.frame(value = c(0.1, 5, 0.2),
                            row.names = c("a", "b", "c")))
  expect_equal(x$get(p1$name), 0.9)
  expect_equal(x$get(p1$id), 0.9)
  expect_equal(x$get(p2$id)[[1]], c(0.1, 5, 0.2))
  expect_equal(rownames(x$get(p2$name)), letters[1:3])
  x$reset(p1$id)
  expect_equal(x$get(p1$id), 0.5)
  expect_equal(x$get(p2$id)[[1]], c(0.1, 5, 0.2))
  expect_equal(rownames(x$get(p2$name)), letters[1:3])
  x$set(p1$id, 0.8)
  x$reset(p1$name)
  expect_equal(x$get(p1$id), 0.5)
  x$reset_all()
  expect_equal(x$get(p1$id), 0.5)
  expect_equal(x$get(p2$id)[[1]], c(0, 10, 6))
  expect_equal(rownames(x$get(p2$name)), letters[1:3])
  # errors
  expect_error(parameters(1, p1))
  expect_error(x$set(data.frame(value = c(0.1, 5, 0.2),
                                row.names = c("a", "b", "d"))))
})
