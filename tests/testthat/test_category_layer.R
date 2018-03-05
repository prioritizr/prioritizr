context("category_layer")

test_that("valid inputs", {
 # create data
 x <- raster::stack(raster::raster(matrix(c(1, 0, 0, 1, NA, 0), byrow = TRUE,
                                          nrow = 3)),
                    raster::raster(matrix(c(0, 1, 0, 0, NA, 0), byrow = TRUE,
                                          nrow = 3)),
                    raster::raster(matrix(c(0, 0, 1, 0, NA, 1), byrow = TRUE,
                                          nrow = 3)))
 # convert to binary stack
 y <- category_layer(x)
 # run tests
  expect_is(y, "RasterLayer")
  expect_equal(raster::nlayers(y), 1)
  expect_true(raster::compareRaster(x, y, stopiffalse = FALSE, tolerance = 1e-5,
                                    crs = TRUE))
  expect_equal(raster::values(y), c(1, 2, 3, 1, NA, 3))
})

test_that("invalid inputs", {
  expect_error({
    category_layer(raster::stack(raster::raster(matrix(c(1, 0)))))
  })
  expect_error({
    category_layer(raster::stack(raster::raster(matrix(c(1, 0))),
                                 raster::raster(matrix(c(1, 1)))))
  })
  expect_error({
    category_layer(raster::stack(raster::raster(matrix(c(1, 0))),
                                 raster::raster(matrix(c(0, 3)))))
  })
  expect_error({
    suppressWarnings({
      category_layer(raster::stack(raster::raster(matrix(c(NA, NA))),
                                  raster::raster(matrix(c(1, 0)))))
    })
  })
})
