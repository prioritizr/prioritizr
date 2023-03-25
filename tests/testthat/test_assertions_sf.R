test_that("is_valid_geometries", {
  x <- sf::st_sf(geom = sf::st_sfc(list(sf::st_point(c(1, 1)))))
  y <- sf::st_sf(geom = sf::st_sfc(list(sf::st_geometrycollection())))
  expect_true(is_valid_geometries(x))
  expect_false(is_valid_geometries(y))
  expect_error(
    assert(
      sf::st_sf(geom = sf::st_sfc(list(sf::st_geometrycollection()))),
      "GEOMETRYCOLLECTION"
    )
  )
})
