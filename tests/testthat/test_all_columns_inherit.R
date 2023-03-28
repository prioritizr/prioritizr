test_that("x = default", {
  expect_error(all_columns_inherit(new_waiver()), "recognized")
})

test_that("x = data.frame", {
  expect_true(all_columns_inherit(data.frame(x = 1, y = 2), "numeric"))
  expect_false(all_columns_inherit(data.frame(x = 1, y = "a"), "numeric"))
  expect_error(
    assert(
      all_columns_inherit(data.frame(x = 1, y = "a"), "numeric")
    ),
    "columns"
  )
})

test_that("x = sf", {
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1))))
  expect_true(
    all_columns_inherit(
      sf::st_sf(data.frame(x = 1, y = 2), geom = g),
      "numeric"
    )
  )
  expect_false(
    all_columns_inherit(
      sf::st_sf(data.frame(x = 1, y = "a"), geom = g),
      "numeric"
    )
  )
  expect_error(
    assert(
      all_columns_inherit(
        sf::st_sf(data.frame(x = 1, y = "a"), geom = g),
        "numeric"
      )
    ),
    "columns"
  )
})

test_that("x = Spatial", {
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1))))
  expect_true(
    all_columns_inherit(
      sf::as_Spatial(sf::st_sf(data.frame(x = 1, y = 2), geom = g)),
      "numeric"
    )
  )
  expect_false(
    all_columns_inherit(
      sf::as_Spatial(sf::st_sf(data.frame(x = 1, y = "a"), geom = g)),
      "numeric"
    )
  )
  expect_error(
    assert(
      all_columns_inherit(
        sf::as_Spatial(sf::st_sf(data.frame(x = 1, y = "a"), geom = g)),
        "numeric"
      )
    ),
    "columns"
  )
})
