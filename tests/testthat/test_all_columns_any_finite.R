test_that("x = default", {
  expect_error(all_columns_any_finite(new_waiver()), "recognized")
})

test_that("x = data.frame", {
  expect_true(all_columns_any_finite(data.frame(x = c(0, 1), y = c(1, 2))))
  expect_true(all_columns_any_finite(data.frame(x = c(1, NA), y = c(NA, 2))))
  expect_false(all_columns_any_finite(data.frame(x = c(1, NA), y = c(NA, NA))))
  expect_error(
    assert(
      all_columns_any_finite(data.frame(x = c(1, NA), y = c(NA, NA)))
    ),
    "columns"
  )
})

test_that("x = sf", {
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)))[rep(1, 3)])
  expect_true(
    all_columns_any_finite(
      sf::st_sf(data.frame(x = c(0, 1, 2), y = c(2, 1, 1), geom = g))
    )
  )
  expect_true(
    all_columns_any_finite(
      sf::st_sf(data.frame(x = c(0, NA, 2), y = c(NA, 1, 1), geom = g))
    )
  )
  expect_false(
    all_columns_any_finite(
      sf::st_sf(data.frame(x = c(0, 1, 2), y = c(NA, NA, NA), geom = g))
    )
  )
  expect_error(
    assert(
      all_columns_any_finite(
        sf::st_sf(data.frame(x = c(0, 1, 2), y = c(NA, NA, NA), geom = g))
      )
    ),
    "columns"
  )
})

test_that("x = Spatial", {
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)))[rep(1, 3)])
  expect_true(
    all_columns_any_finite(
      sf::as_Spatial(
        sf::st_sf(data.frame(x = c(0, 1, 2), y = c(2, 1, 1), geom = g))
      )
    )
  )
  expect_true(
    all_columns_any_finite(
      sf::as_Spatial(
        sf::st_sf(data.frame(x = c(0, NA, 2), y = c(NA, 1, 1), geom = g))
      )
    )
  )
  expect_false(
    all_columns_any_finite(
      sf::as_Spatial(
        sf::st_sf(data.frame(x = c(0, 1, 2), y = c(NA, NA, NA), geom = g))
      )
    )
  )
  expect_error(
    assert(
      all_columns_any_finite(
        sf::as_Spatial(
          sf::st_sf(data.frame(x = c(0, 1, 2), y = c(NA, NA, NA), geom = g))
        )
      )
    ),
    "columns"
  )
})
