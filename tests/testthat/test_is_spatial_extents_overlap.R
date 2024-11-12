test_that("overlapping objects", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_pu_raster <- get_sim_pu_raster()
  # tests
  expect_true(
    is_spatial_extents_overlap(sim_pu_polygons[1, ], sim_pu_polygons[1:10, ])
  )
  expect_true(
    is_spatial_extents_overlap(sim_pu_polygons[1, ], sim_pu_polygons[1:10, ])
  )
  expect_true(
    is_spatial_extents_overlap(sim_pu_polygons[1, ], sim_pu_polygons[1:10, ])
  )
  expect_true(
    is_spatial_extents_overlap(sim_pu_polygons[1, ], sim_pu_raster)
  )
  expect_true(
    is_spatial_extents_overlap(sim_pu_polygons[1, ], sim_pu_raster)
  )
  expect_true(
    is_spatial_extents_overlap(
      terra::crop(sim_pu_raster, terra::ext(sf::st_bbox(sim_pu_polygons[1, ]))),
      sim_pu_raster
    )
  )
})

test_that("non-overlapping objects", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_pu_raster <- get_sim_pu_raster()
  # tests
  expect_tidy_error(
    assert(
      is_spatial_extents_overlap(sim_pu_polygons[1, ], sim_pu_polygons[7, ])
    ),
    "overlapping spatial extents",
    name = NULL
  )
  expect_tidy_error(
    assert(
      is_spatial_extents_overlap(sim_pu_polygons[1, ], sim_pu_polygons[7, ])
    ),
    "overlapping spatial extents",
    name = NULL
  )
  expect_tidy_error(
    assert(
      is_spatial_extents_overlap(sim_pu_polygons[1, ], sim_pu_polygons[7, ])
    ),
    "overlapping spatial extents",
    name = NULL
  )
  expect_tidy_error(
    assert(
      is_spatial_extents_overlap(
        sim_pu_polygons[1, ],
        terra::crop(
          sim_pu_raster,
          terra::ext(sf::st_bbox(sim_pu_polygons[7, ]))
        )
      )
    ),
    "overlapping spatial extents",
    name = NULL
  )
  expect_tidy_error(
    assert(
      is_spatial_extents_overlap(
        sim_pu_polygons[1, ],
        terra::crop(
          sim_pu_raster,
          terra::ext(sf::st_bbox(sim_pu_polygons[7, ]))
        )
      )
    ),
    "overlapping spatial extents",
    name = NULL
  )
  expect_tidy_error(
    assert(
      is_spatial_extents_overlap(
        terra::crop(
          sim_pu_raster,
          terra::ext(sf::st_bbox(sim_pu_polygons[1, ]))
        ),
        terra::crop(
          sim_pu_raster,
          terra::ext(sf::st_bbox(sim_pu_polygons[7, ]))
        )
      )
    ),
    "must have overlapping spatial extents",
    name = NULL
  )
})

test_that("Spatial", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  # tests
  expect_true(
    is_spatial_extents_overlap(
      sf::as_Spatial(sim_pu_polygons[1, ]),
      sf::as_Spatial(sim_pu_polygons[1:10, ])
    )
  )
  expect_tidy_error(
    assert(
      is_spatial_extents_overlap(
        sf::as_Spatial(sim_pu_polygons[1, ]),
        sf::as_Spatial(sim_pu_polygons[7, ])
      )
    ),
    "must have overlapping spatial extents",
    name = NULL
  )
})

test_that("Raster", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_pu_polygons <- get_sim_pu_polygons()
  # prepare data
  r1 <- raster::raster(
    terra::crop(
      sim_pu_raster, terra::ext(sf::st_bbox(sim_pu_polygons[1, ]))
    )
  )
  r2 <- raster::raster(sim_pu_raster)
  r3 <- raster::raster(
    terra::crop(
      sim_pu_raster,
      terra::ext(sf::st_bbox(sim_pu_polygons[7, ]))
    )
  )
  expect_warning(z1 <- zones(r1, r1), "deprecated")
  expect_warning(z2 <- zones(r2, r2), "deprecated")
  # tests
  expect_true(is_spatial_extents_overlap(r1, r2))
  expect_true(is_spatial_extents_overlap(z1, z2))
  expect_tidy_error(
    assert(is_spatial_extents_overlap(r1, r3)),
    "must have overlapping spatial extents",
    name = NULL
  )
})
