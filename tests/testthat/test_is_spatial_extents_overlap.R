context("is_spatial_extents_overlap")

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
    "overlapping extents",
    name = NULL
  )
  expect_tidy_error(
    assert(
      is_spatial_extents_overlap(sim_pu_polygons[1, ], sim_pu_polygons[7, ])
    ),
    "overlapping extents",
    name = NULL
  )
  expect_tidy_error(
    assert(
      is_spatial_extents_overlap(sim_pu_polygons[1, ], sim_pu_polygons[7, ])
    ),
    "overlapping extents",
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
    "overlapping extents",
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
    "overlapping extents",
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
    "overlapping extents",
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
    "overlapping extents",
    name = NULL
  )
})

test_that("Raster", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_pu_polygons <- get_sim_pu_polygons()
  # tests
  expect_true(
    is_spatial_extents_overlap(
      raster::raster(
        terra::crop(
          sim_pu_raster, terra::ext(sf::st_bbox(sim_pu_polygons[1, ]))
        )
      ),
      raster::raster(sim_pu_raster)
    )
  )
  expect_tidy_error(
    assert(
      is_spatial_extents_overlap(
        raster::raster(
          terra::crop(
            sim_pu_raster,
            terra::ext(sf::st_bbox(sim_pu_polygons[1, ]))
          )
        ),
        raster::raster(
          terra::crop(
            sim_pu_raster,
            terra::ext(sf::st_bbox(sim_pu_polygons[7, ]))
          )
        )
      )
    ),
    "overlapping extents",
    name = NULL
  )
})
