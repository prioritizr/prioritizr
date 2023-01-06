context("fast_extract")

# generate data
sim_features <- get_sim_features()
  sim_pu_polygons <- get_sim_pu_polygons()
# make points
p1 <- sf::st_point(c(0.1, 0.1))
p2 <- sf::st_point(c(0.9, 0.9))
p_sfc <- sf::st_sfc(list(p1, p2))
p_sf <- sf::st_sf(p_sfc)
# make lines
ls1 = matrix(c(0.1, 0.1, 0.9, 0.9), byrow = TRUE, nrow = 2) %>% 
  sf::st_linestring()
ls2 = matrix(c(0.1, 0.9, 0.9, 0.1), byrow = TRUE, nrow = 2) %>% 
  sf::st_linestring(pts)
ls_sfc <- sf::st_sfc(list(ls1, ls2))
ls_sf <- sf::st_sf(ls_sfc)
# mixture
p_mix <- sf::st_sfc(list(p1, ls1, sf::st_geometry(sim_pu_polygons)[[1]]))
p_mix_sf <- sf::st_sf(p_mix)

test_that("x=Raster,y=SpatialPolygons", {
  # works as intended
  fe <- fast_extract(sim_features, sim_pu_polygons)
  expect_equal(nrow(fe), nrow(sim_pu_polygons))
  expect_equal(ncol(fe), terra::nlyr(sim_features))
  # sp to sf conversion doesn't affect results
  expect_identical(fe, fast_extract(sim_features, sim_pu_polygons))
})

test_that("x=Raster,y=SpatialPoints", {
  # works as intended
  fe <- fast_extract(sim_features, as_Spatial(p_sf))
  expect_equal(nrow(fe), nrow(p_sf))
  expect_equal(ncol(fe), terra::nlyr(sim_features))
  # sp to sf conversion doesn't affect results
  expect_identical(fe, fast_extract(sim_features, p_sf))
})

test_that("x=Raster,y=SpatialLines", {
  # works as intended
  fe <- fast_extract(sim_features, as_Spatial(ls_sf))
  expect_equal(nrow(fe), nrow(p_sf))
  expect_equal(ncol(fe), terra::nlyr(sim_features))
  # sp to sf conversion doesn't affect results
  expect_identical(fe, fast_extract(sim_features, ls_sf))
})

test_that("x=Raster,y=sfc", {
  # works as intended
  # points
  fe <- fast_extract(sim_features, p_sfc)
  expect_equal(nrow(fe), length(p_sfc))
  expect_equal(ncol(fe), terra::nlyr(sim_features))
  # lines
  fe <- fast_extract(sim_features, ls_sfc)
  expect_equal(nrow(fe), length(ls_sfc))
  expect_equal(ncol(fe), terra::nlyr(sim_features))
  # polygons
  fe <- fast_extract(sim_features, sf::st_geometry(sim_pu_polygons))
  expect_equal(nrow(fe), nrow(sim_pu_polygons))
  expect_equal(ncol(fe), terra::nlyr(sim_features))
  # mixture
  fe <- fast_extract(sim_features, p_mix)
  expect_equal(nrow(fe), length(p_mix))
  expect_equal(ncol(fe), terra::nlyr(sim_features))
})

test_that("x=Raster,y=sf", {
  # works as intended
  # points
  fe <- fast_extract(sim_features, p_sf)
  expect_equal(nrow(fe), nrow(p_sf))
  expect_equal(ncol(fe), terra::nlyr(sim_features))
  # lines
  fe <- fast_extract(sim_features, ls_sf)
  expect_equal(nrow(fe), nrow(ls_sf))
  expect_equal(ncol(fe), terra::nlyr(sim_features))
  # polygons
  fe <- fast_extract(sim_features, sim_pu_polygons)
  expect_equal(nrow(fe), nrow(sim_pu_polygons))
  expect_equal(ncol(fe), terra::nlyr(sim_features))
  # mixture
  fe <- fast_extract(sim_features, p_mix_sf)
  expect_equal(nrow(fe), nrow(p_mix_sf))
  expect_equal(ncol(fe), terra::nlyr(sim_features))
})

test_that("summary functions", {
  # polygon for whole area
  bb <- sf::st_bbox(sim_features) %>% 
    sf::st_as_sfc()
  # check mean
  fe_mean <- fast_extract(sim_features, bb, fun = "mean")[1, , drop = TRUE]
  cs_mean <- raster::cellStats(sim_features, stat = mean) %>% 
    unname()
  expect_equal(fe_mean, cs_mean)
  # check sum
  fe_sum <- fast_extract(sim_features, bb, fun = "sum")[1, , drop = TRUE]
  cs_sum <- raster::cellStats(sim_features, stat = sum) %>% 
    unname()
  expect_equal(fe_sum, cs_sum)
})

test_that("invalid inputs", {
  # bad functions
  expect_error(fast_extract(sim_features, sim_pu_polygons, fun = mean))
  expect_error(fast_extract(sim_features, sim_pu_polygons, fun = "median"))
  expect_error(fast_extract(sim_features, sim_pu_polygons, fun = c("mean", "sum")))
  # incorrect spatial data format
  expect_error(fast_extract(sim_pu_polygons, sim_pu_polygons))
  expect_error(fast_extract(sim_features, sim_features))
  # out of extent
  bad_pt <- sf::st_point(c(10, 10)) %>% 
    sf::st_sfc()
  expect_error(fast_extract(sim_features, bad_pt))
  # wrong crs
  bad_crs <- sim_pu_polygons
  sf::st_crs(bad_crs) <- 4326
  expect_error(fast_extract(sim_features, bad_crs))
})

test_that("process in memory/on disk", {
  # in memory
  in_mem <- fast_extract(sim_features, sim_pu_polygons)
  # on disk
  options(rasterToDisk = TRUE)
  on_disk <- fast_extract(sim_features, sim_pu_polygons)
  options(rasterToDisk = NULL)
  expect_equal(round(in_mem, 5), round(on_disk, 5))
})
