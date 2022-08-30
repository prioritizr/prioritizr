# Initialization
## load packages
library(sf)
library(ape)
library(raster)
library(magrittr)
library(testthat)
library(dplyr)
source("R/zones.R")

## define helper functions
read_rda <- function(file) {
  tmp <- new.env()
  load(file = paste0("data/", file, ".rda"), envir = tmp)
  tmp[[ls(tmp)[1]]]
}

save_rda <- function(x) {
  save(
    list = x, file = paste0("data/", x, ".rda"),
    compress = "xz", version = 3
  )
  invisible(TRUE)
}

expect_equal_sp <- function(x, y) {
  if (!inherits(y, "SpatialPoints")) {

    spChFIDs(y, as.character(seq_len(nrow(y))))
  }
  x <- sf::st_as_sf(x)
  rownames(x) <- NULL
  coordnames(y) <- c("x", "y")
  y <- sf::st_as_sf(y)
  rownames(y) <- NULL
  expect_equal(x, y)
}

expect_equal_sf <- function(x, y) {
  expect_equal(x, sf::st_as_sf(tibble::as_tibble(y)))
}

expect_equal_raster <- function(x, y) {
  if (raster::nlayers(y) == 1) {
    y <- raster::setValues(y, as.numeric(raster::values(y)))
  }
  expect_equal(names(x), names(y))
  expect_true(raster::compareRaster(x, y, stopiffalse = FALSE))
  expect_equal(
    raster::values(x),
    raster::values(y),
    tolerance = 1e-5
  )
}

expect_equal_zones <- function(x, y) {
  expect_equal(class(x), class(y))
  expect_equal(attr(x, "zone_names"), attr(y, "zone_names"))
  expect_equal(attr(x, "feature_names"), attr(y, "feature_names"))
  for (i in seq_along(x)) {
    expect_equal_raster(x[[i]], y[[i]])
  }
}

# Main processing
## tree data
### sim_phylogeny
sim_phylogeny <-
  "data-raw/sim_phylogeny.txt" %>%
  ape::read.tree()

## sf data
### sim_pu_sf
sim_pu_sf <-
  "data-raw/sim_pu_sf.gpkg" %>%
  sf::read_sf() %>%
  dplyr::rename(geometry = geom) %>%
  sf::st_set_geometry("geometry") %>%
  sf::st_set_crs(sf::st_crs(NA))

## sim_pu_zones_sf
sim_pu_zones_sf <-
  "data-raw/sim_pu_zones_sf.gpkg" %>%
  sf::read_sf() %>%
  dplyr::rename(geometry = geom) %>%
  sf::st_set_geometry("geometry") %>%
  sf::st_set_crs(sf::st_crs(NA))

## raster data
## sim_features
sim_pu_raster <-
  "data-raw/sim_pu_raster.tif" %>%
  raster::raster() %>%
  setNames("layer") %>%
  raster::readAll()

## sim_features
sim_features <-
  "data-raw/sim_features.tif" %>%
  raster::stack() %>%
  setNames(c("layer.1", "layer.2", "layer.3", "layer.4", "layer.5")) %>%
  raster::readAll()

## sim_locked_in_raster
sim_locked_in_raster <-
  "data-raw/sim_locked_in_raster.tif" %>%
  raster::raster() %>%
  setNames("layer") %>%
  raster::readAll()

## sim_locked_out_raster
sim_locked_out_raster <-
  "data-raw/sim_locked_out_raster.tif" %>%
  raster::raster() %>%
  setNames("layer") %>%
  raster::readAll()

## sim_pu_zones_stack
sim_pu_zones_stack <-
  "data-raw/sim_pu_zones_stack.tif" %>%
  raster::stack() %>%
  setNames(c("layer.1", "layer.2", "layer.3")) %>%
  raster::readAll()

## zones data
## sim_features_zones
sim_features_zones <- zones(
  zone_1 =
    "data-raw/sim_features_zones_1.tif" %>%
    raster::stack() %>%
    raster::readAll(),
  zone_2 =
    "data-raw/sim_features_zones_2.tif" %>%
    raster::stack() %>%
    raster::readAll(),
  zone_3 =
    "data-raw/sim_features_zones_3.tif" %>%
    raster::stack() %>%
    raster::readAll(),
  zone_names = c("zone_1", "zone_2", "zone_3"),
  feature_names = c(
    "feature_1", "feature_2", "feature_3", "feature_4", "feature_5"
  )
)

## Spatial data
### sim_pu_points
sim_pu_points <-
  "data-raw/sim_pu_points.gpkg" %>%
  sf::read_sf() %>%
  sf::as_Spatial() %>%
  `proj4string<-`(sp::CRS()) %>%
  `coordnames<-`(c("x", "y"))

### sim_pu_lines
sim_pu_lines <-
  "data-raw/sim_pu_lines.gpkg" %>%
  sf::read_sf() %>%
  sf::as_Spatial() %>%
  `proj4string<-`(sp::CRS()) %>%
  `coordnames<-`(c("x", "y"))

### sim_pu_polygons
sim_pu_polygons <-
  "data-raw/sim_pu_polygons.gpkg" %>%
  sf::read_sf() %>%
  sf::as_Spatial() %>%
  `proj4string<-`(sp::CRS()) %>%
  `coordnames<-`(c("x", "y"))

## sim_pu_zones_polygons
sim_pu_zones_polygons <-
  "data-raw/sim_pu_zones_polygons.gpkg" %>%
  sf::read_sf() %>%
  sf::as_Spatial() %>%
  `proj4string<-`(sp::CRS()) %>%
  `coordnames<-`(c("x", "y"))

# Tests
## objects that should be identical
expect_equal(sim_phylogeny, read_rda("sim_phylogeny"))

## sp objects
expect_equal_sp(sim_pu_points, read_rda("sim_pu_points"))
expect_equal_sp(sim_pu_polygons, read_rda("sim_pu_polygons"))
expect_equal_sp(sim_pu_lines, read_rda("sim_pu_lines"))
expect_equal_sp(sim_pu_zones_polygons, read_rda("sim_pu_zones_polygons"))

## sf objects
expect_equal_sf(sim_pu_sf, read_rda("sim_pu_sf"))
expect_equal_sf(sim_pu_zones_sf, read_rda("sim_pu_zones_sf"))

## raster objects
expect_equal_raster(sim_pu_raster, read_rda("sim_pu_raster"))
expect_equal_raster(sim_pu_zones_stack, read_rda("sim_pu_zones_stack"))
expect_equal_raster(sim_features, read_rda("sim_features"))
expect_equal_raster(sim_locked_in_raster, read_rda("sim_locked_in_raster"))
expect_equal_raster(sim_locked_out_raster, read_rda("sim_locked_out_raster"))

## zones objects
expect_equal_zones(sim_features_zones, read_rda("sim_features_zones"))

# Export
## sim_phylogeny
save_rda("sim_phylogeny")

## sim_pu_sf
save_rda("sim_pu_sf")

## sim_pu_zones_sf
save_rda("sim_pu_zones_sf")

## sim_pu_raster
save_rda("sim_pu_raster")

## sim_features
save_rda("sim_features")

## sim_locked_in_raster
save_rda("sim_locked_in_raster")

## sim_locked_out_raster
save_rda("sim_locked_out_raster")

## sim_pu_zones_stack
save_rda("sim_pu_zones_stack")

## sim_features_zones
save_rda("sim_features_zones")

## sim_pu_points
save_rda("sim_pu_points")

## sim_pu_lines
save_rda("sim_pu_lines")

## sim_pu_polygons
save_rda("sim_pu_polygons")

## sim_pu_zones_polygons
save_rda("sim_pu_zones_polygons")
