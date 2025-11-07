# Initialization
## load packages
library(terra)
library(sf)
library(aoh)
library(terra)
library(sfheaders)
library(virtualspecies)

## set rng
set.seed(500)

## define parameters
n_spp <- 100
country_names <- c("portugal", "spain")
data_res <- 5000

## define functions
run <- function() source("inst/scripts/sim_complex_data.R")

sample_patches <- function(x, y, prop) {
  # calculate spatial extent of each patch
  patch_data <-
    terra::freq(x) %>%
    dplyr::left_join(
      terra::zonal(y, x, "mean", na.rm = TRUE) %>%
      setNames(c("value", "cost")),
      by = "value"
    ) %>%
    dplyr::mutate(total = sum(count)) %>%
    dplyr::mutate(prop = count / total) %>%
    dplyr::filter(count > 10)
  # sample patches
  curr_prop <- 0
  curr_patches <- c()
  while(
    (length(curr_patches) < nrow(patch_data)) &&
    (curr_prop < prop)
  ) {
    # sample patch
    curr_sel <- sample.int(n = nrow(patch_data), 1, prob = patch_data$cost)
    curr_patches <- c(curr_patches, patch_data$value[[curr_sel]])
    curr_prop <- curr_prop + patch_data$prop[[curr_sel]]
    # remove patch
    patch_data <- patch_data[-curr_sel, , drop = FALSE]
  }
  # return raster data
  terra::mask(terra::as.int(x %in% curr_patches), x)
}

# Preliminary processing
## create persistent data directory
gd_dir <- rappdirs::user_data_dir("geodata")
dir.create(gd_dir, showWarnings = FALSE, recursive = TRUE)

## load coastline data
boundary_data <-
  rnaturalearth::ne_countries(scale = 10, country = country_names) %>%
  sf::st_as_sf() %>%
  sf::st_union() %>%
  sf::st_cast("POLYGON") %>%
  sf::st_as_sf(id = 1) %>%
  dplyr::mutate(area = sf::st_area(.)) %>%
  dplyr::arrange(dplyr::desc(area)) %>%
  dplyr::filter(seq_along(area) == 1) %>%
  dplyr::select(id) %>%
  sf::st_set_precision(1500) %>%
  sf::st_make_valid() %>%
  dplyr::filter(!sf::st_is_empty(.)) %>%
  {suppressWarnings(sf::st_collection_extract(., "POLYGON"))} %>%
  sfheaders::sf_remove_holes() %>%
  sf::st_transform(
    rappdirs::user_data_dir("aoh") %>%
    aoh::get_lumb_cgls_habitat_data() %>%
    terra::crs()
  )

## load crosswalk data
crosswalk_data <-
  aoh::crosswalk_lumb_cgls_data %>%
  dplyr::left_join(aoh::iucn_habitat_data, by = "code") %>%
  dplyr::filter(is_terrestrial, !is_artificial, !is_introduced) %>%
  dplyr::summarize(
    code = dplyr::first(code), name = dplyr::first(name), .by = "value"
  )

## load land cover data
lulc_data <-
  rappdirs::user_data_dir("aoh") %>%
  aoh::get_lumb_cgls_habitat_data() %>%
  terra::crop(boundary_data %>% terra::vect() %>% terra::ext()) %>%
  terra::segregate() %>%
  terra::aggregate(
    fact = data_res / terra::res(.), FUN = "mean", na.rm = TRUE
  ) %>%
  {terra::mask(., terra::rasterize(terra::vect(boundary_data), .))}
lulc_data <- lulc_data[[which(names(lulc_data) %in% crosswalk_data$value)]]

## load climate data
clim_data <-
  geodata::worldclim_global(var = "bio", res = 10, path = gd_dir) %>%
  terra::crop(
    boundary_data  %>%
    sf::st_buffer(10000) %>%
    sf::st_transform(4326) %>%
    terra::ext()
  ) %>%
  terra::project(lulc_data[[1]], method = "bilinear") %>%
  terra::mask(lulc_data[[1]])

## load pressure data
hfp_data <-
  geodata::footprint(year = 2009, path = gd_dir) %>%
  terra::crop(
    boundary_data  %>%
    sf::st_buffer(10000) %>%
    sf::st_transform(4326) %>%
    terra::ext()
  ) %>%
  terra::project(lulc_data[[1]], method = "bilinear") %>%
  terra::mask(lulc_data[[1]])

## standardize data
mask_data <- sum(sum(lulc_data), sum(clim_data), hfp_data)
lulc_data <- terra::mask(lulc_data, mask_data)
clim_data <- terra::mask(clim_data, mask_data)
hfp_data <- terra::mask(hfp_data, mask_data)

# Main processing
## simulate historical feature data
sim_historical_ft_data <-
  seq_len(n_spp * 1.5) %>%
  lapply(function(x) {
    ### generate raster
    r <- virtualspecies::generateRandomSp(
      clim_data, convert.to.PA = FALSE, plot = FALSE
    )$suitab.raster
    ### generate threshold
    prop_threshold <- max(min(1, rnorm(1, 0.2, 0.15)), 0.01)
    quant_threshold <- quantile(
      terra::values(r), probs = prop_threshold, na.rm = TRUE
    )[[1]]
    ## apply threshold
    terra::mask(terra::as.int(r <= quant_threshold), r)
  }) %>%
  lapply(terra::sieve, 4) %>%
  terra::rast()

## simulate current feature data
sim_ft_data <-
  sim_historical_ft_data %>%
  terra::as.list() %>%
  lapply(function(x) {
    ## compute frequency of classes
    class_wts <-
      terra::mask(lulc_data, x, maskvalue = 0, updatevalue = NA_real_) %>%
      terra::global("sum", na.rm = TRUE) %>%
      dplyr::mutate(wt = sum / sum(sum)) %>%
      dplyr::mutate(value = as.numeric(names(lulc_data)))
    ## sample habitat classes from crosswalk data
    n_class <- sample.int(nrow(class_wts), 1)
    curr_classes <- sample(class_wts$value, n_class, prob = class_wts$wt)
    ## create layer with presence/absence of habitat classes
    curr_habitat <- sum(lulc_data[[which(names(lulc_data) %in% curr_classes)]])
    curr_habitat <- terra::as.int(curr_habitat >= 0.75)
    curr_habitat <- terra::mask(curr_habitat, x)
    ## mask historical data
    terra::mask(x, curr_habitat, maskvalue = 0, updatevalue = 0)
  }) %>%
  terra::rast()

## post-processing for features
ft_idx <- which(
  (terra::global(sim_historical_ft_data, "sum", na.rm = TRUE)[[1]] > 0) &
  (terra::global(sim_ft_data, "sum", na.rm = TRUE)[[1]] > 0)
)
ft_idx <- sample(ft_idx, n_spp)
sim_ft_data <- sim_ft_data[[ft_idx]]
sim_historical_ft_data <- sim_historical_ft_data[[ft_idx]]
names(sim_ft_data) <- paste0("feature_", seq_along(ft_idx))
names(sim_historical_ft_data) <- names(sim_ft_data)

## simulate cost data
raw_cost_data <-
  setNames(hfp_data, "cost") %>%
  terra::as.data.frame(cells = TRUE, na.rm = TRUE)
raw_cost_data$cost <- scales::rescale(
  (raw_cost_data$cost + 1)^1.5,
  to = c(0.01, 1000)
)
sim_cost_data <- hfp_data * 0
sim_cost_data[raw_cost_data$cell] <- raw_cost_data$cost

## simulate climatic zone data
raw_zone_data <-
  clim_data %>%
  terra::as.data.frame(cells = TRUE, xy = TRUE) %>%
  dplyr::mutate(dplyr::across(-cell, scale))
zone_cluster_model <-
  kmeans(raw_zone_data %>% dplyr::select(-cell), 1200, iter.max = 1000)
zone_data <- lulc_data[[1]] * 0
zone_data[raw_zone_data$cell] <- zone_cluster_model$cluster

## simulate locked in and locked out data
sim_locked_in_data <-
  sample_patches(zone_data, 1 / sim_cost_data, prop = 0.05) %>%
  terra::sieve(4)
sim_locked_out_data <-
  sample_patches(zone_data, sim_cost_data, prop = 0.05) %>%
  terra::sieve(4)

## sanity checks
assertthat::assert_that(
  all(terra::global(sim_ft_data, "sum", na.rm = TRUE)[[1]] > 0),
  all(terra::global(sim_locked_in_data, "sum", na.rm = TRUE)[[1]] > 0),
  all(terra::global(sim_locked_out_data, "sum", na.rm = TRUE)[[1]] > 0),
  all(
    terra::global(sim_ft_data, "sum", na.rm = TRUE)[[1]] <=
    terra::global(sim_historical_ft_data, "sum", na.rm = TRUE)[[1]]
  )
)

# Exports
terra::writeRaster(
  sim_cost_data, "inst/extdata/sim_complex_pu_raster.tif",
  datatype = "FLT4S",
  gdal = c("COMPRESS=DEFLATE", "TILED=YES", "PREDICTOR=3"),
  overwrite = TRUE
)
terra::writeRaster(
  sim_locked_in_data, "inst/extdata/sim_complex_locked_in_raster.tif",
  datatype = "INT1U", NAflag = 2,
  gdal = c("COMPRESS=DEFLATE", "TILED=YES", "NBITS=2"),
  overwrite = TRUE
)
terra::writeRaster(
  sim_locked_out_data, "inst/extdata/sim_complex_locked_out_raster.tif",
  datatype = "INT1U", NAflag = 2,
  gdal = c("COMPRESS=DEFLATE", "TILED=YES", "NBITS=2"),
  overwrite = TRUE
)
terra::writeRaster(
  sim_ft_data, "inst/extdata/sim_complex_features.tif",
  datatype = "INT1U", NAflag = 2,
  gdal = c("COMPRESS=DEFLATE", "TILED=YES", "NBITS=2"),
  overwrite = TRUE
)
terra::writeRaster(
  sim_historical_ft_data, "inst/extdata/sim_complex_historical_features.tif",
  datatype = "INT1U", NAflag = 2,
  gdal = c("COMPRESS=DEFLATE", "TILED=YES", "NBITS=2"),
  overwrite = TRUE
)
