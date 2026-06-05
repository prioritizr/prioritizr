# Management zones

Organize data for multiple features for multiple management zones.
Specifically, the data should describe the expected amount of each
feature within each planning unit given each management zone. For
example, the data could describe the occupancy (e.g., presence/absence),
probability of occurrence, or abundance expected for each feature when
each planning unit is allocated to a different zone.

## Usage

``` r
zones(..., zone_names = NULL, feature_names = NULL)
```

## Arguments

- ...:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or `character` objects that pertain to the feature data. See Details
  for more information.

- zone_names:

  `character` names of the management zones. Defaults to `NULL` such
  that zones are assigned names according in sequential integers.

- feature_names:

  `character` names of the features. Defaults to `NULL` such that
  features are assigned names according in sequential integers.

## Value

A `Zones` object containing data for each zone, as well as the names of
the features and zones.

## Details

This function is used to store and organize data for use in a
conservation planning
[`problem()`](https://prioritizr.net/reference/problem.md) that has
multiple management zones. In particular, the data for each zone should
be specified as a separate argument. The correct arguments depends on
the type of planning unit data used when building the conservation
planning [`problem()`](https://prioritizr.net/reference/problem.md).

- [`problem()`](https://prioritizr.net/reference/problem.md) will have
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  planning units:

  Here
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  objects can be specified to specify the expected amount of each
  feature within each planning unit under each management zone. Data for
  each zone should be specified as separate arguments, and the data for
  each feature in a given zone are specified in separate layers in a
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object. Note that all layers for a given zone must have missing (`NA`)
  values in exactly the same cells.

- [`problem()`](https://prioritizr.net/reference/problem.md) will have
  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html) or
  `data.frame` planning units:

  Here `character` vectors containing column names can be used to
  specify the expected amount of each feature under each zone. Note that
  these columns must not contain any missing (`NA`) values.

## See also

See [`problem()`](https://prioritizr.net/reference/problem.md) for
information on using this function to generate a prioritization with
multiple management zones.

## Examples

``` r
# load planning unit data
sim_pu_raster <- get_sim_pu_raster()

# simulate distributions for three species under two management zones
zone_1 <- simulate_species(sim_pu_raster, 3)
zone_2 <- simulate_species(sim_pu_raster, 3)

# create zones using two SpatRaster objects
# each object corresponds to a different zone and each layer corresponds to
# a different species
z <- zones(
  zone_1, zone_2,
  zone_names = c("zone_1", "zone_2"),
  feature_names = c("feature_1", "feature_2", "feature_3")
)
print(z)
#> A zones object <ZonesSpatRaster/Zones>
#> • zones:    "zone_1" and "zone_2" (2 total)
#> • features: "feature_1", "feature_2", and "feature_3" (3 total)

# plot the rasters for the first zone in the Zones object
plot(
 z[[1]],
 main = c("Zone 1 feature 1", "Zone 1 feature 2", "Zone 1 feature 3")
)


# note that the do.call function can also be used to create a Zones object
# this method for creating a Zones object can be helpful when there are many
# management zones
l <- list(
  zone_1, zone_2,
  zone_names = c("zone_1", "zone_2"),
  feature_names = c("feature_1", "feature_2", "feature_3")
)
z <- do.call(zones, l)
print(z)
#> A zones object <ZonesSpatRaster/Zones>
#> • zones:    "zone_1" and "zone_2" (2 total)
#> • features: "feature_1", "feature_2", and "feature_3" (3 total)

# create zones using character vectors corresponding to column names
# of a data.frame or Spatial object that contain the amount
# of each species expected different management zones
z <- zones(
  c("spp1_zone1", "spp2_zone1"),
  c("spp1_zone2", "spp2_zone2"),
  c("spp1_zone3", "spp2_zone3"),
  zone_names = c("zone1", "zone2", "zone3"),
  feature_names = c("spp1", "spp2")
)
print(z)
#> A zones object <ZonesCharacter/Zones>
#> • zones:    "zone1", "zone2", and "zone3" (3 total)
#> • features: "spp1" and "spp2" (2 total)
```
