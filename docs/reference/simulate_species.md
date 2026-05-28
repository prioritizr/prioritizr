# Simulate species habitat suitability data

Generates simulated species data using Gaussian random fields.

## Usage

``` r
simulate_species(x, n, scale)

# S3 method for class 'Raster'
simulate_species(x, n = 1, scale = 0.5)

# S3 method for class 'SpatRaster'
simulate_species(x, n = 1, scale = 0.5)
```

## Arguments

- x:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object to use as a template.

- n:

  `integer` value denoting the number of layers to simulate. Defaults to
  1.

- scale:

  `numeric` value denoting the level of spatial auto-correlation in the
  simulated data. Defaults to 0.5.

## Value

A
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object with values between zero and one.

## See also

Other functions for simulating data:
[`simulate_cost()`](https://prioritizr.net/reference/simulate_cost.md),
[`simulate_data()`](https://prioritizr.net/reference/simulate_data.md)

## Examples

``` r
# \dontrun{
# create raster
r <- terra::rast(
  ncols = 10, nrows = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vals = 1
)

# simulate data for 4 species
spp <- simulate_species(r, 4)

# plot simulated species
plot(spp, main = "simulated species distributions", axes = FALSE)

# }
```
