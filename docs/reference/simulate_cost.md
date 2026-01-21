# Simulate cost data

Generates simulated cost data using Gaussian random fields.

## Usage

``` r
simulate_cost(x, n, intensity, sd, scale)

# S3 method for class 'Raster'
simulate_cost(x, n = 1, intensity = 100, sd = 20, scale = 2.5)

# S3 method for class 'SpatRaster'
simulate_cost(x, n = 1, intensity = 100, sd = 20, scale = 2.5)
```

## Arguments

- x:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object to use as a template.

- n:

  `integer` number of layers to simulate. Defaults to 1.

- intensity:

  `numeric` average value of simulated data. Defaults to 100.

- sd:

  `numeric` standard deviation of simulated data. Defaults to 20.

- scale:

  `numeric` parameter to control level of spatial auto-correlation in
  the simulated data. Defaults to 2.5.

## Value

A
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object with integer values greater than zero.

## See also

Other functions for simulating data:
[`simulate_data()`](https://prioritizr.net/reference/simulate_data.md),
[`simulate_species()`](https://prioritizr.net/reference/simulate_species.md)

## Examples

``` r
# \dontrun{
# create raster
r <- terra::rast(
  ncols = 10, nrows = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vals = 1
)

# simulate data
cost <- simulate_cost(r)

# plot simulated species
plot(cost, main = "simulated cost data", axes = FALSE)

# }
```
