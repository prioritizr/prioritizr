# Simulate data

Simulate spatially auto-correlated data using Gaussian random fields.

## Usage

``` r
simulate_data(x, n, scale, intensity, sd, transform)

# S3 method for class 'Raster'
simulate_data(
  x,
  n = 1,
  scale = 0.5,
  intensity = 0,
  sd = 1,
  transform = identity
)

# S3 method for class 'SpatRaster'
simulate_data(
  x,
  n = 1,
  scale = 0.5,
  intensity = 0,
  sd = 1,
  transform = identity
)
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

- intensity:

  `numeric` value denoting the average value of simulated data. Defaults
  to 0.

- sd:

  `numeric` value denoting the standard deviation of simulated data.
  Defaults to 1.

- transform:

  `function` to transform simulated data. Defaults to the
  [`identity()`](https://rdrr.io/r/base/identity.html) function such
  that values remain the same after simulation.

## Value

A
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object.

## See also

Other functions for simulating data:
[`simulate_cost()`](https://prioritizr.net/reference/simulate_cost.md),
[`simulate_species()`](https://prioritizr.net/reference/simulate_species.md)

## Examples

``` r
# \dontrun{
# create raster
r <- terra::rast(
  ncols = 10, nrows = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vals = 1
)

# simulate data using a Gaussian field
x <- simulate_data(r, n = 1, scale = 0.2)

# plot simulated data
plot(x, main = "simulated data", axes = FALSE)

# }
```
